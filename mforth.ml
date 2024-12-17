(** mforth.ml v1.0.0

    build with: ocamlfind ocamlopt -linkpkg -package base,stdio -o mforth mforth.ml

    Note:
    ❯ ocamlopt --version
      5.2.0+jst

    minimaForth (C) Copyright 2024 Travis Montoya *)
open Base

open Stdio

let version = "1.0.0"

module Memory = struct
  type memory_item =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | Bool of bool
    | Zero

  type t = { items : memory_item list }

  let empty : t = { items = [] }
  let init (local_ (size : int)) = List.init size ~f:(fun _ -> Zero)

  let is_out_of_memory (memory : t) : bool =
    not
      (List.exists
         ~f:(fun x ->
           match x with
           | Zero -> true
           | _ -> false)
         memory.items)
  ;;

  let next_available_space (memory : t) : int option =
    List.find_mapi
      ~f:(fun i x ->
        match x with
        | Zero -> Some i
        | _ -> None)
      memory.items
  ;;

  let get (local_ (addr : int)) ~(memory : t) memory_item option =
    if addr < List.length memory.items then Some (List.nth memory.items addr) else None
  ;;

  let store (item : memory_item) ~(local_ addr : int) ~(memory : t) : t =
    if (not (is_out_of_memory memory)) && addr <= List.length memory.items
    then (
      let beg, tail = List.split_n memory.items addr in
      { items = beg @ [ item ] @ tail })
    else empty
  ;;
end

module Stack = struct
  type stack_item =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | Bool of bool

  type t = { items : stack_item list }

  let empty : t = { items = [] }
  let push (x : stack_item) ~(stack : t) : t = { items = x :: stack.items }

  let pop (stack : t) : (stack_item * t) option =
    match stack.items with
    | [] -> None
    | x :: rest -> Some (x, { items = rest })
  ;;

  let peek (stack : t) : stack_item option =
    match stack.items with
    | [] -> None
    | x :: _ -> Some x
  ;;

  let is_empty (stack : t) : bool = List.is_empty stack.items
  let size (stack : t) : int = List.length stack.items

  let print (stack : t) : unit =
    if size stack <= 0
    then print_endline "Stack is empty"
    else (
      let stack_depth = size stack in
      print_endline ("Stack depth: " ^ Int.to_string stack_depth);
      let rec print_items idx s =
        match s with
        | [] -> ()
        | item :: rest ->
          (match item with
           | Int x -> print_endline (Int.to_string idx ^ ": int " ^ Int.to_string x)
           | Float x -> print_endline (Int.to_string idx ^ ": float " ^ Float.to_string x)
           | Char x -> print_endline (Int.to_string idx ^ ": char " ^ String.make 1 x)
           | String x -> print_endline (Int.to_string idx ^ ": string " ^ x)
           | Bool x -> print_endline (Int.to_string idx ^ ": bool " ^ Bool.to_string x));
          print_items (idx + 1) rest
      in
      print_items 0 stack.items)
  ;;

  let pop_and_print (stack : t) =
    match pop stack with
    | None -> stack
    | Some (x, stack1) ->
      (match x with
       | Int x -> print_endline (Int.to_string x)
       | Float x -> print_endline (Float.to_string x)
       | Char x -> print_endline (String.make 1 x)
       | String x -> print_endline x
       | Bool x -> print_endline (Bool.to_string x));
      stack1
  ;;

  let wrap_stack_item (item : string) : stack_item option =
    match Int.of_string_opt item with
    | Some n -> Some (Int n)
    | None ->
      (match Float.of_string_opt item with
       | Some f -> Some (Float f)
       | None ->
         if String.length item = 1
         then Some (Char (String.get item 0))
         else (
           match String.lowercase item with
           | "true" -> Some (Bool true)
           | "false" -> Some (Bool false)
           | s -> Some (String s)))
  ;;
end

module DataState = struct
  module StringMap = Map.M (String)

  type data_areas =
    { memory : Memory.t
    ; data_stack : Stack.t
    ; return_stack : Stack.t
    ; dictionary : string StringMap.t
    }

  type t = { data : data_areas }

  let init_data () =
    { memory = Memory.empty
    ; data_stack = Stack.empty
    ; return_stack = Stack.empty
    ; dictionary = Map.empty (module String)
    }
  ;;
end

type state =
  | Execute
  | Compile

type func_record =
  { name : string
  ; tokens : string list
  }

let eval_stack_op (d : DataState.data_areas) ~(op : string) : DataState.data_areas =
  let module S = Stack in
  let apply_op () =
    match op with
    | "+" -> ( + ), ( +. )
    | "-" -> ( - ), ( -. )
    | "*" -> ( * ), ( *. )
    | "/" -> ( / ), ( /. )
    | _ -> ( + ), ( +. )
  in
  match S.pop d.data_stack with
  | None -> d
  | Some (x, stack1) ->
    (match S.pop stack1 with
     | None -> d
     | Some (y, stack2) ->
       let int_op, float_op = apply_op () in
       let result =
         match x, y with
         | S.Int x, S.Int y -> S.Int (int_op x y)
         | S.Int x, S.Float y -> S.Float (float_op (Float.of_int x) y)
         | S.Float x, S.Int y -> S.Float (float_op x (Float.of_int y))
         | S.Float x, S.Float y -> S.Float (float_op x y)
         | _ ->
           print_endline "Invalid operation, expecting a numeric";
           S.Int 0
       in
       let new_stack = S.push result ~stack:stack2 in
       { d with data_stack = new_stack })
;;

let parse_function (line : string) : func_record option =
  let words = String.split_on_chars line ~on:[ ' ' ] in
  match words with
  | [] -> None
  | name :: tokens -> Some { name; tokens }
;;

(* Main line parser and REPL *)

let parse_non_builtin (line : string) ~(d : DataState.data_areas) =
  let module S = Stack in
  match line with
  | item ->
    if String.is_prefix item ~prefix:"\"" && String.is_suffix item ~suffix:"\""
    then (
      let cleaned_str = String.sub item ~pos:1 ~len:(String.length item - 2) in
      let new_stack = S.push (S.String cleaned_str) ~stack:d.data_stack in
      { d with data_stack = new_stack })
    else (
      match Int.of_string_opt item with
      | Some n ->
        let new_stack = S.push (S.Int n) ~stack:d.data_stack in
        { d with data_stack = new_stack }
      | None ->
        (match Float.of_string_opt item with
         | Some f ->
           let new_stack = S.push (S.Float f) ~stack:d.data_stack in
           { d with data_stack = new_stack }
         | None ->
           (* This is probably where we should handle testing if its a function to execute *)
           print_endline ("unknown command: " ^ item);
           d))
;;

let parse_line (line : string) ~(d : DataState.data_areas) =
  let module S = Stack in
  match line with
  (* Built-ins NOT built-in functions are compared here compared to dictionary functions or built-in functions *)
  | ".s" ->
    S.print d.data_stack;
    d
  | ".r" ->
    S.print d.return_stack;
    d
  | "." ->
    if S.is_empty d.data_stack
    then (
      print_endline "Stack underflow";
      d)
    else (
      let new_stack = S.pop_and_print d.data_stack in
      { d with data_stack = new_stack })
  | ("+" | "-" | "*" | "/") as op -> eval_stack_op d ~op
  | item -> parse_non_builtin line ~d:d
;;

let () =
  print_endline ("minimaForth " ^ version);
  let data_state = DataState.init_data () in
  let rec get_repl_line (d : DataState.data_areas) () =
    Out_channel.flush stdout;
    print_endline "";
    let line = In_channel.input_line_exn stdin in
    match line with
    | "bye" -> Stdlib.exit 0
    | line ->
      let new_data_state = parse_line line ~d in
      get_repl_line new_data_state ()
  in
  get_repl_line data_state ()
;;
