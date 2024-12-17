(** mforth.ml v1.0.0
    build with: ocamlfind ocamlopt -linkpkg -package base,stdio -o mforth mforth.ml
    Based off of the sector forth implementation: https://github.com/cesarblum/sectorforth

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

  let size (stack : t) : int = List.length stack.items

  let print (stack : t) : unit =
    if size stack <= 0
    then print_endline "Stack is empty"
    else (
      let rec print_items idx s =
        match s with
        | [] -> ()
        | item :: rest ->
          (match item with
           | Int x -> print_endline ("int " ^ Int.to_string x)
           | Float x -> print_endline ("float " ^ Float.to_string x)
           | Char x -> print_endline ("char " ^ String.make 1 x)
           | String x -> print_endline ("string " ^ x)
           | Bool x -> print_endline ("bool " ^ Bool.to_string x));
          print_items (idx + 1) rest
      in
      print_items 0 stack.items)
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
  type data_areas =
    { memory : Memory.t
    ; data_stack : Stack.t
    ; return_stack : Stack.t
    }

  type t = { data : data_areas }

  let init_data () =
    { memory = Memory.empty; data_stack = Stack.empty; return_stack = Stack.empty }
  ;;
end

type state =
  | Execute
  | Compile

type func_record =
  { name : string
  ; tokens : string list
  }

let parse_line (line : string) ~(d : DataState.data_areas) =
  let module S = Stack in
  match line with
  | ".s" ->
    S.print d.data_stack;
    d
  | ".r" ->
    S.print d.return_stack;
    d
  | item ->
    (match S.wrap_stack_item (String.strip item) with
     | Some stack_item ->
       let new_stack = S.push stack_item ~stack:d.data_stack in
       let new_data_state = { d with data_stack = new_stack } in
       new_data_state
     | None ->
       print_endline ("unknown command: " ^ item);
       d)
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
