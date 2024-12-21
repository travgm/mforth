(** mforth.ml v1.0.0

    build with: ocamlfind ocamlopt -linkpkg -package base,stdio,ppx_let -o mforth mforth.ml

    Note:
    ❯ ocamlopt --version
      5.2.0+jst

    minimaForth (C) Copyright 2024 Travis Montoya *)

open Base
open Stdio
open Option.Let_syntax

let version = "minimaForth 1.0.0"

let system_version =
  let module SY = Stdlib.Sys in
  let os = SY.os_type in
  let arch = SY.word_size in
  let platform = SY.ocaml_version in
  Printf.sprintf "%s running on %s/%d-bit, OCaml version: %s" version os arch platform
;;

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

  let drop ~(d : t) : t =
    match pop d with
    | None -> d
    | Some (_, s) -> s
  ;;

  let _2drop ~(d : t) : t =
    match pop d with
    | None -> d
    | Some (_, s1) ->
      (match pop s1 with
       | None -> d
       | Some (_, s2) -> s2)
  ;;

  let dup ~(d : t) : t =
    match peek d with
    | None -> d
    | Some x -> push x ~stack:d
  ;;

  let _2dup ~(d : t) : t =
    match pop d with
    | None -> d
    | Some (x, s1) ->
      (match pop s1 with
       | None -> d
       | Some (y, s2) -> push x ~stack:(push y ~stack:(push x ~stack:s1)))
  ;;

  let swap ~(d : t) : t =
    match pop d with
    | None -> d
    | Some (x, s1) ->
      (match pop s1 with
       | None -> d
       | Some (y, s2) -> push y ~stack:(push x ~stack:s2))
  ;;

  let _2swap ~(d : t) : t =
    match pop d with
    | None -> d
    | Some (w, s1) ->
      (match pop s1 with
       | None -> d
       | Some (x, s2) ->
         (match pop s2 with
          | None -> d
          | Some (y, s3) ->
            (match pop s3 with
             | None -> d
             | Some (z, s4) ->
               push x ~stack:(push w ~stack:(push z ~stack:(push y ~stack:s4))))))
  ;;

  let over ~(d : t) : t =
    match pop d with
    | None -> d
    | Some (x, s1) ->
      (match pop s1 with
       | None -> d
       | Some (y, s2) -> push x ~stack:(push y ~stack:(push x ~stack:s2)))
  ;;

  (* This handles both ( a b c -- b c a ) and ( a b c -- c a b ) depending if the reverse rotate
     flag (~r) is set *)
  let rot ~(d : t) ~(r : bool) : t =
    match pop d with
    | None -> d
    | Some (x, s1) ->
      (match pop s1 with
       | None -> d
       | Some (y, s2) ->
         (match pop s2 with
          | None -> d
          | Some (z, s3) ->
            if r
            then push y ~stack:(push x ~stack:(push z ~stack:s3))
            else push z ~stack:(push x ~stack:(push y ~stack:s3))))
  ;;

  let depth ~(d : t) : t =
    if is_empty d
    then d
    else (
      let item = wrap_stack_item (Int.to_string (size d)) in
      match item with
      | Some (Int x) -> push (Int x) ~stack:d
      | _ -> d)
  ;;
end

module DataState = struct
  module StringMap = Map.M (String)

  type data_areas =
    { memory : Memory.t
    ; data_stack : Stack.t
    ; return_stack : Stack.t
    ; dictionary : string list StringMap.t list
    }

  type t = { data : data_areas }

  let init_data () =
    { memory = Memory.empty
    ; data_stack = Stack.empty
    ; return_stack = Stack.empty
    ; dictionary = [ Map.empty (module String) ]
    }
  ;;

  let dict_item_exists ~(f : string) ~(d : data_areas) : bool =
    List.exists d.dictionary ~f:(fun dict -> Map.mem dict f)
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
    | "mod" -> Int.rem, Float.mod_float
    | _ -> ( + ), ( +. )
  in
  match S.pop d.data_stack with
  | None -> d
  | Some (x, stack1) ->
    (match S.pop stack1 with
     | None -> d
     | Some (y, stack2) ->
       let int_op, float_op = apply_op () in
       if String.equal op "/"
          &&
          match x with
          | S.Int x -> x = 0
          | S.Float x -> Float.equal x 0.0
          | _ -> false
       then (
         let new_stack = S.push (S.Int 0) ~stack:stack2 in
         { d with data_stack = new_stack })
       else (
         let result =
           match y, x with
           | S.Int y, S.Int x -> S.Int (int_op y x)
           | S.Int y, S.Float x -> S.Float (float_op (Float.of_int y) x)
           | S.Float y, S.Int x -> S.Float (float_op y (Float.of_int x))
           | S.Float y, S.Float x -> S.Float (float_op y x)
           | _ ->
             print_endline "Invalid operation, expecting a numeric";
             S.Int 0
         in
         let new_stack = S.push result ~stack:stack2 in
         { d with data_stack = new_stack }))
;;

let try_parse_function (line : string) : func_record option =
  let module S = String in
  if S.is_prefix line ~prefix:":"
  then (
    let line = S.drop_prefix line 1 |> S.strip in
    let rec tokenize acc current_str in_quotes chars =
      match chars with
      | [] ->
        let final_tokens = if S.length current_str > 0 then current_str :: acc else acc in
        List.rev final_tokens
      | '"' :: rest ->
        if in_quotes
        then tokenize (("\"" ^ current_str ^ "\"") :: acc) "" false rest
        else tokenize acc current_str true rest
      | ' ' :: rest when not in_quotes ->
        if S.length current_str > 0
        then tokenize (current_str :: acc) "" false rest
        else tokenize acc "" false rest
      | c :: rest -> tokenize acc (current_str ^ String.make 1 c) in_quotes rest
    in
    let tokens = tokenize [] "" false (S.to_list line) in
    match tokens with
    | name :: rest ->
      let body = List.take_while rest ~f:(fun x -> not (S.equal x ";")) in
      Some { name; tokens = body }
    | _ -> None)
  else None
;;

let try_parse_bool line =
  let module ST = Stack in
  match line with
  | ("true" | "false") as bool_type -> Some (ST.Bool (String.equal bool_type "true"))
  | _ -> None
;;

let try_parse_string line =
  let module ST = Stack in
  let module S = String in
  if S.is_prefix line ~prefix:"\"" && S.is_suffix line ~suffix:"\""
  then Some (ST.String (S.sub line ~pos:1 ~len:(S.length line - 2)))
  else None
;;

let try_parse_number line =
  let module ST = Stack in
  match Int.of_string_opt line with
  | Some n -> Some (ST.Int n)
  | None ->
    let%bind f = Float.of_string_opt line in
    Some (ST.Float f)
;;

(* Main line parsers and REPL *)

let cleanup_line_comment (line : string) : string =
  let module S = String in
  let cleaned = S.strip line in
  if S.is_prefix cleaned ~prefix:"(" && S.is_suffix cleaned ~suffix:")"
  then ""
  else (
    match S.index cleaned '(' with
    | Some idx -> S.sub cleaned ~pos:0 ~len:idx |> S.strip
    | None -> cleaned)
;;

(* This parse_* needs to be refactored to helper functions and soon probably move this
    to modules. *)
let rec parse_non_builtin (line : string) ~(d : DataState.data_areas) =
  let module ST = Stack in
  let return_new_stack item = { d with data_stack = ST.push item ~stack:d.data_stack } in
  match
    Option.first_some
      (try_parse_bool line)
      (Option.first_some (try_parse_string line) (try_parse_number line))
  with
  | Some value -> return_new_stack value
  | None ->
    if DataState.dict_item_exists ~f:line ~d
    then check_and_execute_function ~f:line d
    else (
      match try_parse_function line with
      | Some x ->
        (match d.dictionary with
         | first_dict :: rest ->
           let new_dict = Map.set first_dict ~key:x.name ~data:x.tokens in
           { d with dictionary = new_dict :: rest }
         | [] ->
           let new_dict = Map.empty (module String) in
           let new_dict = Map.set new_dict ~key:x.name ~data:x.tokens in
           { d with dictionary = [ new_dict ] })
      | None ->
        print_endline ("unknown command: " ^ line);
        d)

and check_and_execute_function ~(f : string) (d : DataState.data_areas) =
  match Map.find (List.hd_exn d.dictionary) f with
  | Some tokens -> List.fold tokens ~init:d ~f:(fun acc token -> parse_line token ~d:acc)
  | None -> d

and parse_line (line : string) ~(d : DataState.data_areas) =
  let module S = String in
  let module ST = Stack in
  let return_new_stack ~(s : Stack.t) = { d with data_stack = s } in
  let cleanup_line_comment = cleanup_line_comment line in
  if S.length cleanup_line_comment = 0
  then d
  else (
    match cleanup_line_comment with
    | ".s" ->
      ST.print d.data_stack;
      d
    | ".r" ->
      ST.print d.return_stack;
      d
    | "." ->
      if ST.is_empty d.data_stack
      then (
        print_endline "Stack underflow";
        d)
      else (
        let new_stack = ST.pop_and_print d.data_stack in
        { d with data_stack = new_stack })
    | ".ver" ->
      system_version |> print_endline;
      d
    (* Arithmetic operations *)
    | ("+" | "-" | "*" | "/" | "mod") as op -> eval_stack_op d ~op
    (* Stack operations *)
    | "drop" ->
      let new_stack = ST.drop ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "2drop" ->
      let new_stack = ST._2drop ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "dup" ->
      let new_stack = ST.dup ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "2dup" ->
      let new_stack = ST._2dup ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "swap" ->
      let new_stack = ST.swap ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "2swap" ->
      let new_stack = ST._2swap ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "over" ->
      let new_stack = ST.swap ~d:d.data_stack in
      return_new_stack ~s:new_stack
    | "rot" ->
      let new_stack = ST.rot ~d:d.data_stack ~r:false in
      return_new_stack ~s:new_stack
    | "-rot" ->
      let new_stack = ST.rot ~d:d.data_stack ~r:true in
      return_new_stack ~s:new_stack
    | "depth" ->
      let new_stack = ST.depth ~d:d.data_stack in
      return_new_stack ~s:new_stack
    (* Anything that isn't a builtin is then parsed in parse_non_builtin *)
    | item -> parse_non_builtin line ~d)
;;

let () =
  let module S = String in
  let module D = DataState in
  print_endline (system_version ^ "\n");
  let data_state = D.init_data () in
  let rec get_repl_line (d : D.data_areas) () =
    Out_channel.flush stdout;
    let line = In_channel.input_line_exn stdin in
    match line with
    | "bye" -> Stdlib.exit 0
    | line ->
      (* Most of this needs to be moved to helpers for parse_line and then parse_line 
           should be cleaned up *)
      let comments_removed = cleanup_line_comment line in
      if S.length comments_removed = 0
      then get_repl_line d ()
      else if S.is_prefix comments_removed ~prefix:":"
      then (
        let new_data_state = parse_line comments_removed ~d in
        get_repl_line new_data_state ())
      else (
        let words = S.split_on_chars comments_removed ~on:[ ' '; '\t'; '\n' ] in
        let new_data_state = List.fold words ~init:d ~f:(fun s w -> parse_line w ~d:s) in
        get_repl_line new_data_state ())
  in
  get_repl_line data_state ()
;;
