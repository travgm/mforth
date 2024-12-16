(** mforth.ml v1.0.0
    build with: ocamlfind ocamlopt -linkpkg -package stdio -o mforth mforth.ml
    Based off of the sector forth implementation: https://github.com/cesarblum/sectorforth

    minimaForth (C) Copyright 2024 Travis Montoya *)
open Stdio

let version = "1.0.0"

type state =
  | Execute
  | Compile

type func_record =
  { name : string
  ; tokens : string list
  }

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
  let init (local_ size : int) = List.init size (fun _ -> Zero)

  let get (local_ addr : int) ~(memory : t) memory_item option =
    if addr < List.length memory.items then Some (List.nth memory.items addr) else None
  ;;

  let store (item : memory_item) (addr : int) ~(memory : t) = failwith "Not implemented"
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
  let push (x : stack_item) (stack : t) : t = { items = x :: stack.items }

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

  let print_stack (stack : t) : unit =
    if size stack <= 0
    then print_endline "Stack is empty"
    else (
      let rec print_items idx s =
        match s with
        | [] -> ()
        | item :: rest ->
          (match item with
           | Int x -> print_endline ("int " ^ string_of_int x)
           | Float x -> print_endline ("float " ^ string_of_float x)
           | Char x -> print_endline ("char " ^ String.make 1 x)
           | String x -> print_endline ("string " ^ x)
           | Bool x -> print_endline ("bool " ^ string_of_bool x));
          print_items (idx + 1) rest
      in
      print_items 0 stack.items)
  ;;
end

let mforth_memory = Memory.empty
let data_stack = Stack.empty
let return_stack = Stack.empty

let parse_line (line : string) =
  match line with
  | ".s" -> Stack.print_stack data_stack
  | ".r" -> Stack.print_stack return_stack
  | _ ->
    print_endline ("unknown command: " ^ line);
    ()
;;

let () =
  print_endline ("minimaForth " ^ version);
  let rec get_repl_line () =
    Out_channel.flush stdout;
    print_endline "";
    let line = In_channel.input_line_exn stdin in
    match line with
    | "bye" -> exit 0
    | line ->
      parse_line line;
      get_repl_line ()
  in
  get_repl_line ()
;;
