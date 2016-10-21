open Ast ;;
open Plugin ;;

let print_visibility = function
| Public -> "public"
| Private -> "private"
| Protected -> "protected" ;;

let rec get_unnamed_arguments = function
| Arrow (a, b) -> a :: get_unnamed_arguments b
| NamedArrow ((_, a), b) -> a :: get_unnamed_arguments b
| _ -> [] ;;

let rec get_return_type = function
| Arrow (_, b) -> get_return_type b
| NamedArrow (_, b) -> get_return_type b
| t -> t ;;

let print_tab tab = String.make (2 * tab) ' ' ;;

let rec print_type = function
| Simple s -> s
| Array (t, None) -> print_type t ^ "[]"
| Array (t, Some n) -> print_type t ^ "[" ^ string_of_int n ^ "]"
| (Arrow _) as arr ->
  let args = String.concat ", " (List.map print_type (get_unnamed_arguments arr)) in
  let ret = print_type (get_return_type arr) in
  "std::function<" ^ ret ^ "(" ^ args ^ ")" ^ ">"
| (NamedArrow _) as arr ->
  let args = String.concat ", " (List.map print_type (get_unnamed_arguments arr)) in
  let ret = print_type (get_return_type arr) in
  "std::function<" ^ ret ^ "(" ^ args ^ ")" ^ ">" ;;

let rec get_return_type = function
| Arrow (a, b) -> get_return_type b
| NamedArrow ((_,a), b) -> get_return_type b
| t -> print_type t ;;

let rec get_argument_types = function
| Simple _ -> []
| Array _ -> []
| Arrow (a, b) -> (print_type a) :: get_argument_types b
| NamedArrow ((name, a), b) -> (print_type a ^ " " ^ name) :: get_argument_types b ;;

let compile_attribute tab attr =
  let const_s = if attr.const then "const " else "" in
  let type_s = print_type attr.type_scheme in
  print_tab tab ^ const_s ^ type_s ^ " " ^ attr.name ^ ";" ;;

let compile_method tab mthd =
  let static_s = if mthd.static then "static " else "" in
  let args_s = String.concat ", " (get_argument_types mthd.type_scheme) in
  let ret_s = get_return_type mthd.type_scheme in
  let basis = static_s ^ ret_s ^ " " ^ mthd.name ^ "(" ^ args_s ^ ")" in
  if mthd.abstract then
    print_tab tab ^ "virtual " ^ basis ^ " = 0;"
  else
    print_tab tab ^ basis ^ ";" ;;

type visibility_block = {
  visible: visibility ;
  mthds: fun_struct list ;
  attrs: attribute list
} ;;

let divide_class_by_visibility attrs mthds =
  let keep_attr visible = List.filter (fun (attr : attribute) -> attr.visible = visible) in
  let keep_mthd visible = List.filter (fun (mthd : fun_struct) -> mthd.visible = visible) in

  let (pub : visibility_block) = {
    visible = Public ;
    mthds = keep_mthd Public mthds ;
    attrs = keep_attr Public attrs
  } in

  let (priv : visibility_block) = {
    visible = Private ;
    mthds = keep_mthd Private mthds ;
    attrs = keep_attr Private attrs
  } in

  let (prot : visibility_block) = {
    visible = Protected ;
    mthds = keep_mthd Protected mthds ;
    attrs = keep_attr Protected attrs
  } in [pub, priv, prot] ;;

let compile_class tab cls =
  let super_s = match cls.super with
  | Some s -> " : public " ^ s
  | None -> "" in
  
  let [pub, priv, prot] = divide_class_by_visibility cls.attrs cls.mthds in
  let visible_s p = match (p.mthds, p.attrs) with
  | ([], []) -> ""
  | _ ->
    print_tab (tab+1) ^ print_visibility p.visible ^ ":\n" ^
    String.concat "\n" (List.map (compile_method (tab+2)) p.mthds) ^ "\n" ^
    String.concat "\n" (List.map (compile_attribute (tab+2)) p.attrs) in

  let pub_s = visible_s pub in
  let priv_s = visible_s priv in
  let prot_s = visible_s prot in

  let empty_block p = p.mthds = [] && p.attrs = [] in

  let pub_lf = if empty_block pub then "" else if empty_block priv && empty_block prot then "" else "\n\n" in
  let priv_lf = if empty_block prot then "" else "\n\n" in

  print_tab tab ^ "class " ^ cls.name ^ super_s ^ "\n" ^ print_tab tab ^ "{\n" ^
  pub_s ^ pub_lf ^ prot_s ^ priv_lf ^ priv_s ^ "\n" ^ print_tab tab ^ "}" ;;

let rec compile_package tab (name, defs) =
  let defs_s = String.concat "\n\n" (List.map (compile_definition (tab+1)) defs) in
  print_tab tab ^ "namespace " ^ name ^ "\n" ^ print_tab tab ^ "{\n" ^ defs_s ^ "\n" ^ print_tab tab ^ "}"

and compile_definition tab = function
| Package (name, defs) -> compile_package tab (name, defs)
| Class c -> compile_class tab c
| Function f -> compile_method tab f ;;

let compile_to_cpp = function
| Configuration c -> String.concat "\n\n" (List.map (compile_definition 0) c) ;;

module Cpp : PLUG =
  struct
    let compile = compile_to_cpp
  end ;;

let () =
  plugin := Some (module Cpp : PLUG) ;;
