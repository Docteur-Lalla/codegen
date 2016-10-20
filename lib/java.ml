open Ast ;;
open Plugin ;;

let print_visibility = function
| Public -> "public"
| Private -> "private"
| Protected -> "protected" ;;

let rec print_type = function
| Simple s -> s
| Array (t, None) -> print_type t ^ "[]"
| Array (t, Some _) ->
  begin
    print_endline "Warning: array size is not part of the type in Java" ;
    print_type (Array (t, None))
  end
| Arrow _ -> raise (UnsupportedFeature ("Java", " function as variable")) ;;

let print_inheritance_property = function
| Default -> ""
| Final -> "final "
| Abstract -> "abstract " ;;

let rec get_return_type = function
| Simple s -> s
| Array (t, None) -> print_type t ^ "[]"
| Array (t, Some _) -> print_type (Array (t, None))
| Arrow (a, b) -> get_return_type b ;;

let rec get_argument_types = function
| Simple _ -> []
| Array _ -> []
| Arrow (a, b) -> (print_type a) :: get_argument_types b ;;

let print_tab tab = String.make (2 * tab) ' ' ;;

let rec is_valid_package =
  let rec has_no_package = function
  | [] -> true
  | ((Package _) :: _) -> false
  | _::t -> has_no_package t

  in function
  | Configuration [] -> true
  | Configuration (Package (_, defs)::[]) -> has_no_package defs
  | Configuration (Package (_, _)::_) -> false
  | Configuration (_::t) -> has_no_package t ;;

let compile_attribute tab (attr : attribute) =
  let visible_s = print_visibility attr.visible in
  let const_s = if attr.const then "final " else "" in
  print_tab tab ^ visible_s ^ " " ^ const_s ^ print_type attr.type_scheme ^ " " ^ attr.name ^ ";" ;;

let compile_method tab mthd =
  let visible_s = print_visibility mthd.visible in
  let static_s = if mthd.static then "static " else "" in
  let abstract_s = if mthd.abstract then "abstract " else "" in
  let ret_s = get_return_type mthd.type_scheme in
  let args_s = String.concat ", " (get_argument_types mthd.type_scheme) in
  print_tab tab ^ visible_s ^ " " ^ static_s ^ abstract_s ^ ret_s ^ " " ^ mthd.name ^ "(" ^ args_s ^ ")" ^
  if mthd.abstract then ";" else " {\n" ^ print_tab (tab+1) ^ "\n" ^ print_tab tab ^ "}" ;;

let compile_class tab (cls : class_struct) =
  let rec all_abstract = function
  | [] -> true
  | h::t -> h.abstract && all_abstract t in
  
  let visible_s = print_visibility cls.visible in
  let mthds_s = String.concat "\n\n" (List.map (compile_method (tab+1)) cls.mthds) in

  if cls.inheritance = Abstract && cls.attrs = [] && all_abstract cls.mthds then
  (* Interface *)
    print_tab tab ^ visible_s ^ " interface " ^ cls.name ^ " {\n" ^ mthds_s ^ "\n" ^ print_tab tab ^ "}"
  else
  (* Class *)
  let inherit_s = print_inheritance_property cls.inheritance in
  let attrs_s = String.concat "\n" (List.map (compile_attribute (tab+1)) cls.attrs) in
  let attrs_lf = if cls.mthds = [] || cls.attrs = [] then "" else "\n\n" in
  print_tab tab ^ visible_s ^ " " ^ inherit_s ^ "class " ^ cls.name ^ " {\n" ^ attrs_s ^ attrs_lf ^ mthds_s ^ "\n" ^ print_tab tab ^ "}"

let rec compile_definition = function
| Class cls -> compile_class 0 cls
| Package (name, defs) ->
  let defs_s = String.concat "\n\n" (List.map compile_definition defs) in
  "package " ^ name ^ ";\n\n" ^ defs_s
| Function _ -> raise (UnsupportedFeature ("Java", "stand alone functions")) ;;

let compile_to_java ((Configuration c) as ast) =
  if is_valid_package ast then
    String.concat "\n\n" (List.map compile_definition c)
  else
    raise (UnsupportedFeature ("Java", "multiple or nested packages in one file")) ;;

module Java : PLUG =
  struct
    let compile = compile_to_java
  end ;;

let () =
  plugin := Some (module Java : PLUG) ;;
