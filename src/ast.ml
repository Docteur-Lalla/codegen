(*
 * Copyright (c) 2016, Kévin Le Bon
 * All rights reserved.
 *
 * * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 *
 * Neither the name of Kévin Le Bon nor the names of other
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

type ast =
| Configuration of definition list

and definition =
| Package of string * definition list
| Class of class_struct
| Function of fun_struct

and class_struct = {
  visible: visibility ;
  inheritance: inheritance_property ;
  name: string ;
  attrs: attribute list ;
  mthds: fun_struct list
}

and visibility =
| Public
| Private
| Protected

and inheritance_property =
| Default
| Final
| Abstract

and attribute = {
  visible: visibility ;
  const: bool ;
  name: string ;
  type_scheme: datatype
}

and fun_struct = {
  visible: visibility ;
  static: bool ;
  abstract: bool ;
  name: string ;
  type_scheme: datatype
}

and datatype =
| Simple of string
| Array of datatype * int option
| Arrow of datatype * datatype
| NamedArrow of (string * datatype) * datatype ;;

let rec print_type = function
| Simple t -> t
| Array (Simple t, Some n) -> t ^ "[" ^ string_of_int n ^ "]"
| Array (Array (_,_) as t, Some n) -> print_type t ^ "[" ^ string_of_int n ^ "]"
| Array (t, Some n) -> "(" ^ print_type t ^ ")[" ^ string_of_int n ^ "]"
| Array (Simple t, None) -> t ^ "[]"
| Array (Array (_,_) as t, None) -> print_type t ^ "[]"
| Array (t, None) -> "(" ^ print_type t ^ ")[]"
| Arrow (Arrow (a, b), c) -> "(" ^ print_type (Arrow (a, b)) ^ ") -> " ^ print_type c
| Arrow (a, b) -> print_type a ^ " -> " ^ print_type b
| NamedArrow ((name, a), b) -> "(" ^ print_type a ^ " " ^ name ^ ") -> " ^ print_type b ;;

let rec print_configuration = function
| Configuration [] -> ""
| Configuration [c] -> print_definition 0 c
| Configuration (h::t) -> print_definition 0 h ^ "\n\n" ^ print_configuration (Configuration t)

and print_definition tab =
  let indent = String.make (tab * 2) ' ' in function
  | Class c ->
    let visible_s = print_visibility c.visible in
    let attr_list = List.map (print_attribute (tab+1)) c.attrs in
    let mthd_list = List.map (print_method (tab+1)) c.mthds in
    let attr_lf = if attr_list = [] || mthd_list = [] then "" else "\n" in
    let inherit_s = match c.inheritance with
    | Default -> ""
    | Final -> "final "
    | Abstract -> "abstract " in

    indent ^ "class " ^ visible_s ^ " " ^ inherit_s ^  c.name ^ ":\n" ^
    String.concat "\n" attr_list ^ attr_lf ^ String.concat "\n" mthd_list

  | Package (name, d) ->
    let definition_list = List.map (print_definition (tab+1)) d in
    indent ^ "package " ^ name ^ ":\n" ^ (String.concat "\n" definition_list)

  | Function f ->
    let visible_s = print_visibility f.visible in
    let type_s = print_type f.type_scheme in
    indent ^ "fun " ^ visible_s ^ " " ^ f.name ^ " : " ^ type_s

and print_attribute tab attr = String.make (2 * tab) ' ' ^
  "+ " ^ print_visibility attr.visible ^ " " ^ (if attr.const then "const " else "") ^
  attr.name ^ " : " ^ print_type attr.type_scheme

and print_method tab mthd =
  let static_s = if mthd.static then "static " else "" in
  let abstract_s = if mthd.abstract then "abstract " else "" in
  String.make (2 * tab) ' ' ^
  "- " ^ print_visibility mthd.visible ^ " " ^ static_s ^ abstract_s ^
  mthd.name ^ " : " ^ print_type mthd.type_scheme

and print_visibility = function
| Public -> "public"
| Private -> "private"
| Protected -> "protected" ;;

let new_class v h n a m = {
  visible = v ;
  inheritance = h ;
  name = n ;
  attrs = a ;
  mthds = m
} ;;

let new_attribute v c n t = {
  visible = v ;
  const = c ;
  name = n ;
  type_scheme = t
} ;;

let new_fun v s a n t = {
  visible = v ;
  static = s ;
  abstract = a ;
  name = n ;
  type_scheme = t
} ;;
