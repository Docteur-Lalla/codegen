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

open Ast ;;
open Genlex ;;

let rec many p = parser
| [< e = p ; s >] -> e :: many p s
| [< >] -> [] ;;

let many1 p = parser
| [< e = p ; s >] -> e :: many p s ;;

let optional default p = parser
| [< e = p >] -> e
| [< >] -> default ;;

let rec configuration_parser = parser
| [< d = many1 definition_parser >] -> Configuration d

and definition_parser = parser
| [< 'Kwd "package" ; 'Ident name ; 'Kwd ":" ; d = many1 definition_parser >] -> Package (name, d)
| [< 'Kwd "class" ; c = class_definition_parser >] -> c
| [< 'Kwd "fun" ; f = fun_definition_parser >] -> Function f

and class_definition_parser = parser
| [< v = optional Private visibility_parser ;
  final = optional Default inheritance_property_parser ;
  'Ident name ;
  super = optional None (parser [< 'Kwd "(" ; 'Ident s ; 'Kwd ")" >] -> Some s) ; 'Kwd ":" ; c = class_struct_parser final v name super >] -> Class c

and visibility_parser = parser
| [< 'Kwd "public" >] -> Public
| [< 'Kwd "private" >] -> Private
| [< 'Kwd "protected" >] -> Protected

and inheritance_property_parser = parser
| [< 'Kwd "final" >] -> Final
| [< 'Kwd "abstract" >] -> Abstract

and class_struct_parser inheritance visible name super = parser
| [< attrs = many attribute_parser ; mthds = many method_parser >] ->
  new_class visible inheritance name super attrs mthds

and attribute_parser = parser
| [< 'Kwd "+" ;
  v = optional Private visibility_parser ;
  c = optional false (parser [< 'Kwd "const" >] -> true) ;
  'Ident name ; 'Kwd ":" ; t = type_parser >] ->
  new_attribute v c name t

and method_parser = parser
| [< 'Kwd "-" ;
  v = optional Public visibility_parser ;
  s = optional false (parser [< 'Kwd "static" >] -> true) ;
  a = optional false (parser [< 'Kwd "abstract" >] -> true) ;
  'Ident name ; 'Kwd ":" ; t = type_parser >] ->
  new_fun v s a name t

and fun_definition_parser = parser
| [< v = optional Public visibility_parser ;
  s = optional false (parser [< 'Kwd "static" >] -> true) ;
  'Ident name ; 'Kwd ":" ; t = type_parser >] ->
  new_fun v s false name t

and type_parser =
  let rec aux left name = parser
  | [< 'Kwd "->" ; right = type_parser >] ->
    begin match name with
    | None -> Arrow (left, right)
    | Some n -> NamedArrow ((n, left), right)
    end
  | [< 'Kwd "[" ;
    size = optional None (parser [< 'Int n >] -> Some n) ;
    'Kwd "]" ; s >] -> aux (Array (left, size)) None s
  | [< >] -> left
  
  in let parenthesized t = parser
  | [< 'Kwd ")" ; s >] -> aux t None s
  | [< 'Ident name ; 'Kwd ")" ; s >] -> aux t (Some name) s

  in parser
  | [< 'Kwd "(" ; t = type_parser ; s >] -> parenthesized t s
  | [< 'Ident name ; s >] -> aux (Simple name) None s ;;
