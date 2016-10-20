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

open Genlex ;;
open Parser ;;
open Ast ;;
open Plugin ;;

exception InvalidArgumentCount of int * string ;;

let kwds = [
  "package" ; "class" ; "fun" ;
  ":" ; "-" ; "+" ; "->" ;
  "(" ; ")" ; "[" ; "]" ;
  "public" ; "private" ; "protected" ;
  "final" ; "abstract" ; "const" ; "static" ] ;;

let print_usage () =
  begin
    prerr_endline "USAGE : codegen <lang>" ;
    prerr_endline "USAGE : codegen <lang> <code>"
  end ;;

let compile lang code =
      let lex = make_lexer kwds (Stream.of_string code) in
      let ast = Parser.configuration_parser lex in
      let () = load_plugin ("/usr/lib/codegen/lang/" ^ lang ^ ".cmo") in
      let module Lang = (val get_plugin () : PLUG) in
      Lang.compile ast ;;

let rec string_of_list = function
| [] -> ""
| h::t -> String.make 1 h ^ string_of_list t ;;

let input_code () =
  let rec aux cl =
    try
      aux (input_char stdin :: cl)
    with
    | End_of_file -> string_of_list (List.rev cl) in
  aux [] ;;

let () =
  try
    let arg_count = Array.length Sys.argv in
    match arg_count with
    | 1 -> raise (InvalidArgumentCount (arg_count, "too few arguments"))
    (* Get code from stdin *)
    | 2 ->
      let lang = Sys.argv.(1) in
      let code = input_code () in
      print_endline (compile lang code)
    (* Get code from command line *)
    | 3 ->
      let lang = Sys.argv.(1) in
      let code = Sys.argv.(2) in
      print_endline (compile lang code)
    | _ -> raise (InvalidArgumentCount (arg_count, "too many arguments"))
  with
  | InvalidArgumentCount (n, msg) ->
    prerr_endline ("Argument count = " ^ string_of_int n ^ ": " ^ msg) ;
    print_usage ()
  | NoPluginLoaded -> prerr_endline "No language module has been loaded"
  | PluginLoadingError e -> prerr_endline e
  | UnsupportedFeature (lang, feat) -> prerr_endline ("Language " ^ lang ^ " does not support " ^ feat) ;;
