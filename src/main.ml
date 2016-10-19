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

let kwds = [
  "package" ; "class" ; "fun" ;
  ":" ; "-" ; "+" ; "->" ;
  "(" ; ")" ; "[" ; "]" ;
  "public" ; "private" ; "protected" ;
  "final" ; "abstract" ; "const" ; "static" ] ;;

let () =
  try
    let lang = Sys.argv.(1) in
    let code = Sys.argv.(2) in
    let lex = make_lexer kwds (Stream.of_string code) in
    let ast = Parser.configuration_parser lex in
    let () = load_plugin ("lib/" ^ lang ^ ".cmo") in
    let module Lang = (val get_plugin () : PLUG) in
    print_endline (Lang.compile ast)
  with
  | NoPluginLoaded -> print_endline "No language module has been loaded"
  | PluginLoadingError e -> print_endline e
  | UnsupportedFeature (lang, feat) -> print_endline ("Language " ^ lang ^ " does not support " ^ feat) ;;
