
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx vars=
    print_string ">> ";
    flush stdout;
    try
      let rec read ()=
        let rec auxread = function
          h::(""::_)-> h
          | h::[]-> h ^ " " ^ read()
          | h::t->h ^";"^(auxread t)
          | [] -> read()
        in
        auxread (String.split_on_char ';' (read_line () ) )
      in
      let tm = s token (from_string (read())) in
      match tm with
        VarAsignation (name, tm)->
          let tm' = eval tm vars in
          print_newline ();
          let tyTm = typeof ctx tm in
          let ctx' = addbinding ctx name tyTm in
          print_endline ("  "^name^" = "^string_of_term (tm') ^ " : " ^ string_of_ty tyTm^"\n");
          loop ctx' ((name,tm')::vars)
        | VarValue tm ->
          print_newline ();let tyTm = typeof ctx tm in
          print_endline ("  "^string_of_term (eval tm vars) ^ " : " ^ string_of_ty tyTm^"\n");
          loop ctx vars
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx vars
     | Parse_error ->
         print_endline "syntax error";
         loop ctx vars
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx vars
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx []
  ;;

top_level_loop ()
;;
