open Magic
open Printf

let read_file file = In_channel.with_open_bin file In_channel.input_all

let input_to_tokens input =
  let lexer = Lexer.init input in
  let tokens = Vect.create 0 Token.Illegal in
  let rec loop lexer =
    match Lexer.next_token lexer with
    | lexer, Some token ->
      Vect.push tokens token;
      loop lexer
    | _, None -> ()
  in
  let _ = loop lexer in
  Vect.to_list tokens
;;

let print_tokens tokens = List.iter (fun t -> printf "%s\n " @@ Token.show t) tokens

let () =
  let filename = "bin/main.c" in
  let file = read_file filename in
  let tokens = input_to_tokens file in
  print_tokens tokens
;;
