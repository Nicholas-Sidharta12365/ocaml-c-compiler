open Magic

let read_file file = In_channel.with_open_bin file In_channel.input_all

let () =
  let filename = "bin/main.c" in
  let file = read_file filename in
  let tokens = Lexer.input_to_tokens file in
  Lexer.print_tokens tokens

  (* let lexer = Lexer.init file in *)
  (* let parser = Parser.init lexer in *)
  (* let program = Parser.parse parser in *)
  (* match program with *)
  (* | Ok program -> Parser.print_node program *)
  (* | Error msg -> Fmt.failwith "%a@." Parser.pp_parse_error msg *)
;;
