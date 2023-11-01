open Magic

let read_file file =
  In_channel.with_open_bin file In_channel.input_all


let () = 
  let filename = "bin/main.c" in
  let file = read_file filename in
  let t = Lexer.init file in
  print_endline (Lexer.show t);
  (* print_endline (read_file filename); *)

