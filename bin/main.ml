
let read_file file =
  In_channel.with_open_bin file In_channel.input_all


let () = 
  let filename = "bin/main.c" in
  print_endline (read_file filename);

