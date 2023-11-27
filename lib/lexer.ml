open Base

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]
let init input =
  if String.is_empty input
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let rec next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, token =
      match ch with
      | '=' -> if_peeked lexer '=' ~default:Assign ~matched:Equal
      | '!' -> if_peeked lexer '=' ~default:Bang ~matched:NotEqual
      | '+' -> if_peeked lexer '+' ~default:Plus ~matched:Increment
      | '-' -> advance lexer, Minus
      | '*' -> advance lexer, Multiply
      | '/' -> advance lexer, Divide
      | '<' -> advance lexer, LessThan
      | '>' -> advance lexer, GreaterThan
      | ',' -> advance lexer, Comma
      | ';' -> advance lexer, Semicolon
      | ':' -> advance lexer, Colon
      | '(' -> advance lexer, LeftParen
      | ')' -> advance lexer, RightParen
      | '{' -> advance lexer, LeftBrace
      | '}' -> advance lexer, RightBrace
      | '[' -> advance lexer, LeftBracket
      | ']' -> advance lexer, RightBracket
      | '"' -> read_string lexer
      | ch when is_identifier ch -> read_identifier lexer
      | ch when is_number ch -> read_number lexer
      | ch -> Fmt.failwith "unknown char: %c" ch
    in
    lexer, Some token

and advance lexer =
  if lexer.position >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) })

and peek_char lexer =
  match lexer.position + 1 < String.length lexer.input with
  | true -> Some (String.get lexer.input (lexer.position + 1))
  | false -> None

and seek lexer condition =
  let rec loop lexer =
    match condition lexer.ch with
    | true -> loop (advance lexer)
    | false -> lexer, lexer.position
  in
  let lexer, position = loop lexer in
  lexer, position

and read_while lexer condition =
  let pos_start = lexer.position in
  let lexer, pos_end =
    seek lexer (fun ch ->
      match ch with
      | Some character -> condition character
      | None -> false)
  in
  lexer, String.sub lexer.input ~pos:pos_start ~len:(pos_end - pos_start)

and read_identifier lexer =
  let lexer, ident = read_while lexer is_identifier in
  lexer, Token.lookup_ident ident

and read_number lexer =
  let lexer, int = read_while lexer is_number in
  lexer, Token.Integer int

and read_string lexer =
  let lexer = advance lexer in
  let lexer, str = read_while lexer (fun ch -> Char.(ch <> '"')) in
  let lexer = advance lexer in
  lexer, Token.String str

and skip_whitespace lexer =
  let lexer, _ =
    seek lexer (fun ch ->
      match ch with
      | Some ch -> Char.is_whitespace ch
      | None -> false)
  in
  lexer

and if_peeked lexer ch ~default ~matched =
  let lexer, result =
    match peek_char lexer with
    | Some peeked when Char.(peeked = ch) -> advance lexer, matched
    | _ -> lexer, default
  in
  advance lexer, result

and is_identifier ch = Char.(ch = '_' || is_alpha ch)
and is_number ch = Char.is_digit ch

let input_to_tokens input =
  let rec loop lexer acc =
    match next_token lexer with
    | new_lexer, Some token -> loop new_lexer (token :: acc)
    | _, None -> List.rev acc
  in
  loop (init input) []
;;

let print_tokens tokens = List.iter ~f:(fun t -> Fmt.pr "%s\n " @@ Token.show t) tokens
