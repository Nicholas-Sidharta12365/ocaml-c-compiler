open Base

let show_expression = Ast.show_expression

(** Bind operation for Base.Result *)
let ( let* ) res f = Base.Result.bind res ~f

type precedence =
  [ `Lowest
  | `Equals
  | `LessGreater
  | `Sum
  | `Product
  | `Prefix
  | `Call
  | `Index
  ]
[@@deriving show, ord]

let prec_gte a b = compare_precedence a b >= 0

let token_prec : Token.t -> precedence = function
  | Equal | NotEqual -> `Equals
  | LessThan | GreaterThan -> `LessGreater
  | Plus | Minus -> `Sum
  | Divide | Multiply -> `Product
  | LeftParen -> `Call
  | LeftBracket -> `Index
  | _ -> `Lowest
;;

type t =
  { lexer : Lexer.t
  ; current : Token.t option
  ; peek : Token.t option
  ; double_peek : Token.t option
  }
[@@deriving show]

type parse_error =
  { msg : string
  ; parser : t
  ; statements : Ast.statement list
  }
[@@deriving show]

let err parser msg statements = Error { parser; msg; statements }

let advance parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  (* { lexer; peek; current = parser.peek; double_peek = parser.peek } *)
  { lexer; peek = parser.double_peek; current = parser.peek; double_peek = peek }
;;

let advance_until parser f =
  let parser = ref parser in
  while not (f !parser) do
    parser := advance !parser
  done;
  !parser
;;

let chomp_semicolon parser =
  match parser.peek with
  | Some Token.Semicolon -> advance parser
  | _ -> parser
;;

let next_token parser =
  let parser = advance parser in
  parser, parser.current
;;

let expect_peek parser condition =
  match parser.peek with
  | Some tok ->
    if condition tok
    then Ok (advance parser)
    else Error (Fmt.failwith "missing peeked: %a" pp parser)
  | None -> Error "no peek token"
;;

let peek_is parser token = Option.equal Token.equal parser.peek (Some token)

let double_peek_is parser token = Option.equal Token.equal parser.double_peek (Some token)

(* let ( let* ) res f = Base.Result.bind res ~f *)

let expect_assign parser =
  expect_peek parser (function
    | Token.Assign -> true
    | _ -> false)
;;

let expect_colon parser =
  expect_peek parser (function
    | Token.Colon -> true
    | _ -> false)
;;

let expect_semicolon parser =
  expect_peek parser (function
    | Token.Semicolon -> true
    | _ -> false)
;;

let expect_lparen parser =
  expect_peek parser (function
    | Token.LeftParen -> true
    | _ -> false)
;;

let expect_rparen parser =
  expect_peek parser (function
    | Token.RightParen -> true
    | _ -> false)
;;

let expect_lbrace parser =
  expect_peek parser (function
    | Token.LeftBrace -> true
    | _ -> false)
;;

let expect_rbrace parser =
  expect_peek parser (function
    | Token.RightBrace -> true
    | _ -> false)
;;

let expect_rbracket parser =
  expect_peek parser (function
    | Token.RightBracket -> true
    | _ -> false)
;;

let peek_precedence parser =
  match parser.peek with
  | Some tok -> token_prec tok
  | _ -> `Lowest
;;

let curr_precedence parser =
  match parser.current with
  | Some tok -> token_prec tok
  | _ -> `Lowest
;;

let init lexer =
  let parser = { lexer; current = None; peek = None; double_peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  let parser = advance parser in
  parser
;;

let rec parse parser =
  let rec parse' parser statements =
    match parser.current with
    | Some _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> parse' (advance parser) (stmt :: statements)
       | Error msg -> err parser msg statements)
    | None -> Ok (parser, List.rev statements)
  in
  let* _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.current with

  | Some (Token.Int as tok) | Some (Token.Void as tok) when double_peek_is parser Token.LeftParen -> parse_fn parser tok
  | Some (Token.Int as tok) | Some (Token.Void as tok) -> parse_declaration parser tok
  (* | Token.Void when double_peek_is parser Token.LeftParen -> expr_parse_fn parser *)
  | Some Token.Return -> parse_return parser
  | Some _ -> parse_expression_statement parser
  | None -> Error "no more tokens"

and parse_fn parser tok =
  let* parser, type_def =
    match tok with
    | Token.Int -> Ok (parser, Ast.IntType)
    | Token.Void -> Ok (parser, Ast.VoidType)
    | _ -> Error "unexpected token"
  in

  let* parser, name = parse_identifier parser in
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
      let parser = advance parser in
      let* identifier = read_identifier parser in
      parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionDeclaration { type_def; name; parameters; body })

and parse_declaration parser tok =
  (* Fmt.pr "\n"; *)
  (* Fmt.pr "current %s\n" (Token.show tok); *)
  (* Fmt.pr "peek %s\n" (Token.show @@ Option.value_exn parser.peek); *)
  (* Fmt.pr "double_peek %s\n" (Token.show @@ Option.value_exn parser.double_peek); *)

  let* parser, name = parse_identifier parser in
  let* parser, type_def =
    match tok with
    | Token.Int -> Ok (parser, Ast.IntType)
    | Token.Void -> Ok (parser, Ast.VoidType)
    | _ -> Error "unexpected token"
  in
  (* match expect_semicolon parser with *)
  match parser.peek with
  | Some Token.Semicolon ->
    let parser = advance parser in
    (* let value = None  in *)
    Ok (parser, Ast.Declaration { name; type_def; value = None })
  | Some Token.Assign ->
    let* parser = expect_assign parser in
    (* move parser onto the beginning of the expression *)
    let parser = advance parser in
    let* parser, value = parse_expression parser `Lowest in
    let parser = chomp_semicolon parser in
    Ok (parser, Ast.Declaration { name; type_def; value = Some value })
  | Some tok -> Error (Fmt.failwith "unexpected token %a" Token.pp tok)
  | _ -> Error (Fmt.failwith "missing peeked: %a" pp parser)

and parse_return parser =
  (* Move parser onto expression *)
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Return expr)

and parse_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_identifier parser =
  match parser.peek with
  | Some (Ident identifier) -> Ok (advance parser, { identifier })
  | _ -> Error "missing ident"

and parse_block parser =
  let parser = advance parser in
  let rec parse_block' parser statements =
    match parser.current with
    | Some Token.RightBrace -> Ok (parser, List.rev statements)
    | Some _ ->
      let* parser, statement = parse_statement parser in
      parse_block' (advance parser) (statement :: statements)
    | None -> Error "unexpected eof"
  in
  let* parser, block = parse_block' parser [] in
  Ok (parser, Ast.{ block })

and parse_expression parser prec =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peeked = parser.peek |> Option.value ~default:Token.Illegal in
    let prec_peek = token_prec peeked in
    if peek_is parser Token.Semicolon || prec_gte prec prec_peek
    then Ok (parser, left)
    else (
      match get_infix_fn parser with
      | Some infix_fn ->
        let parser = advance parser in
        let* parser, left = infix_fn parser left in
        parse_expression' parser left
      | None -> Ok (parser, left))
  in
  parse_expression' parser left

and parse_prefix_expression parser =
  let map_parser = Result.map ~f:(fun v -> parser, v) in
  let token = parser.current |> Option.value_exn in
  match token with
  | Token.Ident _ -> expr_parse_identifier parser |> map_parser
  | Token.Integer _ -> expr_parse_number parser |> map_parser
  | Token.String _ -> expr_parse_string parser |> map_parser
  | Token.Bang -> expr_parse_prefix parser token
  | Token.Minus -> expr_parse_prefix parser token
  | Token.LeftParen -> expr_parse_grouped parser
  | Token.If -> expr_parse_if parser
  (* | Token.Void when double_peek_is parser Token.LeftParen -> expr_parse_fn parser *)
  | Token.LeftBracket -> expr_parse_array_literal parser
  | Token.LeftBrace -> expr_parse_hash_literal parser
  | Token.Printf -> expr_parse_printf parser
  | tok -> Error (Fmt.str "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and parse_infix_expression parser left =
  let operator = parser.current |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and parse_call_expression parser fn =
  parse_list_of_exprs parser ~close:Token.RightParen ~final:(fun args ->
    Ast.Call { fn; args })

and expr_parse_printf parser =
  let parser = advance parser in
  let fn = Ast.Identifier { identifier = "printf" } in
  parse_list_of_exprs parser ~close:Token.RightParen ~final:(fun args ->
  Ast.Call { fn; args })


and parse_index_expression parser left =
  let parser = advance parser in
  let* parser, index = parse_expression parser `Lowest in
  let* parser = expect_rbracket parser in
  Ok (parser, Ast.Index { left; index })

and get_infix_fn parser =
  let open Token in
  match parser.peek with
  | Some Plus
  | Some Minus
  | Some Divide
  | Some Multiply
  | Some Equal
  | Some NotEqual
  | Some LessThan
  | Some GreaterThan -> Some parse_infix_expression
  | Some LeftParen -> Some parse_call_expression
  | Some LeftBracket -> Some parse_index_expression
  | _ -> None

and expr_parse_identifier parser =
  match parser.current with
  | Some (Ident identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing number"

and expr_parse_string parser =
  match parser.current with
  | Some (String str) -> Ok (Ast.String str)
  | _ -> Error "missing string"

and expr_parse_number parser =
  match parser.current with
  | Some (Integer num) ->
    let num =
      try Int.of_string num with
      | Failure x -> Fmt.failwith "COULD NOT PARSE: '%s' DUE TO %s" num x
    in
    Ok (Ast.Integer num)
  | _ -> Error "missing number"

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser `Prefix in
  Ok (parser, Ast.Prefix { operator; right })

and expr_parse_grouped parser =
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let* parser =
    expect_peek parser (function
      | Token.RightParen -> true
      | _ -> false)
  in
  Ok (parser, expr)

and parse_list_of_exprs parser ~close ~final =
  let rec parse' parser exprs =
    match parser.peek with
    | Some tok when phys_equal close tok -> Ok (advance parser, final (List.rev exprs))
    | Some Token.Comma ->
      let parser = advance parser in
      let parser = advance parser in
      let* parser, expr = parse_expression parser `Lowest in
      parse' parser (expr :: exprs)
    | _ -> Error "unexpected next token"
  in
  match parser.peek with
  | Some tok when phys_equal close tok -> parse' parser []
  | Some _ ->
    let parser = advance parser in
    let* parser, expr = parse_expression parser `Lowest in
    parse' parser [ expr ]
  | None -> Error "hit eof"

and expr_parse_array_literal parser =
  parse_list_of_exprs parser ~close:Token.RightBracket ~final:(fun exprs ->
    Ast.Array exprs)

and expr_parse_hash_literal parser =
  let rec parse' parser exprs =
    let empty = List.length exprs = 0 in
    match parser.peek with
    | Some Token.RightBrace -> Ok (advance parser, Ast.Hash (List.rev exprs))
    | _ when empty -> parse_key_value parser exprs
    | Some Token.Comma when not empty -> parse_key_value (advance parser) exprs
    | _ -> Error "unexpected next token"
  and parse_key_value parser exprs =
    let parser = advance parser in
    let* parser, key = parse_expression parser `Lowest in
    let* parser = expect_colon parser in
    let parser = advance parser in
    let* parser, value = parse_expression parser `Lowest in
    parse' parser ((key, value) :: exprs)
  in
  parse' parser []

and expr_parse_if parser =
  let* parser = expect_lparen parser in
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = expect_rparen parser in
  let* parser = expect_lbrace parser in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    match parser.peek with
    | Some Token.Else ->
      let parser = advance parser in
      let* parser = expect_lbrace parser in
      let* parser, block = parse_block parser in
      Ok (parser, Some block)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })

and read_identifier parser =
  match parser.current with
  | Some (Token.Ident identifier) -> Ok Ast.{ identifier }
  | _ -> Error "expected to read identifier"

and expr_parse_fn parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
      let parser = advance parser in
      let* identifier = read_identifier parser in
      parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionLiteral { parameters; body })

(* and expr_parse_macro parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
      let parser = advance parser in
      let* identifier = read_identifier parser in
      parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.Macro { parameters; body }) *)

and parse_list_of_parameters parser parameters =
  match parser.peek with
  | Some Token.RightParen -> Ok (advance parser, List.rev parameters)
  | Some Token.Comma ->
    let parser = advance parser in
    let parser = advance parser in
    let* ident = read_identifier parser in
    parse_list_of_parameters parser (ident :: parameters)
  | Some tok -> Error (Fmt.str "unexpected next parameter token %a" Token.pp tok)
  | None -> Error "unexpected end of stream"
;;

let rec string_of_statement = function
  | Ast.Declaration stmt ->
    Fmt.str
      "DECLARATION: %s %s = %s"
      (Ast.show_type_def stmt.type_def)
      (Ast.show_identifier stmt.name)
      (match stmt.value with
       | None -> "Uninitialized"
       | Some value -> show_expression value)
    
  | Ast.FunctionDeclaration stmt ->
    Fmt.str
      "FUNCTION: %s %s %sBODY = {\n  %s\n}"
      (Ast.show_type_def stmt.type_def)
      (Ast.show_identifier stmt.name)
      (String.concat ~sep:", " (List.map stmt.parameters ~f:Ast.show_identifier))
      (String.concat ~sep:"\n  " (List.map stmt.body.block ~f:string_of_statement))
    
  | Return expr -> Fmt.str "RETURN %s" (show_expression expr)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (show_expression expr)
  | BlockStatement _ -> assert false

and string_of_ident ident = Ast.(ident.identifier)

let print_node = function
  | Ast.Program program ->
    Fmt.pr "Program: [@.";
    List.iter program.statements ~f:(fun s -> Fmt.pr "  %s@." (string_of_statement s));
    Fmt.pr "]@."
  | _ -> failwith "yaya"
;;

module Tests = struct
  let expect_program input =
    let lexer = Lexer.init input in
    let parser = init lexer in
    let program = parse parser in
    match program with
    | Ok program -> print_node program
    | Error msg -> Fmt.failwith "%a@." pp_parse_error msg
  ;;

  let%expect_test "series of let statements" =
    expect_program {|
int x;
int y = 1;
    |};
    [%expect
      {|
    Program: [
      DECLARATION: IntType { identifier = "x" } = Uninitialized
      DECLARATION: IntType { identifier = "y" } = (Integer 1)
    ] |}]
  ;;

  let%expect_test "function literal" =
    expect_program {|
int main() {
  
}
    |};
    [%expect
      {|
    Program: [
      FUNCTION: IntType { identifier = "main" } BODY = {

    }
    ] |}]
  ;;


  let%expect_test "function literal" =
    expect_program {|
int main() {
  int x = 1;
  int y = 2;
}
    |};
    [%expect
      {|
    Program: [
      FUNCTION: IntType { identifier = "main" } BODY = {
      DECLARATION: IntType { identifier = "x" } = (Integer 1)
      DECLARATION: IntType { identifier = "y" } = (Integer 2)
    }
    ] |}]
  ;;

  let%expect_test "function literal" =
  expect_program {|
int main() {
int x = 1;
int y = 2;

return 0;
}
  |};
  [%expect
    {|
  Program: [
    FUNCTION: IntType { identifier = "main" } BODY = {
    DECLARATION: IntType { identifier = "x" } = (Integer 1)
    DECLARATION: IntType { identifier = "y" } = (Integer 2)
    RETURN (Integer 0)
  }
  ] |}]
;;

let%expect_test "function literal" =
expect_program {|
int main() {
int x = 1;
int y = 2;

(1+2);

return 0;
}
|};
[%expect
  {|
Program: [
  FUNCTION: IntType { identifier = "main" } BODY = {
  DECLARATION: IntType { identifier = "x" } = (Integer 1)
  DECLARATION: IntType { identifier = "y" } = (Integer 2)
  EXPR: Infix {left = (Integer 1); operator = Token.Plus; right = (Integer 2)};
  RETURN (Integer 0)
}
] |}]
;;

let%expect_test "function literal" =
expect_program {|
int main() {
int x = 1;
int y = 2;

(x+y);

return 0;
}
|};
[%expect
  {|
Program: [
  FUNCTION: IntType { identifier = "main" } BODY = {
  DECLARATION: IntType { identifier = "x" } = (Integer 1)
  DECLARATION: IntType { identifier = "y" } = (Integer 2)
  EXPR: Infix {left = (Identifier { identifier = "x" }); operator = Token.Plus;
  right = (Identifier { identifier = "y" })};
  RETURN (Integer 0)
}
] |}]
;;

let%expect_test "function literal" =
expect_program {|
printf("hello world");
|};
[%expect
  {|
Program: [
  EXPR: Call {fn = (Identifier { identifier = "printf" });
  args = [(String "hello world")]};
] |}]
;;

let%expect_test "function literal" =
expect_program {|
int main() {
  int x = 1;
  int y = 2;

  printf("(x + y) =%d\n", (x + y));

  return 0;
}

|};
[%expect
  {|
Program: [
  FUNCTION: IntType { identifier = "main" } BODY = {
  DECLARATION: IntType { identifier = "x" } = (Integer 1)
  DECLARATION: IntType { identifier = "y" } = (Integer 2)
  EXPR: Call {fn = (Identifier { identifier = "printf" });a
  args =
  [(String "(x + y) =%d\\n");
    Infix {left = (Identifier { identifier = "x" }); operator = Token.Plus;
      right = (Identifier { identifier = "y" })}
    ]};
  RETURN (Integer 0)
}
]
    
|}]
;;
end
