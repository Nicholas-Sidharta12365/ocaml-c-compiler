type t =
  | Illegal
  (* Items *)
  | Ident of string
  | Integer of string
  | String of string
  (* Operators *)
  | Assign
  | Bang
  | Plus
  | Minus
  | Multiply
  | Divide
  | Increment
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  (* Delimiters *)
  | Comma
  | Semicolon
  | Colon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  (* Keywords *)
  | If
  | Else
  | For
  | Return
  | Int
  | Void
  | Printf
  | Sizeof
[@@deriving show, eq, sexp]

let lookup_ident str =
  match str with
  | "if" -> If
  | "else" -> Else
  | "for" -> For
  | "return" -> Return
  | "int" -> Int
  | "void" -> Void
  | "printf" -> Printf
  | "sizeof" -> Sizeof
  | _ -> Ident str
;;

