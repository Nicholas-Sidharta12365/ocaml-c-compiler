type t =
  | Illegal
  (* Items *)
  | Ident of string
  | Integer of string
  | String of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Multiply
  | Divide
  | Modulo
  | Increment
  | Decrement
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | Geq  (* Greater than or equal to *)
  | Leq  (* Less than or equal to *)
  | LogicalAnd
  | LogicalOr
  | LogicalNot
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | BitwiseNot
  | ShiftLeft
  | ShiftRight
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
  | While
  | For
  | Do
  | Switch
  | Case
  | Default
  | Return
  | Int
  | Void
  | Printf
  | Sizeof
  (* Pointer and Address *)
  | Ampersand   (* & *)
  | Asterisk    (* * *)
  (* Type qualifier *)
  | Const
[@@deriving show, eq, sexp]

let lookup_ident str =
  match str with
  | "if" -> If
  | "else" -> Else
  | "while" -> While
  | "for" -> For
  | "do" -> Do
  | "switch" -> Switch
  | "case" -> Case
  | "default" -> Default
  | "return" -> Return
  | "int" -> Int
  | "void" -> Void
  | "printf" -> Printf
  | "sizeof" -> Sizeof
  | "const" -> Const
  | _ -> Ident str
;;

