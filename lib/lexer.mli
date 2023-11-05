type t

val init : string -> t
val next_token : t -> t * Token.t option

(** Useful for pretty printing the lexer *)
val pp: Format.formatter -> t -> unit
val show : t -> string

val input_to_tokens : string -> Token.t list
val print_tokens : Token.t list -> unit
