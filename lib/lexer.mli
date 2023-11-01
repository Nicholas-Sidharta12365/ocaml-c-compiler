type t

val init : string -> t
val next_token : t -> t * Token.t option

(** Useful for pretty printing the lexer *)
val pp: Format.formatter -> t -> unit
val show : t -> string
