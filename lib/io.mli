module type Json_reader = sig
  type t
  val channel : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  val string  : string -> t
end

module type Json_writer = sig
  type t
  val channel : Bi_outbuf.t -> t -> unit
  val string  : ?len:int -> t -> string
end

module type Loader = sig
  type t
  val file : string -> t
end

module type Saver = sig
  type t
  val file : string -> t -> unit
end

module Load (Read:Json_reader) : Loader with type t = Read.t
module Save (Write:Json_writer) : Saver with type t = Write.t
