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

module Load (Read:Json_reader) : Loader with type t = Read.t =
struct
  type t = Read.t
  let file name =
    let channel = open_in name in
    let lex_buf = Lexing.from_channel channel in
    let lexer_state = Yojson.Safe.init_lexer () in
    Read.channel lexer_state lex_buf
end

module Save (Write:Json_writer) : Saver with type t = Write.t =
struct
  type t = Write.t
  let file name = open_out name
                  |> Bi_outbuf.create_channel_writer
                  |> Write.channel
end
