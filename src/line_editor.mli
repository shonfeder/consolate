module type T =
sig
  module Model : sig
    type t
    val empty : t
    val is_empty : t -> bool

    val of_string : string -> t
    val to_string : t -> string

    val to_chars : t -> Uchar.t list

    val front    : t -> Uchar.t list
    val back     : t -> Uchar.t list

    val append : t -> t -> t
    val split  : t -> t * t
  end

  module Update : sig
    type state  = Consolate_term.event * Model.t
    type return = (Model.t, Model.t option) result
    val init : Model.t
    val load : string -> Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model  : Model.t -> Notty.image
    val of_uchars : Uchar.t list -> Notty.image
  end
end

include T
