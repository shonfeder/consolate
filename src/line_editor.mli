(* TODO how can I remove this duplication? *)

module Model : sig
  type t
  val to_chars : t -> Uchar.t list
  val is_empty : t -> bool
  val front    : t -> Uchar.t list
  val back     : t -> Uchar.t list
end

module Update : sig
  type state  = Consolate_term.event * Model.t
  type return = (Model.t, Model.t option) result
  val init : Model.t
  val of_state : state -> return
end

module View : sig
  val of_model  : Model.t -> Notty.image
  val of_uchars : Uchar.t list -> Notty.image
end

module type T =
sig
  module Model : sig
    type t
    val is_empty : t -> bool
    val to_chars : t -> Uchar.t list
    val front    : t -> Uchar.t list
    val back     : t -> Uchar.t list
  end

  module Update : sig
    type state  = Consolate_term.event * Model.t
    type return = (Model.t, Model.t option) result
    val init : Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model  : Model.t -> Notty.image
    val of_uchars : Uchar.t list -> Notty.image
  end
end
