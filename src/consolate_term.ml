open Notty
open Notty_unix

type event = [ Unescape.event | `Resize of (int * int) | `End ]

module type Model =
sig
  type t
end

module type Return =
sig
  type t
end

module type Program =
sig
  module Model : Model
  module Return : Return

  module Update : sig
    type state  = event * Model.t
    type return = (Model.t, Return.t option) result
    (** [Ok] means the input loop continues.
        [Error] means the input should terminate, and the
        optional value returned*)
    val init : Model.t
    val load : string -> Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model : Model.t -> Notty.image
  end
end (* Program *)

(** Make base versions of the standard program modules *)
module Make =
struct

  module Update =
  struct
    module Types (Model:Model) (Return:Return) =
    struct
      type state  = event * Model.t
      type return = (Model.t, Return.t option) result
      let halt : return = Error None
      let return : Return.t -> return =
        fun x -> Error (Some x)
      let cont : Model.t -> return =
        fun x -> Ok x
    end
  end (* Update *)

end (* Make *)

module Loop (Prog:Program) =
struct

  module Update = Prog.Update
  module View   = Prog.View

  let rec view_update_loop term bg model =
    let _ = View.of_model model |> Term.image term in
    let state = (Term.event term, model) in
    match Update.of_state state with
    | Error ret -> ret
    | Ok model' -> view_update_loop term bg model'

  let run_from_init init =
    let term = Term.create() in
    let return = view_update_loop term I.empty init in
    ( Term.release term
    ; return )

  let run () = run_from_init Update.init

  let load string = string |> Update.load |> run_from_init

end (* Loop *)
