open Notty
open Notty_unix

type event = [ Unescape.event | `Resize of (int * int) | `End ]

module type Program =
sig
  module Model : sig
    type t
  end

  module Update : sig
    type state  = event * Model.t
    type return = (Model.t, Model.t option) result
    (** [Ok] means the input loop continues.
        [Error] means the input should terminate, and the
        optional value returned*)
    val init : Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model : Model.t -> Notty.image
  end
end (* Program *)

module Loop (Prog:Program) =
struct

  module Update = Prog.Update
  module View   = Prog.View

  let rec update_view_loop term bg model =
    let state = (Term.event term, model) in
    match Update.of_state state with
    | Error ret -> ret
    | Ok model' ->
      ( View.of_model model' |> Term.image term
      ; update_view_loop term bg model' )

  let run () =
    let term = Term.create() in
    let return = update_view_loop term I.empty Update.init in
    ( Term.release term
    ; return )
end (* Loop *)
