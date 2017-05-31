open Notty
open Notty_unix

module type Program =
sig
  module Model : sig
    type t
  end

  module Message : sig
    type t
    val of_event :
      [ Unescape.event | `Resize of (int * int) | `End ]
      -> t option
  end

  module Update : sig
    type return = (Model.t, Model.t option) result
    (** [Ok] means the input loop continues.
        [Error] means the input should terminate, and the
        optional value returned*)
    val init : Model.t
    val of_state : Message.t option * Model.t -> return
  end

  module View : sig
    val of_model : Model.t -> Notty.image
  end
end (* Program *)

module Loop (Prog:Program) =
struct

  let update :
    [ Unescape.event | `Resize of (int * int) | `End ] * Prog.Model.t
    -> Prog.Update.return =
    fun (event, model) ->
      let msg = Prog.Message.of_event event in
      Prog.Update.of_state (msg, model)

  let view : Notty.image -> Prog.Model.t -> Notty.image =
    fun bg model ->
      I.(bg </> Prog.View.of_model model)

  let rec update_view_loop term bg model =
    match update (Term.event term, model) with
    | Error ret -> ret
    | Ok model' ->
      let image  = view bg model' in
      ( Term.image term image
      ; update_view_loop term bg model' )

  let run () =
    let term = Term.create() in
    let return = update_view_loop term I.empty Prog.Update.init in
    ( Term.release term
    ; return )
end (* Loop *)
