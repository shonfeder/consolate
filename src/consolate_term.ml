open Notty
open Notty_unix

module type Term_Program =
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
    val init : Model.t
    val of_state : Message.t option * Model.t -> Model.t
  end

  module View : sig
    val of_model : Model.t -> Notty.image
  end
end (* Term_Program *)

module Loop (Program:Term_Program) =
struct

  let update :
    [ Unescape.event | `Resize of (int * int) | `End ] * Program.Model.t
    -> Program.Model.t =
    fun (event, model) ->
      let msg = Program.Message.of_event event in
      Program.Update.of_state (msg, model)

  let view : Notty.image -> Program.Model.t -> Notty.image =
    fun bg model ->
      I.(bg </> Program.View.of_model model)

  let rec update_view_loop term bg model =
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | event ->
      let model' = update (event, model) in
      let image  = view bg model' in
      ( Term.image term image
      ; update_view_loop term bg model' )

  let run () =
    let term = Term.create() in
    ( update_view_loop term I.empty Program.Update.init
    ; Term.release term)
end (* Loop *)
