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
    val load : string -> Model.t
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
