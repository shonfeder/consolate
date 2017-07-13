open Notty
open Notty_unix

module CT = Consolate_term

(* TODO Generalize to deal with multiple components *)

(** A Composer is a [Consolate_term.Program] that uses other
    [Consolate_term.Program]s as components.

    E.g., we can compose a text editor that uses a line editor as a component:
      module Editor_composer = Text_editor.Prog(Line_editor)
      module Editor_prog     = Composition.Make(Editor_composer)
      module Editor = Consolate_term.Loop(Editor_prog) *)
module type Composer =
sig
  include Consolate_term.Program

  module Composing : sig
    module Component : Consolate_term.Program
    val selected : Model.t -> Component.Model.t option
    val replace  : Component.Model.t -> Model.t -> Model.t
  end

  module Message : sig
    type t
    val of_state : Update.state -> t option
  end
end

module Make (Composer:Composer) =
struct

  open Composer

  module Component = Composing.Component
  module Model   = Model
  module Return  = Return
  module View    = View

  module Update =
  struct
    include Consolate_term.Make.Update.Basis (Model) (Return)

    let init = Update.init

    let compose_result_of_state (event, model) =
      let open CT.Flow in
      match Composing.selected model with
      | None          -> Continue model
      | Some selected ->
        let selected' =
          match Component.Update.of_state (event, selected) with
            | Continue selected' -> selected'
            | Return selected'   -> selected (* XXX selected' *)
            | Halt _             -> selected
        in
        Continue (Composing.replace selected' model)

    let of_state : state -> return =
      fun state ->
        match Message.of_state state with
        | Some msg -> Composer.Update.of_state state
        | None     -> compose_result_of_state state

    let load : string -> Model.t = Composer.Update.load

  end (* Update *)

end (* Make *)
