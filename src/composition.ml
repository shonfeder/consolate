open Notty
open Notty_unix

(* TODO Generalize to deal with multiple components *)

(** A Composer is a [Consolate_term.Program] that uses other
    [Consolate_term.Program]s as components.  *)
module type Composer =
sig
  include Consolate_term.Program

  module Composing : sig
    module Component : Consolate_term.Program

    (* TODO Remove these from the spec, since this isn't a multiplexer *)
    val prev : Model.t -> Model.t
    val next : Model.t -> Model.t
    val add  : Model.t -> Model.t

    val selected : Model.t -> Component.Model.t option
    val replace  : Component.Model.t -> Model.t -> Model.t
        (* Ok (Slider.replace selected' model) *)
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
  module View    = View

  module Update =
  struct
    type state  = Update.state
    type return = Update.return

    let init = Update.init

    let muxed_result_of_event (event, model) =
      match Composing.selected model with
      | None          -> Ok model
      | Some selected ->
        let selected' =
          match Component.Update.of_state (event, selected) with
            | Ok selected'           -> selected'
            | Error (Some selected') -> selected'
            | Error None             -> selected
        in
        Ok (Composing.replace selected' model)

    let of_state : state -> return =
      fun state ->
        match Message.of_state state with
        | Some msg -> Composer.Update.of_state state
        | None     -> muxed_result_of_event state

    let load : string -> Model.t = Composer.Update.load

  end (* Update *)

end (* Make *)

(** E.g., we can compose a text editor that uses a line editor as a component:
    module Editor_composer = Editor(Line_editor)
    module Editor_prog     = Make(Editor_composer)
    module Editor = Consolate_term.Loop(Editor_prog) *)
