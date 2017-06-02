open Notty
open Notty_unix

(* TODO Rename

   This hasn't ended up as a general multiplex constructor
   It is, rather, a way of gluing together modular programs,
   so that terminal events can be passed along to embedded programs,
   and the embedded models can be updated and viewed
   in the context of the containing program.

   I'm not sure yet what this called... *)

(** A Muxer is a Consolate_term.Program with functions for
    advancing through the model to different foci *)
module type Muxer =
sig
  include Consolate_term.Program

  module Muxing : sig
    module Muxed : Consolate_term.Program

    (* TODO Remove these from the spec, since this isn't a multiplexer *)
    val prev : Model.t -> Model.t
    val next : Model.t -> Model.t
    val add  : Model.t -> Model.t

    val selected : Model.t -> Muxed.Model.t option
    val replace  : Muxed.Model.t -> Model.t -> Model.t
        (* Ok (Slider.replace selected' model) *)
  end

  module Message : sig
    type t
    val of_state : Update.state -> t option
  end
end

module Make (Muxer:Muxer) =
struct

  module Muxing  = Muxer.Muxing
  module Muxed   = Muxing.Muxed
  module Message = Muxer.Message

  module Model   = Muxer.Model
  module View    = Muxer.View

  module Update =
  struct
    type state  = Muxer.Update.state
    type return = Muxer.Update.return

    let init = Muxer.Update.init

    let muxed_result_of_event (event, model) =
      match Muxing.selected model with
      | None          -> Ok model
      | Some selected ->
        let selected' =
          match Muxed.Update.of_state (event, selected) with
            | Ok selected'           -> selected'
            | Error (Some selected') -> selected'
            | Error None             -> selected
        in
        Ok (Muxing.replace selected' model)

    let of_state : state -> return =
      fun state ->
        match Message.of_state state with
        | Some msg -> Muxer.Update.of_state state
        | None     -> muxed_result_of_event state
  end (* Update *)
end (* Make *)

(** E.g., we can multiplex a line editor to implement an editor:
    module Line_editor_mux = Make(EditorProg)
    module Editor = Consolate_term.Loop(TmuxTest) *)

module EditorProg = Editor.Prog(Line_editor)
module Line_editor_mux = Make (EditorProg)
module Editor = Consolate_term.Loop(Line_editor_mux)
