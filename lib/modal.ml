open Notty
open Notty_unix

module CT = Consolate_term


(** A mode of a program is just itself a program. *)
module type Mode = CT.Program

(** Lays the foundation for a Mode *)
module type Mode_basis = sig
  include CT.Frame
  module View : sig
    val of_model : Model.t -> Notty.image
  end
  module Update_basis : sig
    type state  = CT.event * Model.t
    type return = (Model.t, Return.t) CT.Flow.t
    val init : Model.t
    val load : string -> Model.t
  end (* Update_basis *)
end

(** The type of a program which has specified possible modes. *)
module type With_modes = sig
  include CT.Program

  module Modes : sig
    type t
  end (* Mode *)
end

(** The type of a program that can be turned into a modal program satisfying the
    CT.Program interface. *)
module type Modal = sig
  include With_modes
  (** [normal] is the [Mode.t] in which the underlying program defaults to its
      original behavior. *)
  val normal : Modes.t
  (** [quit] is the [Mode.t] telling the program to terminate. *)
  val quit   : Modes.t
  (** [select mode] is the [(module Program : Mode)] implementing the selected
      mode. *)
  val select : Modes.t -> (module Mode
                            with type Model.t  = Model.t
                             and type Return.t = Modes.t )
end (* Mode *)

(** Collects functors to make the components of modal programs. *)
module Make = struct

  (** Collects functors to make the parts of Modes. *)
  module Mode = struct

    (** Construct the basic elements of a program satisfying [Mode] given a
        program with a module of modes. *)
    module Basis (Program:With_modes) : Mode_basis = struct
      module Model  = Program.Model
      module Return = struct type t = Program.Modes.t end
      module View = Program.View

      (** To realize a program mode, the Update_basis must be extended
          with a function [of_state : state -> return] — this is where
          magic happens.

          [of_state] is responsible for providing the model changes in
          response to input events, as well as determining which other
          modes can be transitioned.
      *)
      module Update_basis = struct
        include CT.Make.Update.Basis (Model) (Return)
        let init : Model.t = Program.Update.init
        let load : string -> Model.t = Program.Update.load
      end (* Update_basis *)
    end (* Basis *)

  end (* Mode *)


  (** Given a [Program] satisfying the [With_modes] signature the [With_modes]
      functor creates a new program in which updating and viewing the modal
      program's model is delegated to the appropriate mode module.

      XXX: Normal mode should default to the core of the Program:With_modes? *)
  module Modal (Program:Modal) : CT.Program =
  struct
    module Model  = struct
      type t = { mode  : Program.Modes.t
               ; model : Program.Model.t }
    end
    module Return = Program.Return
    module Update = struct
      include CT.Make.Update.Basis (Model) (Return)
      open Model
      let init = { mode  = Program.normal
                 ; model = Program.Update.init }
      let load = fun _ -> init
      let of_state : state -> return =
        fun (event, {mode; model}) ->
          let mode_module = Program.select mode in
          let module Mode = (val mode_module : Mode
                             with type Model.t  = Program.Model.t
                              and type Return.t = Program.Modes.t )
          in
          (* TODO: Refactor *)
          (* TODO: Support returns values *)
          match Mode.Update.of_state (event, model) with
          | Ok    model       -> CT.Flow.cont {mode; model}
          | Error (Some mode) ->
            if (mode = Program.quit)
            then CT.Flow.halt
            else CT.Flow.cont {mode; model}
          | Error None        -> CT.Flow.cont {mode; model}
    end
    module View = struct
      open Model
      let of_model {mode; model} =
        let mode_module = Program.select mode in
        let module Mode = (val mode_module : Mode
                           with type Model.t  = Program.Model.t
                            and type Return.t = Program.Modes.t )
        in
        Mode.View.of_model model
    end
  end
end (* Make *)
