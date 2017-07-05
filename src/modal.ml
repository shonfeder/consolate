open Notty
open Notty_unix

module CT = Consolate_term


(** A mode of a program is just itself a program. *)
module type Mode = CT.Program
module type Mode_basis = sig
  include CT.Frame
  module View : sig
    val of_model : Model.t -> Notty.image
  end
end

(** The type of a modal program *)
module type Modal = sig
  include CT.Program

  module Modes : sig
    type t
    (** Updating the model means switching modes *)
    (* mode   : message
       update : mode * model -> mode * model *)
    val normal : t
    val quit   : t
    val select : t -> (module Mode
                        with type Model.t = Model.t
                         and type Return.t = t)
  end (* Mode *)
end

(** A general functor returning a trivial Mode of a Modal program *)
module type Normal_mode = functor (Program:Modal) -> sig

  module Model : CT.Model
  module Return : sig
    (** Return.t of a Mode is a Mode.t, as specified by the Modal program *)
    type t = Program.Modes.t
  end

  module Update : sig
    type state = CT.event * Model.t
    type return = (Model.t, Return.t) CT.Flow.t
    val init : Model.t
    val load : string -> Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model : Model.t -> Notty.image
  end
end


(* TODO Need method of specifying which modes can be transitioned into from
   which *)
module Make = struct

  module Mode = struct
    module Basis (Program:Modal) : Mode_basis = struct
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


  (** Given a [Program] satisfying the [Modal] signature
      the [Modal] functor creates a new program in which updating and viewing
      the modal program's model is delegated to the appropriate mode module.

      Normal mode should default to the core of the Modal Program? *)
  module Modal (Program:Modal) : CT.Program = struct
    module Model  = struct
      type t = { mode  : Program.Modes.t
               ; model : Program.Model.t }
    end
    module Return = Program.Return
    module Update = struct
      include CT.Make.Update.Basis (Model) (Return)
      open Model
      let init = { mode  = Program.Modes.normal
                 ; model = Program.Update.init }
      let load = fun _ -> init
      let of_state : state -> return =
        fun (event, {mode; model}) ->
          let mode_module = Program.Modes.select mode in
          let module Mode = (val mode_module : Mode
                             with type Model.t  = Program.Model.t
                              and type Return.t = Program.Modes.t )
          in
          (* TODO: Refactor *)
          (* TODO: Support returns values *)
          match Mode.Update.of_state (event, model) with
          | Ok    model       -> CT.Flow.cont {mode; model}
          | Error (Some mode) ->
            if mode = Program.Modes.quit
            then CT.Flow.halt
            else CT.Flow.cont {mode; model}
          | Error None        -> CT.Flow.cont {mode; model}
    end
    module View = struct
      open Model
      let of_model {mode; model} =
        let mode_module = Program.Modes.select mode in
        let module Mode = (val mode_module : Mode
                           with type Model.t  = Program.Model.t
                            and type Return.t = Program.Modes.t )
        in
        Mode.View.of_model model
    end
  end
end (* Make *)
