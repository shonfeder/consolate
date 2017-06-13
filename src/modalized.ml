open Notty
open Notty_unix
module CT = Consolate_term
module type Modal =
sig
  include CT.Program
  module Mode :
  sig
    type t
    val normal : t
    val of_model_event_mode
      : Update.state -> t -> Model.t * t
  end
end (* Modal *)

module Make (Prog:Modal) : CT.Program =
struct
  module Model =
  struct
    type t = { mode : Prog.Mode.t
             ; repr : Prog.Model.t }
  end

  module Return =
  struct
    type t = Prog.Return.t
  end

  module Update =
  struct
    include CT.Make.Update.Types (Model) (Return)
    let init = Model.{ mode = Prog.Mode.normal
                     ; repr = Prog.Update.init}
    let load _ = init

    let of_state (event, model) =
      let open Model in
      let {mode; repr} = model in
        match Prog.Mode.of_model_event_mode (event, repr)  mode with
        | (repr, mode) -> Ok {mode; repr}
  end

  module View =
  struct
    let of_model _ = I.empty
  end
end
