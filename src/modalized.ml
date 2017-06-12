open Notty
open Notty_unix

module type Modal =
sig
  include Consolate_term.Program
  module Mode :
  sig
    type t
    val normal : t
    val model_of_state : t -> Update.state -> Update.return
  end
end (* Modal *)

module Make (Prog:Modal) : Consolate_term.Program =
struct
  module Model =
  struct
    type t = { mode  : Prog.Mode.t
             ; model : Prog.Model.t }
  end

  module Return =
  struct
    type t = Prog.Return.t
  end

  module Update =
  struct
    include Consolate_term.Make.Update.Types (Model) (Return)
    let init = Model.{ mode  = Prog.Mode.normal
                     ; model = Prog.Update.init}
    let load _ = init
    let of_state state = Ok init
  end

  module View =
  struct
    let of_model _ = I.empty
  end
end
