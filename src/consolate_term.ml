open Notty
open Notty_unix

type event = [ Unescape.event | `Resize of (int * int) | `End ]

(** Program control flow *)
module Flow = struct
  type ('a, 'b) t = ('a, 'b option) result
  let halt : ('a, 'b) t
    = Error None
  let return : 'b -> ('a, 'b) t
    = fun x -> Error (Some x)
  let cont : 'a -> ('a, 'b) t
    = fun x -> Ok x

  module Infix = struct
    let (>>=) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
      = fun m f -> match m with
        | Ok a    -> f a
        | Error b -> Error b
  end
  include Infix

end (* Flow *)

module type Model =
sig
  type t
end

module type Return =
sig
  type t
end

module type Frame = sig
  module Model  : Model
  module Return : Return
end

module type Program =
sig

  module Model : Model
  module Return : Return

  module Update : sig
    type state  = event * Model.t
    type return = (Model.t, Return.t) Flow.t
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


(** Make base versions of the standard program modules *)
module Make =
struct

  module Update = struct

    (** Basic types and functions *)
    module Basis (Model:Model) (Return:Return) = struct
      type state  = event * Model.t
      (* TODO Change form result type to custom return type *)
      (* type ('a, 'b) return = Halt | Cont of 'a | Return of 'b *)
      type return = (Model.t, Return.t) Flow.t

      module Flow = Flow
    end (* Basis *)

  end (* Update *)

  module Message =
  struct

    module Functions (M:sig type t end) =
    struct
      let of_keys : (Notty.Unescape.key * M.t) list
        -> (Notty.Unescape.key -> M.t option)
        = fun assoc -> fun key -> BatList.Exceptionless.assoc key assoc
    end;;
  end (* Message *)

end (* Make *)

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
