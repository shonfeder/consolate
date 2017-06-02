open Notty
open Notty_unix

module type T =
sig
  module Model : sig
    type t
    val is_empty : t -> bool
    val to_chars : t -> Uchar.t list
    val front    : t -> Uchar.t list
    val back     : t -> Uchar.t list
  end

  module Update : sig
    type state  = Consolate_term.event * Model.t
    type return = (Model.t, Model.t option) result
    val init : Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model  : Model.t -> Notty.image
    val of_uchars : Uchar.t list -> Notty.image
  end
end

module Model =
struct
  (** Chars is a slider (a zipper) of unicode chars.
      The selected item indicates the currently selected char.*)
  type t = Uchar.t Slider.t

  let init_spacer = Slider.singleton (Uchar.of_char ' ')
  let insert c chars = Slider.insert c chars |> Slider.fwd
  (** Inserts are made to the right of the selected char *)

  let remove chars   =
    (* The init_spacer should always be at the end of the line *)
    let chars' = Slider.remove chars in
    if Slider.is_empty chars'
    then init_spacer
    else chars'

  let bwd = Slider.bwd
  let fwd = Slider.fwd_till_last
end (* Model *)

module Message =
struct
  type t =
    | Esc
    | Enter
    | Code of Uchar.t
    | Del
    | Bwd
    | Fwd

  let option_msg_of_arrow = function
    | `Up | `Down -> None
    | `Left  -> Some Bwd
    | `Right -> Some Fwd

  let option_msg_of_key = function
    | `Escape     -> Some Esc
    | `Enter      -> Some Enter
    | `Uchar code -> Some (Code code)
    | `Backspace  -> Some Del
    | `Arrow dir  -> option_msg_of_arrow dir
    | _           -> None

  let of_event = function
    | `Key (key, _) -> option_msg_of_key key
    | _  -> None
end (* Message *)

module Update =
struct
  type state  = Consolate_term.event * Model.t
  type return = (Model.t, Model.t option) result

  (** A blank space must always be at the end of the working model.
      This provides a space for the cursor when it's at the end of
      the line. *)
  let init = Slider.singleton (Uchar.of_char ' ')

  let model_from_msg model : Message.t -> return =
    let open Message in
    function
    | Esc    -> Error None
    | Enter  -> Error (Some model)
    | Code c -> Ok (Model.insert c model)
    | Del    -> Ok (model |> Model.bwd |> Model.remove)
    | Bwd    -> Ok (Model.bwd model)
    | Fwd    -> Ok (Model.fwd model)

  let of_state : state -> return =
    fun (event, model) ->
      match Message.of_event event with
      | None     -> Ok model
      | Some msg -> model_from_msg model msg

end (* Update *)

module View =
struct
  let of_uchar attr c = I.uchar attr c 1 1
  let of_uchars cs    = I.hcat @@ List.map (of_uchar A.empty) cs

  let of_model : Model.t -> Notty.image =
    fun model ->
    let front = of_uchars @@ Slider.front model in
    let back  = of_uchars @@ Slider.back model in
    let selected = of_uchar A.(bg red) @@
      match Slider.select model with
      | None      -> Uchar.of_char ' '
      | Some char -> char
    in
    I.(front <|> selected <|> back)
end (* View *)
