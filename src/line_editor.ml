open Notty
open Notty_unix

(* TODO Modularize so it can be used in other terminal apps *)

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
  let fwd = Slider.fwd
end (* Model *)

module Message =
struct
  type t =
    | Esc
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
  (** A blank space must always be at the end of the working model.
      This provides a space for the cursor when it's at the end of
      the line. *)
  let init = Slider.singleton (Uchar.of_char ' ')

  type state = Message.t option * Model.t

  let model_from_msg model : Message.t -> Model.t =
    let open Message in
    function
    | Esc    -> model
    | Code c -> Model.insert c model
    | Del    -> model |> Model.bwd |> Model.remove |> Model.fwd
    | Bwd    -> Model.bwd model
    | Fwd    -> Model.fwd model

  let of_state : state -> Model.t = function
    | (None, model)     -> model
    | (Some msg, model) -> model_from_msg model msg

end (* Update *)

module View =
struct
  let of_uchar attr c = I.uchar attr c 1 1

  let of_model : Model.t -> Notty.image =
    fun model ->
    let front = I.hcat @@ List.map (of_uchar A.empty) @@ Slider.front model in
    let back  = I.hcat @@ List.map (of_uchar A.empty) @@ Slider.back model in
    let selected = of_uchar A.(bg red) @@
      match Slider.select model with
      | None      -> Uchar.of_char ' '
      | Some char -> char
    in
    I.(front <|> selected <|> back)
end (* View *)
