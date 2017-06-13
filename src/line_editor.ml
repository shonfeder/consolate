open Notty
open Notty_unix


module Aux =
struct
  let drop_last list =
    BatList.take (List.length list - 1) list
end (* Aux *)


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

  let empty = Slider.empty
  let is_empty model =
    match Slider.to_list model with
    | []  -> true
    | [c] -> Uchar.of_char ' ' = c
    | _   -> false

  let at_start = Slider.at_first
  let at_end   = Slider.at_last

  let to_start = Slider.fbwd
  let to_end  model = model |> Slider.ffwd |> Slider.bwd

  let front = Slider.front
  let back  = Slider.back

  let bwd = Slider.bwd
  let fwd = Slider.fwd_till_last

  let of_string string =
    (string ^ " ")
    |> BatString.to_list
    |> List.map Uchar.of_char
    |> Slider.of_list
  let to_string model =
    model
    |> Slider.to_list
    |> Aux.drop_last
    |> List.map Uchar.to_char
    |> BatString.of_list

  let to_chars = Slider.to_list

  let drop_last = function
    | (front, [])   -> (Aux.drop_last front, [])
    | (front, back) -> (front, Aux.drop_last back)

  let append model =
    model
    |> drop_last (* Drop the spacer at the end of the first line *)
    |> Slider.ffwd
    |> Slider.append

  let split model =
    let (front, back) = Slider.split model in
    let spacer_if_empty slider =
      if Slider.is_empty slider
      then init_spacer
      else slider
    in
    (Slider.append front init_spacer, spacer_if_empty back)

end (* Model *)


module Return =
struct
  type t = Model.t
end


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

  let load : string -> Model.t = Model.of_string

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


module Mode =
struct
  type t =
    | Normal
    | Insert

  let normal = Normal

  let in_normal state = Update.of_state (* XXX *)
  let in_insert state = Update.of_state

  let of_model_event_mode state = function
    | Normal -> in_normal state
    | Insert -> in_insert state
end
