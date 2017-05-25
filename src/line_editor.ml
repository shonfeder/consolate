open Notty
open Notty_unix

(* TODO Modularize so it can be used in other terminal apps *)

exception Unimplemented

module Model =
struct
  type position = int

  (** Chars is a slider (a zipper) of unicode chars.
      The selected item indicates the currently selected char.*)
  type t = Uchar.t Slider.t

  let init_spacer = Slider.singleton @@ Uchar.of_char ' '
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

module Update =
struct

  type msg =
    | Esc
    | Code of Uchar.t
    | Del
    | Bwd
    | Fwd

  type state = msg option * Model.t

  let model_from_msg model : msg -> Model.t = function
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

module U = Update
module M = Model
module V = View

let init = Slider.singleton (Uchar.of_char ' ')

let option_msg_of_arrow = function
  | `Up | `Down -> None
  | `Left  -> Some U.Bwd
  | `Right -> Some U.Fwd

let option_msg_of_key = function
  | `Escape     -> Some U.Esc
  | `Uchar code -> Some U.(Code code)
  | `Backspace  -> Some U.Del
  | `Arrow dir  -> option_msg_of_arrow dir
  | _           -> None

let option_msg_of_event = function
  | `Key (key, _) -> option_msg_of_key key
  | _             -> None

(* TODO modularize:
   we need to be able to chain these loops together somehow..
   or pass in a background image at least, then return control to the calling
   module. *)

(* module L *)
(* let rec update_loop term state = *)
(*   match state with *)
(*   | (Some U.Esc, _) -> () *)
(*   | (msg, model) -> *)
(*     view_loop term @@ *)
(*     U.of_state state *)

(* TODO Lock this type down *)
let update : 'a * Model.t -> Model.t =
  fun (event, model) ->
    let msg = option_msg_of_event event in
    Update.of_state (msg, model)

(* let rec view_loop term model = *)
(*     let image = V.of_model model in *)
(*     ( Term.image term image *)
(*     ; event_loop term model ) *)
let rec event_loop term model =
  match Term.event term with
  | `Key (`Escape, _) -> ()
  | event ->
    let model' = update (event, model) in
    let image  = View.of_model model' in
    ( Term.image term image
    ; event_loop term model' )

let run () =
  let term = Term.create() in
  ( event_loop term init
  ; Term.release term)
