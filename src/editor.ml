module LE = Line_editor

open Notty
open Notty_unix

(* TODO Refactor *)
(* TODO Load from and save to file *)
(* TODO Modularize:
   - By creating a multiplexer interface?
   - How to handle extensible commands from widgets? *)

module Text_prog =
struct
  module Model =
  struct
    type t = LE.Model.t Slider.t

    (* Move fwd and bwd on the selected line. *)
    let bwd  = Slider.select_map LE.Model.bwd
    let fwd model =
      if Slider.at_last model
      then model
      else Slider.select_map LE.Model.fwd model

    (* Select the next or prev line *)
    let prev = Slider.bwd
    let next model =
      if Slider.at_last model
      then model
      else Slider.fwd model
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
      | Next
      | Prev

    let option_msg_of_arrow = function
      | `Up    -> Some Prev
      | `Down  -> Some Next
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
    type return = (Model.t, Model.t option) result
    type state = Message.t option * Model.t

    let empty_line = LE.Update.init

    let init = Slider.singleton empty_line

    let model_from_msg model : Message.t -> return =
      let open Message in
      function
      | Esc    -> Error None
      | Enter  -> Ok (model |> Slider.fwd |> Slider.insert empty_line)
      (* TODO Refactor *)
      | Code c -> Ok (Slider.select_map (LE.Model.insert c) model)
      | Del    -> Ok (Slider.select_map
                        (fun m ->
                           LE.(m
                               |> Model.bwd
                               |> Model.remove))
                        model)
      | Bwd    -> Ok (Model.bwd model)
      | Fwd    -> Ok (Model.fwd model)
      | Next   -> Ok (Model.next model)
      | Prev   -> Ok (Model.prev model)

    let of_state : state -> return = function
      | (None, model)     -> Ok model
      | (Some msg, model) -> model_from_msg model msg

  end (* Update *)

  module View =
  struct
    let (%) = Batteries.(%) (* compose *)

    let of_model : Model.t -> Notty.image =
      let of_unselected_lines model =
        model
        |> List.map (LE.View.of_uchars % Slider.to_list)
        |> I.vcat
      in
      fun model ->
        let preceding  = of_unselected_lines @@ Slider.front model in
        let succeeding = of_unselected_lines @@ Slider.back model in
        let selected = LE.View.of_model @@
          match Slider.select model with
          | None      -> LE.Update.init
          | Some line -> line
        in
        I.(preceding
           <->
           selected
           <->
           succeeding)
  end (* View *)
end (* Text_prog *)


  module Line = Consolate_term.Loop(LE)
  module Text = Consolate_term.Loop(Text_prog)
