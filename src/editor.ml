open Notty
open Notty_unix

(* TODO Load from and save to file *)
(* TODO Change file name *)

module Prog (LE:Line_editor.T) =
struct

  module Model =
  struct
    type t = LE.Model.t Slider.t
  end

  module Composing =
  struct
    module Component = LE

    let prev : Model.t -> Model.t = Slider.bwd
    let next : Model.t -> Model.t = Slider.fwd_till_last
    let add model = model |> Slider.fwd |> Slider.insert LE.Update.init

    let selected : Model.t -> LE.Model.t option = Slider.select
    let replace (replacement : LE.Model.t) : Model.t -> Model.t =
      (Slider.replace replacement)

    let muxed_of_state = LE.Update.of_state
  end

  module Message =
  struct
    type t =
      | Esc
      | Add
      | Remove
      | Next
      | Prev

    (* TODO Append remaining line to previous when cursor at beginning *)
    let option_remove_of_model model =
      let remove_if_empty line =
        if LE.Model.is_empty line
        then Some Remove
        else None
      in
      let open BatOption.Infix in
      Slider.select model >>= remove_if_empty

    let option_msg_of_arrow = function
      | `Up    -> Some Prev
      | `Down  -> Some Next
      | _      -> None

    let option_msg_of_key model = function
      | `Escape     -> Some Esc
      | `Enter      -> Some Add
      | `Backspace  -> option_remove_of_model model
      | `Arrow dir  -> option_msg_of_arrow dir
      | _           -> None

    let of_state (event, model) =
      match event with
      | `Key (key, _) -> option_msg_of_key model key
      | _  -> None
  end (* Message *)

  module Update =
  struct
    type state  = Consolate_term.event * Model.t
    type return = (Model.t, Model.t option) result

    let empty_line = LE.Update.init

    let init = Slider.singleton empty_line

    let model_from_msg model : Message.t -> return =
      let open Message in
      function
      | Esc    -> Error None
      | Add    -> Ok (Composing.add model)
      | Remove -> Ok (Slider.remove model |> Composing.prev)
      | Next   -> Ok (Composing.next model)
      | Prev   -> Ok (Composing.prev model)

    let of_state : state -> return =
      fun (event, model) ->
        match Message.of_state (event, model) with
        | None     -> Ok model
        | Some msg -> model_from_msg model msg

  end (* Update *)

  module View =
  struct
    let (%) = Batteries.(%) (* compose *)

    let of_model : Model.t -> Notty.image =
      let of_unselected_lines line =
        line
        |> List.map (LE.View.of_uchars % LE.Model.to_chars)
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
end (* Prog *)
