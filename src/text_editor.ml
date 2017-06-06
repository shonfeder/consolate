open Notty
open Notty_unix

(* TODO Load from and save to file *)
(* TODO Change file name *)

module Prog (LE:Line_editor.T) =
struct

  module Model =
  struct
    type t = LE.Model.t Slider.t

    let of_file : string -> t =
      let add_line_to_slider slider line =
        let model_of_line = LE.Model.of_string line in
        Slider.insert model_of_line slider
      in
      fun file_name ->
        file_name
        |> BatFile.lines_of
        |> BatEnum.fold add_line_to_slider Slider.empty

    let to_file : string -> t -> unit =
      fun file_name model ->
        let oc = open_out file_name in
        let write_line_to_file line =
          line
          |> LE.Model.to_string
          |> Printf.fprintf oc "%s\n"
        in
        ( model
          |> Slider.to_list
          |> List.iter write_line_to_file
        ; close_out oc )

  end

  module Composing =
  struct
    module Component = LE
    let selected : Model.t -> LE.Model.t option = Slider.select
    let replace (replacement : LE.Model.t) : Model.t -> Model.t =
      (Slider.replace replacement)
  end

  module Message =
  struct
    type t =
      | Quit
      | Add
      | Remove
      | Next
      | Prev
      (* TODO *)
      (* | Menu *)

    (* TODO Append remaining line to previous when cursor at beginning *)
    let option_remove_of_model model =
      let at_beginning line = (LE.Model.front line = []) in
      let remove_if_at_beginning line =
        if at_beginning line
        then Some Remove
        else None
      in
      let open BatOption.Infix in
      Slider.select model >>= remove_if_at_beginning

    let option_msg_of_arrow = function
      | `Up    -> Some Prev
      | `Down  -> Some Next
      | _      -> None

    let option_msg_of_key model = function
      | `Escape     -> Some Quit
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

    let remove_line model =
      let removed_line =
        match Slider.select model with
        | None      -> LE.Model.empty
        | Some line -> line
      in
      let append_removed_line =
        Slider.select_map (fun line -> LE.Model.append line removed_line)
      in
      model
      |> Slider.remove
      |> Composing.prev
      |> append_removed_line

    let model_from_msg model : Message.t -> return =
      let open Message in
      function
      | Quit    -> Error None
      | Add    -> Ok (Composing.add model)
      | Remove -> Ok (remove_line model)
      | Next   -> Ok (Composing.next model)
      | Prev   -> Ok (Composing.prev model)

    let of_state : state -> return =
      fun (event, model) ->
        match Message.of_state (event, model) with
        | None     -> Ok model
        | Some msg -> model_from_msg model msg

    let load : string -> Model.t = Model.of_file


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
