open Notty
open Notty_unix

(* TODO save to file *)
(* TODO Change file name *)

module type Line_editor =
sig
  module Model : sig
    type t
    val empty : t
    val is_empty : t -> bool

    val at_start : t -> bool
    val at_end   : t -> bool

    val to_start : t -> t
    val to_end   : t -> t

    val of_string : string -> t
    val to_string : t -> string

    val to_chars : t -> Uchar.t list

    val front    : t -> Uchar.t list
    val back     : t -> Uchar.t list

    val append : t -> t -> t
    val split  : t -> t * t
  end

  module Return : Consolate_term.Return

  module Update : sig
    type state  = Consolate_term.event * Model.t
    type return = (Model.t, Model.t option) result
    val init : Model.t
    val load : string -> Model.t
    val of_state : state -> return
  end

  module View : sig
    val of_model  : Model.t -> Notty.image
    val of_uchars : Uchar.t list -> Notty.image
  end
end

module Prog (LE:Line_editor) =
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
  end (* Model *)

  module Return =
  struct
    type t = Model.t option
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
      | FwdLine
      | BwdLine
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

    let option_fwd model =
      match Slider.select model with
      | None      -> None
      | Some line ->
        if LE.Model.at_end line
        then Some FwdLine
        else None

    let option_bwd model =
      match Slider.select model with
      | None      -> None
      | Some line ->
        if LE.Model.at_start line
        then Some BwdLine
        else None

    let option_msg_of_arrow model = function
      | `Up    -> Some Prev
      | `Down  -> Some Next
      | `Left  -> option_bwd model
      | `Right -> option_fwd model
      | _      -> None

    let option_msg_of_key model = function
      | `Escape     -> Some Quit
      | `Enter      -> Some Add
      | `Backspace  -> option_remove_of_model model
      | `Arrow dir  -> option_msg_of_arrow model dir
      | _           -> None

    let of_state (event, model) =
      match event with
      | `Key (key, _) -> option_msg_of_key model key
      | _  -> None
  end (* Message *)

  module Update =
  struct
    include Consolate_term.Make.Update.Types (Model) (Return)

    let empty_line = LE.Update.init

    let init = Slider.singleton empty_line

    let append_to_selected_line line model =
      Slider.select_map (fun selected -> LE.Model.append selected line) model

    let prev : Model.t -> Model.t = Slider.bwd
    let next : Model.t -> Model.t = Slider.fwd_till_last

    let fwd_line model =
      model
      |> Slider.fwd
      |> Slider.select_map LE.Model.to_start
    let bwd_line model =
      model
      |> Slider.bwd
      |> Slider.select_map LE.Model.to_end

    let add model =
      let (front, back) =
        let line = Slider.select model in
        Option.default LE.Update.init line
        |> LE.Model.split
      in
      model
      |> Slider.replace front
      |> Slider.fwd
      |> Slider.insert back

    let remove model =
      let removed_line =
        match Slider.select model with
        | None      -> LE.Model.empty
        | Some line -> line
      in
      let append_removed_line =
        append_to_selected_line removed_line
      in
      model
      |> Slider.remove
      |> prev
      |> append_removed_line

    let model_from_msg model : Message.t -> return =
      let open Message in
      function
      | Quit    -> Error None
      | Add     -> Ok (add model)
      | Remove  -> Ok (remove model)
      | Next    -> Ok (next model)
      | Prev    -> Ok (prev model)
      | FwdLine -> Ok (fwd_line model)
      | BwdLine -> Ok (bwd_line model)

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
