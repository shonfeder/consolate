module Return = Json_config_return
module Model  = Json_config_model

open Model

type msg =
  | Next
  | Prev
  | Edit
  | Esc
  | Quit

include Consolate_term.Make.Update.Types (Model) (Return)
(* type state = msg option * Model.t *)
exception Unimplemented

type dir = Fwd | Rwd

let test_data = Json_io.Load.file "test/data/example.json"

(* TODO Implement full editing capabilities *)
let edit_value = function
  | Str s -> Str "EDITING!!!"
  | _  -> raise Unimplemented

let select   field = {field with state = Selected}
let edit     field =
  match field.state with
  | Selected | Displayed -> {field with state = Editing}
  | Editing              ->
    {field with state = Editing;
                value = edit_value field.value}
let deselect field =
  match field.state with
  | Selected | Displayed -> {field with state = Displayed}
  | Editing              -> {field with state = Selected}

let select_fieldset (state, fieldset) = (Selected, fieldset)
let edit_fieldset (state, fieldset) =
  match state with
  | Displayed -> (Displayed, fieldset)
  | Selected  -> (Editing, (Slider.select_map select fieldset))
  | Editing   -> (Editing, (Slider.select_map edit fieldset))
let deselect_fieldset (state, fieldset) =
  match state with
  | Selected | Displayed -> (Displayed, Slider.reset fieldset)
  | Editing ->
    match (Slider.select fieldset) with
    | None -> (Selected, (Slider.select_map deselect fieldset))
    | Some field ->
      if is_editing field
      then (Editing,  (Slider.select_map deselect fieldset))
      else (Selected, (Slider.select_map deselect fieldset))

let advance_slider dir unset set slider =
      let direction = match dir with
        | Fwd -> Slider.fwd
        | Rwd -> Slider.bwd
      in
      Slider.(slider
              |> select_map unset
              |> direction
              |> select_map set)

let advance_int dir int =
  match dir with
  | Fwd -> int + 1
  | Rwd -> int - 1

let rec advance_field dir (fields:fields) : fields =
  (* TODO Handle None properly  *)
  let field = Option.get @@ Slider.select fields in
  match field.state with
  | Displayed -> Slider.select_map select fields
  | Selected  -> advance_slider dir deselect select fields
  | Editing   -> Slider.select_map (advance_value dir) fields
and advance_value dir field =
  let value =
    match field.value with
    | Config _ -> raise Unimplemented
    | Fieldset fields -> Fieldset (advance_field dir fields)
    | Bool bool       -> Bool (not bool)
    | Int int         -> Int (advance_int dir int)
    | List list       -> raise Unimplemented
    | _ ->  raise Unimplemented
  in
  {field with value = value}

let advance_value dir = function
  | Fieldset fields -> Fieldset (advance_field dir fields)
  | _ -> raise Unimplemented

let advance_fieldsets dir (fieldsets : fieldsets) : fieldsets =
  (* TODO Handle None properly  *)
  match Option.get @@ Slider.select fieldsets with
  | (Editing,  fields)  ->
    let fields' = advance_field dir fields in
    Slider.select_map (fun _ -> (Editing, fields')) fieldsets
  | (_, _) ->
    advance_slider dir deselect_fieldset select_fieldset fieldsets

let advance dir = function
  | Config fieldsets -> Config (advance_fieldsets dir fieldsets)
  | _ -> raise Unimplemented

let next = advance Fwd
let prev = advance Rwd

let edit_model = function
  | Config slider -> Config Slider.(select_map edit_fieldset slider)
  | _             -> raise Unimplemented

let esc_fieldset fieldset = deselect_fieldset fieldset

let esc model = match model with
  | Config slider ->
    (* TODO Handle None properly  *)
    if fieldset_is_selected (Option.get @@ Slider.select slider)
    then Config Slider.(slider
                        |> select_map deselect_fieldset
                        |> reset)
    else Config Slider.(select_map esc_fieldset slider)
  | _ -> raise Unimplemented

(* TODO Make reconfigurable bindings *)
let uchar_code_to_msg code =
  match Uchar.to_int code with
  | 113 -> Some Quit
  | 106 -> Some Next
  | 107 -> Some Prev
  | 101 -> Some Edit
  | _   -> None

let message_of_state (event, model) =
  match event with
  | `Key (`Escape, _)     -> (Some Esc, model)
  | `Key (`Uchar code, _) -> (uchar_code_to_msg code, model)
  | _ -> (None, model)

let of_message_model (option_msg, model) =
  match option_msg with
  | Some Quit -> Error None
  | Some Esc  -> Ok (esc model)
  | Some Next -> Ok (next model)
  | Some Prev -> Ok (prev model)
  | Some Edit -> Ok (edit_model model)
  | _         -> Ok (model)

let init = Model.of_json test_data

let load _ = init

let of_state : state -> (Model.t, Model.t option) result =
  fun state ->
    state
    |> message_of_state
    |> of_message_model

