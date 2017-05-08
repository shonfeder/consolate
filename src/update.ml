open Model
type msg =
  | Next
  | Prev
  | Edit
  | Esc
  | Quit

type state = msg option * Model.t
exception Unimplemented

type dir = Fwd | Rwd

(* let advance_field dir = match field with *)
(*   | {state = Editing}  -> {field with fields = advance_value field.fields} *)
(*   | {state = Selected} -> *)
let advance_slider dir unset set slider =
      let direction = match dir with
        | Fwd -> Slider.fwd
        | Rwd -> Slider.rwd
      in
      Slider.(slider
              |> select_map unset
              |> direction
              |> select_map set)

let advance_field dir (fields:fields) : fields =
  let field = Slider.select fields in
  match field with
  | {state = Displayed} -> Slider.select_map select fields
  | {state = Selected}  -> advance_slider dir deselect select fields
  | {state = Editing}   -> raise Unimplemented

let advance_value dir = function
  | Fieldset fields -> Fieldset (advance_field dir fields)
  | _ -> raise Unimplemented

let advance_fieldsets dir (fieldsets : fieldsets) : fieldsets =
  match Slider.select fieldsets with
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

let edit = function
  | Config slider   -> Config Slider.(select_map edit_fieldset slider)
  | _ -> raise Unimplemented

let esc_fieldset fieldset = raise Unimplemented

let esc model = match model with
  | Config slider -> Config Slider.(select_map esc_fieldset slider)
  | _ -> raise Unimplemented

let of_state : state -> Model.t = function
  | (None, m)      -> m
  | (Some Next, m) -> next m
  | (Some Prev, m) -> prev m
  | (Some Edit, m) -> edit m
  (* | (Some Esc,  m) -> esc m *)
  | (_, m)         -> m
