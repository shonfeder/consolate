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
(*   | {state = Editing}  -> {field with value = advance_value field.value} *)
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

let advance_field dir fields =
  let field = Slider.select fields in
  match field with
  | {state = Displayed} -> Slider.select_map select fields
  | {state = Selected}  -> advance_slider dir deselect select fields
  | {state = Editing}   -> raise Unimplemented

let advance_value dir = function
  | Fieldset fields -> Fieldset (advance_field dir fields)
  | _ -> raise Unimplemented

let advance_fieldset dir = function
  | (Editing,  value)  -> (Editing, advance_value dir value)
  | (_, _) as fieldset -> select_fieldset fieldset

let advance_model dir = function
  | Multi slider  ->
    Multi
      (match Slider.select slider with
       | (Editing, _) ->
         Slider.select_map (advance_fieldset dir) slider
       | (_, _)       ->
         advance_slider dir deselect_fieldset select_fieldset slider)
  | Mono fieldset ->
    Mono
      (advance_fieldset dir fieldset)

let next = advance_model Fwd
let prev = advance_model Rwd

let edit = function
  | Multi slider   -> Multi Slider.(select_map edit_fieldset slider)
  | Mono  fieldset -> Mono (edit_fieldset fieldset)

let of_state = function
  | (None, m)      -> m
  | (Some Next, m) -> next m
  | (Some Prev, m) -> prev m
  | (Some Edit, m) -> edit m
  | (_, m)         -> m
