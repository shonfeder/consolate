open Json_t

let (%) = Batteries.(%) (* compose *)

exception Invalid_config

type state =
  | Selected
  | Editing
  | Displayed

type value =
  | Config   of fieldsets
  | Fieldset of fields
  | Bool     of bool
  | Float    of float
  | Int      of int
  | List     of value list
  | Str      of string
  | Null
and field =
  { label : string
  ; value : value
  ; state : state }
and fields =
  field Slider.t
and fieldset =
  state * fields
and fieldsets =
  fieldset Slider.t

type t = value

let is_selected = function
  | {state = Selected} -> true
  | _ -> false
let is_editing = function
  | {state = Editing} -> true
  | _ -> false
let is_displayed = function
  | {state = Displayed} -> true
  | _ -> false

let fieldset_is_selected = function
  | (Selected, _) -> true
  | _ -> false
let fieldset_is_editing = function
  | (Editing, _) -> true
  | _ -> false
let fieldset_is_displayed = function
  | (Displayed, _) -> true
  | _ -> false

let rec of_value = function
  | `Assoc a  -> Fieldset (of_assoc a)
  | `Bool b   -> Bool b
  | `Float f  -> Float f
  | `Int i    -> Int i
  | `List l   -> List (List.map of_value l)
  | `String s -> Str s
  | `Null     -> Null
  | _         -> raise Invalid_config
and of_assoc assoc =
  let to_field (label, json) =
    { label
    ; value = of_value json
    ; state = Displayed }
  in
  assoc
  |> List.map to_field
  |> Slider.of_list

let of_assoc_exn = function
  | `Assoc assoc -> of_assoc assoc
  | _            -> raise Invalid_config

(* A valid config is an object, or a list of objects *)
let rec of_json : json -> t =
  let deselected fieldset = (Displayed, fieldset) in
  let select_fieldset (state, fieldset) = (Selected, fieldset) in
  let select   field = {field with state = Selected} in
  function
  | `List fieldsets ->
    Config (fieldsets
            |> List.map (deselected % of_assoc_exn)
            |> Slider.of_list
            |> Slider.select_map select_fieldset)
  | `Assoc fieldset ->
    Fieldset (fieldset
              |> of_assoc
              |> Slider.select_map select)
  | _ ->
    raise Invalid_config

let rec to_json = function
  | Config fss  -> `List (from_fieldsets fss)
  | Fieldset fs -> to_assoc fs
  | Bool b      -> `Bool b
  | Float f     -> `Float f
  | Int i       -> `Int i
  | List l      -> `List (List.map to_json l)
  | Str s       -> `String s
  | Null        -> `Null
and to_assoc fieldset =
  let to_pair field = (field.label, to_json field.value)
  in
  `Assoc (fieldset |> Slider.to_list |> List.map to_pair)
and from_fieldsets fieldsets =
  fieldsets |> Slider.to_list |> List.map snd |> List.map to_assoc
