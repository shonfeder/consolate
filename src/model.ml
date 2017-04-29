open Json_t

exception Invalid_config

type state =
  | Selected
  | Editing
  | Display

type value =
  | Fieldset of field Slider.t
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

type fieldset = state * value

type t =
  | Multi of fieldset Slider.t
  | Mono  of fieldset

let select   field = {field with state = Selected}
let edit     field = {field with state = Editing}
let deselect field = {field with state = Display}

let select_fieldset   (state, fieldset) = (Selected, fieldset)
let edit_fieldset     (state, fieldset) = (Editing, fieldset)
let deselect_fieldset (state, fieldset) = (Display, fieldset)

let rec of_value = function
  | `Assoc a  -> of_assoc a
  | `Bool b   -> Bool b
  | `Float f  -> Float f
  | `Int i    -> Int i
  | `List l   -> List (List.map of_value l)
  | `String s -> Str s
  | `Null     -> Null
  | _         -> Null (* XXX *)
(* | `Tuple  t -> of_assoc t *)
(*   (\* |  -> of_string string *\) *)
(*   (\* | `Variant of string * json option *\) *)
(*   (\* | `Floatlit str  -> I.string float_style str *\) *)
and of_assoc assoc =
  let to_field (label, json) =
    { label
    ; value = of_value json
    ; state = Display }
  in
  Fieldset (List.map to_field assoc
            |> Slider.of_list)

(* A valid config is a field set, or a list of fieldsets *)
let rec of_json : json -> t =
  let deselected fieldset = (Display, of_value fieldset)
  in
  function
  | `List fieldsets ->
    Multi (fieldsets
           |> List.map deselected
           |> Slider.of_list
           |> Slider.select_map select_fieldset)
  | `Assoc fieldset ->
    Mono (`Assoc fieldset
          |> deselected
          |> select_fieldset)
  | _ ->
    raise Invalid_config

let rec from_value = function
  | Fieldset fs -> to_assoc fs
  | Bool b      -> `Bool b
  | Float f     -> `Float f
  | Int i       -> `Int i
  | List l      -> `List (List.map from_value l)
  | Str s       -> `String s
  | Null        -> `Null
and to_assoc fieldset =
  let to_pair field = (field.label, from_value field.value)
  in
  `Assoc (fieldset |> Slider.to_list |> List.map to_pair )

let rec to_json : t -> json =
  let from_fieldset (_, fieldset) = from_value fieldset
  in
  function
  | Multi fieldsets -> `List (fieldsets
                              |> Slider.to_list
                              |> List.map from_fieldset)
  | Mono  fieldset  -> from_fieldset fieldset

