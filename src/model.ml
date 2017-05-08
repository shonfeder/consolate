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

let select   field = {field with state = Selected}
let edit     field = {field with state = Editing}
let deselect field = {field with state = Displayed}

let fieldset_is_selected = function
  | (Selected, _) -> true
  | _ -> false
let fieldset_is_editing = function
  | (Editing, _) -> true
  | _ -> false
let fieldset_is_displayed = function
  | (Displayed, _) -> true
  | _ -> false

let select_fieldset   (state, fieldset) = (Selected, fieldset)
let deselect_fieldset (state, fieldset) =
  match state with
  | Selected | Displayed -> (Displayed, fieldset)
  | Editing              -> (Selected, fieldset)
let edit_fieldset     (state, fieldset) =
  (Editing, (Slider.select_map select fieldset))

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
  List.map to_field assoc
  |> Slider.of_list

let of_assoc_exn = function
  | `Assoc assoc -> of_assoc assoc
  | _            -> raise Invalid_config

(* A valid config is an object, or a list of objects *)
let rec of_json : json -> t =
  let deselected fieldset = (Displayed, fieldset) in
  function
  | `List fieldsets ->
    Config (fieldsets
            |> List.map (deselected % of_assoc_exn)
            |> Slider.of_list
            |> Slider.select_map select_fieldset)
  | `Assoc fieldset ->
    Config ((Selected, of_assoc fieldset)
           |> Slider.singleton )
  (*   Mono (`Assoc fieldset *)
  (*         |> deselected *)
  (*         |> select_fieldset) *)
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


(* let rec to_json : t -> json = *)
(*   let from_fieldset (_, fieldset) = to_json fieldset *)
(*   in *)
(*   function *)
(*   | Multi fieldsets -> `List (fieldsets *)
(*                               |> Slider.to_list *)
(*                               |> List.map from_fieldset) *)
(*   | Mono  fieldset  -> from_fieldset fieldset *)

