(* type json = [ `Assoc of (string * json) list *)
(*        | `Bool of bool *)
(*        | `Float of float *)
(*        | `Floatlit of string *)
(*        | `Int of int *)
(*        | `Intlit of string *)
(*        | `List of json list *)
(*        | `Null *)
(*        | `String of string *)
(*        | `Stringlit of string *)
(*        | `Tuple of json list *)
(*        | `Variant of string * json option ]  *)

open Json_t

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

val is_selected  : field -> bool
val is_editing   : field -> bool
val is_displayed : field -> bool

val fieldset_is_selected  : fieldset -> bool
val fieldset_is_editing   : fieldset -> bool
val fieldset_is_displayed : fieldset -> bool

val of_json : json -> t
val to_json : t    -> json
