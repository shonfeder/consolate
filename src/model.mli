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

val select   : field -> field
val edit     : field -> field
val deselect : field -> field

val select_fieldset   : fieldset -> fieldset
val edit_fieldset     : fieldset -> fieldset
val deselect_fieldset : fieldset -> fieldset

val of_json : json -> t
val to_json : t    -> json
