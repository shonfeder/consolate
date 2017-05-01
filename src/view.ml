open Notty
open Json_t

module M = Model

let (%) = Batteries.(%) (* compose *)
let id x = x

let stack (convert:'a -> Notty.image) =
  let stack_img imgs img = I.(imgs <-> convert img) in
  List.fold_left stack_img I.empty

let label_length f = String.length M.(f.label)

(** Determines the style of content based on the state of
    the enclosing fieldset.

    In all following functions `s` = "state".
*)
let bg_of_state = function
  | M.Displayed -> A.(bg black)
  | M.Selected  -> A.(bg (rgb 0 0 5))
  | M.Editing   -> A.(bg (rgb 4 5 4))

let default_style s = A.(fg white       ++ bg_of_state s)
let error_style s   = A.(fg red         ++ bg_of_state s)
let bool_style s    = A.(fg (rgb 0 0 3) ++ bg_of_state s)
let float_style s   = A.(fg (rgb 0 1 4) ++ bg_of_state s)
let int_style s     = A.(fg (rgb 0 1 5) ++ bg_of_state s)
let null_style s    = A.(fg (rgb 0 1 0) ++ bg_of_state s)
let str_style s     = A.(fg (rgb 0 3 1) ++ bg_of_state s)

let lable_bg_of_state = function
  | M.Displayed -> A.(bg (gray 3))
  | M.Selected  -> A.(bg (rgb 0 0 5))
  | M.Editing   -> A.(bg (rgb 4 5 4))
let label_style s = A.(st bold ++ lable_bg_of_state s)

let of_bool s  = I.string (bool_style s) % function
    | true  -> "True"
    | false -> "False"
let of_float s = I.string (float_style s) % string_of_float
let of_int s   = I.string (int_style s)   % string_of_int
let of_str s   = I.string (str_style s)
let null s     = I.string (null_style s) "NULL"

let of_label s w str =
  let colon = I.(pad ~r:1 @@ string (label_style s) " :") in
  let label = I.(hsnap ~align:`Right w @@ string (label_style s) str) in
  I.(label <|> colon)

let rec of_value s = function
  | M.Fieldset fs -> of_fieldset s fs
  | M.Bool b      -> of_bool s b
  | M.Float f     -> of_float s f
  | M.Int i       -> of_int s i
  | M.List l      -> of_list s l
  | M.Str str     -> of_str s str
  | M.Null        -> null s
and of_list s list =
  let init = match list with
    | []         -> I.empty
    | (i::list') -> of_value s i
  in
  let sep = I.string (default_style s) ", " in
  let open_bracket  = I.string (default_style s) "[" in
  let close_bracket = I.string (default_style s) "]" in
  let append_item items item = I.(items <|> sep <|> of_value s item) in
  let items = List.fold_left append_item init list
  in
  I.(open_bracket <|> items <|> close_bracket)
and of_fieldset s fieldset =
  let width    = fieldset
                 |> Slider.to_list
                 |> List.map label_length
                 |> BatList.max
  in
  let of_label = of_label s width in
  let open M in
  let of_field f = I.(of_label f.label <|> of_value f.state f.value)
  in
  I.pad ~t:1 ~l:4 @@ stack of_field @@ Slider.to_list fieldset

let of_model : Model.t -> Notty.image =
  let value_of_fieldset (state, fieldset) = of_value state fieldset in
  function
  | M.Multi fieldsets -> stack value_of_fieldset @@ Slider.to_list fieldsets
  | M.Mono fieldset   -> value_of_fieldset fieldset
