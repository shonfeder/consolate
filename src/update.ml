type msg =
  | Next
  | Prev
  | Edit

type state = msg option * Model.t

let advance dir = let open Model in
  function
  | Multi slider  ->
    let slider' =
      Slider.(slider
              |> select_map deselect_fieldset
              |> dir
              |> select_map select_fieldset)
    in
    Multi slider'
  | Mono fieldset -> Mono (select_fieldset fieldset)

let next = advance Slider.fwd
let prev = advance Slider.rwd

let of_state = function
  | (None, m)      -> m
  | (Some Next, m) -> next m
  | (Some Prev, m) -> prev m
  | (_, m)         -> m
