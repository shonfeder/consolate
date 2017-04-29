type msg =
  | Next
  | Prev
  | Edit

type state = msg option * Model.t

let next = let open Model in
  function
  | Multi slider  -> Multi (Slider.select_map select_fieldset slider)
  | Mono fieldset -> Mono (select_fieldset fieldset)

let of_state = function
  | (None, m) -> m
  | (Some Next, m) -> next m
  | (_,m)     -> m
