type msg =
  | Next
  | Prev
  | Edit
  | Esc
  | Quit

type state = msg option * Model.t

val of_state : state -> Model.t
