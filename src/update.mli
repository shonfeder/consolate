type msg =
  | Next
  | Prev
  | Edit

type state = msg option * Model.t

val of_state : state -> Model.t
