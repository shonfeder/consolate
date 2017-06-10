type msg =
  | Next
  | Prev
  | Edit
  | Esc
  | Quit

type state = msg option * Json_config_model.t

val of_state : state -> Json_config_model.t
