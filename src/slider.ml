type 'a t = T of 'a list * 'a * 'a list
          | E

let empty = E
let is_empty = function
  | E -> true
  | _ -> false

let select = function
  | E           -> None
  | T (_, x, _) -> x

let fwd = function
  | E ->
    Some E
  | T (_, _, []) ->
    None
  | T (front, x, x'::back) ->
    Some (T (x::front, x', back))

let rwd = function
  | E ->
    Some E
  | T ([], _, _) ->
    None
  | T (x'::front, x, back) ->
    Some (T (front, x', x::back))

let select_map f = function
  | E -> E
  | T (front, x, back) ->
    T (front, f x, back)

let map f = function
  | E -> E
  | T (front, x, back) ->
    T (List.map f front, f x, List.map f back)

let of_list = function
  | []    -> E
  | x::xs -> T ([], x, xs)

let to_list = function
  | E -> []
  | T (front, x, back) -> (List.rev front) @ (x :: back)

let fold_left f init slider = slider
                              |> to_list
                              |> List.fold_left f init
                              |> of_list
