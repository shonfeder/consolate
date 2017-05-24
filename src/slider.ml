type 'a t =
  | Empty
  | T of 'a list * 'a * 'a list

exception From_empty

let empty = Empty
let is_empty = function
  | Empty -> true
  | _     -> false

let singleton x = T ([], x, [])
let select = function
  | T (_, x, _) -> Some x
  | Empty       -> None

(* If we get to the end, we cannot advance further in either direction. *)
let fwd slider = match slider with
  | Empty ->
    Empty
  | T (_, _, []) ->
    slider
  | T (front,    x,  x'::back) ->
    T (x::front, x', back)

let rwd slider = match slider with
  | Empty ->
    Empty
  | T ([], _, _) ->
    slider
  | T (x'::front, x, back) ->
    T (front, x', x::back)

let reset = function
  | Empty -> Empty
  | T (front, x, back) ->
    match List.rev front with
    | [] ->
      T (front, x, back)
    | (y::front') ->
      T ([], y, front' @ (x::back))

let select_map f = function
  | Empty -> Empty
  | T (front, x, back) ->
    T (front, f x, back)

let map f = function
  | Empty -> Empty
  | T (front, x, back) ->
    T (List.map f front, f x, List.map f back)

let of_list = function
  | []      -> Empty
  | (x::xs) -> T ([], x, xs)

let to_list = function
  | Empty -> []
  | T (front, x, back) ->
    (List.rev front) @ (x :: back)

let front = function
  | Empty -> []
  | T (front, _, _) -> List.rev front

let back = function
  | Empty -> []
  | T (_, _, back) -> back

let fold_left f init slider =
  slider
  |> to_list
  |> List.fold_left f init

let insert y = function
  | Empty -> singleton y
  | T (front, x, back) ->
    T (y::front, x, back)

let remove = function
  | Empty | T ([], _, []) -> Empty
  | T (x::front, _, back) | T (front, _, x::back) ->
    T (front, x, back)
