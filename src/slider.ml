(** A slider is a zipper with some additional operations *)
type 'a t = 'a list * 'a list

exception From_empty

let empty = ([],[])
let is_empty = function
  | ([],[]) -> true
  | _       -> false
let at_last = function
  | (_, [x]) -> true
  | _       -> false

let singleton x = ([], [x])
let select = function
  | (_, x::_) -> Some x
  | (_, [])   -> None

let fwd slider = match slider with
  | (_,[]) -> slider
  | (front, x::back) -> (x::front, back)

let bwd slider = match slider with
  | ([], _)          -> slider
  | (x::front, back) -> (front, x::back)

(* TODO Test *)
let reset (front, back) = ([], List.rev front @ back)

let select_map f = function
  | (front,[])       -> (front,[])
  | (front, x::back) -> (front, (f x)::back)

let map f (front, back) = (List.map f front, List.map f back)

let of_list list = ([], list)

let to_list (front, back) = (List.rev front) @ back

let front (front, _)  = List.rev front
let back = function
  | (_, x::back) -> back
  | _            -> []

let fold_left f init slider =
  slider
  |> to_list
  |> List.fold_left f init

let insert x = function
  | (front, [])   -> (front, [x])
  | (front, back) -> (front, x::back)

let remove = function
  | (_, [])          -> ([],[])
  | (front, x::back) -> (front, back)
