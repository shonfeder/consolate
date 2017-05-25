(** A slider is a zipper which cannot be unzipped (unless it's empty)
    If there are elements, then one element must always be in focus.*)
type 'a t = 'a list * 'a list

exception From_empty

let empty = ([],[])
let is_empty = function
  | ([],[]) -> true
  | _       -> false

let singleton x = ([], [x])
let select = function
  | (_, x::_) -> Some x
  | (_, [])   -> None
    (* A slider should never have an empty back, unless it's empty,
       otherwise it would have elements but nothing selected. *)

(* If we get to the end, we cannot advance further in either direction. *)
let fwd slider = match slider with
  | (_,[_]) | (_,[]) -> slider
  | (front, x::back) -> (x::front, back)

let bwd slider = match slider with
  | ([], _)          -> slider
  | (x::front, back) -> (front, x::back)

(* TODO Test *)
let reset (front, back) = ([], List.rev front @ back)

let select_map f = function
  | (_,[]) -> ([],[])
  | (front, x::back) -> (front, (f x)::back)

let map f (front, back) = (List.map f front, List.map f back)

let of_list list = ([], list)

let to_list (front, back) = (List.rev front) @ back

let front (front, _)  = List.rev front
let back = function
  | (_, x::back) -> back
  | _ -> []

let fold_left f init slider =
  slider
  |> to_list
  |> List.fold_left f init

let insert x (front, back) = (front, x::back)

let remove = function
  | (_, []) -> ([],[])
  | (front, x::back) -> (front, back)
