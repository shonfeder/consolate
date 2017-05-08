type 'a t = 'a list * 'a * 'a list

exception From_empty

let singleton x      = ([], x, [])
let select (_, x, _) = x

(* If we get to the end, we cannot advance further in either direction. *)
let fwd slider = match slider with
  | (_, _, []) ->
    slider
  | (front,    x,  x'::back) ->
    (x::front, x', back)

let rwd slider = match slider with
  | ([], _, _) ->
    slider
  | (x'::front, x, back) ->
    (front, x', x::back)

let select_map f (front, x, back) = (front, f x, back)

let map f (front, x, back) = (List.map f front, f x, List.map f back)

(* let replace_selected y slider = select_map (fun _ -> y) slider *)

let of_list = function
  | []      -> raise From_empty
  | (x::xs) -> ([], x, xs)

let to_list (front, x, back) =
  (List.rev front) @ (x :: back)

let fold_left f init slider =
  slider
  |> to_list
  |> List.fold_left f init
