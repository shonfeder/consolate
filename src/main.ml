open Notty
open Notty_unix

module M = Model
module V = View
module U = Update

let json = Json_io.Load.file "src/data/example.json"
let init = (None, M.of_json json)

(* let view  = V.of_model model *)

(* TODO Take screen size into account *)
(* let () = Notty_unix.output_image_size (fun (_,_) -> view) *)

(* let rec update t state = *)
(*   Term.image t (img state); loop t state *)
(* and loop t (double, n as state) = *)
(*   match Term.event t with *)
(*   | `Key (`Enter,_)        -> () *)
(*   | `Key (`Arrow `Left,_)  -> update t (double, max 1 (n - 1)) *)
(*   | `Key (`Arrow `Right,_) -> update t (double, min 8 (n + 1)) *)
(*   | `Key (`Uchar 0x20,_)   -> update t (not double, n) *)
(*   | `Resize _              -> update t state *)
(*   | _                      -> loop t state *)
(* in *)
(* let t = Term.create () in *)
(* update t (false, 1); *)
(* Term.release t *)

(* TODO Replace with maps to messages *)
(* TODO Make reconfigurable bindings *)
type ctl = Q | J | K | E | Noop

let uchar_code_to_ctl code =
  match Uchar.to_int code with
  | 113 -> Q
  | 106 -> J
  | 107 -> K
  | 101 -> E
  | _   -> Noop

let rec update term state =
  match state with
  | (Some U.Quit, _) -> ()
  | (None, model)    -> display_loop term model
  | _ -> U.of_state state
         |> display_loop term
and display_loop term model =
    let image = V.of_model model in
    ( Term.image term image
    ; loop term model)
and loop term model =
  update term @@
  match Term.event term with
  | `Key (`Escape, _)     -> (Some U.Esc, model)
  | `Key (`Uchar code, _) ->
    (* TODO Refractor with code_to_msg : code -> U.msg *)
    ( match uchar_code_to_ctl code with
      | Q -> (Some U.Quit, model)
      | J -> (Some U.Next, model)
      | K -> (Some U.Prev, model)
      | E -> (Some U.Edit, model)
      | Noop -> (None, model) )
  | _ -> (None, model)

let show_uchar c = Uchar.(to_int @@ of_char c)

let run () =
  let term = Term.create() in
  ( update term init
  ; Term.release term)
