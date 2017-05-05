open Notty
open Notty_unix

module M = Model
module V = View
module U = Update

let json = Json_io.Load.file "test/data/example.json"
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

(* TODO Make reconfigurable bindings *)
let uchar_code_to_msg code =
  match Uchar.to_int code with
  | 113 -> Some U.Quit
  | 106 -> Some U.Next
  | 107 -> Some U.Prev
  | 101 -> Some U.Edit
  | _   -> None

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
  | `Key (`Uchar code, _) -> (uchar_code_to_msg code, model)
  | _ -> (None, model)

let show_uchar c = Uchar.(to_int @@ of_char c)

let run () =
  let term = Term.create() in
  ( update term init
  ; Term.release term)
