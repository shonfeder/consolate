open Notty
open Notty_unix

module M = Model
module V = View
module U = Update

let json = Json_io.Load.file "src/data/example.json"
let init = (None, M.of_json json)

(* let view  = V.of_model model *)
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

(* state |> U.of_state (msg, model) |> V.of_model |> Term.image *)

type ctl = Q | J | K | Noop

let uchar_code_to_ctl code =
  match Uchar.to_int code with
  | 113 -> Q
  | 106 -> J
  | 107 -> K
  | _   -> Noop

let rec update term state =
  let model = U.of_state state in
  let image = V.of_model model
  in
  ( Term.image term image
  ; loop term model)
and loop term model =
  match Term.event term with
  (* | `Key (`Uchar ch _) -> *)
  (*   ( output_image_endline (I.string A.empty (string_of_int (Uchar.to_int ch))) *)
  (*                          ; update term state ) *)
  | `Key (`Escape, _) -> ()
  | `Key (`Uchar code, _) ->
    ( match uchar_code_to_ctl code with
      | Q -> ()
      | J -> update term (Some U.Next, model)
      | K -> update term (Some U.Prev, model)
      | Noop -> update term (None, model) )
  | _ -> update term (None, model)

let show_uchar c = Uchar.(to_int @@ of_char c)

let run () =
  let term = Term.create() in
  ( update term init
  ; Term.release term)
