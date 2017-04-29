open Notty
open Notty_unix

let json = Json_io.Load.file "src/data/example.json"
let init = (Some Update.Next, Model.of_json json)

(* let view  = View.of_model model *)
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

(* state |> Update.of_state (msg, model) |> View.of_model |> Term.image *)
let rec update term state =
  let (msg, model) = state in
  ( state |> Update.of_state
          |> View.of_model
          |> Term.image term
  ; loop term model
  )
and loop term model =
  match Term.event term with
  (* | `Key (`Uchar ch _) -> *)
  (*   ( output_image_endline (I.string A.empty (string_of_int (Uchar.to_int ch))) *)
  (*                          ; update term state ) *)
  | `Key (`Escape, _) -> ()
  | `Key (`Uchar code, _) ->
    ( match Uchar.to_int code with
      | 113 -> update term (Some Update.Next, model)
      | _ -> update term (None, model) )
  | _ -> update term (None, model)

let show_uchar c = Uchar.(to_int @@ of_char c)

let run () =
  let term = Term.create() in
  ( update term init
  ; Term.release term)
