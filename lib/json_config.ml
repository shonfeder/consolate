open Notty
open Notty_unix

module Model  = Json_config_model
module Return = Json_config_return
module View   = Json_config_view
module Update = Json_config_update


(* let rec update term state = *)
(*   match state with *)
(*   | (Some Update.Quit, _) -> () *)
(*   | (None, model)    -> display_loop term model *)
(*   | _ -> Update.of_state state *)
(*          |> display_loop term *)
(* and display_loop term model = *)
(*     let image = View.of_model model in *)
(*     ( Term.image term image *)
(*     ; loop term model) *)
(* and loop term model = *)
(*   update term @@ *)
(*   match Term.event term with *)
(*   | `Key (`Escape, _)     -> (Some Update.Esc, model) *)
(*   | `Key (`Uchar code, _) -> (uchar_code_to_msg code, model) *)
(*   | _ -> (None, model) *)

(* let run () = *)
(*   let term = Term.create() in *)
(*   ( update term Update.init *)
(*   ; Term.release term) *)
