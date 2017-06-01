open Notty
open Notty_unix

module Make (Prog:Consolate_term.Program) =
struct
  module Model =
  struct
    type t = Prog.Model.t Slider.t

    let prev = Slider.bwd
    let next = Slider.fwd_till_last
  end (* Model *)

  module Message =
  struct
    type t =
      | Prev | Next | Add | Esc

    let of_arrow = function
      | `Up   -> Some Prev
      | `Down -> Some Next
      | _     -> None

    let of_event = function
      | `Key (`Escape, _)    -> Some Esc
      | `Key (`Arrow dir, _) -> of_arrow dir
      | `Key (`Enter, _)     -> Some Add
      | _ -> None

  end (* Message *)

  module Update =
  struct
    type state  = Consolate_term.event * Model.t
    type return = (Model.t, Model.t option) result

    let init = Slider.empty |> Slider.insert Prog.Update.init

    let result_of_msg : Message.t -> Model.t -> return = function
      | Message.Prev -> (fun m -> Ok (Model.prev m))
      | Message.Next -> (fun m -> Ok (Model.next m))
      | Message.Esc  -> (fun _ -> Error None)
      | Message.Add  -> (fun m -> Ok (Slider.insert Prog.Update.init m))

    let result_of_prog_event event model =
      match Slider.select model with
      | None          -> Ok model
      | Some selected ->
        let selected' =
          match Prog.Update.of_state (event, selected) with
            | Ok selected'           -> selected'
            | Error (Some selected') -> selected'
            | Error None             -> selected
        in
        Ok (Slider.replace selected' model)

    let of_state : state -> return =
      fun (event, model) ->
        match Message.of_event event with
        | Some msg -> result_of_msg msg model
        | None     -> result_of_prog_event event model
  end (* Update *)

  module View =
  struct
    let of_model : Model.t -> Notty.image =
      fun model ->
        Slider.to_list model
        |> List.map Prog.View.of_model
        |> I.vcat

  end (* View *)

end (* Make *)

module TmuxTest = Make(Line_editor)
module Test = Consolate_term.Loop(TmuxTest)
