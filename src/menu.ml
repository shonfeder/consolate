open Notty
open Notty_unix

type 'a entry = {title: string; desc : string option; msg : 'a}

module type Message_reader =
sig
  type t
  val menu_items : t entry list
end

module Make (Msg:Message_reader) =
struct
  module CT = Consolate_term

  module Message =
  struct
    type t =
      | Select
      | Next
      | Prev
      | Close

    let option_msg_of_arrow model = function
      | `Up   -> Some Prev
      | `Down -> Some Next
      | _ -> None

    let option_msg_of_key model = function
      | `Escape -> Some Close
      | `Enter  -> Some Select
      | `Arrow dir -> option_msg_of_arrow model dir
      | _ -> None

    let of_state (event, model )=
      match event with
      | `Key (key, _) -> option_msg_of_key model key
      | _ -> None
  end

  module Model =
  struct
    type item = Msg.t entry
    type t = item Slider.t
    let of_menu_items = Slider.of_list
    let selected_msg model =
      match Slider.select model with
      | None       -> None
      | Some entry -> Some entry.msg

    let next model =
      if Slider.at_last model
      then model
      else Slider.fwd model

    let prev = Slider.bwd
  end

  module Return =
  struct
    type t = Msg.t
  end

  module Update =
  struct
    include CT.Make.Update.Types (Model) (Return)

    let init = Model.of_menu_items Msg.menu_items
    (* TODO parse a string in to a menu *)

    let model_of_msg model : Message.t -> return =
      let open Message in
      function
      | Close  -> Error None
      | Select -> Error (Model.selected_msg model)
      | Prev   -> Ok (Model.prev model)
      | Next   -> Ok (Model.next model)

    let load _ = init
    let of_state : state -> return =
      fun (event, model) ->
        match Message.of_state (event, model) with
        | None     -> Ok model
        | Some msg -> model_of_msg model msg
  end

  module View =
  struct

    let sep bg' = I.string A.(fg blue ++ bg') " | "
    let of_title bg title = I.(string bg title)
    let of_desc bg desc = I.string bg (Option.default "" desc)

    let of_entry bg {title; desc} =
      I.(of_title bg title <|> sep bg <|> of_desc bg desc)
    let of_unselected : Model.item -> Notty.image = of_entry A.empty
    let of_selected   : Model.item -> Notty.image = of_entry A.(bg red)

    let of_model model =
      let above = Slider.front model |> List.map of_unselected in
      let below = Slider.back  model |> List.map of_unselected in
      let selected =
        match Slider.select model with
        | None       -> I.empty
        | Some entry -> of_selected entry
      in
      I.(vcat above
         <->
         selected
         <->
         vcat below)
  end

end (* Make *)

module Test =
struct
  type t = int
  let menu_items =
    [
      {title = "Option 1";
       desc  = None;
       msg   = 1}
      ;
      {title = "Option 1";
       desc  = Some "Of option 2";
       msg   = 2}
    ]
end
