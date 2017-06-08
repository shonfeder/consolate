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
  end

  module Model =
  struct
    type t = Msg.t entry Slider.t
    let of_menu_items = Slider.of_list
  end

  module Update =
  struct
    include CT.Make.Update.Types (Model)
    let init = Model.of_menu_items Msg.menu_items
    (* TODO parse a string in to a menu *)
    let load _ = init
    let of_state = raise (Failure "temp")
  end

  module View =
  struct

    let sep = I.string A.(fg blue) " | "
    let of_title title = I.(string A.empty title)
    let of_item {title; desc} =
      let description = Option.default "" desc in
      I.(of_title title <|> sep <|> string A.empty description)

    let of_model model =
      model
      |> Slider.to_list
      |> List.map of_item
      |> I.vcat
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
