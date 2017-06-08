open Notty
open Notty_unix

type 'a entry = {name: string; desc : string option; value : 'a}

module type Menu_spec =
sig
  type t
  val menus : (string * t entry list) list
end

module Make (Menu:Menu_spec) =
struct
  module CT = Consolate_term

  module Message =
  struct
    type t =
      | Select
      | Next
      | Prev
      | NextMenu
      | PrevMenu
      | Close

    let option_msg_of_arrow model = function
      | `Up    -> Some Prev
      | `Down  -> Some Next
      | `Left  -> Some PrevMenu
      | `Right -> Some NextMenu
      | _      -> None

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
    type item = Menu.t entry
    type menu = string * item Slider.t
    type t = menu Slider.t

    let of_menu (title, items) = (title, Slider.of_list items)
    let of_menus menus =
      menus
      |> List.map of_menu
      |> Slider.of_list

    let selected_msg (model : t) : Menu.t option =
      let open BatOption.Infix in
      Slider.select model
      >>= fun (_, menu) -> Slider.select menu
      >>= fun entry     -> Some entry.value

    let next_item (model : t) : t =
      match Slider.select model with
      | None      -> model
      | Some (title, menu) ->
        Slider.replace (title, Slider.fwd_till_last menu) model

    let prev_item (model : t) : t =
      match Slider.select model with
      | None      -> model
      | Some (title, menu) ->
        Slider.replace (title, Slider.bwd menu) model

    let next_menu = Slider.fwd_till_last
    let prev_menu = Slider.bwd

  end

  module Return =
  struct
    type t = Menu.t
  end

  module Update =
  struct
    include CT.Make.Update.Types (Model) (Return)

    let init = Model.of_menus Menu.menus
    (* TODO parse a string in to a menu *)

    let model_of_msg (model : Model.t) : Message.t -> return =
      let open Message in
      function
      | Close    -> Error None
      | Prev     -> Ok (Model.prev_item model)
      | Next     -> Ok (Model.next_item model)
      | PrevMenu -> Ok (Model.prev_menu model)
      | NextMenu -> Ok (Model.next_menu model)
      | Select   -> Error (Model.selected_msg model)

    let load _ = init
    let of_state : state -> return =
      fun (event, model) ->
        match Message.of_state (event, model) with
        | None     -> Ok model
        | Some value -> model_of_msg model value
  end

  module View =
  struct

    (* let edge bg' = I.uchar bg' (Uchar.of_int 0x2502) 1 1 *)
    let sep bg' = I.string A.(fg blue ++ bg') " | "

    let of_selected_title title = I.(string A.(bg yellow ++ fg black) title)
    let of_title title = I.(string A.(fg yellow ++ bg black) title)

    let of_name bg name = I.(string bg name)
    let of_desc bg desc = I.string bg (Option.default "" desc)

    let of_entry bg {name; desc} =
      I.(of_name bg name <|> sep bg <|> of_desc bg desc)
    let of_unselected : Model.item -> Notty.image = of_entry A.empty
    let of_selected   : Model.item -> Notty.image = of_entry A.(bg red)

    let of_menu (title, menu) =
      let items =
        menu
        |> Slider.to_list
        |> List.map of_unselected
      in
      I.(of_title title
         <->
         vcat items)

    let of_selected_menu ((title, menu) : Model.menu) =
      let above = Slider.front menu |> List.map of_unselected |> I.vcat in
      let below = Slider.back  menu |> List.map of_unselected |> I.vcat in
      let selected =
        match Slider.select menu with
        | None       -> I.empty
        | Some entry -> of_selected entry
      in
      I.(of_selected_title title
         <->
         above
         <->
         selected
         <->
         below)

    let of_model (model:Model.t) : Notty.image =
      let left  = Slider.front model |> List.map of_menu |> I.hcat in
      let right = Slider.back  model |> List.map of_menu |> I.hcat in
      let selected =
        match Slider.select model with
        | None      -> I.empty
        | Some menu -> of_selected_menu menu
      in
      I.(left <|> selected <|> right)

  end

end (* Make *)

module Test =
struct
  type t = int

  let menus =
    [ ("Menu 1",
       [{name = "Option 1";
         desc  = None;
         value = 1}
        ;
        {name = "Option 2";
         desc  = Some "Of option 2";
         value = 2}])
      ;
      ("Menu 2",
       [{name = "Option A";
         desc  = Some "This is option A";
         value = 3}
        ;
        {name = "Option B";
         desc  = None;
         value = 4}])
      ;
      ("Menu 3",
       [{name = "Option i";
         desc  = Some "This is option i";
         value = 5}
        ;
        {name = "Option ii";
         desc  = None;
         value = 6}
        ;
        {name = "Option iii";
         desc  = Some "This is option iii";
         value = 7}
       ])
    ]
end
