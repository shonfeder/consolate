open Notty
open Notty_unix

type 'a menu_items = 'a entry Slider.t
and 'a entry =
  { name : string
  ; desc : string option
  ; item : 'a item}
and 'a item =
  | Value of 'a
  | Menu  of 'a sub_menu
and 'a sub_menu =
  { items   : 'a menu_items
  ; visible : bool}

let sub_menu_of_menu ?desc (name, items) : 'a entry =
  let items = Slider.of_list items in
  { name ; desc ; item = Menu {items; visible=false}}

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
    type title = string
    type choice = Menu.t item
    type menu_section = title * Menu.t menu_items
    type t = menu_section Slider.t

    open BatOption.Infix

    let of_menu (title, items) = (title, Slider.of_list items)
    let of_menus menus =
      menus
      |> List.map of_menu
      |> Slider.of_list

    let selected_entry : t -> 'a entry option =
      fun model     -> Slider.select model >>=
      fun (_, menu) -> Slider.select menu

    let value_is_selected : t -> bool = fun model ->
      match selected_entry model with
      | None | Some {item = Value _} -> true
      | _ -> false

    let sub_menu_is_selected : t -> bool = fun model ->
      match selected_entry model with
      | None | Some {item = Menu _} -> true
      | _ -> false

    let sub_menu_is_visible : t -> bool = fun model ->
      match selected_entry model with
      | None | Some {item = Menu {visible = true}} -> true
      | _ -> false

    let selected_item : t -> choice option =
      let rec item_of_entry optional_entry =
        optional_entry >>= fun entry ->
        match entry.item with
        | Value v -> Some (Value v)
        | Menu m  ->
          if m.visible
          then item_of_entry (Slider.select m.items)
          else Some (Menu m)
      in
      fun model -> item_of_entry (selected_entry model)

    let map_item : ('a item -> 'a item) -> t -> t = fun f model ->
      let updated_model =
        Slider.select model >>= fun (title, menu) ->
        Slider.select menu  >>= fun entry ->
        let entry' = {entry with item = f entry.item} in
        let menu'  = Slider.replace entry' menu in
        let model' = Slider.replace (title, menu') model
        in Some model'
      in
      updated_model |? model

    let show_sub_menu : t -> t =
      map_item @@ function
      | Value v -> Value v
      | Menu m  -> Menu {m with visible = true}

    let hide_sub_menu : t -> t
      = map_item @@ function
      | Value v -> Value v
      | Menu m  -> Menu {m with visible = false}

    (* TODO Refactor this mess *)
    let rec next_item_in_sub_menu : 'a sub_menu -> 'a sub_menu = fun menu ->
      let updated_menu = Slider.select menu.items >>=
        fun entry -> match entry with
        | {item = Menu ({visible = true} as m)} ->
          let updated_sub_menu = next_item_in_sub_menu m in
          let updated_entry = {entry with item = Menu updated_sub_menu} in
          let updated_items = Slider.replace updated_entry menu.items in
          Some {menu with items = updated_items}
        | _ ->
          Some {menu with items = Slider.fwd_till_last menu.items}
      in
      updated_menu |? menu

    let rec prev_item_in_sub_menu : 'a sub_menu -> 'a sub_menu = fun menu ->
      let updated_menu = Slider.select menu.items >>=
        fun entry -> match entry with
        | {item = Menu ({visible = true} as m)} ->
          let updated_sub_menu = prev_item_in_sub_menu m in
          let updated_entry = {entry with item = Menu updated_sub_menu} in
          let updated_items = Slider.replace updated_entry menu.items in
          Some {menu with items = updated_items}
        | _ ->
          Some {menu with items = Slider.bwd menu.items}
      in
      updated_menu |? menu

    let next_item : t -> t = fun model ->
      let next = function
        | Value v -> Value v
        | Menu m  -> Menu (next_item_in_sub_menu m)
      in
      let updated_model = if sub_menu_is_visible model
        then selected_entry model >>= fun entry ->
          Some (map_item next model)
        else Slider.select model  >>= fun (title, menu) ->
          Some (Slider.replace (title, Slider.fwd_till_last menu) model)
      in
      updated_model |? model

    let prev_item : t -> t = fun model ->
      let prev = function
        | Value v -> Value v
        | Menu m  -> Menu (prev_item_in_sub_menu m)
      in
      let updated_model = if sub_menu_is_visible model
        then selected_entry model >>= fun entry ->
          Some (map_item prev model)
        else Slider.select model  >>= fun (title, menu) ->
          Some (Slider.replace (title, Slider.bwd menu) model)
      in
      updated_model |? model

    (* Apply f to x if p x else apply g to x *)
    let or_app p f g x = if p x then f x else g x

    let next_menu : t -> t = or_app
        sub_menu_is_selected
        show_sub_menu
        Slider.fwd_till_last

    let prev_menu : t -> t = or_app
        (fun m -> sub_menu_is_selected m && sub_menu_is_visible m)
        hide_sub_menu
        Slider.bwd

  end

  module Return =
  struct
    type t = Menu.t
  end

  module Update =
  struct
    include CT.Make.Update.Types (Model) (Return)
    module Msg = Message

    let init = Model.of_menus Menu.menus
    (* TODO parse a string in to a menu *)

    let of_selection model : return =
      match Model.selected_item model with
      | None           -> Error None
      | Some (Value v) -> Error (Some v)
      | Some (Menu _)  -> Ok (Model.show_sub_menu model)

    let model_of_msg : Model.t -> Message.t -> return =
      fun model -> function
        | Msg.Close    -> Error None
        | Msg.Prev     -> Ok (Model.prev_item model)
        | Msg.Next     -> Ok (Model.next_item model)
        | Msg.PrevMenu -> Ok (Model.prev_menu model)
        | Msg.NextMenu -> Ok (Model.next_menu model)
        | Msg.Select   -> of_selection model

    let load _ = init
    let of_state : state -> return =
      fun (event, model) ->
        match Msg.of_state (event, model) with
        | None     -> Ok model
        | Some item -> model_of_msg model item
  end

  module View =
  struct

    (* let edge bg' = I.uchar bg' (Uchar.of_int 0x2502) 1 1 *)
    let sep bg' = I.string A.(fg blue ++ bg') " | "
    let more = I.string A.(fg green) " > "

    let of_selected_title title = I.(string A.(bg yellow ++ fg black) title)
    let of_title title = I.(string A.(fg yellow ++ bg black) title)

    let of_name bg name = I.(string bg name)
    let of_desc bg = function
      | None      -> I.empty
      | Some desc -> I.(sep bg <|> string bg desc)

    let rec of_entry bg {name; desc; item} =
      let name_desc = I.(of_name bg name <|> of_desc bg desc) in
      let rest =
        match item with
        | Value _ -> I.empty
        | Menu m  ->
          let sub_menu =
            if m.visible
            then of_selected_menu ("...", m.items)
            else I.empty
          in
          I.(more <|> sub_menu)
      in
      I.(name_desc <|> rest)
    and of_unselected : 'a entry -> Notty.image =
      fun entry -> of_entry A.empty entry
    and of_selected   : 'a entry -> Notty.image =
      fun entry -> of_entry A.(bg red) entry
    and of_selected_menu : Model.menu_section -> Notty.image =
      fun (title, menu) ->
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

    let of_menu (title, menu) =
      let items =
        menu
        |> Slider.to_list
        |> List.map of_unselected
      in
      I.(of_title title
         <->
         vcat items)

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

  let sub_menu =
    ("Sub menu",
      [{name = "Sub option 1";
        desc = None;
        item = Value 10}
       ;
       {name = "Sub option 2";
        desc = Some "Disko!";
        item = Value 12}])

  let menus =
    [ ("Menu 1",
       [{name = "Option 1";
         desc = None;
         item = Value 1}
        ;
        {name = "Option 2";
         desc = Some "Of option 2";
         item = Value 2}])
      ;
      ("Menu 2",
       [{name = "Option A";
         desc = Some "This is option A";
         item = Value 3}
        ;
        sub_menu_of_menu sub_menu])
      ;
      ("Menu 3",
       [{name = "Option i";
         desc = Some "This is option i";
         item = Value 5}
        ;
        {name = "Option ii";
         desc = None;
         item = Value 6}
        ;
        {name = "Option iii";
         desc = Some "This is option iii";
         item = Value 7}
       ])
    ]
end
