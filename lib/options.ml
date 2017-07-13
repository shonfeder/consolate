open BatOption.Infix

open Notty
open Notty_unix

module CT = Consolate_term
module CharMap = BatMap.Make (Char)
let find = CharMap.Exceptionless.find

module type Opts =
sig

  (* Lvl 4 *)
  type name        = string
  type description = string option
  type selector    = char

  (* Lvl 3 *)
  type 'a opt =
    { name        : name
    ; thing       : 'a
    ; description : description }

  (* Lvl 2 *)
  type 'a opts =
    { title : name
    ; opts  : 'a choice}
  and 'a choice =
    'a item CharMap.t
  and 'a item =
    | Opt  of 'a opt
    | Opts of 'a opts

  (* Lvl 1 *)
  type 'a t = 'a opts

  val opts_items : 'a t -> 'a choice
  val opts_title : 'a t -> name

  val of_assoc : string -> (selector * 'a item) list -> 'a choice
  val of_opts  : string -> (selector * 'a item) list -> 'a opts

  val opt  : ?description:string -> name -> 'a -> 'a item
  val opts : name -> 'a choice -> 'a item
end


module Opts =
struct

  (* Lvl 5 *)
  type name        = string
  type description = string option
  type selector    = char

  (* Lvl 4 *)
  type 'a opt =
    { name        : name
    ; thing       : 'a
    ; description : description }

  (* Lvl 3 *)
  and 'a opts =
    { title : name
    ; opts  : 'a choice}
  and 'a item =
    | Opt  of 'a opt
    | Opts of 'a opts
  (* Lvl 2 *)
  and 'a choice = 'a item CharMap.t

  (* Lvl 1 *)
  type 'a t = 'a opts

  let opts_items : 'a t -> 'a choice
    = fun opts -> opts.opts

  let opts_title : 'a t -> name
    = fun opts -> opts.title

  let of_assoc : string -> (selector * 'a item) list -> 'a opts
    = fun title assoc ->
    let add_item (selector, item) choice =
      CharMap.add selector item choice
    in
    let opts = List.fold_right add_item assoc CharMap.empty in
    {title; opts}

  let of_opts  : string -> (selector * 'a item) list -> 'a item
    = fun string assoc ->
      Opts (of_assoc string assoc)

  let opt ?description name thing : 'a item =
    Opt {name; thing; description}
  let opts title opts : 'a item =
    Opts {title; opts}
end

module type Menu_spec =
sig
  type t
  val menu : t Opts.t
end

(* TODO For menues longer than the screen (pages),
   make '<' and '>' to go next page *)

module Make (Menu:Menu_spec) : CT.Program with type Return.t = Menu.t =
struct
  module Model =
  struct
    type t = Menu.t Opts.t Slider.t
    let of_menu : 'a Opts.t -> t = Slider.singleton
  end

  module Return =
  struct
    type t = Menu.t
  end

  module Message =
  struct

    module T =
    struct
      type t =
        | Select of Opts.selector
        | Esc
    end
    include T
    module Funs = CT.Make.Message.Functions (T)

    let of_key = function
      (* TODO Catch uchar conversion *)
      | `Uchar c -> Some (Select (Uchar.to_char c))
      | `Escape  -> Some Esc
      | _        -> None

    let of_event : CT.event -> t option = function
      | `Key (key, _) -> of_key key
      | _ -> None
  end

  module Update =
  struct
    include CT.Make.Update.Basis (Model) (Return)
    let init = Model.of_menu Menu.menu
    let load : string -> Model.t   = fun _ -> init

    let of_selection : Opts.selector -> Model.t -> return =
      fun selector model ->
        let open Opts in
        let update =
          Slider.select model >>=
          fun opts -> Some (Opts.opts_items opts) >>=
          find selector       >>= function
          | Opt opt   -> Some (Flow.return opt.thing)
          | Opts opts -> Some (Slider.fwd model
                               |> Slider.insert opts
                               |> Flow.cont)
        in
        update |? Flow.cont model

    let of_esc : Model.t -> return =
      fun model ->
        if Slider.at_first model
        then Flow.halt 0
        else Flow.cont (Slider.bwd model)

    let of_msg : Model.t -> Message.t -> return =
      fun model -> function
        | Message.Select c -> of_selection c model
        | Message.Esc      -> of_esc model
        (* | _ -> Ok model *)

    let of_state : state -> return = fun (event, model) ->
      match Message.of_event event with
      | None     -> Flow.cont model
      | Some msg -> of_msg model msg
  end

  module View =
  struct
    open Opts

    (* auxiliary image functions *)
    let rec hconcat
      : ?sep:Notty.image   -> Notty.image list -> Notty.image
      = fun ?(sep=I.empty) -> function | []    -> I.empty
                                       | [i]   -> i
                                       | i::is -> I.(i <|> sep <|> hconcat ~sep is)


    let opt_sep  = I.(string A.(fg blue) " › ")
    let opts_sep = I.(string A.(fg red) " » ")
    let of_name n     = I.(string A.empty n)
    let of_title t    = I.(string A.(fg magenta) t)
    let of_selector s = I.(char A.(fg cyan) s 1 1)

    let of_opt_item s opt =
      I.(of_selector s <|> opt_sep <|> of_name opt.name)
    let of_opts_item s opts =
      I.(of_selector s <|> opts_sep <|> of_title opts.title)
    let of_item s = function
      | Opt opt   -> of_opt_item s opt
      | Opts opts -> of_opts_item s opts

    let of_opts
      : ?rows:int -> Menu.t Opts.t -> Notty.image
      = fun ?(rows=10) opts ->

        let add_column : Notty.image * Notty.image -> Notty.image
          = fun (column, columns) -> I.(columns <|> pad ~r:2 column)
        in

        let arrange : char -> Menu.t Opts.item
          -> Notty.image * Notty.image
          -> Notty.image * Notty.image
          = fun selector item (column, columns) ->
            let item_img = of_item selector item in
            if I.height column < rows
            then I.(column <-> item_img), columns
            else item_img, add_column (column, columns)
        in
        (I.empty, I.empty)
        |> CharMap.fold arrange opts.opts
        |> add_column

    let bread_crumbs_of_model : Model.t -> Notty.image
      = fun model ->
        model
        |> Slider.fwd
        |> Slider.front
        |> List.map Opts.opts_title
        |> List.map of_title
        |> hconcat ~sep:opts_sep

    let of_model
      : Model.t -> Notty.image
      = fun model ->
        let bread_crumbs = bread_crumbs_of_model model in
      match Slider.select model with
      | None      -> I.empty
      | Some opts -> I.(of_opts ~rows:2 opts
                        <->
                        bread_crumbs)
  end
end (* Make *)

module Test : Menu_spec with type t = int =
struct
  type t = int
  open Opts
  let menu = of_assoc "main menu"
      [ 'a', opt "option a" 1
      ; 'b', opt "option b" 2
      ; 'c', of_opts "more opts"
          [ 'x', opt "option x" 3
          ; 'y', opt "option y" 4
          ; 'z', of_opts "even more opts!"
              [ 'd', opt "option d" 5
              ; 'e', opt "option e" 6
              ; 'f', opt "option f" 7
              ; 'g', opt "option g" 8]
          ]
      ]
end
