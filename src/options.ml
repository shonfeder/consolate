open BatOption.Infix

open Notty
open Notty_unix

module CT = Consolate_term
module CharMap = BatMap.Make (Char)
let find = CharMap.Exceptionless.find

module type Opts =
sig

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
  type 'a opts =
    { title : name
    ; opts  : 'a choice}
  and 'a item =
    | Opt  of 'a opt
    | Opts of 'a opts

  (* Lvl 2 *)
  and 'a choice = 'a item CharMap.t

  (* Lvl 1 *)
  type 'a t = 'a choice Slider.t

  val of_assoc : (selector * 'a item) list -> 'a choice

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
  type 'a t = 'a choice Slider.t

  let of_assoc : (selector * 'a item) list -> 'a choice = fun assoc ->
    let add_item (selector, item) choice =
      CharMap.add selector item choice
    in
    List.fold_right add_item assoc CharMap.empty

  let opt ?description name thing : 'a item =
    Opt {name; thing; description}
  let opts title opts : 'a item =
    Opts {title; opts}
end

module type Menu_spec =
sig
  type t
  val menu : t Opts.choice
end

module Make (Menu:Menu_spec) : CT.Program with type Return.t = Menu.t =
struct
  module Model =
  struct
    type t = Menu.t Opts.t
    let of_menu : 'a Opts.choice -> t = Slider.singleton
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
    include CT.Make.Update.Types (Model) (Return)
    let init = Model.of_menu Menu.menu
    let load : string -> Model.t   = fun _ -> init

    let of_selection : Opts.selector -> Model.t -> return =
      fun selector model ->
        let open Opts in
        let update =
          Slider.select model >>=
          find selector       >>= function
          | Opt opt   -> Some (return opt.thing)
          | Opts opts -> Some (Slider.fwd model
                               |> Slider.insert opts.opts
                               |> cont)
        in
        update |? cont model

    let of_esc : Model.t -> return =
      fun model ->
        if Slider.at_first model
        then halt
        else cont (Slider.bwd model)

    let of_msg : Model.t -> Message.t -> return =
      fun model -> function
        | Message.Select c -> of_selection c model
        | Message.Esc      -> of_esc model
        (* | _ -> Ok model *)

    let of_state : state -> return = fun (event, model) ->
      match Message.of_event event with
      | None     -> cont model
      | Some msg -> of_msg model msg
  end

  module View =
  struct
    open Opts

    let opt_sep  = I.(string A.(fg blue) " › ")
    let opts_sep = I.(string A.(fg red) " » ")
    let of_name n     = I.(string A.empty n)
    let of_title t    = I.(string A.(fg magenta) t)
    let of_selector s = I.(char A.(fg cyan) s 1 1)

    let of_opt s opt =
      I.(of_selector s <|> opt_sep <|> of_name opt.name)
    let of_opts s opts =
      I.(of_selector s <|> opts_sep <|> of_title opts.title)
    let of_item s = function
      | Opt opt   -> of_opt s opt
      | Opts opts -> of_opts s opts

    let of_choice : Menu.t Opts.choice -> Notty.image = fun choice ->
      (* TODO Stack to height of x then fill out horizontally *)
      let stack selector item stacking =
        I.(stacking <-> of_item selector item)
      in
      CharMap.fold stack choice I.empty

    let of_model : Model.t -> Notty.image = fun model ->
      match Slider.select model with
      | None        -> I.empty
      | Some choice -> of_choice choice
  end
end (* Make *)

module Test : Menu_spec with type t = int =
struct
  type t = int
  open Opts
  let menu = of_assoc
      [ 'a', opt "option a" 1
      ; 'b', opt "option b" 2
      ; 'c', opts "more opts" @@
        of_assoc
          [ 'x', opt "option x" 3
          ; 'y', opt "option y" 4
          ; 'z', opt "option z" 5
          ]
      ]
end