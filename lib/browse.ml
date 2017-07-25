(* open Notty *)
(* open Notty_unix *)

let rec (--) a b = if a > b then [] else a :: succ a -- b

let char_range first_char last_char =
  let low  = Char.code first_char in
  let high = Char.code last_char
  in
  List.map Char.chr (low -- high)

let lowercase_chars = char_range 'a' 'z'
let uppercase_chars = char_range 'A' 'Z'
let chars = lowercase_chars @ uppercase_chars

module Files (* : Options.Menu_spec with type t = string *) =
struct

  open Options

  type t =
    | File of string
    | Dir  of (string -> browser_options)
    | Exec of (string -> unit)
  and selection = Opts.selector * t Opts.item
  and browser_options = selection list

  let rec menu_of_dir
    : string -> t Opts.choice
    = fun dir () ->
      let () = Sys.chdir dir in
      let array_of_files  = Sys.readdir dir in
      let number_of_files = Array.length array_of_files in
      let () = Array.sort String.compare array_of_files in
      let labels = Base.List.take chars number_of_files in
      let list_of_files = Array.to_list array_of_files in
      let opts_of_files = List.map to_opt list_of_files in
      let assoc_list = Base.List.zip labels opts_of_files
      in
      match assoc_list with
        | None   -> Opts.empty_choice
        | Some l -> Opts.choices_of_assoc l

  and to_opt
    : string -> t Opts.item =
    fun file_name ->
      if Sys.is_directory file_name
      then Opts.opts file_name (menu_of_dir file_name)
      else Opts.opt  file_name (File file_name)

  let menu =
    let cwd = Sys.getcwd () in
    Opts.of_choice cwd (menu_of_dir cwd)
end

module Program = Options.Make (Files)
