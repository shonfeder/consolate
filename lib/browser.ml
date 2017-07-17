(* open Notty *)
(* open Notty_unix *)

let char_range first_char last_char =
  let low  = Char.code first_char in
  let high = Char.code last_char in
  let range = Batteries.(List.of_enum (low -- high))
  in
  List.map Char.chr range

let lowercase_chars = char_range 'a' 'z'

module Files (* : Options.Menu_spec with type t = string *) =
struct

  open Options

  type t =
    | File of string
    | Dir  of (string -> browser_options)
    | Exec of (string -> unit)
  and selection = Opts.selector * t Opts.item
  and browser_options = selection list

  let rec opts_of_dir : string -> browser_options =
    fun dir ->
      let array_of_files  = Sys.readdir dir in
      let number_of_files = Array.length array_of_files in
      let () = Array.sort String.compare array_of_files in
      let labels = Base.List.take lowercase_chars number_of_files in
      let list_of_files = Array.to_list array_of_files in
      let opts_of_files = List.map to_opt list_of_files in
      match Base.List.zip labels opts_of_files with
      | None -> []
      | Some l -> l
  and to_opt : string -> t Opts.item =
    fun file_name ->
      if Sys.is_directory file_name
      then Opts.opt file_name (Dir opts_of_dir)
      else Opts.opt file_name (File file_name)

  let menu =
    let cwd = Sys.getcwd () in
    Opts.of_assoc cwd (opts_of_dir cwd)
end

module Program = Options.Make (Files)
