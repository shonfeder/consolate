open Solvuu_build.Std

let project_name = "consolate"
let version = "dev"

let project = Project.lib project_name
    ~dir:"src"
    ~style:`Basic
    (* ~pkg:project_name *)
    ~findlib_deps:
      ["atdgen";
       "batteries";
       "ppx_deriving";
       "ppx_deriving.std";
       "calendar";
       "extlib";
       "fileutils";
       "notty";
       "notty.unix";
       "yojson"]

let include_home_init =
  let home = Sys.getenv "HOME" in
  let home_init = Printf.sprintf "%s/.ocamlinit" home in
  if Sys.file_exists home_init
  then Printf.sprintf "#use \"%s\";;" home_init
  else ""

let load_confirmation =
  Printf.sprintf "print_endline \"\\n-- %s .ocamlinit loaded successfully\n\";;" project_name

let ocamlinit_postfix =
  [include_home_init;
   load_confirmation]

let () = Project.basic1 ~project_name ~version [project]
    ~ocamlinit_postfix
