(** Prepare a "prep" tree for odocmkgen:
    - Add listing packages (universes, packages in universes, versions of packages)
    - Add default package page if there is not already one
    - TODO: Add some informations to user's package pages ?
      (dependencies to other packages, list of modules and pages)
    *)

let ( / ) = Fpath.( / )

let fpf = Printf.fprintf

let write_file p f =
  let out = open_out (Fpath.to_string p) in
  Format.eprintf "Create '%a'\n" Fpath.pp p;
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> f out)

let index_page_of_dir d =
  let parent, base = Fpath.split_base d in
  Fpath.( // ) parent (Fpath.set_ext ".mld" base)

let is_dir (_, p) = Sys.is_directory (Fpath.to_string p)

let is_hidden s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false
    else if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

(** Replace '-' by '_' and add the "page-" prefix. That's what [odocmkgen] is
    doing. *)
let page_name_of_string n =
  "page-" ^ String.concat "_" (String.split_on_char '-' n)

let module_name_of_string = String.capitalize_ascii

(** This is an approximation. *)
let module_name_of_path f =
  if Fpath.mem_ext [ ".cmti"; ".cmt"; ".cmi" ] f then
    Some (module_name_of_string (Fpath.basename (Fpath.rem_ext f)))
  else None

(** Search recursively for modules. *)
let rec modules_of_subpkg p =
  let get_module (_, p) =
    match module_name_of_path p with
    | Some m as m' when not (is_hidden m) -> m'
    | _ -> None
  in
  let dirs, files = List.partition is_dir (Util.list_dir p) in
  (* Remove duplicates due to several cm{ti,t,i} per modules. *)
  List.sort_uniq String.compare (List.filter_map get_module files)
  @ List.concat_map (fun (_, p') -> modules_of_subpkg p') dirs

let pp_childpages out =
  List.iter (fun p -> fpf out "- {!childpage:%s}\n" (page_name_of_string p))

let pp_childmodules out = List.iter (fpf out "- {!childmodule:%s}\n")

let gen_universes_list universes out =
  fpf out
    "{0 Universes}\n\
     These universes are for those packages that are compiled against an \
     alternative set of dependencies than those in the 'packages' hierarchy.\n";
  pp_childpages out universes;
  ()

let gen_universe_page universe_name packages out =
  fpf out
    "{0 Universe %s}\n\
     {1 Packages}\n\
     This dependency universe has been used to compile the following packages:\n"
    universe_name;
  pp_childpages out packages;
  ()

let gen_versions_list pkg_name versions out =
  fpf out "{0 Package '%s'}\n{1 Versions}\n" pkg_name;
  pp_childpages out versions;
  ()

let gen_package_page pkg_name version subpkgs out =
  let gen_subpkg (name, modules) =
    fpf out "{1 [%s]}\n" name;
    pp_childmodules out modules;
    ()
  in
  fpf out "{0 Package '%s' version %s}\n" pkg_name version;
  List.iter gen_subpkg subpkgs;
  ()

let prep_package pkg_name version p =
  let subpkgs =
    Util.list_dir (Fpath.( / ) p "lib")
    |> List.filter is_dir
    |> List.map (fun (s, p') -> (s, modules_of_subpkg p'))
  in
  (* TODO: Detect user's package pages (not yet copied by 'prep') *)
  write_file (index_page_of_dir p) (gen_package_page pkg_name version subpkgs)

let prep_package_versions pkg_name p =
  let versions = List.filter is_dir (Util.list_dir p) in
  write_file (index_page_of_dir p)
    (gen_versions_list pkg_name (List.map fst versions));
  List.iter (fun (v, p') -> prep_package pkg_name v p') versions

let prep_universe universe_name p =
  let packages = List.filter is_dir (Util.list_dir p) in
  write_file (index_page_of_dir p)
    (gen_universe_page universe_name (List.map fst packages));
  List.iter (fun (pkg_name, p') -> prep_package_versions pkg_name p') packages

let prep_universes p =
  let universes = List.filter is_dir (Util.list_dir p) in
  write_file (index_page_of_dir p) (gen_universes_list (List.map fst universes));
  List.iter (fun (name, p') -> prep_universe name p') universes

let run () = prep_universes (Prep.top_path / "universes")
