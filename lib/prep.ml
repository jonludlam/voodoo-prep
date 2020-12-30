(* Prep *)

open Listm

(* This module prepares a directory structure for making documentation.

  We go through all the installed cm{t,ti,i}s and find which package/version they are from. We then
  query opam to find out their dependencies. Then we copy the files into the following structure:
  
  prep/universes/<hash>/<package>/<version>/ocaml/... 
  
  *)

type source_info = {
  root : Fpath.t; (** Root path in which this was found *)
  relpath : Fpath.t; (** Path relative to [root] *)
  name : string; (** 'Astring' *)
  package : Opam.package; (* Package in which this file lives ("astring") *)
  universe : Universe.t
}
let top_path = Fpath.v "prep"


(* Given a base Fpath.t (a cmt, cmti or cmi, without extension), figure out the 'best' one - in order or preference
   cmti, cmt, cmi *)


(** Get info given a relative path (to [root]) to an object file (cmt, cmti or cmi). *)
let get_cm_info root package relpath =
  let _, lname = Fpath.split_base relpath in
  let name = String.capitalize (Fpath.to_string lname) in
  try
    let _, universe = Universe.Current.dep_universe package.Opam.name in
    Format.eprintf "%a: universe=%a\n%!" Opam.pp_package package Universe.pp
      universe;
    [ { root; relpath; name; package; universe } ]
  with _ -> []

(** Lower is better *)
let cm_file_preference = function
  | ".cmti" -> Some 1
  | ".cmt" -> Some 2
  | ".cmi" -> Some 3
  | _ -> None

(** Get cm* files out of a list of files.
    Given the choice between a cmti, a cmt and a cmi file, we chose them according to [cm_file_preference] above *)
let get_cm_files files =
  let rec skip f = function hd :: tl when f hd -> skip f tl | x -> x in
  (* Take the first of each group. *)
  let rec dedup acc = function
    | (base, _, p) :: tl ->
        let tl = skip (fun (base', _, _) -> base = base') tl in
        dedup (p :: acc) tl
    | [] -> acc
  in
  (* Sort files by their basename and preference, remove other files *)
  files
  >>= (fun p ->
        let without_ext, ext = Fpath.split_ext p in
        match cm_file_preference ext with
        | Some pref -> [ (Fpath.basename without_ext, pref, p) ]
        | None -> [])
  |> List.sort compare |> dedup []

(** A list of [source_info] for each files of a package.
    Keep only one of the corresponding .cmti, .cmt or .cmi for a module, in
    that order of preference. *)
let infos_of_package pkg =
  let partition_by_kind lib f =
    let segs = Fpath.segs f in
    match segs with
    | "lib" :: _
      when not (List.mem ".private" segs || List.mem ".coq-native" segs) ->
        f :: lib
    | _ -> lib
  in
  let lib =
    List.fold_left partition_by_kind [] (Opam.pkg_contents pkg.Opam.name)
  in
  let lib = get_cm_files lib in
  let prefix = Opam.prefix () |> Fpath.v in
  List.flatten (List.map (get_cm_info prefix pkg) lib)

let run whitelist _roots =
  let packages = Opam.all_opam_packages () in
  let packages = List.filter (fun pkg -> pkg.Opam.name <> "ocaml-secondary-compiler") packages in
  let infos = List.flatten (List.map infos_of_package packages) in

  let infos =
    if List.length whitelist > 0
    then List.filter (fun info -> List.mem info.package.name whitelist) infos
    else infos
  in
  let infos =
    List.filter (fun info -> try ignore (Universe.Current.dep_universe info.package.name); true with _ -> Format.eprintf "pruning %a\n%!" Fpath.pp (Fpath.append info.root info.relpath); false) infos
  in
  let copy info =
    let v_str = Astring.String.cuts ~sep:"." info.package.version in
    let v_str = String.concat "_" v_str in
    let src = Fpath.append info.root info.relpath in
    let dest = Fpath.(top_path / "universes" / info.universe.id / info.package.name / v_str // info.relpath ) in
    Util.mkdir_p (Fpath.parent dest);
    Util.cp src dest
  in
  List.iter copy infos;
  Universe.Current.save top_path
