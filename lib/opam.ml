(* opam *)

let switch = ref None

type package = {
  name : string;
  version : string;
}

let rec get_switch () =
  match !switch with
  | None ->
    let cur_switch = Util.lines_of_process "opam" [ "switch"; "show" ] |> List.hd in
    switch := Some cur_switch;
    get_switch ()
  | Some s ->
    s

let pp_package fmt package =
    Format.fprintf fmt "%s.%s" package.name package.version

let sexp_of v =
  let open Sexplib.Sexp in
  List [
    List [ Atom "name"; Atom v.name ];
    List [ Atom "version"; Atom v.version ];
  ]

let of_sexp s =
  let open Sexplib.Sexp in
  match s with
  | List [
      List [ Atom "name"; Atom name ];
      List [ Atom "version"; Atom version ];
    ] -> { name; version }
  | _ -> failwith "bad sexp"

let save fname v =
  let oc = open_out (Fpath.to_string fname) in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a%!" Sexplib.Sexp.pp_hum (sexp_of v);
  close_out oc

let load fname =
  let ic = open_in (Fpath.to_string fname) in
  let contents = String.concat "\n" (Util.lines_of_channel ic) in
  let sexp = Sexplib.Sexp.of_string contents in
  close_in ic;
  of_sexp sexp

module S = Set.Make(struct type t = package let compare x y = compare x y end)
let deps_of_opam_result =
  fun line -> match Astring.String.fields ~empty:false line with | [name; version] -> [{name; version} ] | _ -> []

let dependencies package =
  let open Listm in
  if package.name = "ocaml" then []
  else
    let package' = Format.asprintf "%a" pp_package package in
    Util.lines_of_process "opam"
      [
        "list";
        "--switch";
        get_switch ();
        "--installed";
        "--required-by";
        package';
        "--depopts";
        "--columns=name,version";
        "--color=never";
        "--short";
      ]
    >>= deps_of_opam_result
    |> List.filter (fun p ->
           not @@ List.mem p.name [ "ocaml-system"; "ocaml-variants" ])

let all_opam_packages () =
  let open Listm in
  Util.lines_of_process "opam"
    [
      "list";
      "--switch";
      get_switch ();
      "--columns=name,version";
      "--color=never";
      "--short";
    ]
  >>= deps_of_opam_result

let prefix () =
  Util.lines_of_process "opam" [ "var"; "--switch"; get_switch (); "prefix" ]
  |> List.hd

(** Relative to [prefix ()]. *)
let pkg_contents pkg =
  let prefix = prefix () in
  let changes_file =
    Format.asprintf "%s/.opam-switch/install/%s.changes" prefix pkg
  in
  let added =
    match open_in changes_file with
    | exception Sys_error _ -> []
    | ic ->
        let changed = OpamFile.Changes.read_from_channel ic in
        close_in ic;
        OpamStd.String.Map.fold
          (fun file x acc ->
            match x with OpamDirTrack.Added _ -> file :: acc | _ -> acc)
          changed []
  in
  List.map Fpath.v added
