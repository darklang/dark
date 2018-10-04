open Core_kernel

module OldStr = Str
open Elm
open Migrate_parsetree.Ast_404
open Ast_helper

let skip_preCommented (a: 'a preCommented) : 'a =
  Tuple.T2.get1 a

let skip_postCommented (a: 'a postCommented) : 'a =
  Tuple.T2.get2 a

let skip_commented (a: 'a commented) : 'a =
  Tuple.T3.get2 a

let skip_located (a: 'a located) : 'a =
  Tuple.T2.get1 a

let to_list a = [a]

let importsO ((_c, i): Elm.imports) : Parsetree.structure =
  i
  |> List.map ~f:(fun (fqn,(_comments, importMethod)) ->
      let fqn = fqn
                |> Longident.unflatten
                |> Option.value_exn ~message:"what" ~error:(Error.of_string "x") ~here:(
                  { pos_fname = "asd";
                    pos_lnum = 456;
                    pos_bol = 234;
                    pos_cnum =123 ;
                  })
              |> Location.mknoloc
                (* |> skip_located *)
      in
      let alias =
        match importMethod.alias with
        | Some (_c, (_c2, alias)) ->
          (* import X as Y -> module Y = X *)
            fqn
            |> Mod.ident
            |> Mb.mk (Location.mknoloc alias)
            |> Str.module_
            |> to_list
        | _ -> []
      in
      let listings =
        match importMethod.exposedVars with
        (* import X -> Nothing, it's implicit *)
        | (_c, (_c2, ClosedListing)) -> []
        (* import X exposing (..) -> open X *)
        | (_c, (_c2, OpenListing (_c3, (), _c4))) ->
            fqn
            |> Opn.mk
            |> Str.open_
            |> to_list
        | (_c, (_c2, ExplicitListing (detailed, _line))) ->
          (* import X exposing (a, b) -> type a = X.a; let b = X.b *)
          let vs = detailed.values
                   |> List.map ~f:Tuple.T2.get1

          in
          let ops = detailed.operators |> List.map ~f:Tuple.T2.get1 in
          (* You can import nested constructors here, but we don't *)
          let types = detailed.types
                      |> List.map ~f:Tuple.T2.get1
          in
          []
      in
      []
    )
  |> List.concat








let topLevelStructureO (i: Elm.declaration Elm.topLevelStructure) : Parsetree.structure =
  []



let moduleO (m: Elm.module_) : Parsetree.structure =
  (* TODO: comments, docs *)
  (* ignore header *)
  let imports = m.imports |> importsO in
  let body = m.body |> List.map ~f:(topLevelStructureO) in
  imports @ List.concat body


let to_ocaml (m: Elm.module_) : (Parsetree.structure * Reason_comment.t list) =
  let file = moduleO m in
  (file, [])

let _ =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--parse"
  then
    try
      In_channel.stdin
      |> Yojson.Basic.from_channel
      |> Elm.moduleJ
      |> Elm.show_module_
      |> OldStr.global_replace (OldStr.regexp "Translate\\.") ""
      |> print_endline
    with (Elm.E (msg, json)) ->
      Printexc.print_backtrace stderr;
      print_endline (Yojson.Basic.pretty_to_string json);
      prerr_endline msg;
      ()
  else
    try
      let m =
        In_channel.stdin
        |> Yojson.Basic.from_channel
        |> Elm.moduleJ
      in
      m
      |> to_ocaml
      |> Reason_toolchain.RE.print_implementation_with_comments
           Format.str_formatter;
      Format.flush_str_formatter ()
      |> print_endline;
      ()
    with
    | (Elm.E (msg, json)) ->
      Printexc.print_backtrace stderr;
      print_endline (Yojson.Basic.pretty_to_string json);
      prerr_endline msg;
      ()
    | e ->
      Printexc.print_backtrace stderr;
      prerr_endline (Exn.to_string e);
      ()


