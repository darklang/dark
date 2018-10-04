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

let nolo = Location.mknoloc

let varname n =
  n
  |> Longident.parse
  |> nolo

let fullname (path : string list) v =
  path @ [v]
  |> Longident.unflatten
  |> fun x -> Option.value_exn x
  |> nolo



let rec patpO (patp: patternp) : Parsetree.pattern =
  match patp with
  | Anything -> Pat.any ()
  | _ -> failwith (show_patternp patp)
and patO ((_r, patp): pattern) : Parsetree.pattern =
  patpO patp


let litO lit : Parsetree.constant =
  match lit with
  | Str (str, _l) -> Const.string str
  | _ -> failwith (show_literal lit)

let rec exprpO (exprp) : Parsetree.expression =
  match exprp with
  | Case (((_c, clause, _c2), _unknown_bool), pats) ->
    let patterns =
      List.map pats
        ~f:(fun ((_c, pat, _c2), (_c3, rhs)) ->
            Exp.case (patO pat) (exprO rhs))
    in
    Exp.match_ (exprO clause) patterns
  | App (fn, args, _line) ->
    Exp.apply
      (exprO fn)
      (List.map args ~f:(fun (_c, a) -> (Asttypes.Nolabel, exprO a)))
  | ELiteral lit ->
    Exp.constant (litO lit)
  | VarExpr (VarRef (path, var)) ->
    Exp.ident (fullname path var)
  | Tuple (exprs, _l) ->
    Exp.tuple (List.map ~f:(fun (_c1, expr, _c2) -> exprO expr) exprs)

  | _ -> failwith (show_exprp exprp)
and exprO (_r, exprp) : Parsetree.expression =
  exprpO exprp



(* let x (a:int) b c = *)
let toplevelLet name (args: string list) (expr: expr) : Parsetree.structure_item =
  let args =
    List.fold args ~init:(exprO expr)
      ~f:(fun prev arg ->
          (Exp.fun_ Asttypes.Nolabel None (Pat.var (nolo arg)) prev))
  in
  let let_ =
    Vb.mk
      (Pat.var (nolo name))
      args
  in
  Str.value Asttypes.Nonrecursive [let_]

let to_list a = [a]

let importsO ((_c, i): Elm.imports) : Parsetree.structure =
  i
  |> List.map ~f:(fun (fqn,(_comments, importMethod)) ->
      let modName = fqn
                    |> Longident.unflatten
                    |> fun x -> Option.value_exn x
                    |> Location.mknoloc
      in
      let alias =
        match importMethod.alias with
        | Some (_c, (_c2, alias)) ->
          (* import X as Y -> module Y = X *)
            modName
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
            modName
            |> Opn.mk
            |> Str.open_
            |> to_list
        | (_c, (_c2, ExplicitListing (detailed, _line))) ->
          (* import X exposing (a, b) -> type a = X.a; let b = X.b *)
          (* let vs = *)
          (*   (* TODO: this doesn't work yet *) *)
          (*   detailed.values *)
          (*   |> List.map ~f:Tuple.T2.get1 *)
          (*   |> List.map *)
          (*     ~f:(fun name -> *)
          (*         let fqn = fqn @ [name] *)
          (*                   |> Longident.unflatten *)
          (*                   |> fun x -> Option.value_exn x *)
          (*                   |> Location.mknoloc *)
          (*         in *)
          (*         let binding = *)
          (*           Vb.mk *)
          (*             (Pat.var (Location.mknoloc name)) *)
          (*             (Exp.ident fqn) *)
          (*         in *)
          (*         Exp.ident fqn *)
          (*         |> Exp.let_ Asttypes.Nonrecursive [] *)
          (*         |> Str.eval *)
          (*       ) *)
          (* in *)
          (* let ops = detailed.operators *)
          (*           |> List.map ~f:Tuple.T2.get1 *)
          (* in *)
          (* (* You can import nested constructors here, but we don't *) *)
          (* let types = detailed.types *)
          (*             |> List.map ~f:Tuple.T2.get1 *)
          (* in *)
          (* vs *)
          (*   (* TODO: add types and ops *) *)
          (* @ types @ ops *)
          []
      in
      alias @ listings
    )
  |> List.concat


let topLevelStructureO (s: Elm.declaration Elm.topLevelStructure) : Parsetree.structure =
  match s with
  | BodyComment _c -> []
  | DocComment _c -> []
  | Entry (_r, decl) ->
    (* TODO: when you have a definition, find the associated type annotation for it. *)
    (* A Definition needs a let *)
    (match decl with
     | TypeAnnotation ((ref_, _c1), (_c2, type_)) ->
       []
       (* let name = *)
       (*   (match ref_ with *)
       (*    | VarRef (names, n) *)
       (*    | TagRef (names, n) -> *)
       (*      names *)
       (*    | OpRef n -> [n]) *)
       (* in *)
       (* let rec type_O (_r, t) = *)
       (*   (match t with *)
       (*    | FunctionType ft -> *)
       (*      let (first, _eol) = ft.first in *)
       (*      let rest = *)
       (*        List.map ~f:(fun (_c1, _c2, type_, _eol) -> type_) *)
       (*          ft.rest *)
       (*      in *)
       (*      [] *)
       (*      (* TODO: List.map ~f:type_O (first :: rest) *) *)
       (*    | TypeConstruction (tc, ts) -> *)
       (*      (match tc with *)
       (*       | TupleConstructor i -> failwith "tupleconstructor" *)
       (*       | NamedConstructor nc -> *)
       (*         failwith "namedconstructor" (*nc*))) *)
       (* in *)
       (* type_O type_ *)
     | Definition ((_, VarPattern name), args, _c, expr) ->
       let args = List.map args
           ~f:(fun (_l, (_c, pat)) ->
               match pat with
               | VarPattern argname -> argname
               | _ -> failwith "definition")
       in
       [toplevelLet name args expr]
     | _ -> failwith (show_declaration decl)
    )



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
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--debug"
  then
    let migration =
      let module Versions = Migrate_parsetree_versions in
      Versions.migrate Versions.ocaml_404 Versions.ocaml_current
    in
    Lexing.from_string "let x a b c = 5"
    |> Reason_toolchain.ML.implementation
    |> migration.copy_structure
    |> Printast.structure 0
      Format.str_formatter;
      Format.flush_str_formatter ()
      |> print_endline;
  else
    try
      let m =
        In_channel.stdin
        |> Yojson.Basic.from_channel
        |> Elm.moduleJ
      in
      m
      |> to_ocaml
      |> Reason_toolchain.ML.print_implementation_with_comments
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


