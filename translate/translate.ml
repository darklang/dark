open Core_kernel

module OldStr = Str
open Elm
open Migrate_parsetree.Ast_404
open Ast_helper

let skip_preCommented (a: 'a preCommented) : 'a =
  Tuple.T2.get2 a

let skip_postCommented (a: 'a postCommented) : 'a =
  Tuple.T2.get1 a

let skip_commented (a: 'a commented) : 'a =
  Tuple.T3.get2 a

let skip_located (a: 'a located) : 'a =
  Tuple.T2.get2 a

let skip_eol (a: 'a withEol) : 'a =
  Tuple.T2.get1 a

let nolo = Location.mknoloc

let correct_name s : string =
  if s = "new"
  then "new_"
  else if s = "end"
  then "end_"
  else s

let varname n : lid =
  n
  |> correct_name
  |> Longident.parse
  |> nolo

let as_var n : str =
  n
  |> correct_name
  |> nolo

let fullname (names: string list) : lid =
  names
  |> List.map ~f:correct_name
  |> Longident.unflatten
  |> fun x -> Option.value_exn x
  |> nolo

let seq2list (s: 'a sequence) : 'a list =
  List.map s
    ~f:(fun (_c, (_c2, (a, _s))) -> a)

let openCommentedList2list (l: 'a openCommentedList) : 'a list =
  let (init, last) = l in
  let newInit = List.map init
      ~f:(fun i -> i
                   |> skip_commented
                   |> skip_eol)
  in
  let newLast = last
                |> skip_preCommented
                |> skip_eol
  in
  newInit @ [newLast]




let todo name str =
  if String.length str >= 20
  then
    "todo (" ^ name ^ "): " ^ (String.slice str 0 20)
  else
    "todo (" ^ name ^ "): " ^ str


let rec patpO (patp: patternp) : Parsetree.pattern =
  match patp with
  | Anything -> Pat.any ()
  | VarPattern name -> Pat.var (as_var name)
  | TuplePattern ps ->
    let ps = List.map ~f:skip_commented ps in
    Pat.tuple (List.map ~f:patO ps)
  | EmptyListPattern _cs ->
    Pat.construct (varname "[]") None
  | ConsPattern { cpFirst; cpRest } ->
    let pats =
      Tuple.T2.get1 cpFirst
      :: (List.map cpRest ~f:(fun (_cs, _cs2, pat, _wtf) -> pat))
    in

    List.fold pats
      ~init:(Pat.construct (varname "[]") None)
      ~f:(fun prev arg ->
          Pat.construct (varname "::") (Some (Pat.tuple [patO arg; prev])))



  | Data (names, args) ->
    let tuple =
      args
      |> List.map ~f:(fun (_c, pat) -> patO pat)
      |> Pat.tuple
    in
    let n = fullname names in
    if args = []
    then
      Pat.construct n None
    else
      Pat.construct n (Some tuple)
  | _ ->
    Pat.constant
      (Const.string (todo "pattern" (show_patternp patp)))
and patO ((_r, patp): pattern) : Parsetree.pattern =
  patpO patp

let litExpO lit : Parsetree.expression =
  match lit with
  | Str (str, _l) -> Exp.constant (Const.string str)
  | Boolean true -> Exp.construct (varname "true") None
  | Boolean false -> Exp.construct (varname "false") None
  | IntNum (i, repr) -> Exp.constant (Const.int i)
  | Chr c -> Exp.constant (Const.char c)
  | _ -> Exp.constant (Const.string (todo "literal" (show_literal lit)))

let rec exprpO (exprp) : Parsetree.expression =
  match exprp with
  | Case (((_c, clause, _c2), _unknown_bool), pats) ->
    let patterns =
      List.map pats
        ~f:(fun ((_c, pat, _c2), (_c3, rhs)) ->
            Exp.case (patO pat) (exprO rhs))
    in
    Exp.match_ (exprO clause) patterns
    (* Constructos with 1 arg *)
  | App ((_r, VarExpr (TagRef (path, var))), [_c, arg], _line) ->
    Exp.construct
      (fullname (path @ [var]))
      (Some (exprO arg))
    (* Constructos with multiple args *)
  | App ((_r, VarExpr (TagRef (path, var))), args, _line) ->
    Exp.construct
      (fullname (path @ [var]))
      (Some
         (Exp.tuple
            (List.map args ~f:(fun (_c, a) -> exprO a))))
  | App (fn, args, _line) ->
    Exp.apply
      (exprO fn)
      (List.map args ~f:(fun a -> a |> skip_preCommented |> as_arg ))
  | ELiteral lit -> litExpO lit
  | VarExpr (VarRef (path, var)) ->
    Exp.ident (fullname (path @ [var]))
  | VarExpr (TagRef (path, var)) ->
    Exp.construct (fullname (path @ [var])) None
  | VarExpr (OpRef name) ->
    Exp.construct (varname name) None
  | RecordExpr { base; fields } ->
    let base = Option.map base
        ~f:(fun (_c, var, _c2) -> Exp.ident (varname var))
    in
    let fields = seq2list fields in
    let fields =
      List.map fields
        ~f:(fun field ->
          ( skip_postCommented field._key |> varname
          , skip_preCommented field._value |> exprO))
    in
    Exp.record fields base

  | TupleExpr (exprs, _l) ->
    Exp.tuple
      (List.map exprs
         ~f:(fun expr -> exprO (skip_commented expr)))
  | TupleFunction count ->
    Exp.ident (varname ("to_tuple" ^ (string_of_int count)))

  | Parens (_c, expr, _c2) ->
    exprO expr
  | Unit _cs ->
    Exp.construct (varname "()") None
  | Access (expr, field) ->
    Exp.field (exprO expr) (varname field)
  | Let (declarations, _c, body) ->
    (* TODO: add the type definitions *)
    List.fold ~init:(exprO body) (List.rev declarations)
      ~f:(fun prev decl ->
            match decl with
            | LetDefinition (pat, [], _cs, rhs) ->
              let vb = Vb.mk (patO pat) (exprO rhs) in
              (Exp.let_ Nonrecursive [vb] prev)
            | LetAnnotation _annot -> Exp.unreachable ()
            | LetComment _c -> Exp.unreachable ()
            | LetDefinition x ->
              failwith "Unexpected let pattern") (* figure this out *)
  | ExplicitList { terms } ->
    let terms = List.map (seq2list terms) ~f:exprO in
    List.fold terms
      ~init:(Exp.construct (varname "[]") None)
      ~f:(fun prev arg ->
          Exp.construct (varname "::") (Some (Exp.tuple [arg; prev])))
  | Lambda (pats, _cs, body, _l) ->
    List.fold (List.rev pats) ~init:(exprO body)
      ~f:(fun prev (_cs, pat) ->
          (Exp.fun_ Asttypes.Nolabel None (patO pat) prev))
  | Binops (lhs, rest, _l) ->
    List.fold ~init:(exprO lhs) rest
      ~f:(fun prev (_cs, ref_, _cs2, rhs) ->
          Exp.apply
            (ref_O ref_)
            [(Asttypes.Nolabel, prev); as_arg rhs])
  | If ((ifcond, ifbody), [], elsebody) ->
    (* TODO: more clauses *)
    Exp.ifthenelse
      (ifcond |> skip_commented |> exprO)
      (ifbody |> skip_commented |> exprO)
      (Some (elsebody |> skip_preCommented |> exprO))

  | _ -> Exp.constant (Const.string (todo "expr" (show_exprp exprp)))

and exprO (_r, exprp) : Parsetree.expression =
  exprpO exprp

and as_arg (expr: expr) : (Asttypes.arg_label * Parsetree.expression) =
  (Asttypes.Nolabel, exprO expr)

and ref_O r =
  match r with
  | VarRef (path, var) ->
    Exp.ident (fullname (path @ [var]))
  | OpRef op ->
    Exp.ident (varname op)
  | TagRef (path, var) ->
    Exp.construct (fullname (path @ [var])) None





(* let x (a:int) b c = *)
let toplevelLet name (args: pattern list) (expr: expr) : Parsetree.structure_item =
  let args =
    List.fold (List.rev args) ~init:(exprO expr)
      ~f:(fun prev arg ->
          (Exp.fun_ Asttypes.Nolabel None (patO arg) prev))
  in
  let let_ =
    Vb.mk
      (Pat.var (as_var name))
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
          (*             (Pat.var (as_var name)) *)
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

let rec type_O (t: type_) : Parsetree.core_type =
  (match skip_located t with
   (* | FunctionType ft -> *)
   (*   let (first, _eol) = ft.first in *)
   (*   let rest = *)
   (*     List.map ~f:(fun (_c1, _c2, type_, _eol) -> type_) *)
   (*       ft.rest *)
   (*   in *)
   (*   List.map ~f:type_O (first :: rest) *)
   (*  *)
   (* Typ.arrow  *)
   (* | TypeConstruction (tc, ts) -> *)
   (*   (match tc with *)
   (*    | TupleConstructor i -> failwith "tupleconstructor" *)
   (*    | NamedConstructor nc -> *)
   (*      failwith "namedconstructor" (*nc*)) *)
   | TypeVariable name ->
     Typ.var name
   | _ -> Typ.var (todo "type" (show_type_ t))
  )



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
       (* type_O type_ *)
     | Definition ((_, VarPattern name), args, _c, expr) ->
       let args = List.map args
           ~f:(fun (_l, pat) -> pat)
       in
       [toplevelLet name args expr]
     | TypeAlias (_cs, names, (_c, type_)) ->
     (* type name is a constr *)
       (* type arguments is constr with args *)
       (* record field is Type.field *)

       (* There isn't a record helper, sp use Ptype_record  *)
       (* use Typ.mk *)

       let kind = Parsetree.Ptype_record [Type.field (as_var "x") (Typ.constr (varname "int") [])] in
       let typ = Type.mk ~kind (as_var "todo") in
       [Str.type_ Nonrecursive [typ]]
     | Datatype { nameWithArgs; tags } ->
       let (name, args) = skip_commented nameWithArgs in
       let params =
         List.map args
           ~f:(fun arg -> (arg
                           |> skip_preCommented
                           |> Typ.var
                          , Asttypes.Invariant))
       in
       let constructors =
         tags
         |> openCommentedList2list
         |> List.map ~f:(fun (name, types) ->
             let args = List.map types
                 ~f:(fun t -> t |> skip_preCommented |> type_O)
             in
             Type.constructor (as_var name)
               ~args:(Parsetree.Pcstr_tuple args))
       in
       let kind = Parsetree.Ptype_variant constructors in
       let name = String.uncapitalize name in
       [Str.type_ Recursive
          [(Type.mk ~params ~kind (as_var name))]]

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
    Lexing.from_channel In_channel.stdin
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


