open Tc
open Prelude

type t = Types.fluidExpr

let id (expr : t) : Types.id =
  match expr with
  | EOldExpr expr ->
      Blank.toID expr
  | EInteger (id, _)
  | EString (id, _)
  | EBool (id, _)
  | ENull id
  | EFloat (id, _, _)
  | EVariable (id, _)
  | EFieldAccess (id, _, _, _)
  | EFnCall (id, _, _, _)
  | ELambda (id, _, _)
  | EBlank id
  | ELet (id, _, _, _, _)
  | EIf (id, _, _, _)
  | EPartial (id, _, _)
  | ERightPartial (id, _, _)
  | EList (id, _)
  | ERecord (id, _)
  | EPipe (id, _)
  | EPipeTarget id
  | EBinOp (id, _, _, _, _)
  | EConstructor (id, _, _, _)
  | EFeatureFlag (id, _, _, _, _, _)
  | EMatch (id, _, _) ->
      id


let literalToString
    (v :
      [> `Bool of bool | `Int of string | `Null | `Float of string * string]) :
    string =
  match v with
  | `Int i ->
      i
  | `String str ->
      "\"" ^ str ^ "\""
  | `Bool b ->
      if b then "true" else "false"
  | `Null ->
      "null"
  | `Float (whole, fraction) ->
      whole ^ "." ^ fraction


let rec toPattern (p : Types.fluidPattern) : Types.pattern =
  match p with
  | FPVariable (_, id, var) ->
      F (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      F (id, PConstructor (name, List.map ~f:toPattern patterns))
  | FPInteger (_, id, i) ->
      F (id, PLiteral (literalToString (`Int i)))
  | FPBool (_, id, b) ->
      F (id, PLiteral (literalToString (`Bool b)))
  | FPString (_, id, str) ->
      F (id, PLiteral (literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      F (id, PLiteral (literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      F (id, PLiteral (literalToString `Null))
  | FPBlank (_, id) ->
      Blank id
  | FPOldPattern (_, pattern) ->
      pattern


let rec toExpr ?(inPipe = false) (expr : t) : Types.expr =
  (* inPipe is whether it's the immediate child of a pipe. *)
  let r = toExpr ~inPipe:false in
  match expr with
  | EInteger (id, num) ->
      F (id, Value (literalToString (`Int num)))
  | EString (id, str) ->
      F (id, Value (literalToString (`String str)))
  | EFloat (id, whole, fraction) ->
      F (id, Value (literalToString (`Float (whole, fraction))))
  | EBool (id, b) ->
      F (id, Value (literalToString (`Bool b)))
  | ENull id ->
      F (id, Value (literalToString `Null))
  | EVariable (id, var) ->
      F (id, Variable var)
  | EFieldAccess (id, obj, fieldID, "") ->
      F (id, FieldAccess (toExpr obj, Blank fieldID))
  | EFieldAccess (id, obj, fieldID, fieldname) ->
      F (id, FieldAccess (toExpr obj, F (fieldID, fieldname)))
  | EFnCall (id, name, args, ster) ->
    ( match args with
    | EPipeTarget _ :: _ when not inPipe ->
        recover "fn has a pipe target but no pipe" ~debug:expr (Blank.new_ ())
    | EPipeTarget _ :: args when inPipe ->
        F
          ( id
          , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
          )
    | _nonPipeTarget :: _ when inPipe ->
        recover "fn has a pipe but no pipe target" ~debug:expr (Blank.new_ ())
    | args ->
        F
          ( id
          , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
          ) )
  | EBinOp (id, name, arg1, arg2, ster) ->
    ( match arg1 with
    | EPipeTarget _ when not inPipe ->
        recover
          "binop has a pipe target but no pipe"
          ~debug:expr
          (Blank.new_ ())
    | EPipeTarget _ when inPipe ->
        F (id, FnCall (F (ID (deID id ^ "_name"), name), [toExpr arg2], ster))
    | _nonPipeTarget when inPipe ->
        recover
          "binop has a pipe but no pipe target"
          ~debug:expr
          (Blank.new_ ())
    | _ ->
        F
          ( id
          , FnCall
              ( F (ID (deID id ^ "_name"), name)
              , [toExpr arg1; toExpr arg2]
              , ster ) ) )
  | ELambda (id, vars, body) ->
      F
        ( id
        , Lambda
            ( List.map vars ~f:(fun (vid, var) -> Types.F (vid, var))
            , toExpr body ) )
  | EBlank id ->
      Blank id
  | ELet (id, lhsID, lhs, rhs, body) ->
      F (id, Let (F (lhsID, lhs), toExpr rhs, toExpr body))
  | EIf (id, cond, thenExpr, elseExpr) ->
      F (id, If (toExpr cond, toExpr thenExpr, toExpr elseExpr))
  | EPartial (id, str, oldVal) ->
      F (id, FluidPartial (str, toExpr ~inPipe oldVal))
  | ERightPartial (id, str, oldVal) ->
      F (id, FluidRightPartial (str, toExpr ~inPipe oldVal))
  | EList (id, exprs) ->
      F (id, ListLiteral (List.map ~f:r exprs))
  | ERecord (id, pairs) ->
      F
        ( id
        , ObjectLiteral
            (List.map pairs ~f:(fun (id, k, v) -> (Types.F (id, k), toExpr v)))
        )
  | EPipe (id, exprs) ->
    ( match exprs with
    | head :: tail ->
        F (id, Thread (r head :: List.map ~f:(toExpr ~inPipe:true) tail))
    | [] ->
        Blank id )
  | EConstructor (id, nameID, name, exprs) ->
      F (id, Constructor (F (nameID, name), List.map ~f:r exprs))
  | EMatch (id, mexpr, pairs) ->
      let pairs = List.map pairs ~f:(fun (p, e) -> (toPattern p, toExpr e)) in
      F (id, Match (toExpr mexpr, pairs))
  | EPipeTarget _ ->
      recover
        "Cant convert pipetargets back to exprs"
        ~debug:expr
        (Blank.new_ ())
  | EFeatureFlag (id, name, nameID, cond, caseA, caseB) ->
      F
        ( id
        , FeatureFlag
            ( F (nameID, name)
            , toExpr cond
            , toExpr ~inPipe caseA
            , toExpr ~inPipe caseB ) )
  | EOldExpr expr ->
      expr


let rec find (target : Types.id) (expr : t) : t option =
  let fe = find target in
  if id expr = target
  then Some expr
  else
    match expr with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | EBool _
    | ENull _
    | EPipeTarget _
    | EFloat _ ->
        None
    | ELet (_, _, _, rhs, next) ->
        fe rhs |> Option.orElse (fe next)
    | EIf (_, cond, ifexpr, elseexpr) ->
        fe cond |> Option.orElse (fe ifexpr) |> Option.orElse (fe elseexpr)
    | EBinOp (_, _, lexpr, rexpr, _) ->
        fe lexpr |> Option.orElse (fe rexpr)
    | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
        fe expr
    | ERecord (_, fields) ->
        fields |> List.map ~f:Tuple3.third |> List.filterMap ~f:fe |> List.head
    | EMatch (_, expr, pairs) ->
        fe expr
        |> Option.orElse
             ( pairs
             |> List.map ~f:Tuple2.second
             |> List.filterMap ~f:fe
             |> List.head )
    | EFnCall (_, _, exprs, _)
    | EList (_, exprs)
    | EConstructor (_, _, _, exprs)
    | EPipe (_, exprs) ->
        List.filterMap ~f:fe exprs |> List.head
    | EOldExpr _ ->
        None
    | EPartial (_, _, oldExpr) | ERightPartial (_, _, oldExpr) ->
        fe oldExpr
    | EFeatureFlag (_, _, _, cond, casea, caseb) ->
        fe cond |> Option.orElse (fe casea) |> Option.orElse (fe caseb)


let findParent (target : Types.id) (expr : t) : t option =
  let rec findParent' ~(parent : t option) (target : Types.id) (expr : t) :
      t option =
    let fp = findParent' ~parent:(Some expr) target in
    if id expr = target
    then parent
    else
      match expr with
      | EInteger _
      | EBlank _
      | EString _
      | EVariable _
      | EBool _
      | ENull _
      | EPipeTarget _
      | EFloat _ ->
          None
      | ELet (_, _, _, rhs, next) ->
          fp rhs |> Option.orElse (fp next)
      | EIf (_, cond, ifexpr, elseexpr) ->
          fp cond |> Option.orElse (fp ifexpr) |> Option.orElse (fp elseexpr)
      | EBinOp (_, _, lexpr, rexpr, _) ->
          fp lexpr |> Option.orElse (fp rexpr)
      | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
          fp expr
      | EMatch (_, _, pairs) ->
          pairs
          |> List.map ~f:Tuple2.second
          |> List.filterMap ~f:fp
          |> List.head
      | ERecord (_, fields) ->
          fields
          |> List.map ~f:Tuple3.third
          |> List.filterMap ~f:fp
          |> List.head
      | EFnCall (_, _, exprs, _)
      | EList (_, exprs)
      | EConstructor (_, _, _, exprs)
      | EPipe (_, exprs) ->
          List.filterMap ~f:fp exprs |> List.head
      | EOldExpr _ ->
          None
      | EPartial (_, _, expr) ->
          fp expr
      | ERightPartial (_, _, expr) ->
          fp expr
      | EFeatureFlag (_, _, _, cond, casea, caseb) ->
          fp cond |> Option.orElse (fp casea) |> Option.orElse (fp caseb)
  in
  findParent' ~parent:None target expr


let isBlank (t : t) : bool = match t with EBlank _ -> true | _ -> false

let isEmpty (t : t) : bool =
  match t with
  | EBlank _ ->
      true
  | ERecord (_, []) ->
      true
  | ERecord (_, l) ->
      l
      |> List.filter ~f:(fun (_, k, v) -> k = "" && not (isBlank v))
      |> List.isEmpty
  | EList (_, l) ->
      l |> List.filter ~f:(not << isBlank) |> List.isEmpty
  | _ ->
      false


let isEmptyId (target : Types.id) (t : t) : bool =
  match find target t with Some e -> isEmpty e | _ -> false


(* f needs to call recurse or it won't go far *)
let recurse ~(f : t -> t) (expr : t) : t =
  match expr with
  | EInteger _
  | EBlank _
  | EString _
  | EVariable _
  | EBool _
  | ENull _
  | EPipeTarget _
  | EFloat _ ->
      expr
  | ELet (id, lhsID, name, rhs, next) ->
      ELet (id, lhsID, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, f cond, f ifexpr, f elseexpr)
  | EBinOp (id, op, lexpr, rexpr, ster) ->
      EBinOp (id, op, f lexpr, f rexpr, ster)
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      EFieldAccess (id, f expr, fieldID, fieldname)
  | EFnCall (id, name, exprs, ster) ->
      EFnCall (id, name, List.map ~f exprs, ster)
  | ELambda (id, names, expr) ->
      ELambda (id, names, f expr)
  | EList (id, exprs) ->
      EList (id, List.map ~f exprs)
  | EMatch (id, mexpr, pairs) ->
      EMatch
        (id, f mexpr, List.map ~f:(fun (name, expr) -> (name, f expr)) pairs)
  | ERecord (id, fields) ->
      ERecord
        (id, List.map ~f:(fun (id, name, expr) -> (id, name, f expr)) fields)
  | EPipe (id, exprs) ->
      EPipe (id, List.map ~f exprs)
  | EConstructor (id, nameID, name, exprs) ->
      EConstructor (id, nameID, name, List.map ~f exprs)
  | EOldExpr _ ->
      expr
  | EPartial (id, str, oldExpr) ->
      EPartial (id, str, f oldExpr)
  | ERightPartial (id, str, oldExpr) ->
      ERightPartial (id, str, f oldExpr)
  | EFeatureFlag (id, msg, msgid, cond, casea, caseb) ->
      EFeatureFlag (id, msg, msgid, f cond, f casea, f caseb)


let update ~(f : t -> t) (target : Types.id) (ast : t) : t =
  let rec run e = if target = id e then f e else recurse ~f:run e in
  run ast


let removePipe (id : Types.id) (ast : t) (index : int) : t =
  let index =
    (* remove expression in front of pipe, not behind it *)
    index + 1
  in
  update id ast ~f:(fun e ->
      match e with
      | EPipe (_, [e1; _]) ->
          e1
      | EPipe (id, exprs) ->
          EPipe (id, List.removeAt ~index exprs)
      | _ ->
          e )


let removeListSepToken (id : Types.id) (ast : t) (index : int) : t =
  let index =
    (* remove expression in front of sep, not behind it *)
    index + 1
  in
  update id ast ~f:(fun e ->
      match e with
      | EList (id, exprs) ->
          EList (id, List.removeAt ~index exprs)
      | _ ->
          e )


let insertInList ~(index : int) ~(newExpr : t) (id : Types.id) (ast : t) : t =
  update id ast ~f:(fun e ->
      match e with
      | EList (id, exprs) ->
          EList (id, List.insertAt ~index ~value:newExpr exprs)
      | _ ->
          recover "not a list in insertInList" ~debug:e e )


(* Add a blank after the expr indicated by id, which we presume is in a list *)
let addBlankToList (target : Types.id) (ast : t) : t =
  let parent = findParent target ast in
  match parent with
  | Some (EList (pID, exprs)) ->
    ( match List.findIndex ~f:(fun e -> id e = target) exprs with
    | Some index ->
        insertInList ~index:(index + 1) ~newExpr:(EBlank (gid ())) pID ast
    | _ ->
        ast )
  | _ ->
      ast


(* Slightly modified version of `AST.uses` (pre-fluid code) *)
let rec updateVariableUses (oldVarName : string) (ast : t) ~(f : t -> t) : t =
  let u = updateVariableUses oldVarName ~f in
  match ast with
  | EVariable (_, varName) ->
      if varName = oldVarName then f ast else ast
  | ELet (id, id', lhs, rhs, body) ->
      if oldVarName = lhs (* if variable name is rebound *)
      then ast
      else ELet (id, id', lhs, u rhs, u body)
  | ELambda (id, vars, lexpr) ->
      if List.map ~f:Tuple2.second vars |> List.member ~value:oldVarName
         (* if variable name is rebound *)
      then ast
      else ELambda (id, vars, u lexpr)
  | EMatch (id, cond, pairs) ->
      let pairs =
        List.map
          ~f:(fun (pat, expr) ->
            if Pattern.hasVariableNamed oldVarName (toPattern pat)
            then (pat, expr)
            else (pat, u expr) )
          pairs
      in
      EMatch (id, u cond, pairs)
  | _ ->
      recurse ~f:u ast


let renameVariableUses (oldName : string) (newName : string) (ast : t) : t =
  updateVariableUses oldName ast ~f:(function
      | EVariable (id, _) ->
          EVariable (id, newName)
      | _ as e ->
          e )


let removeVariableUse (oldVarName : string) (ast : t) : t =
  updateVariableUses oldVarName ast ~f:(fun _ -> EBlank (gid ()))
