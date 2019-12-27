open Prelude
open Tc
open Types

type t = Types.fluidExpr [@@deriving show]

let newB () = EBlank (gid ())

let id (expr : t) : Types.id =
  match expr with
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
      | EPartial (_, _, expr) ->
          fp expr
      | ERightPartial (_, _, expr) ->
          fp expr
      | EFeatureFlag (_, _, _, cond, casea, caseb) ->
          fp cond |> Option.orElse (fp casea) |> Option.orElse (fp caseb)
  in
  findParent' ~parent:None target expr


(* It's too hard to thread the state everywhere it's needed. At some stage we
 * will remove this need to convert from expressions and then we don't need
 * this at all. *)
let functions = ref []

let rec fromNExpr' ?(inPipe = false) (expr : Types.expr) : t =
  let fns =
    assertFn ~f:(( <> ) []) "empty functions passed to fromNExpr" !functions
  in
  let f = fromNExpr' ~inPipe:false in
  let varToName var = match var with Blank _ -> "" | F (_, name) -> name in
  let parseString str :
      [> `Bool of bool
      | `Int of string
      | `Null
      | `Float of string * string
      | `Unknown ] =
    let asBool =
      if str = "true"
      then Some (`Bool true)
      else if str = "false"
      then Some (`Bool false)
      else if str = "null"
      then Some `Null
      else None
    in
    let asInt = if FluidUtil.is63BitInt str then Some (`Int str) else None in
    let asFloat =
      try
        (* for the exception *)
        ignore (float_of_string str) ;
        match String.split ~on:"." str with
        | [whole; fraction] ->
            Some (`Float (whole, fraction))
        | _ ->
            None
      with _ -> None
    in
    let asString =
      if String.startsWith ~prefix:"\"" str && String.endsWith ~suffix:"\"" str
      then
        Some
          (`String
            (str |> String.dropLeft ~count:1 |> String.dropRight ~count:1))
      else None
    in
    asInt
    |> Option.or_ asString
    |> Option.or_ asBool
    |> Option.or_ asFloat
    |> Option.withDefault ~default:`Unknown
  in
  match expr with
  | Blank id ->
      EBlank id
  | F (id, nExpr) ->
    ( match nExpr with
    | Let (name, rhs, body) ->
        ELet (id, Blank.toID name, varToName name, f rhs, f body)
    | Variable varname ->
        EVariable (id, varname)
    | If (cond, thenExpr, elseExpr) ->
        EIf (id, f cond, f thenExpr, f elseExpr)
    | ListLiteral exprs ->
        EList (id, List.map ~f exprs)
    | ObjectLiteral pairs ->
        ERecord
          ( id
          , List.map pairs ~f:(fun (k, v) -> (Blank.toID k, varToName k, f v))
          )
    | FieldAccess (expr, field) ->
        EFieldAccess (id, f expr, Blank.toID field, varToName field)
    | FnCall (name, args, ster) ->
        let args = List.map ~f args in
        (* add a pipetarget in the front *)
        let args = if inPipe then EPipeTarget (gid ()) :: args else args in
        let fnCall = EFnCall (id, varToName name, args, ster) in
        let fn = List.find fns ~f:(fun fn -> fn.fnName = varToName name) in
        ( match fn with
        | Some fn when fn.fnInfix ->
          ( match args with
          | [a; b] ->
              EBinOp (id, varToName name, a, b, ster)
          | _ ->
              fnCall )
        | _ ->
            fnCall )
    | Thread exprs ->
      ( match exprs with
      | head :: tail ->
          EPipe (id, f head :: List.map ~f:(fromNExpr' ~inPipe:true) tail)
      | _ ->
          recover "empty pipe" ~debug:expr (newB ()) )
    | Lambda (varnames, exprs) ->
        ELambda
          ( id
          , List.map varnames ~f:(fun var -> (Blank.toID var, varToName var))
          , f exprs )
    | Value str ->
      ( match parseString str with
      | `Bool b ->
          EBool (id, b)
      | `Int i ->
          EInteger (id, i)
      | `String s ->
          EString (id, s)
      | `Null ->
          ENull id
      | `Float (whole, fraction) ->
          EFloat (id, whole, fraction)
      | `Unknown ->
          EBlank id )
    | Constructor (name, exprs) ->
        EConstructor (id, Blank.toID name, varToName name, List.map ~f exprs)
    | Match (mexpr, pairs) ->
        let mid = id in
        let rec fromPattern (p : pattern) : fluidPattern =
          match p with
          | Blank id ->
              FPBlank (mid, id)
          | F (id, np) ->
            ( match np with
            | PVariable name ->
                FPVariable (mid, id, name)
            | PConstructor (name, patterns) ->
                FPConstructor (mid, id, name, List.map ~f:fromPattern patterns)
            | PLiteral str ->
              ( match parseString str with
              | `Bool b ->
                  FPBool (mid, id, b)
              | `Int i ->
                  FPInteger (mid, id, i)
              | `String s ->
                  FPString (mid, id, s)
              | `Null ->
                  FPNull (mid, id)
              | `Float (whole, fraction) ->
                  FPFloat (mid, id, whole, fraction)
              | `Unknown ->
                  FPBlank (mid, id) ) )
        in
        let pairs = List.map pairs ~f:(fun (p, e) -> (fromPattern p, f e)) in
        EMatch (id, f mexpr, pairs)
    | FeatureFlag (msg, cond, casea, caseb) ->
        EFeatureFlag
          ( id
          , varToName msg
          , Blank.toID msg
          , f cond
          , fromNExpr' ~inPipe casea
          , fromNExpr' ~inPipe caseb )
    | FluidPartial (str, oldExpr) ->
        EPartial (id, str, fromNExpr' ~inPipe oldExpr)
    | FluidRightPartial (str, oldExpr) ->
        ERightPartial (id, str, fromNExpr' ~inPipe oldExpr) )


let fromNExpr (expr : Types.expr) : t = fromNExpr' expr

let toNExpr (expr : t) : Types.expr =
  let open Types in
  let rec toNExpr' ?(inPipe = false) expr =
    (* inPipe is whether it's the immediate child of a pipe. *)
    let r = toNExpr' ~inPipe:false in
    match expr with
    | EInteger (id, num) ->
        F (id, Value (FluidUtil.literalToString (`Int num)))
    | EString (id, str) ->
        F (id, Value (FluidUtil.literalToString (`String str)))
    | EFloat (id, whole, fraction) ->
        F (id, Value (FluidUtil.literalToString (`Float (whole, fraction))))
    | EBool (id, b) ->
        F (id, Value (FluidUtil.literalToString (`Bool b)))
    | ENull id ->
        F (id, Value (FluidUtil.literalToString `Null))
    | EVariable (id, var) ->
        F (id, Variable var)
    | EFieldAccess (id, obj, fieldID, "") ->
        F (id, FieldAccess (toNExpr' obj, Blank fieldID))
    | EFieldAccess (id, obj, fieldID, fieldname) ->
        F (id, FieldAccess (toNExpr' obj, F (fieldID, fieldname)))
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
          F
            ( id
            , FnCall (F (ID (deID id ^ "_name"), name), [toNExpr' arg2], ster)
            )
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
                , [toNExpr' arg1; toNExpr' arg2]
                , ster ) ) )
    | ELambda (id, vars, body) ->
        F
          ( id
          , Lambda
              ( List.map vars ~f:(fun (vid, var) -> Types.F (vid, var))
              , toNExpr' body ) )
    | EBlank id ->
        Blank id
    | ELet (id, lhsID, lhs, rhs, body) ->
        F (id, Let (F (lhsID, lhs), toNExpr' rhs, toNExpr' body))
    | EIf (id, cond, thenExpr, elseExpr) ->
        F (id, If (toNExpr' cond, toNExpr' thenExpr, toNExpr' elseExpr))
    | EPartial (id, str, oldVal) ->
        F (id, FluidPartial (str, toNExpr' ~inPipe oldVal))
    | ERightPartial (id, str, oldVal) ->
        F (id, FluidRightPartial (str, toNExpr' ~inPipe oldVal))
    | EList (id, exprs) ->
        F (id, ListLiteral (List.map ~f:r exprs))
    | ERecord (id, pairs) ->
        F
          ( id
          , ObjectLiteral
              (List.map pairs ~f:(fun (id, k, v) ->
                   (Types.F (id, k), toNExpr' v))) )
    | EPipe (id, exprs) ->
      ( match exprs with
      | head :: tail ->
          F (id, Thread (r head :: List.map ~f:(toNExpr' ~inPipe:true) tail))
      | [] ->
          Blank id )
    | EConstructor (id, nameID, name, exprs) ->
        F (id, Constructor (F (nameID, name), List.map ~f:r exprs))
    | EMatch (id, mexpr, pairs) ->
        let pairs =
          List.map pairs ~f:(fun (p, e) ->
              (FluidPattern.toPattern p, toNExpr' e))
        in
        F (id, Match (toNExpr' mexpr, pairs))
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
              , toNExpr' cond
              , toNExpr' ~inPipe caseA
              , toNExpr' ~inPipe caseB ) )
  in
  toNExpr' expr


let isBlank (expr : t) = match expr with EBlank _ -> true | _ -> false

let isEmpty (expr : t) : bool =
  match expr with
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


let hasEmptyWithId (id : Types.id) (expr : t) : bool =
  match find id expr with Some e -> isEmpty e | _ -> false


let walk ~(f : t -> t) (expr : t) : t =
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
  | EPartial (id, str, oldExpr) ->
      EPartial (id, str, f oldExpr)
  | ERightPartial (id, str, oldExpr) ->
      ERightPartial (id, str, f oldExpr)
  | EFeatureFlag (id, msg, msgid, cond, casea, caseb) ->
      EFeatureFlag (id, msg, msgid, f cond, f casea, f caseb)


let update ?(failIfMissing = true) ~(f : t -> t) (target : Types.id) (ast : t) :
    t =
  let found = ref false in
  let rec run e =
    if target = id e
    then (
      found := true ;
      f e )
    else walk ~f:run e
  in
  let finished = run ast in
  if failIfMissing
  then
    asserT
      ~debug:(target, ast)
      "didn't find the id in the expression to update"
      !found ;
  finished


(* FIXME: [replace] is just [update] with a hack for EPipe.
 * It's very unclear which to use at what point and likely to cause bugs.
 * We should either hide [update] from the public interface of FluidExpression
 * or remove [replace] and put the special-case EPipe logic into the calling code. *)
let replace ~(replacement : t) (target : Types.id) (ast : t) : t =
  let open Types in
  (* If we're putting a pipe into another pipe, fix it up *)
  let target', newExpr' =
    match (findParent target ast, replacement) with
    | Some (EPipe (parentID, oldExprs)), EPipe (newID, newExprs) ->
        let before, elemAndAfter =
          List.splitWhen ~f:(fun nested -> id nested = target) oldExprs
        in
        let after = List.tail elemAndAfter |> Option.withDefault ~default:[] in
        (parentID, EPipe (newID, before @ newExprs @ after))
    | _ ->
        (target, replacement)
  in
  update target' ast ~f:(fun _ -> newExpr')


(* Slightly modified version of `AST.uses` (pre-fluid code) *)
let rec updateVariableUses (oldVarName : string) ~(f : t -> t) (ast : t) : t =
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
            if Pattern.hasVariableNamed oldVarName (FluidPattern.toPattern pat)
            then (pat, expr)
            else (pat, u expr))
          pairs
      in
      EMatch (id, u cond, pairs)
  | _ ->
      walk ~f:u ast


let renameVariableUses ~(oldName : string) ~(newName : string) (ast : t) : t =
  let f expr =
    match expr with EVariable (id, _) -> EVariable (id, newName) | _ -> expr
  in
  updateVariableUses oldName ~f ast


let removeVariableUse (oldVarName : string) (ast : t) : t =
  let f _ = EBlank (gid ()) in
  updateVariableUses oldVarName ~f ast


let rec clone (expr : t) : t =
  let c e = clone e in
  let cl es = List.map ~f:c es in
  match expr with
  | ELet (_, _, lhs, rhs, body) ->
      ELet (gid (), gid (), lhs, c rhs, c body)
  | EIf (_, cond, ifbody, elsebody) ->
      EIf (gid (), c cond, c ifbody, c elsebody)
  | EFnCall (_, name, exprs, r) ->
      EFnCall (gid (), name, cl exprs, r)
  | EBinOp (_, name, left, right, r) ->
      EBinOp (gid (), name, c left, c right, r)
  | ELambda (_, vars, body) ->
      ELambda (gid (), List.map vars ~f:(fun (_, var) -> (gid (), var)), c body)
  | EPipe (_, exprs) ->
      EPipe (gid (), cl exprs)
  | EFieldAccess (_, obj, _, field) ->
      EFieldAccess (gid (), c obj, gid (), field)
  | EString (_, v) ->
      EString (gid (), v)
  | EInteger (_, v) ->
      EInteger (gid (), v)
  | EBool (_, v) ->
      EBool (gid (), v)
  | EFloat (_, whole, fraction) ->
      EFloat (gid (), whole, fraction)
  | ENull _ ->
      ENull (gid ())
  | EBlank _ ->
      EBlank (gid ())
  | EVariable (_, name) ->
      EVariable (gid (), name)
  | EList (_, exprs) ->
      EList (gid (), cl exprs)
  | ERecord (_, pairs) ->
      ERecord (gid (), List.map ~f:(fun (_, k, v) -> (gid (), k, c v)) pairs)
  | EFeatureFlag (_, name, _, cond, a, b) ->
      EFeatureFlag (gid (), name, gid (), c cond, c a, c b)
  | EMatch (_, matchExpr, cases) ->
      let mid = gid () in
      EMatch
        ( mid
        , c matchExpr
        , List.map ~f:(fun (k, v) -> (FluidPattern.clone mid k, c v)) cases )
  | EConstructor (_, _, name, args) ->
      EConstructor (gid (), gid (), name, cl args)
  | EPartial (_, str, oldExpr) ->
      EPartial (gid (), str, c oldExpr)
  | ERightPartial (_, str, oldExpr) ->
      ERightPartial (gid (), str, c oldExpr)
  | EPipeTarget _ ->
      EPipeTarget (gid ())
