open Prelude

type t = Types.fluidExpr [@@deriving show]

let functions = ref []

let newB () = EBlank (gid ())

let id (expr : t) : Types.id =
  match expr with
  | EInteger (id, _)
  | EString (id, _)
  | EBool (id, _)
  | ENull id
  | EFloat (id, _, _)
  | EVariable (id, _)
  | EFieldAccess (id, _, _)
  | EFnCall (id, _, _, _)
  | ELambda (id, _, _)
  | EBlank id
  | ELet (id, _, _, _)
  | EIf (id, _, _, _)
  | EPartial (id, _, _)
  | ERightPartial (id, _, _)
  | EList (id, _)
  | ERecord (id, _)
  | EPipe (id, _)
  | EPipeTarget id
  | EBinOp (id, _, _, _, _)
  | EConstructor (id, _, _)
  | EFeatureFlag (id, _, _, _, _)
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
    | ELet (_, _, rhs, next) ->
        fe rhs |> Option.orElseLazy (fun () -> fe next)
    | EIf (_, cond, ifexpr, elseexpr) ->
        fe cond
        |> Option.orElseLazy (fun () -> fe ifexpr)
        |> Option.orElseLazy (fun () -> fe elseexpr)
    | EBinOp (_, _, lexpr, rexpr, _) ->
        fe lexpr |> Option.orElseLazy (fun () -> fe rexpr)
    | EFieldAccess (_, expr, _) | ELambda (_, _, expr) ->
        fe expr
    | ERecord (_, fields) ->
        fields |> List.map ~f:Tuple2.second |> List.findMap ~f:fe
    | EMatch (_, expr, pairs) ->
        fe expr
        |> Option.orElseLazy (fun () ->
               pairs |> List.map ~f:Tuple2.second |> List.findMap ~f:fe)
    | EFnCall (_, _, exprs, _)
    | EList (_, exprs)
    | EConstructor (_, _, exprs)
    | EPipe (_, exprs) ->
        List.findMap ~f:fe exprs
    | EPartial (_, _, oldExpr) | ERightPartial (_, _, oldExpr) ->
        fe oldExpr
    | EFeatureFlag (_, _, cond, casea, caseb) ->
        fe cond
        |> Option.orElseLazy (fun () -> fe casea)
        |> Option.orElseLazy (fun () -> fe caseb)


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
      | ELet (_, _, rhs, next) ->
          List.findMap ~f:fp [rhs; next]
      | EIf (_, cond, ifexpr, elseexpr) ->
          List.findMap ~f:fp [cond; ifexpr; elseexpr]
      | EBinOp (_, _, lexpr, rexpr, _) ->
          List.findMap ~f:fp [lexpr; rexpr]
      | EFieldAccess (_, expr, _) | ELambda (_, _, expr) ->
          fp expr
      | EMatch (_, _, pairs) ->
          pairs |> List.map ~f:Tuple2.second |> List.findMap ~f:fp
      | ERecord (_, fields) ->
          fields |> List.map ~f:Tuple2.second |> List.findMap ~f:fp
      | EFnCall (_, _, exprs, _)
      | EList (_, exprs)
      | EConstructor (_, _, exprs)
      | EPipe (_, exprs) ->
          List.findMap ~f:fp exprs
      | EPartial (_, _, expr) ->
          fp expr
      | ERightPartial (_, _, expr) ->
          fp expr
      | EFeatureFlag (_, _, cond, casea, caseb) ->
          List.findMap ~f:fp [cond; casea; caseb]
  in
  findParent' ~parent:None target expr


let isBlank (expr : t) = match expr with EBlank _ -> true | _ -> false

let isEmpty (expr : t) : bool =
  match expr with
  | EBlank _ ->
      true
  | ERecord (_, []) ->
      true
  | ERecord (_, l) ->
      l
      |> List.filter ~f:(fun (k, v) -> k = "" && not (isBlank v))
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
  | ELet (id, name, rhs, next) ->
      ELet (id, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, f cond, f ifexpr, f elseexpr)
  | EBinOp (id, op, lexpr, rexpr, ster) ->
      EBinOp (id, op, f lexpr, f rexpr, ster)
  | EFieldAccess (id, expr, fieldname) ->
      EFieldAccess (id, f expr, fieldname)
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
      ERecord (id, List.map ~f:(fun (name, expr) -> (name, f expr)) fields)
  | EPipe (id, exprs) ->
      EPipe (id, List.map ~f exprs)
  | EConstructor (id, name, exprs) ->
      EConstructor (id, name, List.map ~f exprs)
  | EPartial (id, str, oldExpr) ->
      EPartial (id, str, f oldExpr)
  | ERightPartial (id, str, oldExpr) ->
      ERightPartial (id, str, f oldExpr)
  | EFeatureFlag (id, msg, cond, casea, caseb) ->
      EFeatureFlag (id, msg, f cond, f casea, f caseb)


let filterMap ~(f : t -> 'a option) (expr : t) : 'a list =
  let results = ref [] in
  let rec myWalk (e : t) : t =
    ( match f e with
    | Some r ->
        results := r :: !results ;
        ()
    | None ->
        () ) ;
    walk ~f:myWalk e
  in
  ignore (myWalk expr) ;
  List.reverse !results


let filter ~(f : t -> bool) (expr : t) : 'a list =
  filterMap ~f:(fun t -> if f t then Some t else None) expr


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
    if not !found
       (* prevents the significant performance cost of show_fluidExpr *)
    then
      asserT
        ~debug:(show_id target, show_fluidExpr ast)
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
  | ELet (id, lhs, rhs, body) ->
      if oldVarName = lhs (* if variable name is rebound *)
      then ast
      else ELet (id, lhs, u rhs, u body)
  | ELambda (id, vars, lexpr) ->
      if List.map ~f:Tuple2.second vars |> List.member ~value:oldVarName
         (* if variable name is rebound *)
      then ast
      else ELambda (id, vars, u lexpr)
  | EMatch (id, cond, pairs) ->
      let pairs =
        List.map
          ~f:(fun (pat, expr) ->
            if Pattern.hasVariableNamed oldVarName pat
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
  | ELet (_, lhs, rhs, body) ->
      ELet (gid (), lhs, c rhs, c body)
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
  | EFieldAccess (_, obj, field) ->
      EFieldAccess (gid (), c obj, field)
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
      ERecord (gid (), List.map ~f:(fun (k, v) -> (k, c v)) pairs)
  | EFeatureFlag (_, name, cond, a, b) ->
      EFeatureFlag (gid (), name, c cond, c a, c b)
  | EMatch (_, matchExpr, cases) ->
      let mid = gid () in
      EMatch
        ( mid
        , c matchExpr
        , List.map ~f:(fun (k, v) -> (FluidPattern.clone mid k, c v)) cases )
  | EConstructor (_, name, args) ->
      EConstructor (gid (), name, cl args)
  | EPartial (_, str, oldExpr) ->
      EPartial (gid (), str, c oldExpr)
  | ERightPartial (_, str, oldExpr) ->
      ERightPartial (gid (), str, c oldExpr)
  | EPipeTarget _ ->
      EPipeTarget (gid ())
