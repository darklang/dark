include Tc
open Shared

type sendToRail =
  | Rail
  | NoRail
[@@deriving show {with_path = false}, eq, ord, yojson {optional = true}]

(* See .mli for comments/descriptions of fields *)
type t =
  | EInteger of id * string
  | EBool of id * bool
  | EString of id * string
  | EFloat of id * string * string
  | ENull of id
  | EBlank of id
  | ELet of id * string * t * t
  | EIf of id * t * t * t
  | EBinOp of id * string * t * t * sendToRail
  | ELambda of id * (analysisID * string) list * t
  | EFieldAccess of id * t * string
  | EVariable of id * string
  | EFnCall of id * string * t list * sendToRail
  | EPartial of id * string * t
  | ERightPartial of id * string * t
  | ELeftPartial of Shared.id * string * t
  | EList of id * t list
  | ERecord of id * (string * t) list
  | EPipe of id * t list
  | EConstructor of id * string * t list
  | EMatch of id * t * (FluidPattern.t * t) list
  | EPipeTarget of id
  | EFeatureFlag of id * string * t * t * t
[@@deriving show {with_path = false}, eq, ord, yojson {optional = true}]

type fluidPatOrExpr =
  | Expr of t
  | Pat of FluidPattern.t
[@@deriving show {with_path = false}, eq]

let newB () = EBlank (gid ())

let toID (expr : t) : id =
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
  | ELeftPartial (id, _, _)
  | EList (id, _)
  | ERecord (id, _)
  | EPipe (id, _)
  | EPipeTarget id
  | EBinOp (id, _, _, _, _)
  | EConstructor (id, _, _)
  | EFeatureFlag (id, _, _, _, _)
  | EMatch (id, _, _) ->
      id


let rec findExprOrPat (target : id) (within : fluidPatOrExpr) :
    fluidPatOrExpr option =
  let id, patOrExprs =
    match within with
    | Expr expr ->
      ( match expr with
      | EInteger (id, _)
      | EBool (id, _)
      | EString (id, _)
      | EFloat (id, _, _)
      | ENull id
      | EBlank id
      | EVariable (id, _)
      | EPipeTarget id ->
          (id, [])
      | ELet (id, _, e1, e2) | EBinOp (id, _, e1, e2, _) ->
          (id, [Expr e1; Expr e2])
      | EIf (id, e1, e2, e3) | EFeatureFlag (id, _, e1, e2, e3) ->
          (id, [Expr e1; Expr e2; Expr e3])
      | ELambda (id, _, e1)
      | EFieldAccess (id, e1, _)
      | EPartial (id, _, e1)
      | ERightPartial (id, _, e1)
      | ELeftPartial (id, _, e1) ->
          (id, [Expr e1])
      | EFnCall (id, _, exprs, _)
      | EList (id, exprs)
      | EPipe (id, exprs)
      | EConstructor (id, _, exprs) ->
          (id, List.map exprs ~f:(fun e1 -> Expr e1))
      | ERecord (id, nameAndExprs) ->
          (id, List.map nameAndExprs ~f:(fun (_, e1) -> Expr e1))
      | EMatch (id, e1, pairs) ->
          ( id
          , Expr e1
            :: ( pairs
               |> List.map ~f:(fun (p1, e1) -> [Pat p1; Expr e1])
               |> List.flatten ) ) )
    | Pat pat ->
      ( match pat with
      | FPVariable (_, pid, _)
      | FPInteger (_, pid, _)
      | FPBool (_, pid, _)
      | FPNull (_, pid)
      | FPBlank (_, pid)
      | FPFloat (_, pid, _, _)
      | FPString {matchID = _; patternID = pid; str = _} ->
          (pid, [])
      | FPConstructor (_, pid, _, pats) ->
          (pid, List.map pats ~f:(fun p1 -> Pat p1)) )
  in
  if id = target
  then Some within
  else patOrExprs |> List.findMap ~f:(fun pOrE -> findExprOrPat target pOrE)


let rec find (target : id) (expr : t) : t option =
  let fe = find target in
  if toID expr = target
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
    | EPartial (_, _, oldExpr)
    | ERightPartial (_, _, oldExpr)
    | ELeftPartial (_, _, oldExpr) ->
        fe oldExpr
    | EFeatureFlag (_, _, cond, casea, caseb) ->
        fe cond
        |> Option.orElseLazy (fun () -> fe casea)
        |> Option.orElseLazy (fun () -> fe caseb)


let children (expr : t) : t list =
  match expr with
  (* None *)
  | EInteger _
  | EString _
  | EBool _
  | EFloat _
  | ENull _
  | EBlank _
  | EPipeTarget _
  | EVariable _ ->
      []
  (* One *)
  | EPartial (_, _, expr)
  | ERightPartial (_, _, expr)
  | ELeftPartial (_, _, expr)
  | ELambda (_, _, expr)
  | EFieldAccess (_, expr, _) ->
      [expr]
  (* Two *)
  | EBinOp (_, _, c0, c1, _) | ELet (_, _, c0, c1) ->
      [c0; c1]
  (* Three *)
  | EFeatureFlag (_, _, c0, c1, c2) | EIf (_, c0, c1, c2) ->
      [c0; c1; c2]
  (* List *)
  | EFnCall (_, _, exprs, _)
  | EList (_, exprs)
  | EConstructor (_, _, exprs)
  | EPipe (_, exprs) ->
      exprs
  (* Special *)
  | ERecord (_, pairs) ->
      pairs |> List.map ~f:Tuple2.second
  | EMatch (_, matchExpr, cases) ->
      let casePointers = cases |> List.map ~f:Tuple2.second in
      matchExpr :: casePointers


let findParent (target : id) (expr : t) : t option =
  let rec findParent' ~(parent : t option) (target : id) (expr : t) : t option =
    if toID expr = target
    then parent
    else
      let f = findParent' ~parent:(Some expr) target in
      List.findMap ~f (children expr)
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


let hasEmptyWithId (id : id) (expr : t) : bool =
  match find id expr with Some e -> isEmpty e | _ -> false


let rec preTraversal ~(f : t -> t) (expr : t) : t =
  let r = preTraversal ~f in
  let expr = f expr in
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
      ELet (id, name, r rhs, r next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, r cond, r ifexpr, r elseexpr)
  | EBinOp (id, op, lexpr, rexpr, ster) ->
      EBinOp (id, op, r lexpr, r rexpr, ster)
  | EFieldAccess (id, expr, fieldname) ->
      EFieldAccess (id, r expr, fieldname)
  | EFnCall (id, name, exprs, ster) ->
      EFnCall (id, name, List.map ~f:r exprs, ster)
  | ELambda (id, names, expr) ->
      ELambda (id, names, r expr)
  | EList (id, exprs) ->
      EList (id, List.map ~f:r exprs)
  | EMatch (id, mexpr, pairs) ->
      EMatch
        (id, r mexpr, List.map ~f:(fun (name, expr) -> (name, r expr)) pairs)
  | ERecord (id, fields) ->
      ERecord (id, List.map ~f:(fun (name, expr) -> (name, r expr)) fields)
  | EPipe (id, exprs) ->
      EPipe (id, List.map ~f:r exprs)
  | EConstructor (id, name, exprs) ->
      EConstructor (id, name, List.map ~f:r exprs)
  | EPartial (id, str, oldExpr) ->
      EPartial (id, str, r oldExpr)
  | ERightPartial (id, str, oldExpr) ->
      ERightPartial (id, str, r oldExpr)
  | ELeftPartial (id, str, oldExpr) ->
      ELeftPartial (id, str, r oldExpr)
  | EFeatureFlag (id, msg, cond, casea, caseb) ->
      EFeatureFlag (id, msg, r cond, r casea, r caseb)


let rec postTraversal ~(f : t -> t) (expr : t) : t =
  let r = postTraversal ~f in
  let result =
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
        ELet (id, name, r rhs, r next)
    | EIf (id, cond, ifexpr, elseexpr) ->
        EIf (id, r cond, r ifexpr, r elseexpr)
    | EBinOp (id, op, lexpr, rexpr, ster) ->
        EBinOp (id, op, r lexpr, r rexpr, ster)
    | EFieldAccess (id, expr, fieldname) ->
        EFieldAccess (id, r expr, fieldname)
    | EFnCall (id, name, exprs, ster) ->
        EFnCall (id, name, List.map ~f:r exprs, ster)
    | ELambda (id, names, expr) ->
        ELambda (id, names, r expr)
    | EList (id, exprs) ->
        EList (id, List.map ~f:r exprs)
    | EMatch (id, mexpr, pairs) ->
        EMatch
          (id, r mexpr, List.map ~f:(fun (name, expr) -> (name, r expr)) pairs)
    | ERecord (id, fields) ->
        ERecord (id, List.map ~f:(fun (name, expr) -> (name, r expr)) fields)
    | EPipe (id, exprs) ->
        EPipe (id, List.map ~f:r exprs)
    | EConstructor (id, name, exprs) ->
        EConstructor (id, name, List.map ~f:r exprs)
    | EPartial (id, str, oldExpr) ->
        EPartial (id, str, r oldExpr)
    | ERightPartial (id, str, oldExpr) ->
        ERightPartial (id, str, r oldExpr)
    | ELeftPartial (id, str, oldExpr) ->
        ELeftPartial (id, str, r oldExpr)
    | EFeatureFlag (id, msg, cond, casea, caseb) ->
        EFeatureFlag (id, msg, r cond, r casea, r caseb)
  in
  f result


let deprecatedWalk ~(f : t -> t) (expr : t) : t =
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
  | ELeftPartial (id, str, oldExpr) ->
      ELeftPartial (id, str, f oldExpr)
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
    deprecatedWalk ~f:myWalk e
  in
  ignore (myWalk expr) ;
  List.reverse !results


let filter ~(f : t -> bool) (expr : t) : t list =
  filterMap ~f:(fun t -> if f t then Some t else None) expr


let decendants (expr : t) : Shared.id list =
  let res = ref [] in
  preTraversal expr ~f:(fun e ->
      res := toID e :: !res ;
      e)
  |> ignore ;
  !res


let update ?(failIfMissing = true) ~(f : t -> t) (target : id) (ast : t) : t =
  let found = ref false in
  let rec run e =
    if target = toID e
    then (
      found := true ;
      f e )
    else deprecatedWalk ~f:run e
  in
  let finished = run ast in
  if failIfMissing
  then
    if not !found
    then
      (* prevents the significant performance cost of show *)
      Recover.asserT
        ~debug:(show_id target, show ast)
        "didn't find the id in the expression to update"
        !found ;
  finished


(* FIXME: [replace] is just [update] with a hack for EPipe.
 * It's very unclear which to use at what point and likely to cause bugs.
 * We should either hide [update] from the public interface of FluidExpression
 * or remove [replace] and put the special-case EPipe logic into the calling code. *)
let replace ~(replacement : t) (target : id) (ast : t) : t =
  (* If we're putting a pipe into another pipe, fix it up *)
  let target', newExpr' =
    match (findParent target ast, replacement) with
    | Some (EPipe (parentID, oldExprs)), EPipe (newID, newExprs) ->
        let before, elemAndAfter =
          List.splitWhen ~f:(fun nested -> toID nested = target) oldExprs
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
            if FluidPattern.hasVariableNamed oldVarName pat
            then (pat, expr)
            else (pat, u expr))
          pairs
      in
      EMatch (id, u cond, pairs)
  | _ ->
      deprecatedWalk ~f:u ast


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
  | ELeftPartial (id, str, oldExpr) ->
      ELeftPartial (id, str, c oldExpr)
  | EPipeTarget _ ->
      EPipeTarget (gid ())


let blanks = filter ~f:isBlank

let ids (ast : t) : id list = filter ast ~f:(fun _ -> true) |> List.map ~f:toID

let ancestors (id : id) (expr : t) : t list =
  let rec rec_ancestors (tofind : id) (walk : t list) (exp : t) =
    let rec_ id_ e_ walk_ = rec_ancestors id_ (e_ :: walk_) in
    let reclist id_ e_ walk_ exprs =
      exprs |> List.map ~f:(rec_ id_ e_ walk_) |> List.concat
    in
    if toID exp = tofind
    then walk
    else
      match exp with
      | EInteger _
      | EString _
      | EBool _
      | EFloat _
      | ENull _
      | EBlank _
      | EPipeTarget _ ->
          []
      | EVariable _ ->
          []
      | ELet (_, _, rhs, body) ->
          reclist id exp walk [rhs; body]
      | EIf (_, cond, ifbody, elsebody) ->
          reclist id exp walk [cond; ifbody; elsebody]
      | EFnCall (_, _, exprs, _) ->
          reclist id exp walk exprs
      | EBinOp (_, _, lhs, rhs, _) ->
          reclist id exp walk [lhs; rhs]
      | ELambda (_, _, lexpr) ->
          rec_ id exp walk lexpr
      | EPipe (_, exprs) ->
          reclist id exp walk exprs
      | EFieldAccess (_, obj, _) ->
          rec_ id exp walk obj
      | EList (_, exprs) ->
          reclist id expr walk exprs
      | ERecord (_, pairs) ->
          pairs |> List.map ~f:Tuple2.second |> reclist id expr walk
      | EFeatureFlag (_, _, cond, a, b) ->
          reclist id exp walk [cond; a; b]
      | EMatch (_, matchExpr, cases) ->
          reclist id exp walk (matchExpr :: List.map ~f:Tuple2.second cases)
      | EConstructor (_, _, args) ->
          reclist id exp walk args
      | EPartial (_, _, oldExpr) ->
          rec_ id exp walk oldExpr
      | ERightPartial (_, _, oldExpr) ->
          rec_ id exp walk oldExpr
      | ELeftPartial (_, _, oldExpr) ->
          rec_ id exp walk oldExpr
  in
  rec_ancestors id [] expr


let rec testEqualIgnoringIds (a : t) (b : t) : bool =
  (* helpers for recursive calls *)
  let eq = testEqualIgnoringIds in
  let eq2 (e, e') (f, f') = eq e e' && eq f f' in
  let eq3 (e, e') (f, f') (g, g') = eq e e' && eq f f' && eq g g' in
  let eqList l1 l2 =
    List.length l1 = List.length l2
    && List.map2 ~f:eq l1 l2 |> List.all ~f:identity
  in
  let rec peq (a : FluidPattern.t) (b : FluidPattern.t) =
    let peqList l1 l2 =
      List.length l1 = List.length l2
      && Tc.List.map2 ~f:peq l1 l2 |> Tc.List.all ~f:Tc.identity
    in
    match (a, b) with
    | FPVariable (_, _, name), FPVariable (_, _, name') ->
        name = name'
    | ( FPConstructor (_, _, name, patterns)
      , FPConstructor (_, _, name', patterns') ) ->
        name = name' && peqList patterns patterns'
    | FPString {str; _}, FPString {str = str'; _} ->
        str = str'
    | FPInteger (_, _, l), FPInteger (_, _, l') ->
        l = l'
    | FPFloat (_, _, w, f), FPFloat (_, _, w', f') ->
        (w, f) = (w', f')
    | FPBool (_, _, l), FPBool (_, _, l') ->
        l = l'
    | FPNull (_, _), FPNull (_, _) ->
        true
    | FPBlank (_, _), FPBlank (_, _) ->
        true
    | FPVariable _, _
    | FPConstructor _, _
    | FPString _, _
    | FPInteger _, _
    | FPFloat _, _
    | FPBool _, _
    | FPNull _, _
    | FPBlank _, _ ->
        false
  in
  match (a, b) with
  (* expressions with no values *)
  | ENull _, ENull _ | EBlank _, EBlank _ | EPipeTarget _, EPipeTarget _ ->
      true
  (* expressions with single string values *)
  | EInteger (_, v), EInteger (_, v')
  | EString (_, v), EString (_, v')
  | EVariable (_, v), EVariable (_, v') ->
      v = v'
  | EBool (_, v), EBool (_, v') ->
      v = v'
  | EFloat (_, whole, frac), EFloat (_, whole', frac') ->
      whole = whole' && frac = frac'
  | ELet (_, lhs, rhs, body), ELet (_, lhs', rhs', body') ->
      lhs = lhs' && eq2 (rhs, rhs') (body, body')
  | EIf (_, con, thn, els), EIf (_, con', thn', els') ->
      eq3 (con, con') (thn, thn') (els, els')
  | EList (_, l), EList (_, l') ->
      eqList l l'
  | EFnCall (_, name, args, toRail), EFnCall (_, name', args', toRail') ->
      name = name' && eqList args args' && toRail = toRail'
  | EBinOp (_, name, lhs, rhs, toRail), EBinOp (_, name', lhs', rhs', toRail')
    ->
      name = name' && eq2 (lhs, lhs') (rhs, rhs') && toRail = toRail'
  | ERecord (_, pairs), ERecord (_, pairs') ->
      let sort = List.sortBy ~f:(fun (k, _) -> k) in
      List.map2
        ~f:(fun (k, v) (k', v') -> k = k' && eq v v')
        (sort pairs)
        (sort pairs')
      |> List.all ~f:identity
  | EFieldAccess (_, e, f), EFieldAccess (_, e', f') ->
      eq e e' && f = f'
  | EPipe (_, l), EPipe (_, l') ->
      eqList l l'
  | EFeatureFlag (_, _, cond, old, knew), EFeatureFlag (_, _, cond', old', knew')
    ->
      eq3 (cond, cond') (old, old') (knew, knew')
  | EConstructor (_, s, ts), EConstructor (_, s', ts') ->
      s = s' && eqList ts ts'
  | ERightPartial (_, str, e), ERightPartial (_, str', e')
  | ELeftPartial (_, str, e), ELeftPartial (_, str', e')
  | EPartial (_, str, e), EPartial (_, str', e') ->
      str = str' && eq e e'
  | ELambda (_, vars, e), ELambda (_, vars', e') ->
      eq e e'
      && List.all
           ~f:identity
           (List.map2 vars vars' ~f:(fun (_, v) (_, v') -> v = v'))
  | EMatch (_, e, branches), EMatch (_, e', branches') ->
      eq e e'
      && Tc.List.map2
           ~f:(fun (p, v) (p', v') -> peq p p' && eq v v')
           branches
           branches'
         |> Tc.List.all ~f:Tc.identity
  | ENull _, _
  | EBlank _, _
  | EPipeTarget _, _
  | EInteger _, _
  | EString _, _
  | EVariable _, _
  | EBool _, _
  | EFloat _, _
  | ELet _, _
  | EIf _, _
  | EList _, _
  | EFnCall _, _
  | EBinOp _, _
  | ERecord _, _
  | EFieldAccess _, _
  | EPipe _, _
  | EFeatureFlag _, _
  | EConstructor _, _
  | ELeftPartial _, _
  | ERightPartial _, _
  | EPartial _, _
  | ELambda _, _
  | EMatch _, _ ->
      (* exhaustiveness check *)
      false


let toHumanReadable (expr : t) : string =
  let rec recurse ~(indent : int) (expr : t) : string =
    let iStr = String.repeat ~count:indent " " in
    let r = recurse ~indent in
    let rin = recurse ~indent:(indent + 2) in
    let newlineList exprs = exprs |> List.map ~f:rin |> String.join ~sep:"\n" in
    let eStr =
      match expr with
      | EBlank _ ->
          "(blank)"
      | EString (_, str) ->
          ( if String.length str > 20
          then String.slice ~from:0 ~to_:20 str ^ "..."
          else str )
          |> Printf.sprintf {|(str "%s")|}
      | EBool (_, true) ->
          "(true)"
      | EBool (_, false) ->
          "(false)"
      | EFloat (_, whole, fractional) ->
          Printf.sprintf {|(%s.%s)|} whole fractional
      | EInteger (_, i) ->
          Printf.sprintf {|(%s)|} i
      | ENull _ ->
          "(null)"
      | EPipeTarget _ ->
          "(pt)"
      | EPartial (_, str, e) ->
          Printf.sprintf {|(partial "%s" %s)|} str (r e)
      | ERightPartial (_, str, e) ->
          Printf.sprintf {|(rpartial "%s" %s)|} str (r e)
      | ELeftPartial (_, str, e) ->
          Printf.sprintf {|(lpartial "%s" %s)|} str (r e)
      | EFnCall (_, name, [], _) ->
          Printf.sprintf "(fn \"%s\")" name
      | EFnCall (_, name, exprs, _) ->
          Printf.sprintf "(fn \"%s\"\n%s)" name (newlineList exprs)
      | EBinOp (_, name, lhs, rhs, _) ->
          Printf.sprintf "(binop \"%s\"\n%s\n%s)" name (r lhs) (r rhs)
      | EVariable (_, name) ->
          Printf.sprintf {|(%s)|} name
      | EFieldAccess (_, e, name) ->
          Printf.sprintf "(fieldAccess \"%s\"\n%s)" name (r e)
      | EMatch (_, cond, matches) ->
          let rec pToTestcase (p : FluidPattern.t) : string =
            let quoted str = "\"" ^ str ^ "\"" in
            let listed elems = "[" ^ String.join ~sep:";" elems ^ "]" in
            let spaced elems = String.join ~sep:" " elems in
            match p with
            | FPBlank _ ->
                "pBlank"
            | FPString {str; _} ->
                spaced ["pString"; quoted str]
            | FPBool (_, _, true) ->
                spaced ["pBool true"]
            | FPBool (_, _, false) ->
                spaced ["pBool false"]
            | FPFloat (_, _, whole, fractional) ->
                spaced ["pFloat'"; whole; fractional]
            | FPInteger (_, _, int) ->
                spaced ["pInt"; int]
            | FPNull _ ->
                "pNull"
            | FPVariable (_, _, name) ->
                spaced ["pVar"; quoted name]
            | FPConstructor (_, _, name, args) ->
                spaced
                  [ "pConstructor"
                  ; quoted name
                  ; listed (List.map args ~f:pToTestcase) ]
          in
          let matchStrs =
            List.map matches ~f:(fun (p, e) ->
                "(" ^ pToTestcase p ^ ", " ^ r e ^ ")")
          in
          Printf.sprintf
            {|(match\n%s\n%s)|}
            (r cond)
            (String.join ~sep:"\n" matchStrs)
      | ERecord (_, []) ->
          "(record)"
      | ERecord (_, pairs) ->
          let pairStrs =
            List.map pairs ~f:(fun (k, v) ->
                Printf.sprintf {|%s("%s" %s)|} iStr k (String.trim (r v)))
          in
          Printf.sprintf "(record\n%s)" (String.join ~sep:"\n" pairStrs)
      | EList (_, []) ->
          "(list)"
      | EList (_, exprs) ->
          Printf.sprintf "(list\n%s)" (newlineList exprs)
      | EPipe (_, exprs) ->
          Printf.sprintf "(pipe\n%s)" (newlineList exprs)
      | EConstructor (_, name, exprs) ->
          Printf.sprintf "(constructor \"%s\"\n%s)" name (newlineList exprs)
      | EIf (_, cond, then', else') ->
          Printf.sprintf "(if %s\n%s\n%s)" (r cond) (rin then') (rin else')
      | ELet (_, lhs, rhs, body) ->
          Printf.sprintf "(let %s\n%s\n%s)" lhs (rin rhs) (r body)
      | ELambda (_, _names, body) ->
          Printf.sprintf "(lambda \n%s)" (r body)
      | EFeatureFlag (_, _, cond, old, new') ->
          Printf.sprintf "(flag %s\n%s\n%s)" (rin cond) (rin old) (rin new')
    in
    iStr ^ eStr
  in
  recurse ~indent:0 expr
