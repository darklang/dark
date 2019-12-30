open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module E = FluidExpression
module P = Pointer

(* -------------------------------- *)
(* PointerData *)
(* -------------------------------- *)

let recoverPD (msg : string) (pd : blankOrData option) : blankOrData =
  recoverOpt ("invalidPD: " ^ msg) pd ~default:(PExpr (E.newB ()))


let rec allData (expr : E.t) : blankOrData list =
  let e2ld e = PExpr e in
  let rl exprs = exprs |> List.map ~f:allData |> List.concat in
  [e2ld expr]
  @
  match expr with
  | EVariable _
  | EFloat _
  | ENull _
  | EInteger _
  | EString _
  | EBool _
  | EBlank _
  | EPipeTarget _ ->
      []
  | ELet (id, lhs, rhs, body) ->
      [PVarBind (id, lhs)] @ rl [rhs; body]
  | EIf (_, cond, ifbody, elsebody) ->
      rl [cond; ifbody; elsebody]
  | EFnCall (id, name, exprs, _) ->
      [PFnCallName (id, name)] @ rl exprs
  | EBinOp (id, name, lhs, rhs, _) ->
      [PFnCallName (id, name)] @ rl [lhs; rhs]
  | EConstructor (id, name, exprs) ->
      PConstructorName (id, name) :: rl exprs
  | ELambda (_, vars, body) ->
      List.map ~f:(fun (id, name) -> PVarBind (id, name)) vars @ allData body
  | EPipe (_, exprs) ->
      rl exprs
  | EFieldAccess (_, obj, fieldid, field) ->
      allData obj @ [PField (fieldid, field)]
  | EList (_, exprs) ->
      rl exprs
  | ERecord (_, pairs) ->
      pairs
      |> List.map ~f:(fun (k, v) -> PKey (E.id v, k) :: allData v)
      |> List.concat
  | EFeatureFlag (id, msg, cond, a, b) ->
      [PFFMsg (id, msg)] @ rl [cond; a; b]
  | EMatch (_, matchExpr, cases) ->
      let matchData = allData matchExpr in
      let caseData =
        cases
        |> List.map ~f:(fun (p, e) -> Pattern.allData p @ rl [e])
        |> List.concat
      in
      matchData @ caseData
  | EPartial (_, _, oldExpr) ->
      allData oldExpr
  | ERightPartial (_, _, oldExpr) ->
      allData oldExpr


(* Note the difference with FluidExpression.find - that returns only
 * expressions, this returns allDatas, and so can encode non-expression
 * information. *)
let find (id : id) (expr : E.t) : blankOrData option =
  expr
  |> allData
  |> List.filter ~f:(fun d -> id = Pointer.toID d)
  |> assertFn "no data with ID found" ~debug:(expr, id) ~f:(fun list ->
         List.length list > 0 || id = FluidToken.fakeid)
  (* guard against dups *)
  |> List.head


let isDefinitionOf (var : string) (expr : E.t) : bool =
  match expr with
  | ELet (_, lhs, _, _) ->
      lhs = var && lhs <> ""
  | ELambda (_, vars, _) ->
      vars
      |> List.map ~f:Tuple2.second
      |> List.any ~f:(fun v -> v = var && v <> "")
  | EMatch (_, _, cases) ->
      let shadowsName p =
        let originalNames = FluidPattern.variableNames p in
        List.member ~value:var originalNames
      in
      cases |> List.map ~f:Tuple2.first |> List.any ~f:shadowsName
  | _ ->
      false


let rec uses (var : string) (expr : E.t) : E.t list =
  let u = uses var in
  if isDefinitionOf var expr
  then []
  else
    match expr with
    | EInteger _
    | EString _
    | EBool _
    | EFloat _
    | ENull _
    | EBlank _
    | EPipeTarget _ ->
        []
    | EVariable (_, potential) ->
        if potential = var then [expr] else []
    | ELet (_, _, rhs, body) ->
        List.concat [u rhs; u body]
    | EIf (_, cond, ifbody, elsebody) ->
        List.concat [u cond; u ifbody; u elsebody]
    | EFnCall (_, _, exprs, _) ->
        exprs |> List.map ~f:u |> List.concat
    | EBinOp (_, _, lhs, rhs, _) ->
        u lhs @ u rhs
    | EConstructor (_, _, exprs) ->
        exprs |> List.map ~f:u |> List.concat
    | ELambda (_, _, lexpr) ->
        u lexpr
    | EPipe (_, exprs) ->
        exprs |> List.map ~f:u |> List.concat
    | EFieldAccess (_, obj, _, _) ->
        u obj
    | EList (_, exprs) ->
        exprs |> List.map ~f:u |> List.concat
    | ERecord (_, pairs) ->
        pairs |> List.map ~f:Tuple2.second |> List.map ~f:u |> List.concat
    | EFeatureFlag (_, _, cond, a, b) ->
        List.concat [u cond; u a; u b]
    | EMatch (_, matchExpr, cases) ->
        let exprs = cases |> List.map ~f:Tuple2.second in
        u matchExpr @ exprs
    | EPartial (_, _, oldExpr) ->
        u oldExpr
    | ERightPartial (_, _, oldExpr) ->
        u oldExpr


(* ------------------------- *)
(* Children *)
(* ------------------------- *)
let children (expr : E.t) : blankOrData list =
  let ces exprs = List.map ~f:(fun e -> PExpr e) exprs in
  match expr with
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
  | EIf (_, cond, ifbody, elsebody) ->
      [PExpr cond; PExpr ifbody; PExpr elsebody]
  | EFnCall (_, _, exprs, _) ->
      ces exprs
  | EBinOp (_, _, lhs, rhs, _) ->
      ces [lhs; rhs]
  | EConstructor (id, name, exprs) ->
      PConstructorName (id, name) :: ces exprs
  | ELambda (_, vars, lexpr) ->
      List.map ~f:(fun (id, vb) -> PVarBind (id, vb)) vars @ [PExpr lexpr]
  | EPipe (_, exprs) ->
      ces exprs
  | EFieldAccess (_, obj, fieldid, field) ->
      [PExpr obj; PField (fieldid, field)]
  | ELet (id, lhs, rhs, body) ->
      [PVarBind (id, lhs); PExpr rhs; PExpr body]
  | ERecord (_, pairs) ->
      pairs
      |> List.map ~f:(fun (k, v) -> [PKey (E.id v, k); PExpr v])
      |> List.concat
  | EList (_, elems) ->
      ces elems
  | EFeatureFlag (id, msg, cond, a, b) ->
      [PFFMsg (id, msg); PExpr cond; PExpr a; PExpr b]
  | EMatch (_, matchExpr, cases) ->
      (* We list all the descendents of the pattern here. This isn't ideal,
       * but it's challenging with the current setup to do otherwise, because
       * all of these things take exprs *)
      let casePointers =
        cases
        |> List.map ~f:(fun (p, e) ->
               let ps = Pattern.allData p in
               ps @ [PExpr e])
        |> List.concat
      in
      PExpr matchExpr :: casePointers
  | EPartial (_, _, oldExpr) ->
      [PExpr oldExpr]
  | ERightPartial (_, _, oldExpr) ->
      [PExpr oldExpr]


(* Look through an AST for the expr with the id, then return its children. *)
let rec childrenOf (pid : id) (expr : E.t) : blankOrData list =
  let co = childrenOf pid in
  if pid = E.id expr
  then children expr
  else
    match expr with
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
        co body @ co rhs
    | EIf (_, cond, ifbody, elsebody) ->
        co cond @ co ifbody @ co elsebody
    | EFnCall (_, _, exprs, _) ->
        List.map ~f:co exprs |> List.concat
    | EBinOp (_, _, lhs, rhs, _) ->
        co lhs @ co rhs
    | EConstructor (_, _, exprs) ->
        List.map ~f:co exprs |> List.concat
    | ELambda (_, _, lexpr) ->
        co lexpr
    | EPipe (_, exprs) ->
        List.map ~f:co exprs |> List.concat
    | EFieldAccess (_, obj, _, _) ->
        co obj
    | ERecord (_, pairs) ->
        pairs |> List.map ~f:Tuple2.second |> List.map ~f:co |> List.concat
    | EList (_, pairs) ->
        pairs |> List.map ~f:co |> List.concat
    | EFeatureFlag (_, _, cond, a, b) ->
        co cond @ co a @ co b
    | EMatch (_, matchExpr, cases) ->
        let cCases =
          cases |> List.map ~f:Tuple2.second |> List.map ~f:co |> List.concat
        in
        co matchExpr @ cCases
    | EPartial (_, _, oldExpr) ->
        co oldExpr
    | ERightPartial (_, _, oldExpr) ->
        co oldExpr


(* ------------------------- *)
(* Parents *)
(* ------------------------- *)
let rec findParentOfWithin_ (eid : id) (haystack : E.t) : E.t option =
  let fpow = findParentOfWithin_ eid in
  (* the `or` of all items in the list *)
  let fpowList xs =
    xs |> List.map ~f:fpow |> List.filterMap ~f:identity |> List.head
  in
  if List.member ~value:eid (haystack |> children |> List.map ~f:P.toID)
  then Some haystack
  else
    match haystack with
    | EInteger _
    | EString _
    | EBool _
    | EFloat _
    | ENull _
    | EBlank _
    | EPipeTarget _ ->
        None
    | EVariable _ ->
        None
    | ELet (_, _, rhs, body) ->
        fpowList [rhs; body]
    | EIf (_, cond, ifbody, elsebody) ->
        fpowList [cond; ifbody; elsebody]
    | EFnCall (_, _, exprs, _) ->
        fpowList exprs
    | EBinOp (_, _, lhs, rhs, _) ->
        fpowList [lhs; rhs]
    | EConstructor (_, _, exprs) ->
        fpowList exprs
    | ELambda (_, _, lexpr) ->
        fpow lexpr
    | EPipe (_, exprs) ->
        fpowList exprs
    | EFieldAccess (_, obj, _, _) ->
        fpow obj
    | EList (_, exprs) ->
        fpowList exprs
    (* we don't check the children because it's done up top *)
    | ERecord (_, pairs) ->
        pairs |> List.map ~f:Tuple2.second |> fpowList
    | EFeatureFlag (_, _, cond, a, b) ->
        fpowList [cond; a; b]
    | EMatch (_, matchExpr, cases) ->
        fpowList (matchExpr :: (cases |> List.map ~f:Tuple2.second))
    | EPartial (_, _, oldExpr) ->
        fpow oldExpr
    | ERightPartial (_, _, oldExpr) ->
        fpow oldExpr


let findParentOfWithin (id : id) (haystack : E.t) : E.t =
  findParentOfWithin_ id haystack
  |> recoverOpt "findParentOfWithin" ~default:(E.newB ())


(* ------------------------- *)
(* EPipe stuff *)
(* ------------------------- *)
let grandparentIsThread (expr : E.t) (parent : E.t option) : bool =
  parent
  |> Option.map ~f:(fun p ->
         match findParentOfWithin_ (E.id p) expr with
         | Some (EPipe (_, ts)) ->
             ts
             |> List.head
             |> Option.map ~f:(( <> ) p)
             |> Option.withDefault ~default:true
         | _ ->
             false)
  |> Option.withDefault ~default:false


let getParamIndex (expr : E.t) (id : id) : (string * int) option =
  let parent = findParentOfWithin_ id expr in
  match parent with
  | Some (EFnCall (_, name, args, _)) ->
      args
      |> List.findIndex ~f:(fun a -> E.id a = id)
      |> Option.map ~f:(fun i -> (name, i))
  | _ ->
      None


let threadPrevious (id : id) (ast : E.t) : E.t option =
  let parent = E.findParent id ast in
  match parent with
  | Some (EPipe (_, exprs)) ->
      exprs
      |> List.find ~f:(fun e -> E.id e = id)
      |> Option.andThen ~f:(fun value -> Util.listPrevious ~value exprs)
  | _ ->
      None


let allCallsToFn (s : string) (e : E.t) : E.t list =
  e
  |> allData
  |> List.filterMap ~f:(fun pd ->
         match pd with
         | PExpr (EFnCall (id, name, params, r)) ->
             if name = s then Some (EFnCall (id, name, params, r)) else None
         | _ ->
             None)


(* ------------------------- *)
(* Ancestors *)
(* ------------------------- *)
let ancestors (id : id) (expr : E.t) : E.t list =
  let rec rec_ancestors (tofind : id) (walk : E.t list) (exp : E.t) =
    let rec_ id_ e_ walk_ = rec_ancestors id_ (e_ :: walk_) in
    let reclist id_ e_ walk_ exprs =
      exprs |> List.map ~f:(rec_ id_ e_ walk_) |> List.concat
    in
    if E.id exp = tofind
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
      | EFieldAccess (_, obj, _, _) ->
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
  in
  rec_ancestors id [] expr


let getValueParent (p : blankOrData) (expr : E.t) : fluidExpr option =
  let parent = findParentOfWithin_ (P.toID p) expr in
  match (p, parent) with
  | PExpr e, Some (EPipe (_, exprs)) ->
      exprs |> Util.listPrevious ~value:e
  | PField _, Some (EFieldAccess (_, obj, _, _)) ->
      Some obj
  | PPattern _, Some (EMatch (_, cond, _)) ->
      Some cond
  | _ ->
      None


let freeVariables (ast : E.t) : (id * string) list =
  (* Find all variable lookups that lookup a variable that
   * is also _defined_ in this expression. We create a set of
   * these IDs so we can filter them out later. *)
  let definedAndUsed =
    ast
    |> allData
    |> List.filterMap ~f:(fun n ->
           match n with
           | PExpr e ->
             ( match e with
             (* Grab all uses of the `lhs` of a Let in its body *)
             | ELet (_, lhs, _, body) ->
                 Some (uses lhs body)
             (* Grab all uses of the `vars` of a Lambda in its body *)
             | ELambda (_, vars, body) ->
                 vars
                 |> List.map ~f:Tuple2.second
                 |> List.filter ~f:(( <> ) "")
                 |> List.map ~f:(fun v -> uses v body)
                 |> List.concat
                 |> fun x -> Some x
             | EMatch (_, _, cases) ->
                 cases
                 (* Grab all uses of the variable bindings in a `pattern`
                  * in the `body` of each match case *)
                 |> List.map ~f:(fun (pattern, body) ->
                        let vars = FluidPattern.variableNames pattern in
                        List.map ~f:(fun v -> uses v body) vars)
                 |> List.concat
                 |> List.concat
                 |> fun x -> Some x
             | _ ->
                 None )
           | _ ->
               None)
    |> List.concat
    |> List.map ~f:(E.id >> deID)
    |> StrSet.fromList
  in
  ast
  |> allData
  |> List.filterMap ~f:(fun n ->
         match n with
         | PExpr e ->
           ( match e with
           | EVariable (id, name) ->
               (* Don't include EVariable lookups that we know are looking
                * up a variable bound in this expression *)
               if StrSet.member ~value:(deID id) definedAndUsed
               then None
               else Some (id, name)
           | _ ->
               None )
         | _ ->
             None)
  |> List.uniqueBy ~f:(fun (_, name) -> name)


module VarDict = StrDict
module IDTable = Belt.MutableMap.String

type sym_set = id VarDict.t

type sym_store = sym_set IDTable.t

let rec sym_exec ~(trace : E.t -> sym_set -> unit) (st : sym_set) (expr : E.t) :
    unit =
  let sexe = sym_exec ~trace in
  ignore
    ( match expr with
    | EInteger _
    | EString _
    | EBool _
    | EFloat _
    | ENull _
    | EBlank _
    | EPipeTarget _ ->
        ()
    | EVariable _ ->
        ()
    | ELet (id, lhs, rhs, body) ->
        sexe st rhs ;
        let bound =
          if lhs <> ""
          then VarDict.update ~key:lhs ~f:(fun _v -> Some id) st
          else st
        in
        sexe bound body
    | EFnCall (_, _, exprs, _) ->
        List.iter ~f:(sexe st) exprs
    | EBinOp (_, _, lhs, rhs, _) ->
        List.iter ~f:(sexe st) [lhs; rhs]
    | EIf (_, cond, ifbody, elsebody)
    | EFeatureFlag (_, _, cond, elsebody, ifbody) ->
        sexe st cond ;
        sexe st ifbody ;
        sexe st elsebody
    | ELambda (_, vars, body) ->
        let new_st =
          vars
          |> List.foldl ~init:st ~f:(fun (id, varname) d ->
                 VarDict.update ~key:varname ~f:(fun _v -> Some id) d)
        in
        sexe new_st body
    | EPipe (_, exprs) ->
        List.iter ~f:(sexe st) exprs
    | EFieldAccess (_, obj, _, _) ->
        sexe st obj
    | EList (_, exprs) ->
        List.iter ~f:(sexe st) exprs
    | EMatch (_, matchExpr, cases) ->
        let rec variablesInPattern p =
          match p with
          | FPInteger _
          | FPNull _
          | FPString _
          | FPFloat _
          | FPBool _
          | FPBlank _ ->
              []
          | FPVariable (id, _, v) ->
              [(id, v)]
          | FPConstructor (_, _, _, inner) ->
              inner |> List.map ~f:variablesInPattern |> List.concat
        in
        sexe st matchExpr ;
        List.iter cases ~f:(fun (p, caseExpr) ->
            let new_st =
              p
              |> variablesInPattern
              |> List.foldl ~init:st ~f:(fun (id, varname) d ->
                     VarDict.update ~key:varname ~f:(fun _v -> Some id) d)
            in
            sexe new_st caseExpr)
    | ERecord (_, exprs) ->
        exprs |> List.map ~f:Tuple2.second |> List.iter ~f:(sexe st)
    | EConstructor (_, _, args) ->
        List.iter ~f:(sexe st) args
    | EPartial (_, _, oldExpr) ->
        sexe st oldExpr
    | ERightPartial (_, _, oldExpr) ->
        sexe st oldExpr ) ;
  trace expr st


let variablesIn (ast : E.t) : avDict =
  let sym_store = IDTable.make () in
  let trace expr st = IDTable.set sym_store (deID (E.id expr)) st in
  sym_exec ~trace VarDict.empty ast ;
  sym_store |> IDTable.toList |> StrDict.fromList
