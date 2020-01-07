open Prelude

(* Dark *)
module B = BlankOr
module E = FluidExpression
module P = Pointer

(* -------------------------------- *)
(* PointerData *)
(* -------------------------------- *)

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
    | EFieldAccess (_, obj, _) ->
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
let children (expr : E.t) : E.t list =
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
      [cond; ifbody; elsebody]
  | EFnCall (_, _, exprs, _) ->
      exprs
  | EBinOp (_, _, lhs, rhs, _) ->
      [lhs; rhs]
  | EConstructor (_, _, exprs) ->
      exprs
  | ELambda (_, _, lexpr) ->
      [lexpr]
  | EPipe (_, exprs) ->
      exprs
  | EFieldAccess (_, obj, _) ->
      [obj]
  | ELet (_, _, rhs, body) ->
      [rhs; body]
  | ERecord (_, pairs) ->
      pairs |> List.map ~f:Tuple2.second
  | EList (_, elems) ->
      elems
  | EFeatureFlag (_, _, cond, a, b) ->
      [cond; a; b]
  | EMatch (_, matchExpr, cases) ->
      let casePointers = cases |> List.map ~f:Tuple2.second in
      matchExpr :: casePointers
  | EPartial (_, _, oldExpr) ->
      [oldExpr]
  | ERightPartial (_, _, oldExpr) ->
      [oldExpr]


(* ------------------------- *)
(* Parents *)
(* ------------------------- *)
let rec findParentOfWithin_ (eid : id) (haystack : E.t) : E.t option =
  let fpow = findParentOfWithin_ eid in
  (* the `or` of all items in the list *)
  let fpowList xs =
    xs |> List.map ~f:fpow |> List.filterMap ~f:identity |> List.head
  in
  if List.member ~value:eid (haystack |> children |> List.map ~f:E.id)
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
    | EFieldAccess (_, obj, _) ->
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
  in
  rec_ancestors id [] expr


let freeVariables (ast : E.t) : (id * string) list =
  (* Find all variable lookups that lookup a variable that
   * is also _defined_ in this expression. We create a set of
   * these IDs so we can filter them out later. *)
  let definedAndUsed =
    ast
    |> E.filterMap ~f:(function
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
               None)
    |> List.concat
    |> List.map ~f:(E.id >> deID)
    |> StrSet.fromList
  in
  ast
  |> E.filterMap ~f:(function
         | EVariable (id, name) ->
             (* Don't include EVariable lookups that we know are looking
              * up a variable bound in this expression *)
             if StrSet.member ~value:(deID id) definedAndUsed
             then None
             else Some (id, name)
         | _ ->
             None)
  |> List.uniqueBy ~f:(fun (_, name) -> name)


let blanks (ast : E.t) : E.t list = E.filter ast ~f:E.isBlank

let ids (ast : E.t) : id list =
  E.filter ast ~f:(fun _ -> true) |> List.map ~f:E.id


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
    | EFieldAccess (_, obj, _) ->
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


let removePartials (ast : E.t) : E.t =
  let findAllPartials = function
    | EPartial (id, _, e) | ERightPartial (id, _, e) ->
        Some (id, e)
    | _ ->
        None
  in
  let replacePartials partial ast =
    let replaceID, validExpr = partial in
    E.replace ~replacement:validExpr replaceID ast
  in
  ast
  |> E.filterMap ~f:findAllPartials
  |> List.foldl ~init:ast ~f:replacePartials
