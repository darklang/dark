open Prelude

(* Dark *)
module B = BlankOr
module E = FluidExpression
open FluidExpression
open FluidPattern

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
    | ELeftPartial (_, _, oldExpr) ->
        u oldExpr


(* ------------------------- *)
(* EPipe stuff *)
(* ------------------------- *)

(* If the expression at `id` is one of the expressions in a pipe, this returns
 * the previous expression in that pipe (eg, the one that is piped into this
 * one) *)
let pipePrevious (id : ID.t) (ast : FluidAST.t) : E.t option =
  match FluidAST.findParent id ast with
  | Some (EPipe (_, exprs)) ->
      exprs
      |> List.find ~f:(fun e -> E.toID e = id)
      |> Option.andThen ~f:(fun value -> Util.listPrevious ~value exprs)
  | _ ->
      None


(* If the expression at `id` is one of the expressions in a pipe, this returns
 * the next expression in that pipe (eg, the one that the expr at `id` pipes into) *)
let pipeNext (id : ID.t) (ast : FluidAST.t) : E.t option =
  match FluidAST.findParent id ast with
  | Some (EPipe (_, exprs)) ->
      exprs
      |> List.find ~f:(fun e -> E.toID e = id)
      |> Option.andThen ~f:(fun value -> Util.listNext ~value exprs)
  | _ ->
      None


(* Given the ID of a function call or binop, return its arguments. Takes pipes into account. *)
let getArguments (id : ID.t) (ast : FluidAST.t) : E.t list =
  let pipePrevious = pipePrevious id ast in
  let caller = FluidAST.find id ast in
  let defaultArgs =
    match caller with
    | Some (EFnCall (_, _, args, _)) ->
        args
    | Some (EBinOp (_, _, arg0, arg1, _)) ->
        [arg0; arg1]
    | _ ->
        []
  in
  match (pipePrevious, defaultArgs) with
  | Some previous, EPipeTarget _ :: rest ->
      (* pipetarget should be a pipetarget, but technically we might
       * allow something invalid here, esp due to copy/paste *)
      previous :: rest
  | _ ->
      defaultArgs


(* Search for `id`, and if it is an argument of a function (including if it is
 * being piped into afunction), return the function name and the index of the
 * parameter it corresponds to.
 *
 * eg: Int::add 4 3 => if `id` was the id of the `4` expression, then we'd
 *                     return (`Int::add`, 0)
 *)
let getParamIndex (id : ID.t) (ast : FluidAST.t) : (string * int) option =
  let parent =
    pipeNext id ast |> Option.orElseLazy (fun () -> FluidAST.findParent id ast)
  in
  match parent with
  | Some (EFnCall (fnID, name, _, _)) | Some (EBinOp (fnID, name, _, _, _)) ->
      getArguments fnID ast
      |> List.findIndex ~f:(fun e -> E.toID e = id)
      |> Option.map ~f:(fun index -> (name, index))
  | _ ->
      None


(* ------------------------- *)
(* Ancestors *)
(* ------------------------- *)

let freeVariables (ast : E.t) : (ID.t * string) list =
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
    |> List.map ~f:(E.toID >> ID.toString)
    |> StrSet.fromList
  in
  ast
  |> E.filterMap ~f:(function
         | EVariable (id, name) ->
             (* Don't include EVariable lookups that we know are looking
              * up a variable bound in this expression *)
             if StrSet.member ~value:(ID.toString id) definedAndUsed
             then None
             else Some (id, name)
         | _ ->
             None)
  |> List.uniqueBy ~f:(fun (_, name) -> name)


module VarDict = StrDict
module IDTable = Belt.MutableMap.String

type sym_set = ID.t VarDict.t

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
    | ELet (_id, lhs, rhs, body) ->
        sexe st rhs ;
        let bound =
          if lhs <> ""
          then VarDict.update ~key:lhs ~f:(fun _v -> Some (E.toID rhs)) st
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
          | FPVariable (_, patternID, v) ->
              [(patternID, v)]
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
        sexe st oldExpr
    | ELeftPartial (_, _, oldExpr) ->
        sexe st oldExpr ) ;
  trace expr st


(** [variablesIn ast] produces a map of every expression id in the [ast] to its corresponding symbol table.
 * Each symbol table maps from every available variable name to the id of the corresponding value expression bound to that name. *)
let variablesIn (ast : E.t) : avDict =
  let sym_store = IDTable.make () in
  let trace expr st = IDTable.set sym_store (ID.toString (E.toID expr)) st in
  sym_exec ~trace VarDict.empty ast ;
  sym_store |> IDTable.toList |> StrDict.fromList


let removePartials (expr : E.t) : E.t =
  E.postTraversal expr ~f:(function
      | EPartial (_, _, e)
      | ERightPartial (_, _, e)
      | ELeftPartial (_, _, e)
      | e
      -> e)


(** Reorder function calls which call fnName, moving the argument at [oldPos] to [newPos],
 * pushing the element currently at [newPos] to [newPos+1]. It then handles situations
 * where the args may be in a different position due to pipes. *)
let rec reorderFnCallArgs
    (fnName : string) (oldPos : int) (newPos : int) (ast : E.t) : E.t =
  let rec replaceArgs expr =
    match expr with
    | EFnCall (id, name, args, sendToRail) when name = fnName ->
        let newArgs =
          List.moveInto ~oldPos ~newPos args |> List.map ~f:replaceArgs
        in
        EFnCall (id, name, newArgs, sendToRail)
    | EPipe (id, first :: rest) ->
        let newFirst = reorderFnCallArgs fnName oldPos newPos first in
        let newRest =
          (* If the pipetarget is involved, we're really going to have to wrap
           * it in a lambda instead of shifting things around (we could move
           * the argument up if it's the first thing being piped into, but that
           * might be ugly. *)
          List.map rest ~f:(fun pipeArg ->
              if oldPos == 0 || newPos == 0
              then
                match pipeArg with
                | EFnCall (fnID, name, _pipeTarget :: args, sendToRail)
                  when name = fnName ->
                    (* We replace the pipeTarget with a variable in a lambda fn *)
                    let newArg = EVariable (gid (), "x") in
                    let newArgs =
                      List.moveInto ~oldPos ~newPos (newArg :: args)
                      |> List.map ~f:replaceArgs
                    in
                    (* The fncall is no longer a piped fn. # args shown == # params.
                     * For example, if we moved the first param to last param:
                     * Before: a |> someFun b c d
                     * After:  a |> \x -> someFun b c d x
                     *)
                    ELambda
                      ( gid ()
                      , [(gid (), "x")]
                      , EFnCall (fnID, name, newArgs, sendToRail) )
                | ELambda (id, args, lambdaExpr) ->
                    ELambda
                      ( id
                      , args
                      , reorderFnCallArgs fnName oldPos newPos lambdaExpr )
                | _ ->
                    reorderFnCallArgs fnName oldPos newPos pipeArg
              else
                (* The pipetarget isn't involved, so just do it normally. *)
                reorderFnCallArgs fnName oldPos newPos pipeArg)
        in
        EPipe (id, newFirst :: newRest)
    | e ->
        E.deprecatedWalk ~f:replaceArgs e
  in
  replaceArgs ast
