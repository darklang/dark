module LibExecution.ProgramTypesAst

open Prelude
open ProgramTypes

/// TODO type symbols, too
let rec symbolsUsedIn (expr : Expr) : Set<string> =
  let r = symbolsUsedIn

  match expr with
  // simple values
  | EUnit _
  | EBool _

  | EInt8 _
  | EUInt8 _
  | EInt16 _
  | EUInt16 _
  | EInt32 _
  | EUInt32 _
  | EInt64 _
  | EUInt64 _
  | EInt128 _
  | EUInt128 _

  | EFloat _

  | EChar _ -> Set.empty

  | EString(_, segments) ->
    segments
    |> List.map (fun s ->
      match s with
      | StringText _ -> Set.empty
      | StringInterpolation e -> r e)
    |> Set.unionMany

  // simple structures
  | ETuple(_, first, second, theRest) ->
    [ r first; r second; theRest |> List.map r |> Set.unionMany ] |> Set.unionMany

  | EList(_, exprs) -> exprs |> List.map r |> Set.unionMany

  | EDict(_, pairs) -> pairs |> List.map (fun (_k, v) -> r v) |> Set.unionMany


  // variables
  | EVariable(_, var) -> Set.singleton var

  | ELet(_, _, rhs, next) -> Set.union (r rhs) (r next)


  // flow control
  | EIf(_, condExpr, ifExpr, elseExprMaybe) ->
    match elseExprMaybe with
    | None -> Set.union (r condExpr) (r ifExpr)
    | Some elseExpr -> Set.unionMany [ r condExpr; r ifExpr; r elseExpr ]

  | EMatch(_, target, cases) ->
    let targetVars = r target
    let whenVars =
      cases
      |> List.map (fun c ->
        match c.whenCondition with
        | None -> Set.empty
        | Some w -> r w)
      |> Set.unionMany
    let rhsVars = cases |> List.map _.rhs |> List.map r |> Set.unionMany
    Set.unionMany [ targetVars; whenVars; rhsVars ]


  // custom data
  | EEnum(_, _, _, _, fields) -> fields |> List.map r |> Set.unionMany

  | ERecord(_, _, _, fields) ->
    fields |> List.map (fun (_, e) -> r e) |> Set.unionMany

  | ERecordFieldAccess(_, expr, _) -> r expr

  // things that can be applied
  | EFnName(_, _) -> Set.empty
  | ELambda(_, _, body) -> r body
  | EApply(_, thingToApply, _, args) ->
    Set.unionMany
      [ r thingToApply; args |> NEList.toList |> List.map r |> Set.unionMany ]
