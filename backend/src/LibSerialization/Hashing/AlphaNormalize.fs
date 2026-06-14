/// Alpha-normalization for meaning-stable hashing.
///
/// The content hash of a package item is over its canonical serialized form. That form must not depend
/// on incidental BOUND-VARIABLE NAMES — two items identical up to a `let`/lambda/match binder rename are
/// the same item and must hash the same. (Function parameters are already handled elsewhere: a parameter
/// use is lowered to `EArg index`, and the parameter name isn't hashed — see `Canonical.writeParameter`.
/// This pass is what covers the binders the parser keeps named: `let`, lambda, and `match`.)
///
/// It renames every such bound variable to a canonical, position-derived name (`$0`, `$1`, … assigned in
/// a fixed structural traversal), consistently at its binder and all its uses (`EVariable`/`EPipeVariable`).
/// Alpha-equivalent expressions normalize to the SAME tree, so they serialize and hash identically. Free
/// variables (not locally bound) are left untouched, as are ids, types, and fully-qualified references.
/// `computeFnHash`/`computeValueHash` run this before serializing.
module LibSerialization.Hashing.AlphaNormalize

open Prelude
module PT = LibExecution.ProgramTypes


// ── the variables a pattern BINDS, left-to-right (deduped within an or-pattern, whose alternatives
//    bind the same names) ──

let rec private letPatternVars (p : PT.LetPattern) : List<string> =
  match p with
  | PT.LPVariable(_, name) -> [ name ]
  | PT.LPUnit _ -> []
  | PT.LPWildcard _ -> []
  | PT.LPTuple(_, first, second, rest) ->
    letPatternVars first @ letPatternVars second @ List.collect letPatternVars rest

let rec private matchPatternVars (p : PT.MatchPattern) : List<string> =
  match p with
  | PT.MPVariable(_, name) -> [ name ]
  | PT.MPEnum(_, _, fields) -> List.collect matchPatternVars fields
  | PT.MPTuple(_, first, second, rest) ->
    matchPatternVars first
    @ matchPatternVars second
    @ List.collect matchPatternVars rest
  | PT.MPList(_, pats) -> List.collect matchPatternVars pats
  | PT.MPListCons(_, head, tail) -> matchPatternVars head @ matchPatternVars tail
  // an or-pattern's alternatives bind the SAME variables — collect them once (first occurrence order)
  | PT.MPOr(_, pats) ->
    pats |> NEList.toList |> List.collect matchPatternVars |> List.distinct
  | _ -> [] // literal patterns bind nothing


// ── rewrite a pattern's bound-variable names per a name→canonical map ──

let rec private renameLetPattern
  (m : Map<string, string>)
  (p : PT.LetPattern)
  : PT.LetPattern =
  match p with
  | PT.LPVariable(id, name) ->
    PT.LPVariable(id, Map.tryFind name m |> Option.defaultValue name)
  | PT.LPUnit _ -> p
  | PT.LPWildcard _ -> p
  | PT.LPTuple(id, first, second, rest) ->
    PT.LPTuple(
      id,
      renameLetPattern m first,
      renameLetPattern m second,
      List.map (renameLetPattern m) rest
    )

let rec private renameMatchPattern
  (m : Map<string, string>)
  (p : PT.MatchPattern)
  : PT.MatchPattern =
  match p with
  | PT.MPVariable(id, name) ->
    PT.MPVariable(id, Map.tryFind name m |> Option.defaultValue name)
  | PT.MPEnum(id, caseName, fields) ->
    PT.MPEnum(id, caseName, List.map (renameMatchPattern m) fields)
  | PT.MPTuple(id, first, second, rest) ->
    PT.MPTuple(
      id,
      renameMatchPattern m first,
      renameMatchPattern m second,
      List.map (renameMatchPattern m) rest
    )
  | PT.MPList(id, pats) -> PT.MPList(id, List.map (renameMatchPattern m) pats)
  | PT.MPListCons(id, head, tail) ->
    PT.MPListCons(id, renameMatchPattern m head, renameMatchPattern m tail)
  | PT.MPOr(id, pats) -> PT.MPOr(id, NEList.map (renameMatchPattern m) pats)
  | _ -> p // literal patterns have no names


// ── the core: rewrite an expr so every bound variable is a canonical `$n` name ──

// The counter is threaded as a `ref` mutated in a FIXED structural traversal order, so two
// alpha-equivalent trees (identical structure, different names) get identical `$n` assignments.

let private mergeEnv
  (env : Map<string, string>)
  (m : Map<string, string>)
  : Map<string, string> =
  // inner bindings shadow outer ones
  Map.fold (fun acc k v -> Map.add k v acc) env m

let private bind (counter : int ref) (vars : List<string>) : Map<string, string> =
  vars
  |> List.map (fun v ->
    let n = counter.Value
    counter.Value <- n + 1
    (v, "$" + string n))
  |> Map.ofList

let private lookup (env : Map<string, string>) (name : string) : string =
  // a bound variable → its canonical name; a free variable (not locally bound) → unchanged
  Map.tryFind name env |> Option.defaultValue name

let rec private norm
  (c : int ref)
  (env : Map<string, string>)
  (e : PT.Expr)
  : PT.Expr =
  let r = norm c env // recurse with the same scope (non-binding children)
  match e with
  // uses of variables — the whole point
  | PT.EVariable(id, name) -> PT.EVariable(id, lookup env name)

  // binders
  | PT.ELet(id, pat, rhs, body) ->
    let rhs = norm c env rhs // `let` is non-recursive: the rhs is in the OUTER scope
    let m = bind c (letPatternVars pat)
    PT.ELet(id, renameLetPattern m pat, rhs, norm c (mergeEnv env m) body)
  | PT.ELambda(id, pats, body) ->
    let m = bind c (pats |> NEList.toList |> List.collect letPatternVars)
    PT.ELambda(
      id,
      NEList.map (renameLetPattern m) pats,
      norm c (mergeEnv env m) body
    )
  | PT.EMatch(id, scrutinee, cases) ->
    let scrutinee = norm c env scrutinee
    let cases =
      cases
      |> List.map (fun case ->
        let m = bind c (matchPatternVars case.pat)
        let env = mergeEnv env m
        let normalized : PT.MatchCase =
          { pat = renameMatchPattern m case.pat
            whenCondition = Option.map (norm c env) case.whenCondition
            rhs = norm c env case.rhs }
        normalized)
    PT.EMatch(id, scrutinee, cases)

  // structural recursion (no new bindings) — every child that is an Expr is normalized
  | PT.EString(id, segments) ->
    PT.EString(id, List.map (normStringSegment c env) segments)
  | PT.EIf(id, cond, thenExpr, elseExpr) ->
    PT.EIf(id, r cond, r thenExpr, Option.map r elseExpr)
  | PT.ERecordFieldAccess(id, expr, field) ->
    PT.ERecordFieldAccess(id, r expr, field)
  | PT.EApply(id, fn, typeArgs, args) ->
    PT.EApply(id, r fn, typeArgs, NEList.map r args)
  | PT.EList(id, exprs) -> PT.EList(id, List.map r exprs)
  | PT.ERecord(id, typeName, typeArgs, fields) ->
    PT.ERecord(id, typeName, typeArgs, List.map (fun (n, ex) -> (n, r ex)) fields)
  | PT.ERecordUpdate(id, record, updates) ->
    PT.ERecordUpdate(id, r record, NEList.map (fun (n, ex) -> (n, r ex)) updates)
  | PT.EEnum(id, typeName, typeArgs, caseName, fields) ->
    PT.EEnum(id, typeName, typeArgs, caseName, List.map r fields)
  | PT.ETuple(id, first, second, rest) ->
    PT.ETuple(id, r first, r second, List.map r rest)
  | PT.EInfix(id, op, left, right) -> PT.EInfix(id, op, r left, r right)
  | PT.EDict(id, pairs) -> PT.EDict(id, List.map (fun (k, ex) -> (k, r ex)) pairs)
  | PT.EStatement(id, first, next) -> PT.EStatement(id, r first, r next)
  | PT.EPipe(id, expr, pipes) ->
    PT.EPipe(id, r expr, List.map (normPipe c env) pipes)

  // leaves / no Expr children / no bound names — unchanged
  | PT.EInt64 _
  | PT.EUInt64 _
  | PT.EInt8 _
  | PT.EUInt8 _
  | PT.EInt16 _
  | PT.EUInt16 _
  | PT.EInt32 _
  | PT.EUInt32 _
  | PT.EInt128 _
  | PT.EUInt128 _
  | PT.EBool _
  | PT.EChar _
  | PT.EFloat _
  | PT.EUnit _
  | PT.EValue _
  | PT.EFnName _
  | PT.ESelf _
  | PT.EArg _ -> e

and private normStringSegment
  (c : int ref)
  (env : Map<string, string>)
  (seg : PT.StringSegment)
  : PT.StringSegment =
  match seg with
  | PT.StringText _ -> seg
  | PT.StringInterpolation expr -> PT.StringInterpolation(norm c env expr)

and private normPipe
  (c : int ref)
  (env : Map<string, string>)
  (p : PT.PipeExpr)
  : PT.PipeExpr =
  match p with
  // a pipe into a variable is a USE — normalize the name like EVariable
  | PT.EPipeVariable(id, name, args) ->
    PT.EPipeVariable(id, lookup env name, List.map (norm c env) args)
  | PT.EPipeLambda(id, pats, body) ->
    let m = bind c (pats |> NEList.toList |> List.collect letPatternVars)
    PT.EPipeLambda(
      id,
      NEList.map (renameLetPattern m) pats,
      norm c (mergeEnv env m) body
    )
  | PT.EPipeInfix(id, op, expr) -> PT.EPipeInfix(id, op, norm c env expr)
  | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
    PT.EPipeFnCall(id, fnName, typeArgs, List.map (norm c env) args)
  | PT.EPipeEnum(id, typeName, caseName, fields) ->
    PT.EPipeEnum(id, typeName, caseName, List.map (norm c env) fields)


// ── public entry points ──

/// Alpha-normalize a standalone expression: its `let`/lambda/match binders become canonical.
let expr (e : PT.Expr) : PT.Expr = norm (ref 0) Map.empty e

/// Alpha-normalize a value: its body's binders become canonical.
let value (v : PT.PackageValue.PackageValue) : PT.PackageValue.PackageValue =
  { v with body = expr v.body }

/// Alpha-normalize a function: normalize the body's binders. Parameters need no handling here — a
/// parameter reference in the body is already positional (`EArg index`), and the parameter name isn't
/// part of the hash (see `Canonical.writeParameter`), so a parameter rename can't affect the result.
let fn (f : PT.PackageFn.PackageFn) : PT.PackageFn.PackageFn =
  { f with body = expr f.body }
