/// Type mismatches an item carries that nothing else will notice until somebody calls it.
///
/// Authoring is live-on-write and permissive: `dark fn Foo.bar "(x: Int64) : String = x"` is accepted and
/// fails the first time it runs. Right for a half-finished thought, wrong for a mistake with a definite
/// answer -- so this reports rather than refuses, and Dark files what it finds as a CONSTRAINT, the same
/// at-rest lifecycle as an outdated usage.
///
/// SOUND, NOT COMPLETE, and the asymmetry is the design: a false positive is a standing finding about
/// correct code, which teaches people to stop reading the list. So a rule reports only when both types are
/// fully concrete and no substitution reconciles them. Type variables, custom types (which may be
/// aliases), and anything needing inference are left alone.
///
/// Not a type checker. `LibExecution.TypeChecker` is the real one and works on VALUES at run time; a static
/// checker over `Expr` does not exist. This is the part that needs no inference: types written down,
/// propagated through lets, compared where they meet.
///
/// COVERAGE IS PARTIAL and deliberately visible. The traversal reaches everywhere an expression nests, but
/// there are only two RULES. It knowingly looks away from a pipe's arguments: the piped value is prepended
/// at runtime, so AST index 0 is parameter 1, and reporting a position it could get wrong is worse than
/// reporting nothing.
///
/// A traversal, not a decision -- the same split as `UnresolvedCheck`.
module LibDB.TypeSurface

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes


/// Where the two types met.
type Site =
  /// The function's body against the return type it declares.
  | ReturnValue
  /// An argument against the parameter it was passed to, by 0-based position.
  | Argument of index : int


/// One mismatch: two concrete types that met and disagreed.
type Mismatch =
  {
    site : Site
    /// what the declaration asked for
    expected : PT.TypeReference
    /// what is actually there
    actual : PT.TypeReference
  }


/// Types that are fully determined and have no internal structure to unify: if two of these differ, no
/// substitution makes them agree.
///
/// `TCustomType` is deliberately NOT here even when it looks concrete, because it may be an alias for
/// anything, and resolving aliases is inference by another name.
let private isRigidScalar (t : PT.TypeReference) : bool =
  match t with
  | PT.TUnit
  | PT.TBool
  | PT.TInt8
  | PT.TUInt8
  | PT.TInt16
  | PT.TUInt16
  | PT.TInt32
  | PT.TUInt32
  | PT.TInt64
  | PT.TUInt64
  | PT.TInt128
  | PT.TUInt128
  | PT.TInt
  | PT.TFloat
  | PT.TChar
  | PT.TString
  | PT.TUuid
  | PT.TDateTime
  | PT.TBlob -> true
  | _ -> false


/// Can these two types NEVER be the same thing?
///
/// Only two shapes are confident enough to say so:
///
///   - two rigid scalars that differ. `Int64` is not `String`, under any substitution.
///   - a function against a rigid scalar. This is the one that catches an under-applied call: partial
///     application is ordinary in Dark, so `f a b` where `f` takes three is not itself a mistake -- it is a
///     function, and the mistake is handing that function to something expecting a `String`. Nothing else
///     notices, because the value is perfectly valid right up to the point something tries to use it.
///
/// Everything else returns false, including pairs that are probably wrong. That is the intended bias.
let private definitelyDisagree
  (expected : PT.TypeReference)
  (actual : PT.TypeReference)
  : bool =
  match expected, actual with
  | _, _ when isRigidScalar expected && isRigidScalar actual -> expected <> actual
  | PT.TFn _, a when isRigidScalar a -> true
  | e, PT.TFn _ when isRigidScalar e -> true
  | _ -> false


/// The type of an expression, ONLY when it is written down rather than inferred.
///
/// `None` means "not sure", and not-sure is the common answer. Every caller treats it as "say nothing".
/// What is in scope, by the two different ways a name gets there.
///
/// PARAMETERS are positional: a reference to `x` in `let f (x: Int64) = ...` is `EArg(id, 0)`, not a
/// variable, so the declared types live in a list indexed the same way. LETs are by name.
type private Env =
  { args : List<PT.TypeReference>; locals : Map<string, PT.TypeReference> }


let rec private typeOf
  (getFn : PT.FQFnName.Package -> Ply<Option<PT.PackageFn.PackageFn>>)
  (env : Env)
  (expr : PT.Expr)
  : Ply<Option<PT.TypeReference>> =
  uply {
    match expr with
    | PT.EUnit _ -> return Some PT.TUnit
    | PT.EBool _ -> return Some PT.TBool
    | PT.EInt8 _ -> return Some PT.TInt8
    | PT.EUInt8 _ -> return Some PT.TUInt8
    | PT.EInt16 _ -> return Some PT.TInt16
    | PT.EUInt16 _ -> return Some PT.TUInt16
    | PT.EInt32 _ -> return Some PT.TInt32
    | PT.EUInt32 _ -> return Some PT.TUInt32
    | PT.EInt64 _ -> return Some PT.TInt64
    | PT.EUInt64 _ -> return Some PT.TUInt64
    | PT.EInt128 _ -> return Some PT.TInt128
    | PT.EUInt128 _ -> return Some PT.TUInt128
    | PT.EInt _ -> return Some PT.TInt
    | PT.EFloat _ -> return Some PT.TFloat
    | PT.EChar _ -> return Some PT.TChar

    // Interpolation or not, the result is a string.
    | PT.EString _ -> return Some PT.TString

    // A parameter, by position.
    | PT.EArg(_, index) -> return List.tryItem index env.args

    // Something a `let` bound to a known type.
    | PT.EVariable(_, name) -> return Map.tryFind name env.locals

    | PT.ELet(_, PT.LPVariable(_, name), value, body) ->
      let! valueType = typeOf getFn env value
      let env =
        match valueType with
        | Some t -> { env with locals = Map.add name t env.locals }
        // shadowing something we knew, with something we don't
        | None -> { env with locals = Map.remove name env.locals }
      return! typeOf getFn env body

    // Any other let pattern binds names this doesn't track, so drop them rather than let a stale binding
    // of the same name stay visible in the body.
    | PT.ELet(_, pattern, _, body) ->
      let locals =
        PT.LetPattern.symbolsUsed pattern
        |> Set.fold (fun e n -> Map.remove n e) env.locals
      return! typeOf getFn { env with locals = locals } body

    | PT.EStatement(_, _, next) -> return! typeOf getFn env next

    // Infix, which is worth having because `x + 1L` is what people actually write and it is the shortest
    // path to a body whose type disagrees with the signature.
    //
    // Comparisons and logic are Bool whatever they are given. Arithmetic is only claimed when both sides
    // are the SAME known rigid scalar -- Dark has no implicit numeric widening, so that is the only case
    // where the answer is obvious without knowing which overload applies.
    | PT.EInfix(_, PT.InfixFnCall op, lhs, rhs) ->
      match op with
      | PT.ComparisonGreaterThan
      | PT.ComparisonGreaterThanOrEqual
      | PT.ComparisonLessThan
      | PT.ComparisonLessThanOrEqual
      | PT.ComparisonEquals
      | PT.ComparisonNotEquals -> return Some PT.TBool
      | PT.StringConcat -> return Some PT.TString
      | PT.ArithmeticPlus
      | PT.ArithmeticMinus
      | PT.ArithmeticMultiply
      | PT.ArithmeticDivide
      | PT.ArithmeticModulo
      | PT.ArithmeticPower ->
        let! l = typeOf getFn env lhs
        let! r = typeOf getFn env rhs
        match l, r with
        | Some lt, Some rt when lt = rt && isRigidScalar lt -> return Some lt
        | _ -> return None

    | PT.EInfix(_, PT.BinOp _, _, _) -> return Some PT.TBool

    // A call, and the one case that needs the package manager. A FULL application is its return type; a
    // PARTIAL one is a function, which is exactly the fact that makes an under-applied call findable.
    | PT.EApply(_, PT.EFnName(_, nr), [], args) ->
      match nr.resolved with
      | Error _ -> return None
      | Ok resolved ->
        match resolved.name with
        | PT.FQFnName.Builtin _ -> return None
        | PT.FQFnName.Package hash ->
          match! getFn hash with
          | None -> return None
          | Some fn ->
            // A signature mentioning type variables is not something this can reason about.
            if not (List.isEmpty fn.typeParams) then
              return None
            else
              let ps = NEList.toList fn.parameters
              let given = NEList.length args
              if given = List.length ps then
                return Some fn.returnType
              elif given < List.length ps then
                let remaining = ps |> List.skip given |> List.map (fun p -> p.typ)
                match remaining with
                | first :: rest ->
                  return Some(PT.TFn(NEList.ofList first rest, fn.returnType))
                | [] -> return None
              else
                // Over-applied. Its type depends on whether the return type is itself a function, which is
                // the kind of question this deliberately does not answer.
                return None

    | _ -> return None
  }


/// Walk an expression, checking every argument that meets a parameter whose type is written down.
let rec private checkExpr
  (getFn : PT.FQFnName.Package -> Ply<Option<PT.PackageFn.PackageFn>>)
  (env : Env)
  (expr : PT.Expr)
  : Ply<List<Mismatch>> =
  uply {
    match expr with
    | PT.EApply(_, PT.EFnName(_, nr), [], args) ->
      let argList = NEList.toList args
      // Check inside the arguments first, whatever happens at this call.
      let! nested =
        argList
        |> Ply.List.mapSequentially (fun a -> checkExpr getFn env a)
        |> Ply.map List.concat

      match nr.resolved with
      | Error _ -> return nested
      | Ok resolved ->
        match resolved.name with
        | PT.FQFnName.Builtin _ -> return nested
        | PT.FQFnName.Package hash ->
          match! getFn hash with
          | None -> return nested
          | Some fn when not (List.isEmpty fn.typeParams) -> return nested
          | Some fn ->
            let ps = NEList.toList fn.parameters
            // Only positions that exist on BOTH sides. Truncating one list is not enough: an over-applied
            // call has more arguments than parameters, and `List.zip` on lists of different lengths throws
            // -- which took down the whole package reload rather than skipping one call.
            let n = min (List.length ps) (List.length argList)
            let! here =
              List.zip (List.truncate n ps) (List.truncate n argList)
              |> List.indexed
              |> Ply.List.mapSequentially (fun (i, (p, arg)) ->
                uply {
                  let! actual = typeOf getFn env arg
                  match actual with
                  | Some a when definitelyDisagree p.typ a ->
                    return [ { site = Argument i; expected = p.typ; actual = a } ]
                  | _ -> return []
                })
              |> Ply.map List.concat

            return nested @ here

    | PT.ELet(_, PT.LPVariable(_, name), value, body) ->
      let! inValue = checkExpr getFn env value
      let! valueType = typeOf getFn env value
      let env =
        match valueType with
        | Some t -> { env with locals = Map.add name t env.locals }
        | None -> { env with locals = Map.remove name env.locals }
      let! inBody = checkExpr getFn env body
      return inValue @ inBody

    | PT.ELet(_, pattern, value, body) ->
      let! inValue = checkExpr getFn env value
      let locals =
        PT.LetPattern.symbolsUsed pattern
        |> Set.fold (fun e n -> Map.remove n e) env.locals
      let! inBody = checkExpr getFn { env with locals = locals } body
      return inValue @ inBody

    | PT.EStatement(_, first, next) ->
      let! a = checkExpr getFn env first
      let! b = checkExpr getFn env next
      return a @ b

    | PT.EIf(_, cond, thenExpr, elseExpr) ->
      let! c = checkExpr getFn env cond
      let! t = checkExpr getFn env thenExpr
      let! e =
        match elseExpr with
        | Some e -> checkExpr getFn env e
        | None -> Ply []
      return c @ t @ e

    | PT.EList(_, items) ->
      return!
        items
        |> Ply.List.mapSequentially (fun i -> checkExpr getFn env i)
        |> Ply.map List.concat

    | PT.ETuple(_, first, second, rest) ->
      return!
        (first :: second :: rest)
        |> Ply.List.mapSequentially (fun i -> checkExpr getFn env i)
        |> Ply.map List.concat

    | PT.EInfix(_, _, lhs, rhs) ->
      let! l = checkExpr getFn env lhs
      let! r = checkExpr getFn env rhs
      return l @ r

    // A lambda's parameters are untyped here, so its body is walked with those names dropped rather than
    // guessed at.
    | PT.ELambda(_, pats, body) ->
      let bound =
        pats
        |> NEList.toList
        |> List.collect (PT.LetPattern.symbolsUsed >> Set.toList)
      let locals = bound |> List.fold (fun e n -> Map.remove n e) env.locals
      return! checkExpr getFn { env with locals = locals } body

    // The forms below are TRAVERSED but contribute no rule of their own. Descending costs nothing in
    // confidence -- a finding still only comes from two concrete types disagreeing at a call or a return --
    // and not descending meant a mismatch inside a `match` arm, which is most Dark code, was simply never
    // looked for.
    | PT.EMatch(_, arg, cases) ->
      let! inArg = checkExpr getFn env arg
      let! inCases =
        cases
        |> Ply.List.mapSequentially (fun (case_ : PT.MatchCase) ->
          uply {
            // EVERY local is dropped inside an arm, not just the ones the pattern binds. There is no
            // `symbolsUsed` for a match pattern, and keeping a local that the pattern happens to shadow
            // would hand this the outer type for an inner name -- a confidently WRONG finding, which is
            // the one outcome worth spending completeness to avoid.
            let env = { env with locals = Map.empty }
            let! inGuard =
              match case_.whenCondition with
              | Some c -> checkExpr getFn env c
              | None -> Ply []
            let! inRhs = checkExpr getFn env case_.rhs
            return inGuard @ inRhs
          })
        |> Ply.map List.concat
      return inArg @ inCases

    | PT.ERecord(_, _, _, fields) ->
      return!
        fields
        |> Ply.List.mapSequentially (fun (_, e) -> checkExpr getFn env e)
        |> Ply.map List.concat

    | PT.ERecordUpdate(_, record, updates) ->
      let! inRecord = checkExpr getFn env record
      let! inUpdates =
        updates
        |> NEList.toList
        |> Ply.List.mapSequentially (fun (_, e) -> checkExpr getFn env e)
        |> Ply.map List.concat
      return inRecord @ inUpdates

    | PT.ERecordFieldAccess(_, record, _) -> return! checkExpr getFn env record

    | PT.EDict(_, pairs) ->
      return!
        pairs
        |> Ply.List.mapSequentially (fun (_, e) -> checkExpr getFn env e)
        |> Ply.map List.concat

    | PT.EEnum(_, _, _, _, fields) ->
      return!
        fields
        |> Ply.List.mapSequentially (fun e -> checkExpr getFn env e)
        |> Ply.map List.concat

    // Pipes are traversed but their call ARGUMENTS are deliberately not checked positionally: the piped
    // value is prepended at runtime, so the argument at AST index 0 is parameter 1, and reporting
    // "argument 0" about it would be pointing at the wrong thing. Sub-expressions still get walked.
    | PT.EPipe(_, lhs, parts) ->
      let! inLhs = checkExpr getFn env lhs
      let! inParts =
        parts
        |> Ply.List.mapSequentially (fun part ->
          match part with
          | PT.EPipeLambda(_, pats, body) ->
            let bound =
              pats
              |> NEList.toList
              |> List.collect (PT.LetPattern.symbolsUsed >> Set.toList)
            let locals = bound |> List.fold (fun e n -> Map.remove n e) env.locals
            checkExpr getFn { env with locals = locals } body
          | PT.EPipeInfix(_, _, rhs) -> checkExpr getFn env rhs
          | PT.EPipeFnCall(_, _, _, args) ->
            args
            |> Ply.List.mapSequentially (fun a -> checkExpr getFn env a)
            |> Ply.map List.concat
          | PT.EPipeEnum(_, _, _, fields) ->
            fields
            |> Ply.List.mapSequentially (fun f -> checkExpr getFn env f)
            |> Ply.map List.concat
          | PT.EPipeVariable(_, _, args) ->
            args
            |> Ply.List.mapSequentially (fun a -> checkExpr getFn env a)
            |> Ply.map List.concat)
        |> Ply.map List.concat
      return inLhs @ inParts

    | _ -> return []
  }


/// Every mismatch this can be certain of in one function.
let inFn
  (getFn : PT.FQFnName.Package -> Ply<Option<PT.PackageFn.PackageFn>>)
  (fn : PT.PackageFn.PackageFn)
  : Ply<List<Mismatch>> =
  uply {
    // A generic function's own body is beyond this.
    if not (List.isEmpty fn.typeParams) then
      return []
    else
      let env =
        { args = fn.parameters |> NEList.toList |> List.map (fun p -> p.typ)
          locals = Map.empty }

      let! inBody = checkExpr getFn env fn.body

      let! bodyType = typeOf getFn env fn.body
      let returnMismatch =
        match bodyType with
        | Some t when definitelyDisagree fn.returnType t ->
          [ { site = ReturnValue; expected = fn.returnType; actual = t } ]
        | _ -> []

      return inBody @ returnMismatch
  }


// ---------------------
// The cache
// ---------------------

/// Hashes already checked under THIS build, and found clean.
///
/// Two reasons this lives in F# next to the check rather than in the Dark caller. It is an implementation
/// detail of the check, not a decision, so it belongs on this side of the line. And the fill has to be
/// callable from the reload, which is F#, for the reason below.
module Cache =

  /// Check every hash not already known-clean, record the clean ones, and return what was wrong.
  ///
  /// WARMING THIS AT RELOAD IS NOT AN OPTIMISATION, it is what keeps the check off the hot path. `dark
  /// status` asks for constraints on every invocation, and a cold cache turned that into a walk of the
  /// whole package tree. In a traced process the same walk also writes every call's arguments -- whole
  /// ASTs -- which took the test suite to 44 GB resident before it was killed.
  ///
  /// So the cost belongs where the tree is already being rebuilt, once, and every later question is a
  /// lookup. A known-BAD hash is deliberately not recorded: a finding has to keep existing until the code
  /// changes, and re-checking a handful of them is free.
  let checkAndRecord
    (getFn : PT.FQFnName.Package -> Ply<Option<PT.PackageFn.PackageFn>>)
    (hashes : List<string>)
    : Task<List<string * Mismatch>> =
    task {
      let build = LibConfig.Config.buildHash

      let! known =
        Sql.query "SELECT item_hash FROM type_checked WHERE build_hash = @build"
        |> Sql.parameters [ "build", Sql.string build ]
        |> Sql.executeAsync (fun read -> read.string "item_hash")

      let known = Set.ofList known
      let todo = hashes |> List.filter (fun h -> not (Set.contains h known))

      // Per item, and CAUGHT per item. This runs inside `reload-packages`, so anything that escapes here
      // takes down the package load for the whole store -- which is not a hypothetical: a `List.zip` on
      // lists of different lengths did exactly that, and the reload failed with an F# arity error nowhere
      // near the actual cause.
      //
      // An advisory check has no business being able to do that. A traversal that throws on one item
      // reports nothing for that item and the store still loads. Deliberately NOT recorded as clean
      // either, so it is retried rather than silently written off.
      let! results =
        todo
        |> Ply.List.mapSequentially (fun h ->
          uply {
            try
              match! getFn (PT.Hash h) with
              | None -> return Some(h, [])
              | Some fn ->
                let! ms = inFn getFn fn
                return Some(h, ms)
            with e ->
              System.Console.Error.WriteLine($"type check skipped {h}: {e.Message}")
              return None
          })
        |> Ply.map (List.choose (fun x -> x))
        |> Ply.toTask

      let clean =
        results |> List.filter (fun (_, ms) -> List.isEmpty ms) |> List.map fst

      for h in clean do
        do!
          Sql.query
            "INSERT OR IGNORE INTO type_checked (item_hash, build_hash) VALUES (@h, @build)"
          |> Sql.parameters [ "h", Sql.string h; "build", Sql.string build ]
          |> Sql.executeStatementAsync

      // Another build's verdicts are dead once this one has recorded its own.
      do!
        Sql.query "DELETE FROM type_checked WHERE build_hash <> @build"
        |> Sql.parameters [ "build", Sql.string build ]
        |> Sql.executeStatementAsync

      return
        results |> List.collect (fun (h, ms) -> ms |> List.map (fun m -> (h, m)))
    }


  /// Every live fn, checked and recorded.
  ///
  /// It reads the hash list itself rather than being handed one. The caller that wants this is asking
  /// "what is wrong in the store", and passing four thousand hashes in so the same four thousand can be
  /// filtered back out cost more than the check does once the cache is warm.
  let checkAll
    (getFn : PT.FQFnName.Package -> Ply<Option<PT.PackageFn.PackageFn>>)
    : Task<List<string * Mismatch>> =
    task {
      let! hashes =
        Sql.query
          """
          SELECT DISTINCT item_hash FROM locations
          WHERE unlisted_at IS NULL AND item_type = 'fn'
          """
        |> Sql.executeAsync (fun read -> read.string "item_hash")

      return! checkAndRecord getFn hashes
    }


  /// Called by the reload, so nothing later pays for a cold cache.
  let warm
    (getFn : PT.FQFnName.Package -> Ply<Option<PT.PackageFn.PackageFn>>)
    : Task<int> =
    task {
      let! found = checkAll getFn
      return List.length found
    }
