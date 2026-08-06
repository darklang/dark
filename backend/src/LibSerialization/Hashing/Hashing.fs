/// Content-addressable hashing for package items.
///
/// Computes SHA-256 hashes of canonical serialized forms, with SCC-aware
/// batch hashing for mutually-recursive definitions (Tarjan's algorithm).
///
/// The hash is **meaning-stable**: bound-variable names don't affect it. A parameter use is positional
/// (`EArg index`) and `Canonical.writeParameter` doesn't hash the parameter name; let/lambda/match binders
/// are alpha-normalized before hashing (below). So two functions identical up to a rename
/// share one content hash.
namespace LibSerialization.Hashing

open System.IO
open System.Security.Cryptography
open Prelude
open LibExecution.ProgramTypes
module PT = LibExecution.ProgramTypes

module Common = LibSerialization.Binary.Serializers.Common
module PTC = LibSerialization.Binary.Serializers.PT.Common


[<AutoOpen>]
module Hashing =

  type HashRefMode = Canonical.HashRefMode
  let Normal = Canonical.Normal


  // =====================
  // Hash computation helpers
  // =====================

  let private fromSHA256Bytes (bytes : byte array) : Hash =
    Hash(System.Convert.ToHexString(bytes).ToLowerInvariant())

  /// Serialize to bytes using a writer function, then SHA-256 hash to Hash
  let private hashWithWriter (writerFn : BinaryWriter -> unit) : Hash =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)
    writerFn w
    let bytes = ms.ToArray()
    SHA256.HashData(bytes) |> fromSHA256Bytes

  // ── alpha-normalization: this IS how a fn/value hashes ──────────────────────────────────────────────
  // Bound-variable names (let/lambda/match binders) are incidental — two items identical up to a rename are
  // the same item and must hash the same. So computeFnHash/computeValueHash rename every binder to a
  // canonical `$0`,`$1`,… (in a fixed structural traversal) before serializing. Parameters are already
  // positional (`EArg index`) and not hashed by name (see Canonical.writeParameter), so only these binders
  // need handling. Free variables, ids, types, and qualified refs are left untouched.

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
    | PT.EInt _
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


  // ── the entry points computeFnHash/computeValueHash use (public so the normalization is directly testable) ──

  /// Alpha-normalize a standalone expression: its `let`/lambda/match binders become canonical.
  let normalizeExpr (e : PT.Expr) : PT.Expr = norm (ref 0) Map.empty e

  /// Alpha-normalize a value: its body's binders become canonical.
  let normalizeValue
    (v : PT.PackageValue.PackageValue)
    : PT.PackageValue.PackageValue =
    { v with body = normalizeExpr v.body }

  /// Alpha-normalize a function: normalize the body's binders. Parameters need no handling here — a
  /// parameter reference in the body is already positional (`EArg index`), and the parameter name isn't
  /// part of the hash (see `Canonical.writeParameter`), so a parameter rename can't affect the result.
  let normalizeFn (f : PT.PackageFn.PackageFn) : PT.PackageFn.PackageFn =
    { f with body = normalizeExpr f.body }



  // =====================
  // Hash computation functions
  // =====================

  /// Hash a PackageType (skip id, description, deprecated)
  let computeTypeHash (mode : HashRefMode) (t : PT.PackageType.PackageType) : Hash =
    hashWithWriter (fun w -> Canonical.writeType mode w t)


  /// Hash a PackageFn (skip id, description, deprecated, param descriptions).
  /// MEANING-STABLE: alpha-normalize first, so bound-variable names (parameters, let/lambda/match
  /// binders) don't affect the hash — `fn add x y = x + y` and `fn add a b = a + b` hash identically.
  let computeFnHash (mode : HashRefMode) (fn : PT.PackageFn.PackageFn) : Hash =
    hashWithWriter (fun w -> Canonical.writeFn mode w (normalizeFn fn))


  /// Hash a PackageValue (skip id, description, deprecated)
  let computeValueHash
    (mode : HashRefMode)
    (v : PT.PackageValue.PackageValue)
    : Hash =
    // meaning-stable: alpha-normalize the body's binders first (see computeFnHash)
    hashWithWriter (fun w -> Canonical.writeValue mode w (normalizeValue v))


  /// Hash a PackageOp (reuse existing PackageOp.write — ops have no metadata to skip)
  let computeOpHash (op : PT.PackageOp) : Hash =
    hashWithWriter (fun w ->
      LibSerialization.Binary.Serializers.PT.PackageOp.write w op)


  /// An op's row id in `package_ops`: its content hash, truncated to the 16 bytes a uuid column holds.
  ///
  /// Defined here, beside the hash it truncates, because an op's id has to be computed identically by
  /// every path that mints or looks one up — authoring, the fold, branch tagging. Each of those used to
  /// carry its own copy of the two lines below, and a copy that drifts is a store where the same op has two
  /// ids.
  ///
  /// The truncation is the part to be careful with: it is what makes the id fit a uuid column, and it is
  /// also the only place a collision could be introduced. Removing it is a bigger change than it looks,
  /// because the op blob embeds the id and the deserializer verifies it; see
  /// notes/fresh-arch/OP-ID-WIDENING.md.
  let computeOpRowId (op : PT.PackageOp) : System.Guid =
    let (Hash h) = computeOpHash op
    System.Guid(System.Convert.FromHexString(h)[0..15])


  // =====================
  // Tarjan's SCC algorithm
  // =====================

  /// Find all strongly-connected components in a dependency graph.
  /// Returns list of SCCs, each an NEList of node IDs.
  let findSCCs
    (nodes : List<'id>)
    (edges : 'id -> List<'id>)
    : List<NEList<'id>> when 'id : equality and 'id : comparison =

    let mutable index = 0
    let mutable stack : List<'id> = []
    let mutable indices = Map.empty<'id, int>
    let mutable lowlinks = Map.empty<'id, int>
    let mutable onStack = Set.empty<'id>
    let mutable result : List<NEList<'id>> = []

    let nodeSet = Set.ofList nodes

    let rec strongConnect (v : 'id) =
      indices <- Map.add v index indices
      lowlinks <- Map.add v index lowlinks
      index <- index + 1
      stack <- v :: stack
      onStack <- Set.add v onStack

      for w in edges v do
        if Set.contains w nodeSet then
          if not (Map.containsKey w indices) then
            strongConnect w
            lowlinks <-
              Map.add
                v
                (min (Map.findUnsafe v lowlinks) (Map.findUnsafe w lowlinks))
                lowlinks
          elif Set.contains w onStack then
            lowlinks <-
              Map.add
                v
                (min (Map.findUnsafe v lowlinks) (Map.findUnsafe w indices))
                lowlinks

      if Map.findUnsafe v lowlinks = Map.findUnsafe v indices then
        let mutable scc = []
        let mutable keepPopping = true
        while keepPopping do
          match stack with
          | w :: rest ->
            stack <- rest
            onStack <- Set.remove w onStack
            scc <- w :: scc
            if w = v then keepPopping <- false
          | [] -> keepPopping <- false

        match scc with
        | head :: tail -> result <- { head = head; tail = tail } :: result
        | [] -> ()

    for node in nodes do
      if not (Map.containsKey node indices) then strongConnect node

    result |> List.rev


  // =====================
  // SCC batch hashing
  // =====================

  type private ItemInfo =
    | TypeItem of
      PT.PackageType.PackageType *
      string *
      Hash *
      Option<PT.PackageLocation>
    | FnItem of PT.PackageFn.PackageFn * string * Hash * Option<PT.PackageLocation>
    | ValueItem of
      PT.PackageValue.PackageValue *
      string *
      Hash *
      Option<PT.PackageLocation>

  let private getItemFQN (item : ItemInfo) : string =
    match item with
    | TypeItem(_, fqn, _, _) -> fqn
    | FnItem(_, fqn, _, _) -> fqn
    | ValueItem(_, fqn, _, _) -> fqn

  let private getItemOldHash (item : ItemInfo) : Hash =
    match item with
    | TypeItem(_, _, h, _) -> h
    | FnItem(_, _, h, _) -> h
    | ValueItem(_, _, h, _) -> h

  let private getItemLocation (item : ItemInfo) : Option<PT.PackageLocation> =
    match item with
    | TypeItem(_, _, _, loc) -> loc
    | FnItem(_, _, _, loc) -> loc
    | ValueItem(_, _, _, loc) -> loc

  let private computeItemHash (mode : HashRefMode) (item : ItemInfo) : Hash =
    match item with
    | TypeItem(t, _, _, _) -> computeTypeHash mode t
    | FnItem(fn, _, _, _) -> computeFnHash mode fn
    | ValueItem(v, _, _, _) -> computeValueHash mode v

  let private serializeItemBytes
    (mode : Canonical.HashRefMode)
    (item : ItemInfo)
    : byte array =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)
    // meaning-stable: alpha-normalize fns/values so the batch (SCC) hash, like the single-item hash,
    // ignores bound-variable names. Types have no binders, so they pass through unchanged.
    match item with
    | TypeItem(t, _, _, _) -> Canonical.writeType mode w t
    | FnItem(fn, _, _, _) -> Canonical.writeFn mode w (normalizeFn fn)
    | ValueItem(v, _, _, _) -> Canonical.writeValue mode w (normalizeValue v)
    ms.ToArray()


  /// Given a set of package items (keyed by FQN) and their dependencies, compute
  /// hashes handling SCCs via batch hashing with name-ref substitution.
  /// Maps are keyed by FQN (string) to avoid collisions when multiple items share
  /// the same Hash (e.g. type aliases with unresolved refs on first parse).
  /// Each item tuple is `(item, oldHash, location)` — location may be
  /// `None` for items without one (test fixtures); production callers
  /// (HashStabilization) always pass `Some`.
  ///
  /// `seed` injects out-of-batch substitutions — e.g. propagation
  /// passes `(sourceLocation → newSourceHash)` and
  /// `(oldSourceHash → newSourceHash)`. As each in-batch item is
  /// hashed, the seed grows with its substitution so subsequent SCCs
  /// see it via either lookup path.
  let computeHashesWithSCCs
    (seed : Canonical.Substitution)
    (types :
      Map<string, PT.PackageType.PackageType * Hash * Option<PT.PackageLocation>>)
    (fns : Map<string, PT.PackageFn.PackageFn * Hash * Option<PT.PackageLocation>>)
    (values :
      Map<string, PT.PackageValue.PackageValue * Hash * Option<PT.PackageLocation>>)
    (getDeps : string -> List<string>)
    : Map<string, Hash> =

    // Build unified item map (keyed by FQN)
    let items =
      Map.fold
        (fun acc fqn (t, oldHash, loc) ->
          Map.add fqn (TypeItem(t, fqn, oldHash, loc)) acc)
        Map.empty
        types
      |> Map.fold (fun acc fqn (fn, oldHash, loc) ->
        Map.add fqn (FnItem(fn, fqn, oldHash, loc)) acc)
      <| fns
      |> Map.fold (fun acc fqn (v, oldHash, loc) ->
        Map.add fqn (ValueItem(v, fqn, oldHash, loc)) acc)
      <| values

    let allIds = items |> Map.keys |> Seq.toList

    // Detect SCCs (operating on FQN strings as node IDs)
    let sccs = findSCCs allIds getDeps

    // FQN → finalHash result map
    let mutable hashMap = Map.empty<string, Hash>
    // Seed grows as we hash each SCC; later SCCs see all prior new hashes.
    let mutable subst = seed

    // Record an item's hash transition in both substitution maps so
    // later SCCs see the new hash via either lookup path.
    let recordSubstitution (item : ItemInfo) (newHash : Hash) =
      let oldHash = getItemOldHash item
      let byHash' = Map.add oldHash newHash subst.byHash
      let byLocation' =
        match getItemLocation item with
        | Some loc -> Map.add loc newHash subst.byLocation
        | None -> subst.byLocation
      subst <- { byLocation = byLocation'; byHash = byHash' }

    for scc in sccs do
      let sccIds = scc.head :: scc.tail

      // A size-1 SCC without a self-loop is a true singleton — hash normally.
      // A size-1 SCC WITH a self-loop (e.g. recursive type Expr referencing itself)
      // must use SccNameRef mode, otherwise the hash depends on the previous
      // iteration's hash and never converges.
      let hasSelfLoop =
        List.length sccIds = 1 && getDeps scc.head |> List.contains scc.head

      if List.length sccIds = 1 && not hasSelfLoop then
        // Single-item SCC without self-reference: hash with finalized deps
        let fqn = scc.head
        let item = Map.findUnsafe fqn items

        let mode : Canonical.HashRefMode =
          { subst = subst; sccNames = Canonical.emptySccNames }

        let hash = computeItemHash mode item
        hashMap <- Map.add fqn hash hashMap
        recordSubstitution item hash
      else
        // Multi-item SCC: batch hash with FQN substitution + finalized deps.
        // Location-keyed cycle refs avoid same-hash collisions inside the SCC;
        // hash-keyed lookup remains only for null-location refs.
        let sccNameMap : Canonical.SccNames =
          let mutable byLocation = Map.empty
          let mutable byHash = Map.empty
          for fqn in sccIds do
            let item = Map.findUnsafe fqn items
            match getItemLocation item with
            | Some loc -> byLocation <- Map.add loc fqn byLocation
            | None -> ()
            byHash <- Map.add (getItemOldHash item) fqn byHash
          { byLocation = byLocation; byHash = byHash }

        let mode : Canonical.HashRefMode = { subst = subst; sccNames = sccNameMap }

        // Sort by FQN for determinism
        let sortedItems =
          sccIds
          |> List.map (fun fqn -> (fqn, Map.findUnsafe fqn items))
          |> List.sortBy (fun (_, item) -> getItemFQN item)

        // Concatenate canonical bytes of all items in the SCC
        let groupBytes =
          sortedItems
          |> List.map (fun (_, item) -> serializeItemBytes mode item)
          |> Array.concat

        let groupHash = SHA256.HashData(groupBytes) |> fromSHA256Bytes

        // Each item's hash = hash(groupHash + FQN)
        for (fqn, item) in sortedItems do
          let itemFQN = getItemFQN item
          let itemHash =
            hashWithWriter (fun w ->
              PTC.Hash.write w groupHash
              Common.String.write w itemFQN)
          hashMap <- Map.add fqn itemHash hashMap
          recordSubstitution item itemHash

    hashMap
