/// Write onto each trait-method call the implementation it resolves to, BEFORE the batch is
/// hashed, so a stored call goes on running what it was written against and a newer
/// implementation arrives as an ordinary repoint rather than changing the call underneath it.
///
/// The at-rest checker says which implementations apply at each call node; the store says
/// which of them is newer. This module is only the join of those two answers onto the ops.
///
/// It lives in Builtins rather than in LibDB because the checker is written in Dark, so
/// resolving a call means EXECUTING Dark, which needs an execution state.
module Builtins.Matter.Libs.PM.TraitCalls

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PTAst = LibExecution.ProgramTypesAst
module PackageRefs = LibExecution.PackageRefs
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval
module Dependencies = LibDB.DependencyExtractor


// --------------------
// What the checker worked out, decoded from its report.
// --------------------

/// One `LanguageTools.AtRestTypeChecker.Model.Resolution`, as F# sees it.
///
/// `at` is widened to `id` here rather than in Dark: node ids are `Int64` throughout the
/// solver (`Model.Constraint.at` and the rest), and `id` is `uint64`, so the one conversion
/// belongs at the language boundary where everything else is being decoded anyway.
type private Resolution =
  | Call of at : id * method_ : string * impls : List<PT.Hash>
  | Deferred of at : id * param : string
  | CallerBound of
    at : id *
    param : string *
    trait_ : PT.FQTraitName.Package *
    impls : List<PT.Hash>
  | CallerBoundDeferred of
    at : id *
    param : string *
    trait_ : PT.FQTraitName.Package *
    fromParam : string


/// Decoders that ANSWER NOTHING rather than raising, unlike `PT2DT`'s.
///
/// Type arguments are matched with a wildcard rather than against `[]`. A DEnum built by Dark
/// does not necessarily carry the same type arguments as the same case built by `PT2DT`, and a
/// pattern that insists on `[]` then matches nothing while looking completely reasonable: it
/// cost an hour of hunting for a producer that was working the whole time.
///
/// A resolution this build cannot read must mean "no pin", exactly as a call the checker
/// could not resolve means "no pin": the op goes un-rewritten and the runtime dispatches.
/// Raising here would turn a report we merely failed to understand into a failed save, and
/// the whole point of the three-state `ImplChoice` is that not knowing is representable.
module private Decode =
  let hash (d : Dval) : Option<PT.Hash> =
    match d with
    | DEnum(_, _, _, "Hash", [ DString h ]) -> Some(PT.Hash h)
    | _ -> None

  let hashes (d : Dval) : List<PT.Hash> =
    match d with
    | DList(_, items) -> items |> List.choose hash
    | _ -> []

  let private nodeId (d : Dval) : Option<id> =
    match d with
    | DInt64 i when i >= 0L -> Some(uint64 i)
    | _ -> None

  let resolution (d : Dval) : Option<Resolution> =
    match d with
    | DEnum(_, _, _, "Call", [ at; DString method_; impls ]) ->
      nodeId at |> Option.map (fun at -> Call(at, method_, hashes impls))
    | DEnum(_, _, _, "Deferred", [ at; DString param ]) ->
      nodeId at |> Option.map (fun at -> Deferred(at, param))
    | DEnum(_, _, _, "CallerBound", [ at; DString param; trait_; impls ]) ->
      match nodeId at, hash trait_ with
      | Some at, Some trait_ -> Some(CallerBound(at, param, trait_, hashes impls))
      | _ -> None
    | DEnum(_, _, _, "CallerBoundDeferred", [ at; DString param; trait_; DString from ]) ->
      match nodeId at, hash trait_ with
      | Some at, Some trait_ -> Some(CallerBoundDeferred(at, param, trait_, from))
      | _ -> None
    | _ -> None

  let private field (name : string) (fields : DvalMap) : Option<Dval> =
    Map.tryFind name fields

  /// The item each `ItemReport` is about, and what it resolved, for the two kinds that have
  /// a body to resolve anything in. A type, trait or impl reports no resolutions.
  let private itemReport (d : Dval) : Option<PT.Hash * List<Resolution>> =
    match d with
    | DRecord(_, _, _, fields) ->
      let resolutions =
        match field "resolutions" fields with
        | Some(DList(_, items)) -> items |> List.choose resolution
        | _ -> []
      match field "item" fields with
      | Some(DEnum(_, _, _, ("PackageFn" | "PackageValue"), [ h ])) ->
        hash h |> Option.map (fun h -> (h, resolutions))
      | _ -> None
    | _ -> None

  /// `LanguageTools.AtRestTypeChecker.Report` -> what each item resolved, by item hash.
  let report (d : Dval) : Map<PT.Hash, List<Resolution>> =
    match d with
    | DRecord(_, _, _, fields) ->
      match field "items" fields with
      | Some(DList(_, items)) ->
        items
        |> List.choose itemReport
        |> List.filter (fun (_, resolutions) -> not (List.isEmpty resolutions))
        |> Map.ofList
      | _ -> Map.empty
    | _ -> Map.empty

// --------------------
// Reusing what a previous reload already worked out.
// --------------------

/// Resolutions a previous run worked out, so a reload does not re-check a tree that has not
/// changed.
///
/// WHY THIS EXISTS. A whole-tree check is about 84 seconds of interpreted Dark, roughly 12ms
/// across 6,800 items with no single hotspot, and the dev reload runs one on every `.dark`
/// change because that is how pinning learns what to pin. Nothing makes 6,800 interpreted
/// type inferences fast; the only move is to not redo the ones that cannot have changed.
///
/// WHAT MAKES IT SOUND. An item is keyed by its CONTENT-ADDRESSED hash, taken from the
/// unpinned ops. Content addressing is doing the hard part for free: an item's hash covers
/// what it references, so if a dependency changes, its hash changes, so this item's hash
/// changes, so the entry misses. That is the whole dependency-invalidation problem solved by
/// the store's own design rather than by bookkeeping here.
///
/// TWO THINGS CONTENT ADDRESSING DOES NOT COVER, and both go in the fingerprint:
///  - which IMPLEMENTATIONS exist, since a resolution is a choice among them, and nothing
///    about the calling item changes when a rival appears.
///  - THE CHECKER ITSELF, which is Dark code in `packages/` and is edited like any other. A
///    cached entry computed by a different checker is worthless, and a version constant would
///    be a thing to forget to bump, so the fingerprint takes the content hashes of the
///    checker's own package items. Editing it invalidates by the same mechanism as everything
///    else, and its dependencies ride along, again because the hashes are content-addressed.
///
/// Dev tooling: the authoring path passes no cache and is unaffected, because it checks the
/// one item somebody just wrote rather than a tree.
module private Cache =
  /// The checker's own items, whose hashes stand in for "which checker produced this".
  let private checkerModules = [ "LanguageTools"; "AtRestTypeChecker" ]

  let private isCheckerItem (location : PT.PackageLocation) : bool =
    let rec startsWith (prefix : List<string>) (xs : List<string>) =
      match prefix, xs with
      | [], _ -> true
      | p :: ps, x :: rest when p = x -> startsWith ps rest
      | _ -> false
    startsWith checkerModules location.modules

  /// What the cache is only valid against: the implementations in play, and the checker.
  /// <param stabilized> must be the CONTENT-ADDRESSED ops, not the ones straight off the
  /// parser. The provisional hashes a parse hands out are not stable between runs, so a
  /// fingerprint taken from them changes on every reload and the cache never hits: it was
  /// written, read, rejected and rewritten, costing the full check every time while looking
  /// like it worked.
  let fingerprint
    (stamps : Map<string, string>)
    (deprecated : Set<string>)
    (stabilized : List<PT.PackageOp>)
    : string =
    let checkerHashes =
      stabilized
      |> List.choose (fun op ->
        match op with
        | PT.PackageOp.SetName(location, target, _) when isCheckerItem location ->
          let (PT.Hash h) = target.hash
          Some h
        | _ -> None)
      |> List.sort
    let implPart =
      stamps
      |> Map.toList
      |> List.sortBy fst
      |> List.map (fun (h, ts) -> $"{h}@{ts}")
    let deprecatedPart = deprecated |> Set.toList |> List.sort
    String.concat "\n" (implPart @ deprecatedPart @ checkerHashes)
    |> System.Text.Encoding.UTF8.GetBytes
    |> System.Security.Cryptography.SHA256.HashData
    |> System.Convert.ToHexString

  /// The content-addressed hash of each op's item, in the order the ops came, for anything
  /// that carries a body worth resolving. `None` for an op that is not a declaration.
  let keysFor (stabilized : List<PT.PackageOp>) : List<Option<PT.Hash>> =
    stabilized
    |> List.map (fun op ->
      match op with
      | PT.PackageOp.AddFn fn -> Some fn.hash
      | PT.PackageOp.AddValue value -> Some value.hash
      | _ -> None)

  let private render (resolutions : List<Resolution>) : List<string> =
    resolutions
    |> List.map (fun r ->
      let hashes (hs : List<PT.Hash>) =
        hs |> List.map (fun (PT.Hash h) -> h) |> String.concat ","
      match r with
      | Call(at, method_, impls) -> $"C {at} {method_} {hashes impls}"
      | Deferred(at, param) -> $"D {at} {param}"
      | CallerBound(at, param, PT.Hash t, impls) ->
        $"B {at} {param} {t} {hashes impls}"
      | CallerBoundDeferred(at, param, PT.Hash t, fromParam) ->
        $"P {at} {param} {t} {fromParam}")

  let private parse (line : string) : Option<Resolution> =
    let hashes (s : string) =
      if s = "" then
        []
      else
        s.Split(',') |> Array.toList |> List.map PT.Hash
    match line.Split(' ') |> Array.toList with
    | [ "C"; at; method_; impls ] ->
      match System.UInt64.TryParse at with
      | true, at -> Some(Call(at, method_, hashes impls))
      | _ -> None
    | [ "D"; at; param ] ->
      match System.UInt64.TryParse at with
      | true, at -> Some(Deferred(at, param))
      | _ -> None
    | [ "B"; at; param; t; impls ] ->
      match System.UInt64.TryParse at with
      | true, at -> Some(CallerBound(at, param, PT.Hash t, hashes impls))
      | _ -> None
    | [ "P"; at; param; t; fromParam ] ->
      match System.UInt64.TryParse at with
      | true, at -> Some(CallerBoundDeferred(at, param, PT.Hash t, fromParam))
      | _ -> None
    | _ -> None

  /// Read the cache, or an empty one when it is absent, unreadable, or was written against a
  /// different fingerprint. Every one of those is an ordinary miss rather than an error: the
  /// worst case is paying the check we were going to pay anyway.
  let read (path : string) (fingerprint : string) : Map<PT.Hash, List<Resolution>> =
    try
      if not (System.IO.File.Exists path) then
        Map.empty
      else
        let lines = System.IO.File.ReadAllLines path |> Array.toList
        match lines with
        | header :: rest when header = $"F {fingerprint}" ->
          let mutable acc = Map.empty
          let mutable current = None
          for line in rest do
            if line.StartsWith "I " then
              current <- Some(PT.Hash(line.Substring 2))
              acc <- Map.add (PT.Hash(line.Substring 2)) [] acc
            else
              match current, parse line with
              | Some key, Some r ->
                acc <- Map.add key (r :: (Map.tryFind key acc |> Option.defaultValue [])) acc
              | _ -> ()
          acc
        | _ -> Map.empty
    with _ ->
      Map.empty

  /// Write the cache. A failure here is not worth failing a build over, and the next run
  /// simply misses.
  let write
    (path : string)
    (fingerprint : string)
    (entries : Map<PT.Hash, List<Resolution>>)
    : unit =
    try
      let lines =
        [ $"F {fingerprint}" ]
        @ (entries
           |> Map.toList
           |> List.collect (fun (PT.Hash h, resolutions) ->
             $"I {h}" :: render resolutions))
      System.IO.File.WriteAllLines(path, lines)
    with _ ->
      ()


// --------------------
// Resolving.
// --------------------

let private checkPackageOpsName () =
  RT.FQFnName.fqPackage (
    PackageRefs.Fn.LanguageTools.AtRestTypeChecker.checkPackageOpsOn ()
  )

let private packageOpKT () =
  KTCustomType(
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.ProgramTypes.packageOp ()),
    []
  )


/// Ask the Dark checker what every call in this batch resolves to.
///
/// A checker that fails to answer leaves the batch alone, the same way a call it could not
/// resolve does. This is dev tooling and an authoring path, not a place to lose a save over
/// the checker having a bad day.
let private askChecker
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Ply<Map<PT.Hash, List<Resolution>>> =
  uply {
    // The branch explicitly, not whatever `exeState` happens to carry: a call is pinned to the
    // implementation live on the branch being SAVED to.
    let args =
      NEList.ofList
        (DUuid branchId.Guid)
        [ Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT) ]
    match! Exe.executeFunction exeState (checkPackageOpsName ()) [] args with
    | Ok report -> return Decode.report report
    | Error(rte, _) ->
      // A swallowed checker failure and a checker with nothing to say are the same zero, and
      // the zero is what nobody re-checks, so say which one happened. Kept deliberately: the
      // Dark side catches per item and turns each failure into an ordinary verdict, so this is
      // the only place a SYSTEMIC failure of the check would otherwise be invisible.
      print $"  [traitcalls] the at-rest checker did not answer: {rte}"
      return Map.empty
  }


/// Whether this batch could have anything to resolve at all.
///
/// An ordinary save of a fn with no trait call and no operator pays only these AST walks: the
/// checker is not cheap, and on a whole-tree reload it is most of the work. An operator IS a
/// trait method, so the cheap test is "does this mention a trait, or any infix at all".
let rec private hasInfix (expr : PT.Expr) : bool =
  match expr with
  | PT.EInfix _ -> true
  | PT.EPipe(_, first, parts) ->
    hasInfix first
    || parts
       |> List.exists (fun p ->
         match p with
         | PT.EPipeInfix _ -> true
         | _ -> false)
    || (PTAst.subExprs expr |> List.exists hasInfix)
  | _ -> PTAst.subExprs expr |> List.exists hasInfix


/// A trait method named outright (`Show.show x`), as opposed to an operator.
let rec private namesATraitMethod (expr : PT.Expr) : bool =
  match expr with
  | PT.EFnName(_, { resolved = Ok { name = PT.FQFnName.TraitMethod _ } }, _) -> true
  | _ -> PTAst.subExprs expr |> List.exists namesATraitMethod


/// Whether this batch could have anything to resolve at all.
///
/// An ordinary save of a fn with no trait call and no operator pays only these AST walks.
let private worthChecking (ops : List<PT.PackageOp>) : bool =
  ops
  |> List.exists (fun op ->
    match op with
    | PT.PackageOp.AddFn fn ->
      hasInfix fn.body
      || (Dependencies.extractFromFn fn
          |> List.exists (fun d -> d.itemKind = PT.ItemKind.Trait))
    // A value's body is an expression too, and `let scale = 3L * 4L` is an operator call the
    // checker types the same way. It is evaluated once at load rather than per call, so this
    // is not about speed; it is about the value meaning the same thing after someone else's
    // implementation arrives.
    | PT.PackageOp.AddValue value -> hasInfix value.body
    | _ -> false)


/// Write onto each trait-method call in <param ops> the implementation it resolves to.
///
/// <param branchId> separately from <param pm>, because `pm` carries the branch's names and
/// not its deprecations.
/// <param cachePath> reuses what a previous run worked out, for the dev reload, which checks
/// a whole tree rather than the one item somebody just wrote. `None` for authoring.
let resolveTraitCalls
  (cachePath : Option<string>)
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (pm : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp>> =
  uply {
    if not (worthChecking ops) then
      return ops
    else
      // A deprecated implementation is not a candidate at run time, so it must not be one
      // here either: `dark constraints` tells you to deprecate one of two rivals, and pinning
      // the one you just retired would make that advice a trap.
      let! deprecated = LibDB.Queries.getDeprecatedTraitImplHashesFor branchId
      let! stamps = LibDB.Queries.getTraitImplStamps ()

      // The item's own hash as the OPS carry it, which is what the checker's report is keyed
      // by and what the rewrite below looks up. The cache is keyed by the CONTENT hash
      // instead, so the two have to be matched up positionally, which is sound because
      // `keysFor` maps the list one to one.
      let opHash (op : PT.PackageOp) : Option<PT.Hash> =
        match op with
        | PT.PackageOp.AddFn fn -> Some fn.hash
        | PT.PackageOp.AddValue value -> Some value.hash
        | _ -> None

      let! resolved =
        match cachePath with
        | None -> askChecker exeState branchId ops
        | Some path ->
          uply {
            // Once, for both: these are the hashes the items have BEFORE any choice is
            // recorded, which is exactly the identity "this item's source, unresolved", and
            // being content-addressed is what makes a dependency change show up here.
            let stabilized = LibDB.HashStabilization.computeRealHashes ops
            let fingerprint = Cache.fingerprint stamps deprecated stabilized
            let cached = Cache.read path fingerprint
            let keys = Cache.keysFor stabilized

            // An op is a HIT when its content hash is in the cache, and its resolutions are
            // then reused as they stand. A miss is asked about. An op with no body is neither.
            let paired = List.zip ops keys

            let hits, misses =
              paired
              |> List.partition (fun (_, key) ->
                match key with
                | Some k -> Map.containsKey k cached
                | None -> false)

            let fromCache =
              hits
              |> List.choose (fun (op, key) ->
                match opHash op, key with
                | Some h, Some k ->
                  Map.tryFind k cached |> Option.map (fun rs -> (h, rs))
                | _ -> None)
              |> Map.ofList

            let toCheck = misses |> List.map fst

            // Nothing new to look at: every item with a body was answered before, under this
            // same fingerprint, so there is no check to run at all.
            let! fresh =
              if List.isEmpty toCheck then
                Ply Map.empty
              else
                askChecker exeState branchId toCheck

            // Write back what this batch knows, keyed by content hash. Only this batch's
            // items, so the file tracks the tree rather than growing forever.
            let forNextTime =
              paired
              |> List.choose (fun (op, key) ->
                match opHash op, key with
                | Some h, Some k ->
                  match Map.tryFind h fresh with
                  | Some rs -> Some(k, rs)
                  | None ->
                    // An item that was asked about and resolved NOTHING is still an answer,
                    // and caching it is what makes a second reload cheap rather than
                    // re-asking about every item that had no trait call in it.
                    match Map.tryFind k cached with
                    | Some rs -> Some(k, rs)
                    | None -> Some(k, [])
                | _ -> None)
              |> Map.ofList

            Cache.write path fingerprint forNextTime

            return
              Map.fold (fun acc k v -> Map.add k v acc) fromCache fresh
          }

      if Map.isEmpty resolved then
        return ops
      else

      /// The winner among the implementations that apply, and the fn it names for the method.
      let implFnFor
        (method_ : string)
        (implHashes : List<PT.Hash>)
        : Ply<Option<PT.ResolvedName<PT.FQFnName.Package>>> =
        uply {
          let implHashes =
            implHashes
            |> List.filter (fun (PT.Hash h) -> not (Set.contains h deprecated))
          // One implementation is not an ordering question: it is the answer. Several are,
          // and an unstamped pair has no answer, so the call is left to resolve at run time
          // and `dark constraints` reports the pair.
          let winner =
            match implHashes with
            | [] -> None
            | [ only ] -> Some only
            | several ->
              several
              |> List.map (fun (PT.Hash h as hash) ->
                (hash, stamps |> Map.tryFind h |> Option.defaultValue "", h))
              |> LibExecution.Lww.winnerOf
          match winner with
          | None -> return None
          | Some winner ->
            let! impl = pm.getTraitImpl winner
            // The implementation's own reference to the fn, location and all, which is why
            // the edge this produces reads like any other and a rename reaches it.
            return
              impl
              |> Option.bind (fun i ->
                i.methods
                |> List.tryPick (fun (name, nr) ->
                  if name <> method_ then
                    None
                  else
                    match nr.resolved with
                    | Ok { name = PT.FQFnName.Package h; location = loc } ->
                      Some { name = h; location = loc }
                    | _ -> None))
        }

      let pinsByItem = Dictionary<PT.Hash, Map<id, PT.FQFnName.ImplChoice>>()
      let boundsByItem = Dictionary<PT.Hash, Map<id, List<PT.FQFnName.BoundImpl>>>()

      for KeyValue(itemHash, resolutions) in resolved do
        // Accumulated in loops rather than folds: the trait and the impl fn are both awaited,
        // so the fold version is three nested `uply` continuations deep.
        let mutable pins = Map.empty
        let mutable bounds = Map.empty

        for resolution in resolutions do
          match resolution with
          | Call(at, method_, implHashes) ->
            match! implFnFor method_ implHashes with
            | Some implFn -> pins <- Map.add at (PT.FQFnName.Chosen implFn) pins
            | None -> ()
          | _ -> ()

        // A call whose self type is one of the item's own type params is not a call nobody
        // could work out: it is waiting for its caller, and it says so. Second, so that a
        // call the checker actually resolved wins over a deferral at the same node.
        for resolution in resolutions do
          match resolution with
          | Deferred(at, param) ->
            if not (Map.containsKey at pins) then
              pins <- Map.add at (PT.FQFnName.FromTypeParam param) pins
          | _ -> ()

        // The other half: what this item's CALLS worked out for the bounds of the fns they
        // name. That is what makes a call into a bounded fn static: the callee's body defers
        // to its type param, and the call says which implementation that param implied.
        for resolution in resolutions do
          match resolution with
          | CallerBound(at, param, traitHash, implHashes) ->
            match! pm.getTrait traitHash with
            | Some trait_ ->
              // One entry per method of the trait, so the callee's body finds a fn for
              // whichever method it calls without reading the implementation item at run time.
              for m in NEList.toList trait_.methods do
                match! implFnFor m.name implHashes with
                | Some implFn ->
                  let entry : PT.FQFnName.BoundImpl =
                    { param = param
                      trait_ = traitHash
                      method_ = m.name
                      choice = PT.FQFnName.Chosen implFn }
                  let existing = Map.tryFind at bounds |> Option.defaultValue []
                  bounds <- Map.add at (entry :: existing) bounds
                | None -> ()
            | None -> ()
          | CallerBoundDeferred(at, param, traitHash, fromParam) ->
            match! pm.getTrait traitHash with
            | Some trait_ ->
              for m in NEList.toList trait_.methods do
                let entry : PT.FQFnName.BoundImpl =
                  { param = param
                    trait_ = traitHash
                    method_ = m.name
                    choice = PT.FQFnName.FromTypeParam fromParam }
                let existing = Map.tryFind at bounds |> Option.defaultValue []
                bounds <- Map.add at (entry :: existing) bounds
            | None -> ()
          | _ -> ()

        if not (Map.isEmpty pins) then pinsByItem[itemHash] <- pins
        if not (Map.isEmpty bounds) then boundsByItem[itemHash] <- bounds

      let mappingFor (hash : PT.Hash) : Option<LibDB.AstTransformer.HashMapping> =
        let pins =
          match pinsByItem.TryGetValue hash with
          | true, pins -> pins
          | _ -> Map.empty
        let bounds =
          match boundsByItem.TryGetValue hash with
          | true, bounds -> bounds
          | _ -> Map.empty
        if Map.isEmpty pins && Map.isEmpty bounds then
          None
        else
          Some
            { LibDB.AstTransformer.emptyMapping with
                pins = pins
                boundImpls = bounds }

      return
        ops
        |> List.map (fun op ->
          match op with
          | PT.PackageOp.AddFn fn ->
            match mappingFor fn.hash with
            | Some mapping ->
              PT.PackageOp.AddFn(LibDB.AstTransformer.transformFn mapping fn)
            | None -> op
          | PT.PackageOp.AddValue value ->
            match mappingFor value.hash with
            | Some mapping ->
              PT.PackageOp.AddValue(
                LibDB.AstTransformer.transformValue mapping value
              )
            | None -> op
          | _ -> op)
  }
