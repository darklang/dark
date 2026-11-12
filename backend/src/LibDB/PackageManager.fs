module LibDB.PackageManager

open Prelude
open LibExecution.ProgramTypes

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibDB.Caching

module PMPT = ProgramTypes
module PMRT = RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


/// Layer two lookups: ask <param overlay> first, and fall back to <param fallback> only
/// when it has no answer. Per call site, only which pair of lookups is composed varies.
let private overlayFirst
  (overlay : 'k -> Ply<Option<'v>>)
  (fallback : 'k -> Ply<Option<'v>>)
  (k : 'k)
  : Ply<Option<'v>> =
  uply {
    match! overlay k with
    | Some v -> return Some v
    | None -> return! fallback k
  }

/// Layer two list lookups (locations, impls): the overlay's answers first, then the fallback's.
/// Per call site, only which pair of getters is composed varies.
let private concatLocs
  (overlay : 'k -> Ply<List<'v>>)
  (fallback : 'k -> Ply<List<'v>>)
  (k : 'k)
  : Ply<List<'v>> =
  uply {
    let! overlayLocs = overlay k
    let! fallbackLocs = fallback k
    return overlayLocs @ fallbackLocs
  }

/// Answer from the store, falling back to the process's ephemeral (script-declared)
/// locations only when the store holds no binding for the hash. Per call site, only
/// which kind's stored getter and ephemeral table are paired varies.
let private storedOrEphemeral
  (stored : Hash -> Ply<List<'a>>)
  (ephemeral : Hash -> List<'a>)
  (id : Hash)
  : Ply<List<'a>> =
  uply {
    match! stored id with
    | [] -> return ephemeral id
    | locs -> return locs
  }


// Cache of Harmful fn hashes, as underlying hex strings: PT.Hash and RT.Hash are distinct CLR
// types, and storing strings avoids threading either wrapper through the cache layer. Main's: a
// fn is harmful wherever it runs once main says so. `invalidateHarmful` is for a long-lived process that
// mutates deprecation state and must not keep answering from a stale Set.
let mutable private harmfulCache : Option<Set<string>> = None

let private loadHarmful () : Set<string> =
  match harmfulCache with
  | Some cached -> cached
  | None ->
    let harmful = Queries.getHarmfulFnHashesSync () |> Set.map (fun (PT.Hash h) -> h)
    harmfulCache <- Some harmful
    harmful

/// Fill the Harmful cache ahead of the first lookup. `isHarmful` is synchronous by contract
/// (the interpreter asks it before every package call); its miss path reads synchronously, so
/// this is a warm-up, not a workaround: nothing blocks without it.
let preloadHarmful () : System.Threading.Tasks.Task<unit> =
  task {
    let! harmful = Queries.getHarmfulFnHashes ()
    harmfulCache <- Some(harmful |> Set.map (fun (PT.Hash h) -> h))
  }

/// Drop the Harmful set so the next lookup re-reads `deprecations`.
let invalidateHarmful () : unit = harmfulCache <- None

// Registered so a fold drops it along with everything else: unregistered, `deprecate --kind
// harmful` in a REPL session halts nothing until the process restarts.
Caching.register invalidateHarmful





/// <param pm>'s LOCATED results, with each name's own doc in place of its declaration's.
///
/// Only the located reads: a search result knows which NAME it answered for, and that is what a doc
/// is scoped to. `getFn hash` does not, and keeps the declaration's words -- which is the honest
/// answer to a question that named no name.
///
/// One query for the whole batch, and none at all when nothing matched.
let withLocationDocs
  (branchId : PT.BranchId)
  (pm : PT.PackageManager)
  : PT.PackageManager =
  { pm with
      search =
        fun query ->
          uply {
            let! r = pm.search query
            let! referenceNames = ReferenceNames.load branchId
            let r = ReferenceNames.patchResults referenceNames r

            let locations =
              (r.types |> List.map _.location)
              @ (r.values |> List.map _.location)
              @ (r.fns |> List.map _.location)
              @ (r.traits |> List.map _.location)
              @ (r.impls |> List.map _.location)
              @ (r.tests |> List.map _.location)

            let! docs = Docs.docsForLocations branchId locations

            if Map.isEmpty docs then
              return r
            else
              let patch
                (set : PT.DocPart -> string -> 'item -> 'item)
                (i : PT.LocatedItem<'item>)
                =
                match Map.tryFind i.location docs with
                | None -> i
                | Some parts ->
                  { i with
                      entity =
                        parts
                        |> List.fold
                          (fun acc (part, text) -> set part text acc)
                          i.entity }

              return
                { r with
                    types = r.types |> List.map (patch Docs.onType)
                    values = r.values |> List.map (patch Docs.onValue)
                    fns =
                      r.fns
                      |> List.map (fun i ->
                        let patched = patch Docs.onFn i
                        { patched with
                            entity = Docs.keepMeaning i.entity patched.entity })
                    traits = r.traits |> List.map (patch Docs.onTrait)
                    impls = r.impls |> List.map (patch Docs.onImpl)
                    tests = r.tests |> List.map (patch Docs.Test.set) }
          } }


/// The PT PackageManager for <param branch>: name resolution over the branch's view of the layered
/// `locations` (`LibDB.Layers`), content by hash. Built once per branch per process; its caches are
/// dropped with every other one on a fold.
let private ptOn (branch : PT.BranchId) : PT.PackageManager =
  let findTypeCached = withCache (fun location -> PMPT.Type.find branch location)
  let findValueCached = withCache (fun location -> PMPT.Value.find branch location)
  let findFnCached = withCache (fun location -> PMPT.Fn.find branch location)
  let findTestCached = withCache (fun location -> PMPT.Test.find branch location)
  let findTraitCached =
    Caching.withNegativeCache (fun location -> PMPT.Trait.find branch location)
  let findImplCached =
    withCache (fun location -> PMPT.TraitImpl.find branch location)

  // Not `withCache`: its key would be unit, which a dictionary cannot hold. Same
  // lifetime as the others (dropped by `invalidateAll`).
  let traitNamesCached =
    let mutable cached : Option<HashSet<string>> = None
    Caching.register (fun () -> cached <- None)
    fun () ->
      match cached with
      | Some names -> Ply names
      | None ->
        uply {
          let! names = PMPT.Trait.names branch
          cached <- Some names
          return names
        }

  { findType = findTypeCached
    findValue = findValueCached
    findFn = findFnCached
    findTrait = findTraitCached
    findTraitImpl = findImplCached
    traitNames = traitNamesCached
    findTest = findTestCached

    getType = withCache PMPT.Type.get
    getFn = withCache PMPT.Fn.get
    getValue = withCache PMPT.Value.get
    getTrait = withCache PMPT.Trait.get
    getTraitImpl = withCache PMPT.TraitImpl.get
    getTest = withCache PMPT.Test.get

    // A CLI script's declarations are never in the store, so without a fallback
    // they render as hashes. Only as a fallback, though: hashes are content
    // addressed, so a script's private name for some shape is also a name for
    // every stored declaration of that shape, and `pickLocation` breaks ties by
    // shortest path, which a script's one-segment path always wins. Consulted
    // ahead of the store, `type MyErr = | BadFormat` in a script would rename
    // `Stdlib.BigInt.ParseError` for the rest of the process.
    getTypeLocations =
      storedOrEphemeral
        (PMPT.Type.getLocations branch)
        EphemeralPackages.typeLocations
    getValueLocations =
      storedOrEphemeral
        (PMPT.Value.getLocations branch)
        EphemeralPackages.valueLocations
    getFnLocations =
      storedOrEphemeral (PMPT.Fn.getLocations branch) EphemeralPackages.fnLocations
    getTraitLocations =
      storedOrEphemeral
        (PMPT.Trait.getLocations branch)
        EphemeralPackages.traitLocations
    getTraitImplLocations =
      storedOrEphemeral
        (PMPT.TraitImpl.getLocations branch)
        EphemeralPackages.implLocations
    getTestLocations = PMPT.Test.getLocations branch

    search = fun query -> PMPT.search branch query

    impls = Impls.impls
    implsWithMethod = Impls.implsWithMethod

    init = uply { return () } }

/// The PT PackageManager for MAIN. Branch-aware callers want `ptForBranch`.
let pt : PT.PackageManager = ptOn PT.BranchId.Main


/// An in-memory PackageManager built by applying `ops` in sequence. Used for transient state
/// during parsing and testing, for ops that are not stored.
///
/// <param below> is the manager this one is layered over, when it is a layer: an in-memory
/// SetName can name content these ops never carried an Add for, because an op is identified by its
/// content and main already held that body under another name. Resolving the name needs nothing extra
/// (the combined manager falls through by hash), but search builds its rows from items, so an item the
/// layer names but does not hold is fetched from `below`; that is what keeps `ls`, `view` and `search`
/// agreeing with `eval`.
let createInMemoryOver
  (below : Option<PT.PackageManager>)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  // Folded in op order, latest wins per location, as the `locations` fold does for main. One name holds
  // ONE item, so binding a fn over a name that held a value drops the value's binding, and an `Unbind`
  // drops whatever the name held.
  let bindings : Map<PT.PackageLocation, PT.Reference> =
    List.collect PT.packageOpBindingEffects ops
    |> List.fold
      (fun bindings op ->
        match op with
        | PT.PackageOp.SetName(loc, target, _) -> Map.add loc target bindings
        | PT.PackageOp.Unbind(loc, _) -> Map.remove loc bindings
        | PT.PackageOp.AddType _
        | PT.PackageOp.AddValue _
        | PT.PackageOp.AddFn _
        | PT.PackageOp.AddTrait _
        | PT.PackageOp.AddTraitImpl _
        | PT.PackageOp.AddTest _ -> bindings

        // None of these change what a name points at -- an ack records what a person decided ABOUT a
        // name, an UpdateDoc changes what a NAME says about itself (`LibDB.Docs` answers those) -- so
        // an overlay of bindings has nothing to do here.
        | PT.PackageOp.Deprecate _
        | PT.PackageOp.Undeprecate _
        | PT.PackageOp.Rename _
        | PT.PackageOp.SetReferenceNames _
        | PT.PackageOp.UpdateDoc _
        | PT.PackageOp.Decision(_, _, _, PT.DecisionKind.Ack _) -> bindings

        // An override binds a name like a SetName does; the overlay only cares about the binding.
        | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Restore(_, target, _))
        | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) ->
          Map.add loc target bindings)
      Map.empty

  // Items are keyed by the hash the item CARRIES, which after stabilization is the hash its SetName
  // names. Pairing by ADJACENCY -- "the Add just before this SetName" -- is wrong for an overlay:
  // chain ops are ordered by origin_ts ACROSS authors, so after a bundle import the Add before your
  // SetName can be somebody else's, and your name would resolve to their body. Adjacency survives
  // only as the fallback for an item with no hash yet (pre-stabilization input: the Wasm REPL's raw
  // pass, parser tests), and only for a SetName no stamped item answers.
  let pairItems
    (isAdd : PT.PackageOp -> Option<'item>)
    (hashOf : 'item -> Hash)
    (withHash : 'item -> Hash -> 'item)
    (isSet : PT.PackageOp -> Option<Hash>)
    : Map<Hash, 'item> =
    let stamped =
      ops
      |> List.choose (fun op ->
        isAdd op
        |> Option.filter (fun i -> hashOf i <> Hash "")
        |> Option.map (fun i -> (hashOf i, i)))
      |> Map.ofList
    ops
    |> List.fold
      (fun (map, pending : Option<'item>) op ->
        match isAdd op, isSet op with
        | Some i, _ when hashOf i = Hash "" -> (map, Some i)
        | _, Some h when not (Map.containsKey h stamped) ->
          match pending with
          | Some i -> (Map.add h (withHash i h) map, None)
          | None -> (map, pending)
        | _ -> (map, pending))
      (stamped, None)
    |> fst

  let typeMap =
    pairItems
      (function
      | PT.PackageOp.AddType t -> Some t
      | _ -> None)
      (fun t -> t.hash)
      (fun t h -> { t with hash = h })
      (function
       | PT.PackageOp.SetName(_, PT.PackageType h, _) -> Some h
       | _ -> None)

  let fnMap =
    pairItems
      (function
      | PT.PackageOp.AddFn f -> Some f
      | _ -> None)
      (fun f -> f.hash)
      (fun f h -> { f with hash = h })
      (function
       | PT.PackageOp.SetName(_, PT.PackageFn h, _) -> Some h
       | _ -> None)

  let valueMap =
    pairItems
      (function
      | PT.PackageOp.AddValue v -> Some v
      | _ -> None)
      (fun v -> v.hash)
      (fun v h -> { v with hash = h })
      (function
       | PT.PackageOp.SetName(_, PT.PackageValue h, _) -> Some h
       | _ -> None)

  let traitMap =
    pairItems
      (function
      | PT.PackageOp.AddTrait t -> Some t
      | _ -> None)
      (fun t -> t.hash)
      (fun t h -> { t with hash = h })
      (function
       | PT.PackageOp.SetName(_, PT.PackageTrait h, _) -> Some h
       | _ -> None)

  let implMap =
    pairItems
      (function
      | PT.PackageOp.AddTraitImpl i -> Some i
      | _ -> None)
      (fun i -> i.hash)
      (fun i h -> { i with hash = h })
      (function
       | PT.PackageOp.SetName(_, PT.PackageTraitImpl h, _) -> Some h
       | _ -> None)

  let testMap =
    pairItems
      (function
      | PT.PackageOp.AddTest test -> Some test
      | _ -> None)
      (fun test -> test.hash)
      (fun test h -> { test with hash = h })
      (function
       | PT.PackageOp.SetName(_, PT.PackageTest h, _) -> Some h
       | _ -> None)

  let locsOf
    (hashOf : PT.Reference -> Option<Hash>)
    : Map<PT.PackageLocation, Hash> =
    bindings
    |> Map.toSeq
    |> Seq.choose (fun (loc, target) ->
      hashOf target |> Option.map (fun h -> (loc, h)))
    |> Map.ofSeq
  let typeLocMap =
    locsOf (function
      | PT.PackageType h -> Some h
      | _ -> None)
  let valueLocMap =
    locsOf (function
      | PT.PackageValue h -> Some h
      | _ -> None)
  let fnLocMap =
    locsOf (function
      | PT.PackageFn h -> Some h
      | _ -> None)
  let traitLocMap =
    locsOf (function
      | PT.PackageTrait h -> Some h
      | _ -> None)
  let implLocMap =
    locsOf (function
      | PT.PackageTraitImpl h -> Some h
      | _ -> None)
  let testLocMap =
    locsOf (function
      | PT.PackageTest h -> Some h
      | _ -> None)

  // Reverse multi-maps (hash -> every location still bound to it).
  let invert
    (m : Map<PT.PackageLocation, Hash>)
    : Map<Hash, List<PT.PackageLocation>> =
    m
    |> Map.toSeq
    |> Seq.fold
      (fun acc (loc, id) ->
        let existing = Map.tryFind id acc |> Option.defaultValue []
        Map.add id (loc :: existing) acc)
      Map.empty
  let typeIdToLocs = invert typeLocMap
  let valueIdToLocs = invert valueLocMap
  let fnIdToLocs = invert fnLocMap
  let traitIdToLocs = invert traitLocMap
  let implIdToLocs = invert implLocMap

  let ownTraitNames =
    HashSet<string>(traitLocMap |> Map.toSeq |> Seq.map (fun (l, _) -> l.name))
  let ownImpls = implMap |> Map.toList |> List.map snd
  let testIdToLocs = invert testLocMap

  { findType = fun loc -> Ply(Map.tryFind loc typeLocMap)
    findValue = fun loc -> Ply(Map.tryFind loc valueLocMap)
    findFn = fun loc -> Ply(Map.tryFind loc fnLocMap)
    findTrait = fun loc -> Ply(Map.tryFind loc traitLocMap)
    findTraitImpl = fun loc -> Ply(Map.tryFind loc implLocMap)
    traitNames =
      match below with
      | None -> fun () -> Ply ownTraitNames
      | Some below ->
        PT.PackageManager.unionTraitNames ownTraitNames below.traitNames
    findTest = fun loc -> Ply(Map.tryFind loc testLocMap)

    getType = fun id -> Ply(Map.tryFind id typeMap)
    getValue = fun id -> Ply(Map.tryFind id valueMap)
    getFn = fun id -> Ply(Map.tryFind id fnMap)
    getTrait = fun id -> Ply(Map.tryFind id traitMap)
    getTraitImpl = fun id -> Ply(Map.tryFind id implMap)
    getTest = fun id -> Ply(Map.tryFind id testMap)

    getTypeLocations =
      fun id -> Ply(Map.tryFind id typeIdToLocs |> Option.defaultValue [])
    getValueLocations =
      fun id -> Ply(Map.tryFind id valueIdToLocs |> Option.defaultValue [])
    getFnLocations =
      fun id -> Ply(Map.tryFind id fnIdToLocs |> Option.defaultValue [])
    getTraitLocations =
      fun id -> Ply(Map.tryFind id traitIdToLocs |> Option.defaultValue [])
    getTraitImplLocations =
      fun id -> Ply(Map.tryFind id implIdToLocs |> Option.defaultValue [])

    // Small and in memory: a scan over the layer's own impls.
    impls =
      fun traitHash ->
        Ply(
          ownImpls
          |> List.filter (fun i ->
            match i.trait_.resolved with
            | Ok { name = PT.FQTraitName.Package t } -> t = traitHash
            | _ -> false)
        )
    implsWithMethod =
      fun methodName ->
        Ply(
          ownImpls
          |> List.filter (fun i ->
            i.methods |> List.exists (fun (m, _) -> m = methodName))
        )
    getTestLocations =
      fun id -> Ply(Map.tryFind id testIdToLocs |> Option.defaultValue [])

    search =
      fun query ->
        // Query-aware in-memory search so the in-memory ops' items show up in ls/view/tree/search,
        // not just eval. Locations here come from SetName ops' PackageLocation, which is cleanly
        // structured (owner separate, modules a proper list) unlike the `locations` table's
        // owner-in-modules ambiguity. `combine` appends these to main's results, so this only
        // contributes the overlay's matching items.
        let cm = query.currentModule
        let text = query.text
        let rec isPrefix (p : List<string>) (l : List<string>) =
          match p, l with
          | [], _ -> true
          | ph :: pt, lh :: lt when ph = lh -> isPrefix pt lt
          | _ -> false
        let fullModule (loc : PT.PackageLocation) = loc.owner :: loc.modules
        let moduleMatches (loc : PT.PackageLocation) =
          let fm = fullModule loc
          match cm, query.searchDepth with
          | [], PT.Search.SearchDepth.AllDescendants -> true
          | [], PT.Search.SearchDepth.OnlyDirectDescendants -> List.length fm = 1
          | _, PT.Search.SearchDepth.OnlyDirectDescendants -> fm = cm
          | _, PT.Search.SearchDepth.AllDescendants -> fm = cm || isPrefix cm fm
        // Match the QUALIFIED path as well as the bare name. Main's SQL tests the query against
        // `owner`, `modules` and `owner || '.' || modules`, so `search Probe.Ctx` finds what lives
        // under it. On the bare name alone the overlay answers such a query with nothing while main
        // answers it with main's items, which makes a branch's own work invisible to exactly the
        // search someone types to find it.
        let qualified (loc : PT.PackageLocation) =
          String.concat "." (fullModule loc @ [ loc.name ])
        let nameMatches (loc : PT.PackageLocation) =
          if text = "" then
            true
          elif query.exactMatch then
            // As main's SQL: a qualified query is the whole path, or a whole-segment suffix of it.
            if text.Contains "." then
              let q = qualified loc
              q = text || q.EndsWith("." + text)
            else
              loc.name = text
          else
            let t = text.ToLowerInvariant()
            loc.name.ToLowerInvariant().Contains t
            || (qualified loc).ToLowerInvariant().Contains t
        let itemMatches (loc : PT.PackageLocation) =
          moduleMatches loc && nameMatches loc
        // One entry per LOCATION, bound to what it currently binds -- never one per
        // hash: enumerating the hash map yields every version ever bound, in hash
        // order, so `view` could show one version while `eval` runs another. Going
        // through the location maps makes search agree with `findFn` by
        // construction, and matches main's SQL search, which reads `locations`.
        let liveAt
          (locMap : Map<PT.PackageLocation, Hash>)
          (items : Map<Hash, 'item>)
          (fetchBelow : Hash -> Ply<Option<'item>>)
          : Ply<List<PT.LocatedItem<'item>>> =
          locMap
          |> Map.toList
          |> Ply.List.filterMapSequentially (fun (loc, hash) ->
            uply {
              let! item =
                match Map.tryFind hash items with
                | Some item -> Ply(Some item)
                | None -> fetchBelow hash
              return
                item
                |> Option.map (fun item ->
                  ({ entity = item; location = loc } : PT.LocatedItem<_>))
            })

        let none (_ : Hash) : Ply<Option<'item>> = Ply None
        let getTypeBelow =
          match below with
          | Some b -> (fun (h : Hash) -> b.getType h)
          | None -> none
        let getValueBelow =
          match below with
          | Some b -> (fun (h : Hash) -> b.getValue h)
          | None -> none
        let getFnBelow =
          match below with
          | Some b -> (fun (h : Hash) -> b.getFn h)
          | None -> none
        let getTraitBelow =
          match below with
          | Some b -> (fun (h : Hash) -> b.getTrait h)
          | None -> none
        let getImplBelow =
          match below with
          | Some b -> (fun (h : Hash) -> b.getTraitImpl h)
          | None -> none
        let getTestBelow =
          match below with
          | Some b -> (fun (h : Hash) -> b.getTest h)
          | None -> none

        uply {
          let! typesWithLocs = liveAt typeLocMap typeMap getTypeBelow
          let! valuesWithLocs = liveAt valueLocMap valueMap getValueBelow
          let! fnsWithLocs = liveAt fnLocMap fnMap getFnBelow
          let! traitsWithLocs = liveAt traitLocMap traitMap getTraitBelow
          let! implsWithLocs = liveAt implLocMap implMap getImplBelow
          let! testsWithLocs = liveAt testLocMap testMap getTestBelow

          // Submodules = the direct child module (cm ++ next segment) of any overlay item strictly
          // below cm. Only surfaced when browsing (empty text): a text search returns items, not
          // folders. Main's SQL search still contributes its own submodules via the fallback.
          let allLocs =
            (typesWithLocs |> List.map (fun i -> i.location))
            @ (valuesWithLocs |> List.map (fun i -> i.location))
            @ (fnsWithLocs |> List.map (fun i -> i.location))
            @ (traitsWithLocs |> List.map (fun i -> i.location))
            @ (implsWithLocs |> List.map (fun i -> i.location))
            @ (testsWithLocs |> List.map (fun i -> i.location))
          let submodules =
            if text <> "" then
              []
            else
              allLocs
              |> List.choose (fun loc ->
                let fm = fullModule loc
                if isPrefix cm fm && List.length fm > List.length cm then
                  Some(List.truncate (List.length cm + 1) fm)
                else
                  None)
              |> List.distinct

          // Honour the kind filter the way the SQL search does, or `search x --fn` on a
          // branch lists the branch's types next to main's fns.
          let wanted kind (items : List<PT.LocatedItem<'item>>) =
            if query.entityTypes.IsEmpty || List.contains kind query.entityTypes then
              items |> List.filter (fun i -> itemMatches i.location)
            else
              []

          return
            { PT.Search.SearchResults.submodules = submodules
              types = wanted PT.Search.EntityType.Type typesWithLocs
              values = wanted PT.Search.EntityType.Value valuesWithLocs
              fns = wanted PT.Search.EntityType.Fn fnsWithLocs
              traits = wanted PT.Search.EntityType.Trait traitsWithLocs
              impls = wanted PT.Search.EntityType.TraitImpl implsWithLocs
              tests = wanted PT.Search.EntityType.Test testsWithLocs }
        }

    init = uply { return () } }


let createInMemory (ops : List<PT.PackageOp>) : PT.PackageManager =
  createInMemoryOver None ops


/// Combine two PackageManagers: check `overlay` first, then fall back to `fallback`.
/// This is used to layer transient/uncommitted definitions on top of persistent ones.
let combine
  (overlay : PT.PackageManager)
  (fallback : PT.PackageManager)
  : PT.PackageManager =
  { findType = overlayFirst overlay.findType fallback.findType
    findValue = overlayFirst overlay.findValue fallback.findValue
    findFn = overlayFirst overlay.findFn fallback.findFn
    findTrait = overlayFirst overlay.findTrait fallback.findTrait
    findTraitImpl = overlayFirst overlay.findTraitImpl fallback.findTraitImpl
    traitNames =
      // One-slot memo: the union is kept while both inputs are the same objects, so a
      // resolution pass pays for it once. One publish of one tuple, and the sets are never
      // written after they are handed out.
      let mutable last : Option<HashSet<string> * HashSet<string> * HashSet<string>> =
        None
      fun () ->
        uply {
          let! o = overlay.traitNames ()
          let! f = fallback.traitNames ()
          match last with
          | Some(o', f', u) when
            obj.ReferenceEquals(o, o') && obj.ReferenceEquals(f, f')
            ->
            return u
          | _ ->
            let u = HashSet<string>(f)
            u.UnionWith o
            last <- Some(o, f, u)
            return u
        }
    findTest = overlayFirst overlay.findTest fallback.findTest

    getType = overlayFirst overlay.getType fallback.getType
    getValue = overlayFirst overlay.getValue fallback.getValue
    getFn = overlayFirst overlay.getFn fallback.getFn
    getTrait = overlayFirst overlay.getTrait fallback.getTrait
    getTraitImpl = overlayFirst overlay.getTraitImpl fallback.getTraitImpl
    getTest = overlayFirst overlay.getTest fallback.getTest

    getTypeLocations = concatLocs overlay.getTypeLocations fallback.getTypeLocations
    getValueLocations =
      concatLocs overlay.getValueLocations fallback.getValueLocations
    getFnLocations = concatLocs overlay.getFnLocations fallback.getFnLocations
    getTraitLocations =
      concatLocs overlay.getTraitLocations fallback.getTraitLocations
    getTraitImplLocations =
      concatLocs overlay.getTraitImplLocations fallback.getTraitImplLocations

    impls = concatLocs overlay.impls fallback.impls
    implsWithMethod = concatLocs overlay.implsWithMethod fallback.implsWithMethod
    getTestLocations = concatLocs overlay.getTestLocations fallback.getTestLocations

    search =
      fun query ->
        uply {
          // OVERLAY WINS: a name the overlay rebinds (an in-memory rebind of a stored item) must
          // appear ONCE, as the branch's version. Overlay results come first, so
          // distinctBy-location keeps them over the fallback's stale entry.
          let! overlayResults = overlay.search query
          let! fallbackResults = fallback.search query
          let locKey (i : PT.LocatedItem<'a>) =
            (i.location.owner, i.location.modules, i.location.name)
          let dedup items = items |> List.distinctBy locKey
          return
            { PT.Search.SearchResults.submodules =
                List.append overlayResults.submodules fallbackResults.submodules
                |> List.distinct
              types = dedup (List.append overlayResults.types fallbackResults.types)
              values =
                dedup (List.append overlayResults.values fallbackResults.values)
              fns = dedup (List.append overlayResults.fns fallbackResults.fns)
              traits =
                dedup (List.append overlayResults.traits fallbackResults.traits)
              impls = dedup (List.append overlayResults.impls fallbackResults.impls)
              tests = dedup (List.append overlayResults.tests fallbackResults.tests) }
        }

    init =
      uply {
        do! overlay.init
        do! fallback.init
      } }

/// The locations <param ops> leave UNBOUND: an `Unbind` with no later binding of the same name. An
/// overlay of bindings can only add; this is what it takes away from whatever is underneath.
let unboundBy (ops : List<PT.PackageOp>) : Set<PT.PackageLocation> =
  ops
  |> List.collect PT.packageOpBindingEffects
  |> List.fold
    (fun (hidden : Set<PT.PackageLocation>) op ->
      match op with
      | PT.PackageOp.Unbind(loc, _) -> Set.add loc hidden
      | PT.PackageOp.SetName(loc, _, _)
      | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Restore(_, _, _))
      | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override _) ->
        Set.remove loc hidden
      | _ -> hidden)
    Set.empty

/// <param pm> with <param hidden> masked: those names resolve to nothing, list nowhere, and are not
/// among a hash's locations. What a branch's `Unbind` does to main's projection underneath it.
let hide
  (hidden : Set<PT.PackageLocation>)
  (pm : PT.PackageManager)
  : PT.PackageManager =
  if Set.isEmpty hidden then
    pm
  else
    let find
      (f : PT.PackageLocation -> Ply<Option<Hash>>)
      (loc : PT.PackageLocation)
      =
      if Set.contains loc hidden then Ply None else f loc
    let locs (f : Hash -> Ply<List<PT.PackageLocation>>) (h : Hash) =
      uply {
        let! all = f h
        return all |> List.filter (fun l -> not (Set.contains l hidden))
      }
    let shown (items : List<PT.LocatedItem<'a>>) =
      items |> List.filter (fun i -> not (Set.contains i.location hidden))
    { pm with
        findType = find pm.findType
        findValue = find pm.findValue
        findFn = find pm.findFn
        findTrait = find pm.findTrait
        findTraitImpl = find pm.findTraitImpl
        getTypeLocations = locs pm.getTypeLocations
        getValueLocations = locs pm.getValueLocations
        getFnLocations = locs pm.getFnLocations
        getTraitLocations = locs pm.getTraitLocations
        getTraitImplLocations = locs pm.getTraitImplLocations
        findTest = find pm.findTest
        getTestLocations = locs pm.getTestLocations
        search =
          fun query ->
            uply {
              let! r = pm.search query
              return
                { r with
                    types = shown r.types
                    values = shown r.values
                    fns = shown r.fns
                    traits = shown r.traits
                    impls = shown r.impls
                    tests = shown r.tests }
            } }

/// `basePM` with `ops` overlaid on top: the PM for ops that are not stored (tests, from-disk
/// parsing).
let withExtraOps
  (basePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  let opsPM = createInMemoryOver (Some basePM) ops
  combine opsPM (hide (unboundBy ops) basePM)


// THE BRANCH THIS PROCESS IS ON.
//
// Resolved once at the CLI entry point from `--branch` / `DARK_BRANCH` / `current_branch`. That is
// a DEFAULT, not the mechanism: the entry point hands it to `ExecutionState.branchId` and it is
// passed from there, which is what lets a long-lived process answer about a branch it is not
// sitting on, and lets `switch` change branch without restarting.

/// The active branch's ID (for authoring routing), or None = author to main.
let mutable private currentBranchIdOpt : Option<PT.BranchId> = None

/// Resolve a dotted package name or an exact #hash. Names follow updates;
/// hashes pin a caller to one version, which is how the test runner names its worker. Main
/// only: the boot reads `exec.policy` and `--entry-point` through this, and `configSet` checks
/// `exec.policy` with it, so a policy is refused on set exactly when the boot could not find it.
let resolveFnRef (dotted : string) : Option<RT.FQFnName.FQFnName> =
  if dotted.StartsWith "#" then
    let hash = dotted.Substring 1
    if hash.Length <> 64 || not (Seq.forall System.Uri.IsHexDigit hash) then
      None
    else
      (pt.getFn (PT.Hash hash)).Result
      |> Option.map (fun _ -> RT.FQFnName.fqPackage hash)
  else
    match List.rev (dotted.Split('.') |> Array.toList) with
    | name :: revRest ->
      let owner, modules =
        match List.rev revRest with
        | o :: mods -> o, mods
        | [] -> "Darklang", []
      let location : PT.PackageLocation =
        { owner = owner; modules = modules; name = name }
      (pt.findFn location).Result
      |> Option.map (fun fqPkg ->
        RT.FQFnName.Package(PT2RT.FQFnName.Package.toRT fqPkg))
    | [] -> None


/// The branch this process is on. Main when nothing else was selected.
let currentBranchId () : PT.BranchId =
  currentBranchIdOpt |> Option.defaultValue PT.BranchId.Main

let private branchPMs =
  System.Collections.Concurrent.ConcurrentDictionary<PT.BranchId, PT.PackageManager>()

/// The PT PM for <branchId>: names as that branch sees them, with each name's own doc in place of
/// its declaration's in located results.
let ptForBranch (branchId : PT.BranchId) : PT.PackageManager =
  branchPMs.GetOrAdd(
    branchId,
    fun b -> withLocationDocs b (if b.IsMain then pt else ptOn b)
  )

/// The stamp of the op that added an impl, put on the candidate. The item does not carry it (it is
/// not content), so the store is the only place it can come from; a candidate the store has never
/// seen keeps "" and cannot win a tie with a stamped rival.
let private stampedWith
  (stamps : Map<string, string>)
  (c : RT.ImplCandidate)
  : RT.ImplCandidate =
  let (RT.Hash h) = c.source
  { c with stamp = stamps |> Map.tryFind h |> Option.defaultValue "" }


/// What dispatch on <param branch> may pick among <param impls>: the ones the branch binds at a name,
/// not deprecated there, each stamped with its op's time. One query per branch for the bound set,
/// cached with everything else, where asking each impl's names cost a query or two per impl.
let private candidatesOn =
  let live =
    Caching.withCache (fun (branch : PT.BranchId) ->
      PMPT.TraitImpl.liveHashes branch |> Ply.map Some)
  let stamps : ref<Option<Map<string, string>>> = ref None
  Caching.register (fun () -> stamps.Value <- None)
  fun (branch : PT.BranchId) (impls : List<PT.TraitImpl.TraitImpl>) ->
    uply {
      let! bound = live branch
      let! deprecated = Queries.getDeprecatedTraitImplHashesFor branch
      let! stampMap =
        match stamps.Value with
        | Some m -> Ply m
        | None ->
          uply {
            let! m = Queries.getTraitImplStamps ()
            stamps.Value <- Some m
            return m
          }
      let isCandidate (i : PT.TraitImpl.TraitImpl) =
        let (Hash h) = i.hash
        (bound |> Option.exists (fun b -> b.Contains h))
        && not (Set.contains h deprecated)
      return
        impls
        |> List.filter isCandidate
        |> List.choose PT2RT.ImplCandidate.ofImpl
        |> List.distinctBy (fun c -> c.source)
        |> List.map (stampedWith stampMap)
    }


// TODO: bring back eager loading
let rt : RT.PackageManager =
  { getType = withCache PMRT.Type.get
    getFn = withCache PMRT.Fn.get
    getValue = withCache PMRT.Value.get
    getTrait =
      withCache (fun (RT.Hash h) ->
        PMPT.Trait.get (PT.Hash h) |> Ply.map (Option.map PT2RT.Trait.toRT))
    getBytes = PMRT.Bytes.get
    persistBytes = PMRT.Bytes.insert

    // A deprecation is a fact about a hash, and a hash means the same thing on every branch, so
    // the set needs no branch key.
    isHarmful = fun (RT.Hash h) -> Set.contains h (loadHarmful ())

    // Branch-scoped: the candidates are whatever the branch's PT pm can see and still
    // names. Cached per (branch, trait) and dropped with the other caches on every fold.
    // A deprecated implementation is not a candidate: deprecating one of two rivals is
    // how the ambiguity finding says to pick a winner. Same cache lifetime as the
    // candidates (a `Deprecate` op folds, which drops every cache).
    implCandidates =
      let cached =
        Caching.withCache (fun (branchId : PT.BranchId, RT.Hash traitHash) ->
          uply {
            let! impls = Impls.impls (PT.Hash traitHash)
            let! cs = candidatesOn branchId impls
            return Some cs
          })
      fun branchId traitHash ->
        uply {
          match! cached (branchId, traitHash) with
          | Some cs -> return cs
          | None -> return []
        }
    implCandidatesByMethod =
      let cached =
        Caching.withCache (fun (branchId : PT.BranchId, methodName : string) ->
          uply {
            let! impls = Impls.implsWithMethod methodName
            let! cs = candidatesOn branchId impls
            return Some cs
          })
      fun branchId methodName ->
        uply {
          match! cached (branchId, methodName) with
          | Some cs -> return cs
          | None -> return []
        }
    implSelectionMemo = RT.ImplSelectionMemo.create ()
    implGeneration = Caching.generation

    init =
      uply {
        //eagerLoad
        return ()
      } }


/// Make <branchId> the branch this process is on. Used at boot and by `switch`, so a long-lived
/// process changes branch without a restart.
let selectBranch (branchId : PT.BranchId) : unit =
  currentBranchIdOpt <- if branchId.IsMain then None else Some branchId
