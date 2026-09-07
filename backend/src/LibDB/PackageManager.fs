module LibDB.PackageManager

open Prelude
open LibExecution.ProgramTypes

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibDB.Caching

module PMPT = ProgramTypes
module PMRT = RuntimeTypes


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

/// Layer two location lookups: the overlay's locations first, then the fallback's.
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
// types, and storing strings avoids threading either wrapper through the cache layer. Not
// branch-scoped, since `deprecations` isn't. `invalidateHarmful` is for a long-lived process that
// mutates deprecation state and must not keep answering from a stale Set.
let mutable private harmfulCache : Option<Set<string>> = None

let private loadHarmful () : Set<string> =
  match harmfulCache with
  | Some cached -> cached
  | None ->
    let harmful =
      Queries.getHarmfulFnHashes ()
      |> Async.AwaitTask
      |> Async.RunSynchronously
      |> Set.map (fun (PT.Hash h) -> h)
    harmfulCache <- Some harmful
    harmful

/// Drop the Harmful set so the next lookup re-reads `deprecations`.
let invalidateHarmful () : unit = harmfulCache <- None

// Registered so a fold drops it along with everything else: unregistered, `deprecate --kind
// harmful` in a REPL session halts nothing until the process restarts.
Caching.register invalidateHarmful


// TODO: bring back eager loading
let rt : RT.PackageManager =
  { getType = withCache PMRT.Type.get
    getFn = withCache PMRT.Fn.get
    getValue = withCache PMRT.Value.get
    getBlob = PMRT.Blob.get
    persistBlob = PMRT.Blob.insert

    // A deprecation is a fact about a hash, and a hash means the same thing on every branch, so
    // the set needs no branch key.
    isHarmful = fun (RT.Hash h) -> Set.contains h (loadHarmful ())

    init =
      uply {
        //eagerLoad
        return ()
      } }


/// The PT PackageManager for MAIN: name resolution against `locations`, which by design holds only
/// main's bindings. A branch is this plus its delta ops -- branch-aware callers want `ptForBranch`.
let pt : PT.PackageManager =
  // `withCache` allocates a fresh `ConcurrentDictionary` per invocation, so hoist the cached
  // lambdas out here to reuse one dict. Caching by location is safe precisely because this PM only
  // ever answers about main; a branch's answers come from the overlay in front of it, built per
  // branch id and never sharing this dict.
  let findTypeCached = withCache (fun location -> PMPT.Type.find location)
  let findValueCached = withCache (fun location -> PMPT.Value.find location)
  let findFnCached = withCache (fun location -> PMPT.Fn.find location)

  { findType = findTypeCached
    findValue = findValueCached
    findFn = findFnCached

    getType = withCache PMPT.Type.get
    getFn = withCache PMPT.Fn.get
    getValue = withCache PMPT.Value.get

    // A CLI script's declarations are never in the store, so without a fallback
    // they render as hashes. Only as a fallback, though: hashes are content
    // addressed, so a script's private name for some shape is also a name for
    // every stored declaration of that shape, and `pickLocation` breaks ties by
    // shortest path, which a script's one-segment path always wins. Consulted
    // ahead of the store, `type MyErr = | BadFormat` in a script would rename
    // `Stdlib.Int.ParseError` for the rest of the process.
    getTypeLocations =
      storedOrEphemeral PMPT.Type.getLocations EphemeralPackages.typeLocations
    getValueLocations =
      storedOrEphemeral PMPT.Value.getLocations EphemeralPackages.valueLocations
    getFnLocations =
      storedOrEphemeral PMPT.Fn.getLocations EphemeralPackages.fnLocations

    search = fun query -> PMPT.search query

    init = uply { return () } }


/// Main's manager with an account's version pins narrowing FN resolution. A pin maps a
/// logical name to the approved hash and only narrows normal name resolution; update
/// commands use the raw lookup for latest. Built per entry (a script or eval run), so a
/// later `permissions` change is picked up by the next one. Pins ride ON TOP of the one
/// shared main manager rather than being baked into it, because `pt` is branch-blind and
/// account-blind by design.
let ptForAccount (accountID : System.Guid option) : PT.PackageManager =
  let pins = PolicyStore.functionPins accountID
  if Map.isEmpty pins then
    pt
  else
    { pt with
        findFn =
          fun location ->
            uply {
              match! pt.findFn location with
              | None -> return None
              | Some hash ->
                match Map.tryFind (PT.PackageLocation.toFQN location) pins with
                | Some pinned ->
                  // A pin whose hash disappeared after a reset or partial sync gets
                  // a clear diagnostic instead of failing later as an unknown name.
                  match! PMPT.Fn.get (Hash pinned) with
                  | Some _ -> return Some(Hash pinned)
                  | None ->
                    return
                      Exception.raiseInternal
                        ("A pinned function version no longer exists in the package store. "
                         + "Run `dark permissions unpin <fn>` to release the pin.")
                        [ "location", PT.PackageLocation.toFQN location
                          "pinned", pinned ]
                | None -> return Some hash
            } }


/// An in-memory PackageManager built by applying `ops` in sequence. Used for transient state
/// during parsing, testing, and branch overlays.
///
/// <param below> is the manager this one is layered over, when it is a layer: a branch overlay's
/// SetName can name content the branch never carried an Add for, because an op is identified by its
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
  let typeLocs = System.Collections.Generic.Dictionary<PT.PackageLocation, Hash>()
  let valueLocs = System.Collections.Generic.Dictionary<PT.PackageLocation, Hash>()
  let fnLocs = System.Collections.Generic.Dictionary<PT.PackageLocation, Hash>()
  let unbind loc =
    typeLocs.Remove loc |> ignore<bool>
    valueLocs.Remove loc |> ignore<bool>
    fnLocs.Remove loc |> ignore<bool>
  let bind loc target =
    unbind loc
    match target with
    | PT.PackageType h -> typeLocs[loc] <- h
    | PT.PackageValue h -> valueLocs[loc] <- h
    | PT.PackageFn h -> fnLocs[loc] <- h

  for op in ops do
    match op with
    | PT.PackageOp.SetName(loc, target, _) -> bind loc target
    | PT.PackageOp.Unbind(loc, _) -> unbind loc
    | PT.PackageOp.AddType _
    | PT.PackageOp.AddValue _
    | PT.PackageOp.AddFn _ -> ()

    // None of these change what a name points at -- an ack or a policy records what a person decided ABOUT a
    // name, a BranchEvent is about the branch -- so an overlay of bindings has nothing to do here.
    | PT.PackageOp.Deprecate _
    | PT.PackageOp.Undeprecate _
    | PT.PackageOp.Decision(_,
                            _,
                            _,
                            (PT.DecisionKind.Ack _ | PT.DecisionKind.Propagation _))
    | PT.PackageOp.BranchEvent _ -> ()

    // An override binds a name like a SetName does; the overlay only cares about the binding.
    | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) ->
      bind loc target

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
    let mutable map = stamped
    let mutable pending : Option<'item> = None
    for op in ops do
      match isAdd op, isSet op with
      | Some i, _ when hashOf i = Hash "" -> pending <- Some i
      | _, Some h when not (Map.containsKey h stamped) ->
        match pending with
        | Some i ->
          map <- Map.add h (withHash i h) map
          pending <- None
        | None -> ()
      | _ -> ()
    map

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

  let toMap (d : System.Collections.Generic.Dictionary<PT.PackageLocation, Hash>) =
    d |> Seq.map (fun (KeyValue(k, v)) -> (k, v)) |> Map.ofSeq
  let typeLocMap = toMap typeLocs
  let valueLocMap = toMap valueLocs
  let fnLocMap = toMap fnLocs

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

  { findType = fun loc -> Ply(Map.tryFind loc typeLocMap)
    findValue = fun loc -> Ply(Map.tryFind loc valueLocMap)
    findFn = fun loc -> Ply(Map.tryFind loc fnLocMap)

    getType = fun id -> Ply(Map.tryFind id typeMap)
    getValue = fun id -> Ply(Map.tryFind id valueMap)
    getFn = fun id -> Ply(Map.tryFind id fnMap)

    getTypeLocations =
      fun id -> Ply(Map.tryFind id typeIdToLocs |> Option.defaultValue [])
    getValueLocations =
      fun id -> Ply(Map.tryFind id valueIdToLocs |> Option.defaultValue [])
    getFnLocations =
      fun id -> Ply(Map.tryFind id fnIdToLocs |> Option.defaultValue [])

    search =
      fun query ->
        // Query-aware in-memory search so a BRANCH overlay's items show up in ls/view/tree/search,
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
          uply {
            let found = ResizeArray<PT.LocatedItem<'item>>()
            for KeyValue(loc, hash) in locMap do
              let! item =
                match Map.tryFind hash items with
                | Some item -> Ply(Some item)
                | None -> fetchBelow hash
              match item with
              | Some item ->
                found.Add({ entity = item; location = loc } : PT.LocatedItem<_>)
              | None -> ()
            return List.ofSeq found
          }

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

        uply {
          let! typesWithLocs = liveAt typeLocMap typeMap getTypeBelow
          let! valuesWithLocs = liveAt valueLocMap valueMap getValueBelow
          let! fnsWithLocs = liveAt fnLocMap fnMap getFnBelow

          // Submodules = the direct child module (cm ++ next segment) of any overlay item strictly
          // below cm. Only surfaced when browsing (empty text): a text search returns items, not
          // folders. Main's SQL search still contributes its own submodules via the fallback.
          let allLocs =
            (typesWithLocs |> List.map (fun i -> i.location))
            @ (valuesWithLocs |> List.map (fun i -> i.location))
            @ (fnsWithLocs |> List.map (fun i -> i.location))
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

          return
            { PT.Search.SearchResults.submodules = submodules
              types = typesWithLocs |> List.filter (fun i -> itemMatches i.location)
              values =
                valuesWithLocs |> List.filter (fun i -> itemMatches i.location)
              fns = fnsWithLocs |> List.filter (fun i -> itemMatches i.location) }
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

    getType = overlayFirst overlay.getType fallback.getType
    getValue = overlayFirst overlay.getValue fallback.getValue
    getFn = overlayFirst overlay.getFn fallback.getFn

    getTypeLocations = concatLocs overlay.getTypeLocations fallback.getTypeLocations
    getValueLocations =
      concatLocs overlay.getValueLocations fallback.getValueLocations
    getFnLocations = concatLocs overlay.getFnLocations fallback.getFnLocations

    search =
      fun query ->
        uply {
          // OVERLAY WINS: a name the overlay rebinds (a branch override of a main item) must
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
              fns = dedup (List.append overlayResults.fns fallbackResults.fns) }
        }

    init =
      uply {
        do! overlay.init
        do! fallback.init
      } }

/// The process-wide manager for the outer CLI and its tools, with the
/// anonymous account's pins as of startup.
let pt : PT.PackageManager = ptForAccount None


/// The locations <param ops> leave UNBOUND: an `Unbind` with no later binding of the same name. An
/// overlay of bindings can only add; this is what it takes away from whatever is underneath.
let unboundBy (ops : List<PT.PackageOp>) : Set<PT.PackageLocation> =
  ops
  |> List.fold
    (fun (hidden : Set<PT.PackageLocation>) op ->
      match op with
      | PT.PackageOp.Unbind(loc, _) -> Set.add loc hidden
      | PT.PackageOp.SetName(loc, _, _)
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
        getTypeLocations = locs pm.getTypeLocations
        getValueLocations = locs pm.getValueLocations
        getFnLocations = locs pm.getFnLocations
        search =
          fun query ->
            uply {
              let! r = pm.search query
              return
                { r with
                    types = shown r.types
                    values = shown r.values
                    fns = shown r.fns }
            } }

/// `basePM` with `ops` overlaid on top: the branch overlay, and the parse-time PM for tests and
/// from-disk parsing.
let withExtraOps
  (basePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  let opsPM = createInMemoryOver (Some basePM) ops
  combine opsPM (hide (unboundBy ops) basePM)


// BRANCH OVERLAYS.
//
// A branch is not a copy: it is delta ops (stored `effective = 0`, tagged in `op_branches`)
// overlaid on core, so a branch's PM is `withExtraOps pt ops`. That derivation needs nothing but
// the branch id, so it works for ANY branch on demand: `opsForBranch` / `ptForBranch`.
//
// The process also has "the branch I am on", resolved once at the CLI entry point from `--branch` /
// `DARK_BRANCH` / `current_branch`. That is a DEFAULT, not the mechanism: the entry point hands it
// to `ExecutionState.branchId` and it is passed from there. Nothing deep in the stack reads it
// ambiently, which is what lets a long-lived process answer about a branch it is not sitting on,
// and lets `switch` change branch without restarting.

let mutable private branchOverlayOps : List<PT.PackageOp> = []

/// The active branch's ID (for authoring routing), or None = author to main.
let mutable private currentBranchIdOpt : Option<PT.BranchId> = None

/// Delta ops for branches OTHER than the active one, loaded on demand. Bounded by how many
/// branches a process actually asks about, which for a CLI is one or two.
let private otherBranchOps =
  System.Collections.Concurrent.ConcurrentDictionary<PT.BranchId, List<PT.PackageOp>>()

// Dropped on every fold, like the rest: DB-derived state held for the life of the process with no
// other way to expire. The user-visible readers of another branch (`diff`, `conflicts branch`) query
// SQLite directly, so this guards the in-process readers only.
Caching.register (fun () -> otherBranchOps.Clear())

/// Select the active branch's delta ops for this process (empty = main/core only). Prefer
/// `selectBranch`, which loads them; this is for callers already holding an explicit op list.
let setBranchOverlay (ops : List<PT.PackageOp>) : unit = branchOverlayOps <- ops

/// Drop the memoized op list for <branchId>, so the next read of it goes back to the store.
///
/// `opsForBranch` memoizes every branch that isn't the current one, and that memo is otherwise
/// only cleared by a fold. Authoring to a branch you aren't sitting on is supported, and a branch
/// write with no content ops folds nothing, so without this a process that had read that branch
/// once would keep serving the pre-write list for the rest of its life.
let forgetBranch (branchId : PT.BranchId) : unit =
  otherBranchOps.TryRemove branchId |> ignore<bool * List<PT.PackageOp>>

/// The branch this process is on. Main when nothing else was selected.
let currentBranchId () : PT.BranchId =
  currentBranchIdOpt |> Option.defaultValue PT.BranchId.Main

/// Delta ops for ANY branch, walking its parent chain. The active branch answers from the process
/// overlay (already loaded); any other is loaded once and memoized.
///
/// Main has none by construction -- its ops ARE the core, and an overlay is what a branch adds on top --
/// so it answers empty without a query. Hence a plain `BranchId` rather than an Option: main is a
/// branch id like any other, and wrapping it would make `None` and `Some main` two spellings of one thing.
let opsForBranch (branchId : PT.BranchId) : List<PT.PackageOp> =
  if branchId.IsMain then
    []
  elif currentBranchIdOpt = Some branchId then
    branchOverlayOps
  else
    otherBranchOps.GetOrAdd(branchId, (fun id -> (Branches.loadDeltaOps id).Result))

/// The PT PM for <branchId>: core with that branch's overlay, or plain core on main.
/// Used at parse/lowering time so a branch fn resolves name->hash.
///
/// The overlay is prefixed with the main-draft mask (`Queries.mainDraftMaskOps`): a branch
/// resolves through COMMITTED main plus its own work, so main's uncommitted draft never leaks
/// into a branch's view. The branch's own ops come later in the list and win over the mask.
let ptForBranch (branchId : PT.BranchId) : PT.PackageManager =
  if branchId.IsMain then
    pt
  else
    match (Queries.mainDraftMaskOps ()).Result @ opsForBranch branchId with
    | [] -> pt
    | ops -> withExtraOps pt ops

/// Where a branch binds <param hash>, for hash-to-NAME lookups.
///
/// `locations` holds main's bindings only; a branch's SetNames deliberately never fold into it,
/// which is the isolation guarantee. So anything resolving a hash back to a name has to ask the
/// overlay too, or a branch-authored item has no name at all and renders as `<hash:d6f972b3>`.
///
/// Latest binding wins within the overlay (ops arrive oldest-first), and a name the branch REBOUND
/// to something else no longer counts as a location for the old hash, same as main.
let branchLocationsFor
  (branchId : PT.BranchId)
  (kind : PT.ItemKind)
  (hash : Hash)
  : List<PT.PackageLocation> =
  opsForBranch branchId
  |> List.fold
    (fun (acc : Map<string, PT.PackageLocation * Hash>) op ->
      match op with
      | PT.PackageOp.SetName(loc, target, _)
      | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) when
        target.kind = kind
        ->
        let modules = String.concat "." loc.modules
        let key = $"{loc.owner}/{modules}/{loc.name}"
        Map.add key (loc, target.hash) acc
      | PT.PackageOp.Unbind(loc, _) ->
        let modules = String.concat "." loc.modules
        Map.remove $"{loc.owner}/{modules}/{loc.name}" acc
      | _ -> acc)
    Map.empty
  |> Map.toList
  |> List.choose (fun (_, (loc, h)) -> if h = hash then Some loc else None)

/// Has this branch bound anything under <param owner>? Main's answer comes from
/// `locations`, which a branch never writes, so branch-only work must be counted
/// here or the caller treats the owner as empty.
///
/// A list scan, deliberately: called per frame, the ops are already in memory and
/// few, and only after main said no.
let branchOwnerHasItems (branchId : PT.BranchId) (owner : string) : bool =
  opsForBranch branchId
  |> List.exists (fun op ->
    match op with
    | PT.PackageOp.SetName(loc, _, _)
    | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override _) ->
      loc.owner = owner
    | _ -> false)


/// Every location this branch has EVER bound <param hash> to, live or superseded.
/// `branchLocationsFor` answers only "what does this hash hold NOW"; superseded
/// versions on a branch have no other name source (`getLocationsEverNamed` reads
/// `locations`, which a branch never writes).
///
/// Strictly a FALLBACK for when the live lookup found nothing, or a moved-off
/// version would keep answering to the name that moved on.
let branchLocationsEverNamed
  (branchId : PT.BranchId)
  (kind : PT.ItemKind)
  (hash : Hash)
  : List<PT.PackageLocation> =
  opsForBranch branchId
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.SetName(loc, target, _)
    | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) when
      target.kind = kind && target.hash = hash
      ->
      Some loc
    | _ -> None)
  |> List.distinct


/// Locations for <param hash>: main's, then any the branch adds.
///
/// Main FIRST, deliberately: callers render a label and take the head, and identical content is
/// one item, so a hash is routinely live at several names. Branch-first would render a MAIN item
/// whose body happens to match something you wrote on a branch under the branch's name, the right
/// content under the wrong label. This way it is purely additive: the branch supplies names for
/// hashes main cannot name at all, and changes nothing main could already answer.
let locationsFor
  (branchId : PT.BranchId)
  (kind : PT.ItemKind)
  (hash : Hash)
  (fromMain : List<PT.PackageLocation>)
  : List<PT.PackageLocation> =
  match branchLocationsFor branchId kind hash with
  | [] -> fromMain
  | branchLocs ->
    fromMain @ (branchLocs |> List.filter (fun l -> not (List.contains l fromMain)))

/// Make <branchId> the branch this process is on: load its delta ops and set both globals. Used at
/// boot and by `ops switch`, so a long-lived process changes branch without a restart. Drops the
/// on-demand memo, since a re-select is the moment a stale overlay would show.
let selectBranch (branchId : PT.BranchId) : unit =
  otherBranchOps.Clear()

  if branchId.IsMain then
    branchOverlayOps <- []
    currentBranchIdOpt <- None
  else
    branchOverlayOps <- (Branches.loadDeltaOps branchId).Result
    currentBranchIdOpt <- Some branchId
