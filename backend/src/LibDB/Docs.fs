/// Reading and writing the prose a `DocPart` names, and the key a location's doc is stored under.
///
/// One implementation, shared by the fold (which reaches through a serialized blob) and by the
/// in-memory overlay and search patching (which reach through a loaded item). Two would drift, and it is the READ that
/// decides whether an incoming edit descends from what a store holds or diverges from it -- so a
/// disagreement between them would be a disagreement about what counts as a conflict.
///
/// Every read answers `None` when the part does not fit: a record field on a function, a parameter
/// the signature does not have. That is what makes an op for a part that no longer exists fold to
/// nothing rather than raise, which matters because such an op can only arrive from another
/// instance and one bad op must not refuse the batch it came in.
module LibDB.Docs

open System.Threading.Tasks
open FSharp.Control.Tasks
open Fumble
open Microsoft.Data.Sqlite

open Prelude
open LibExecution.ProgramTypes
open LibDB.Sqlite
open LibDB.PreparedBatch
open LibSerialization.Hashing

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


// ---------------------
// The storage key: (location, kind, within)
// ---------------------

/// Which KIND of part, as `location_docs` stores it.
let kind (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> "item"
  | PT.RecordField _ -> "record-field"
  | PT.EnumCase _ -> "enum-case"
  | PT.Parameter _ -> "parameter"

/// WHICH part, by its name in the declaration; "" for the declaration's own doc. With `kind`, and
/// the location, the key.
let within (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> ""
  | PT.RecordField n
  | PT.EnumCase n -> n
  | PT.Parameter i -> string i

/// How it reads in a listing or a conflict's reason.
let describe (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> "the item"
  | PT.RecordField n -> $"field {n}"
  | PT.EnumCase n -> $"case {n}"
  | PT.Parameter i -> $"parameter {i + 1}"


// ---------------------
// The lens
// ---------------------

let private named
  (name : string)
  (nameOf : 'a -> string)
  (docOf : 'a -> string)
  (items : NEList<'a>)
  : Option<string> =
  items |> NEList.find (fun i -> nameOf i = name) |> Option.map docOf

let private redoc
  (name : string)
  (nameOf : 'a -> string)
  (setDoc : 'a -> 'a)
  (items : NEList<'a>)
  : NEList<'a> =
  items |> NEList.map (fun i -> if nameOf i = name then setDoc i else i)


let inFn (part : PT.DocPart) (fn : PT.PackageFn.PackageFn) : Option<string> =
  match part with
  | PT.WholeItem -> Some fn.description
  | PT.Parameter index ->
    fn.parameters
    |> NEList.toList
    |> List.tryItem index
    |> Option.map (fun p -> p.description)
  | PT.RecordField _
  | PT.EnumCase _ -> None

let onFn
  (part : PT.DocPart)
  (text : string)
  (fn : PT.PackageFn.PackageFn)
  : PT.PackageFn.PackageFn =
  match part with
  | PT.WholeItem -> { fn with description = text }
  | PT.Parameter index ->
    { fn with
        parameters =
          fn.parameters
          |> NEList.mapWithIndex (fun i p ->
            if i = index then { p with description = text } else p) }
  | PT.RecordField _
  | PT.EnumCase _ -> fn


let inType (part : PT.DocPart) (t : PT.PackageType.PackageType) : Option<string> =
  match part, t.declaration.definition with
  | PT.WholeItem, _ -> Some t.description
  | PT.RecordField name, PT.TypeDeclaration.Record fields ->
    named
      name
      (fun (f : PT.TypeDeclaration.RecordField) -> f.name)
      (fun f -> f.description)
      fields
  | PT.EnumCase name, PT.TypeDeclaration.Enum cases ->
    named
      name
      (fun (c : PT.TypeDeclaration.EnumCase) -> c.name)
      (fun c -> c.description)
      cases
  | _ -> None

let onType
  (part : PT.DocPart)
  (text : string)
  (t : PT.PackageType.PackageType)
  : PT.PackageType.PackageType =
  let redefine definition =
    { t with declaration = { t.declaration with definition = definition } }

  match part, t.declaration.definition with
  | PT.WholeItem, _ -> { t with description = text }
  | PT.RecordField name, PT.TypeDeclaration.Record fields ->
    redoc
      name
      (fun (f : PT.TypeDeclaration.RecordField) -> f.name)
      (fun f -> { f with description = text })
      fields
    |> PT.TypeDeclaration.Record
    |> redefine
  | PT.EnumCase name, PT.TypeDeclaration.Enum cases ->
    redoc
      name
      (fun (c : PT.TypeDeclaration.EnumCase) -> c.name)
      (fun c -> { c with description = text })
      cases
    |> PT.TypeDeclaration.Enum
    |> redefine
  | _ -> t


/// A trait's whole-item doc, or a method's (the method is named like a record field).
let inTrait (part : PT.DocPart) (t : PT.Trait.Trait) : Option<string> =
  match part with
  | PT.WholeItem -> Some t.description
  | PT.RecordField name ->
    t.methods
    |> NEList.toList
    |> List.tryFind (fun (m : PT.Trait.Method) -> m.name = name)
    |> Option.map (fun m -> m.description)
  | PT.EnumCase _
  | PT.Parameter _ -> None

let onTrait
  (part : PT.DocPart)
  (text : string)
  (t : PT.Trait.Trait)
  : PT.Trait.Trait =
  match part with
  | PT.WholeItem -> { t with description = text }
  | PT.RecordField name ->
    { t with
        methods =
          t.methods
          |> NEList.map (fun (m : PT.Trait.Method) ->
            if m.name = name then { m with description = text } else m) }
  | PT.EnumCase _
  | PT.Parameter _ -> t

let inImpl (part : PT.DocPart) (i : PT.TraitImpl.TraitImpl) : Option<string> =
  match part with
  | PT.WholeItem -> Some i.description
  | _ -> None

let onImpl
  (part : PT.DocPart)
  (text : string)
  (i : PT.TraitImpl.TraitImpl)
  : PT.TraitImpl.TraitImpl =
  match part with
  | PT.WholeItem -> { i with description = text }
  | _ -> i

let inValue (part : PT.DocPart) (v : PT.PackageValue.PackageValue) : Option<string> =
  match part with
  | PT.WholeItem -> Some v.description
  | _ -> None

let onValue
  (part : PT.DocPart)
  (text : string)
  (v : PT.PackageValue.PackageValue)
  : PT.PackageValue.PackageValue =
  match part with
  | PT.WholeItem -> { v with description = text }
  | _ -> v

/// A test's doc. A test has no fields, cases or parameters, so only `WholeItem`
/// has one.
module Test =
  /// The doc <param part> of <param test> declares, or None for a part a test does not have.
  let get (part : PT.DocPart) (test : PT.PackageTest.PackageTest) : Option<string> =
    match part with
    | PT.WholeItem -> Some test.description
    | _ -> None

  /// <param test> with <param part>'s doc replaced by <param text>: a copy, nothing changes in
  /// place. A part a test does not have leaves it as it was.
  let set
    (part : PT.DocPart)
    (text : string)
    (test : PT.PackageTest.PackageTest)
    : PT.PackageTest.PackageTest =
    match part with
    | PT.WholeItem -> { test with description = text }
    | _ -> test


// ---------------------
// What a location says of its own
// ---------------------

/// The part a stored (kind, within) pair names, or None for a row this build cannot read.
let partOf (kind : string) (within : string) : Option<PT.DocPart> =
  match kind with
  | "item" -> Some PT.WholeItem
  | "record-field" -> Some(PT.RecordField within)
  | "enum-case" -> Some(PT.EnumCase within)
  | "parameter" ->
    match System.Int32.TryParse within with
    | true, i -> Some(PT.Parameter i)
    | _ -> None
  | _ -> None


/// The part as ONE string, for a column that has to carry it whole: "item",
/// "record-field:alongwards", "parameter:0". `partFromKey` reads it back.
let partKey (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> "item"
  | _ -> $"{kind part}:{within part}"

/// The part a `partKey` names, or None for a string this build cannot read.
let partFromKey (key : string) : Option<PT.DocPart> =
  match key.Split(':', 2) with
  | [| k |] -> partOf k ""
  | [| k; w |] -> partOf k w
  | _ -> None


/// What a view's visible doc rows look like, over <param where> (a condition on `visible`).
let private visibleDocsSql (where : string) : string =
  Layers.view "@" Layers.docs
  + " SELECT owner, modules, name, kind, within, text FROM visible WHERE "
  + where

/// Every doc <param location> has OF ITS OWN on <param branchId>, keyed by (kind, within).
///
/// Empty for almost every name: a doc a name has not overridden lives in the declaration, which is
/// where a reader finds it without asking. This answers the other question -- "does this NAME say
/// something else" -- which is what makes ten `ParseError` types able to mean ten things.
let docsAt
  (branchId : PT.BranchId)
  (location : PT.PackageLocation)
  : Task<List<PT.DocPart * string>> =
  task {
    let! rows =
      Sql.query (
        visibleDocsSql "owner = @owner AND modules = @modules AND name = @name"
      )
      |> Sql.parameters (
        [ "owner", Sql.string location.owner
          "modules", Sql.string (String.concat "." location.modules)
          "name", Sql.string location.name ]
        @ Layers.sqlParams branchId true
      )
      |> Sql.executeAsync (fun read ->
        (read.string "kind", read.string "within", read.string "text"))

    return
      rows
      |> List.choose (fun (k, w, text) ->
        partOf k w |> Option.map (fun part -> part, text))
  }


/// The same, for MANY locations at once, as the map a search result is patched from.
///
/// One query, because a listing is a few hundred rows and a query each would be a few hundred
/// queries. Empty when no name in the batch has ever had a doc edited, which is the ordinary case
/// and the one that has to stay cheap.
let docsForLocations
  (branchId : PT.BranchId)
  (locations : List<PT.PackageLocation>)
  : Task<Map<PT.PackageLocation, List<PT.DocPart * string>>> =
  task {
    if List.isEmpty locations then
      return Map.empty
    else
      let wanted = Set.ofList locations
      let! rows =
        Sql.query (visibleDocsSql "1 = 1")
        |> Sql.parameters (Layers.sqlParams branchId true)
        |> Sql.executeAsync (fun read ->
          let loc : PT.PackageLocation =
            { owner = read.string "owner"
              modules =
                (match read.string "modules" with
                 | "" -> []
                 | m -> m.Split('.') |> List.ofArray)
              name = read.string "name" }
          (loc, (read.string "kind", read.string "within", read.string "text")))

      return
        rows
        |> List.filter (fun (l, _) -> Set.contains l wanted)
        |> List.choose (fun (l, (k, w, text)) ->
          partOf k w |> Option.map (fun part -> (l, (part, text))))
        |> List.fold
          (fun (acc : Map<PT.PackageLocation, List<PT.DocPart * string>>) (l, pt) ->
            Map.add l ((Map.tryFind l acc |> Option.defaultValue []) @ [ pt ]) acc)
          Map.empty
  }


// ---------------------
// The fold
// ---------------------
//
// Applying an `UpdateDoc` lives here rather than beside the other `applyX` functions in
// `PackageOpPlayback`, because everything else it needs is in this file: the lens that reads a
// declaration, the key a row is stored under, and the fallback when a name has said nothing of its
// own. Split across two files it was two places to look for one rule.

/// What the DECLARATION at <param location> says at <param part>, if anything.
///
/// The fallback under a name's own doc, and the thing a first edit is made against. `None` when the
/// name holds nothing, or when the part is not in that declaration -- a record field on a function,
/// a parameter the signature does not have. Dropped rather than raised: such an op can only arrive
/// from another instance, and one bad op must not refuse the batch it came in.
let private declaredAt
  (ctx : Ctx)
  (branch : PT.BranchId)
  (location : PT.PackageLocation)
  (part : PT.DocPart)
  : Task<Option<string>> =
  task {
    let! bound =
      pairOption
        ctx
        (Layers.view "$" Layers.locations
         + " SELECT item_hash, item_type FROM visible
             WHERE owner = $owner AND modules = $modules AND name = $name AND item_hash <> ''")
        (fun cmd ->
          pLoc cmd location
          Layers.bind cmd branch true)

    match bound with
    | None
    | Some(_, None) -> return None
    | Some(hashStr, Some itemType) ->
      let hash = Hash hashStr

      let table, read =
        match PT.ItemKind.fromString itemType with
        | PT.ItemKind.Fn ->
          "package_functions",
          (fun bytes -> inFn part (BS.PT.PackageFn.deserialize hash bytes))
        | PT.ItemKind.Type ->
          "package_types",
          (fun bytes -> inType part (BS.PT.PackageType.deserialize hash bytes))
        | PT.ItemKind.Value ->
          "package_values",
          (fun bytes -> inValue part (BS.PT.PackageValue.deserialize hash bytes))
        | PT.ItemKind.Trait ->
          "package_traits",
          (fun bytes -> inTrait part (BS.PT.Trait.deserialize hash bytes))
        | PT.ItemKind.TraitImpl ->
          "package_trait_impls",
          (fun bytes -> inImpl part (BS.PT.TraitImpl.deserialize hash bytes))
        | PT.ItemKind.Test ->
          "package_tests",
          (fun bytes -> Test.get part (BS.PT.PackageTest.deserialize hash bytes))

      let! stored =
        bytesOption ctx $"SELECT pt_def FROM {table} WHERE hash = $hash" (fun cmd ->
          p cmd "$hash" hashStr)

      return stored |> Option.bind read
  }


/// Record that two people wrote different prose for one name, neither having seen the other's.
///
/// Auto-resolved and pending, exactly like a name divergence: the newer statement is applied so the
/// store stays usable, and the row is what makes the loser's words findable instead of gone. The id
/// is derived from both texts, so both instances mint the same one and re-folding the same pair
/// updates one row rather than piling up duplicates.
let private recordDocConflict
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (ts : string)
  (location : PT.PackageLocation)
  (part : PT.DocPart)
  (ours : string)
  (standingTs : string)
  (theirs : string)
  (incomingWins : bool)
  : Task<unit> =
  task {
    let (Hash ourText) = Hashing.hashText ours
    let (Hash theirText) = Hashing.hashText theirs
    let modules = String.concat "." location.modules

    let material =
      let sorted = List.sort [ ourText; theirText ]
      $"{location.owner}/{modules}/{location.name}/{kind part}/{within part}|"
      + String.concat "," sorted

    let id =
      material
      |> UTF8.toBytes
      |> System.Security.Cryptography.SHA256.HashData
      |> System.Convert.ToHexString
      |> fun h -> "doc" + h.Substring(0, 8).ToLowerInvariant()

    // The candidates are hashes of the TEXTS, not of items: a doc edit never moves an item's hash.
    // Same field names as a name divergence's candidates, because the same reader decodes both and
    // a listing that cannot find its own sides says "auto" for every row. Built by hand: the
    // reflection serializer is disabled under AOT.
    // The TEXT rides along too, because settling a wording disagreement means writing one of these
    // words, and a hash cannot be turned back into the sentence it hashed.
    let candidate (side : string) (hash : string) (text : string) (stamp : string) =
      let escaped = System.Text.Json.JsonEncodedText.Encode(text).ToString()

      // `removed` is false for every doc candidate: a wording disagreement has two texts, never an
      // absence. Present because the Dark `Candidate` has the field and a missing one fails the parse.
      $"""{{"side":"{side}","hash":"{hash}","removed":false,"text":"{escaped}","originTs":"{stamp}","author":""}}"""

    let candidates =
      "["
      + candidate "local" ourText ours standingTs
      + ","
      + candidate "incoming" theirText theirs ts
      + "]"

    do!
      exec ctx "INSERT INTO conflicts
           (id, owner, modules, name, item_type, part, kind, candidates, auto_resolved_to, reason,
            status, origin_ts, branch_id)
         VALUES ($id, $owner, $modules, $name, '', $part, 'doc-divergence', $candidates,
                 $winner, $reason, 'pending', $ts, $branch)
         ON CONFLICT(id) DO UPDATE SET
           auto_resolved_to = excluded.auto_resolved_to,
           reason = excluded.reason,
           origin_ts = excluded.origin_ts" (fun cmd ->
        p cmd "$id" id
        pLoc cmd location
        p cmd "$part" (partKey part)
        p cmd "$candidates" candidates
        p cmd "$winner" (if incomingWins then theirText else ourText)
        p
          cmd
          "$reason"
          $"two texts for {describe part}, neither made from the other"
        p cmd "$ts" ts
        p cmd "$branch" (string branchId))
  }


/// Apply an `UpdateDoc`: what the name at <param location> says at <param part> becomes <param
/// text>.
///
/// A row in `location_docs` and nothing else. The declaration keeps its own `///` -- that is shared
/// content, and rewriting it would put one name's words on every other name holding that body,
/// which is the whole reason this op is scoped to a location.
///
/// Two questions decide whether the row moves, and they are different:
///   - WHO IS NEWER, by the ops' own `origin_ts`, so two machines folding the same pair in
///     different orders land in the same place. An older statement arriving late is a late arrival,
///     not a new decision.
///   - WHETHER THE WRITER SAW WHAT WE HOLD, by `previous`. An edit made on top of our text is
///     collaboration and applies quietly. One made against a text we never had is a divergence: the
///     newer text still wins, and the conflict record keeps the loser's words findable.
let applyUpdateDoc
  (ctx : Ctx)
  (layer : Layers.Layer)
  (opId : System.Guid)
  (ts : string)
  (location : PT.PackageLocation)
  (part : PT.DocPart)
  (text : string)
  (previous : Option<Hash>)
  : Task<unit> =
  task {
    let kind = kind part
    let within = within part

    let bindKey (cmd : SqliteCommand) =
      pLoc cmd location
      p cmd "$kind" kind
      p cmd "$within" within

    // What the name says now, as this branch sees it: its own words if any layer it sees holds
    // some, else what its declaration says. "" when the name holds nothing at all, which is the
    // ordinary case for a doc op that arrived ahead of the `SetName` that binds its subject.
    let! seen =
      pairOption
        ctx
        (Layers.view "$" Layers.docs
         + " SELECT text, origin_ts FROM visible
             WHERE owner = $owner AND modules = $modules AND name = $name
               AND kind = $kind AND within = $within")
        (fun cmd ->
          bindKey cmd
          Layers.bind cmd layer.branch true)

    let! current =
      match seen with
      | Some(text, _) -> Task.FromResult text
      | None ->
        declaredAt ctx layer.branch location part
        |> Task.map (Option.defaultValue "")

    // Staleness is the layer's own question: an older statement than the one this layer already
    // holds is a late arrival.
    let! standingTs =
      textOption ctx "SELECT origin_ts FROM location_docs
         WHERE owner = $owner AND modules = $modules AND name = $name
           AND kind = $kind AND within = $within
           AND branch_id = $branch AND draft = $draft AND live = 1" (fun cmd ->
        bindKey cmd
        p cmd "$branch" (string layer.branch)
        p cmd "$draft" (if layer.draft then 1L else 0L))

    let standingTs = standingTs |> Option.defaultValue ""
    let seenTs = seen |> Option.bind snd |> Option.defaultValue ""

    if current = text then
      return ()
    else
      let stale = standingTs <> "" && ts < standingTs

      // Did whoever wrote this see the text we hold? `previous` naming its hash says yes. None says
      // the writer found nothing there, which is only consistent with what we hold if we have
      // nothing either.
      let descends =
        match previous with
        | Some p -> p = Hashing.hashText current
        | None -> current = ""

      // A divergence needs TWO statements. An empty local text is not a quiet disagreement, it is
      // nobody here having said anything yet.
      if not descends && current <> "" then
        do!
          recordDocConflict
            ctx
            layer.branch
            ts
            location
            part
            current
            seenTs
            text
            (not stale)

      if not stale then
        do! Layers.supersede ctx Layers.docs layer bindKey
        do!
          exec
            ctx
            "INSERT INTO location_docs
               (branch_id, draft, owner, modules, name, kind, within, text, origin_ts, op_id)
             VALUES ($branch, $draft, $owner, $modules, $name, $kind, $within, $text, $ts, $op_id)"
            (fun cmd ->
              bindKey cmd
              p cmd "$branch" (string layer.branch)
              p cmd "$draft" (if layer.draft then 1L else 0L)
              p cmd "$text" text
              p cmd "$ts" ts
              p cmd "$op_id" (string opId))
  }
