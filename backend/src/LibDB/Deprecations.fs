/// What deprecation STANDS at a name, what that leaves deprecated by content, and the annotation
/// encoding the `deprecations` projection stores.
///
/// One reader: authoring asks whether the op it is about to drop as a duplicate says what already
/// stands (`Inserts`), and the CLI reads it to display (`Queries`).
module LibDB.Deprecations

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes

/// Serialize a DeprecationKind + message for the `annotation_blob` column.
/// Keeps the on-disk representation close to the binary op serializer so one
/// reader can surface both op-log history and current projected state.
let serializeAnnotation (kind : PT.DeprecationKind) (message : string) : byte array =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  LibSerialization.Binary.Serializers.PT.PackageOp.DeprecationKind.write w kind
  LibSerialization.Binary.Serializers.Common.String.write w message
  ms.ToArray()

/// An `annotation_blob` back into the kind and message it holds; None when it will not parse,
/// which is the same tolerance every other blob reader has.
let deserializeAnnotation (blob : byte array) : Option<PT.DeprecationKind * string> =
  try
    use ms = new System.IO.MemoryStream(blob)
    use r = new System.IO.BinaryReader(ms)
    let kind =
      LibSerialization.Binary.Serializers.PT.PackageOp.DeprecationKind.read r
    let message = LibSerialization.Binary.Serializers.Common.String.read r
    Some(kind, message)
  with _ ->
    None

/// The deprecation standing on (<param itemHash>, <param itemKind>) at <param location>, as
/// <param branch> sees it, or None when it is not deprecated there.
let standing
  (branch : PT.BranchId)
  (location : PT.PackageLocation)
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let (Hash itemHashStr) = itemHash

    let! rows =
      Sql.query
        "SELECT branch_id, draft, state, annotation_blob FROM deprecations
         WHERE item_hash = @item_hash AND item_kind = @item_kind AND live = 1
           AND owner = @owner AND modules = @modules AND name = @name"
      |> Sql.parameters
        [ "item_hash", Sql.string itemHashStr
          "item_kind", Sql.string (itemKind.toString ())
          "owner", Sql.string location.owner
          "modules", Sql.string (String.concat "." location.modules)
          "name", Sql.string location.name ]
      |> Sql.executeAsync (fun read ->
        (read.string "branch_id",
         read.int64 "draft" <> 0L,
         (read.string "state", read.bytesOrNone "annotation_blob")))
    let row = Layers.pick (Layers.chainOf branch) rows

    match row with
    | Some("deprecated", Some blob) -> return deserializeAnnotation blob
    | _ -> return None
  }


/// Whether an annotation says Harmful. A blob that will not parse answers no, so the fn runs: failing
/// open on a safety marking, chosen so one corrupt row cannot brick a function.
let isHarmful (blob : byte array) : bool =
  match deserializeAnnotation blob with
  | Some(PT.Harmful, _) -> true
  | _ -> false

/// A deprecated name as <param branch> sees it: the name, the content it held when deprecated, that
/// content's kind, and the annotation.
type AtName =
  { owner : string
    modules : string
    name : string
    hash : string
    kind : string
    blob : Option<byte array> }

/// Every name <param branch> sees deprecated. Synchronous, for `Queries.getHarmfulFnHashesSync`,
/// which cannot wait.
let deprecatedNamesSync (branch : PT.BranchId) : List<AtName> =
  Sql.query (
    Layers.view "@" Layers.deprecations
    + " SELECT owner, modules, name, item_hash, item_kind, annotation_blob
        FROM visible WHERE state = 'deprecated'"
  )
  |> Sql.parameters (Layers.sqlParams branch true)
  |> Sql.execute (fun read ->
    { owner = read.string "owner"
      modules = read.string "modules"
      name = read.string "name"
      hash = read.string "item_hash"
      kind = read.string "item_kind"
      blob = read.bytesOrNone "annotation_blob" })
  |> Result.unwrap

/// The content <param rows> leave deprecated: (hash, kind, annotation), one row per name that
/// deprecated it.
///
/// Deprecated and deleted are said at a name, and content is shared, so they leave content
/// deprecated only when no name holds it undeprecated: deleting `Probe.v = 5` leaves every other
/// `= 5` live. Harmful is about the code, so one name saying it marks the content.
let private contentOf
  (branch : PT.BranchId)
  (rows : List<AtName>)
  : List<string * string * Option<byte array>> =
  if List.isEmpty rows then
    []
  else
    let deprecatedAt =
      rows
      |> List.map (fun r -> ((r.owner, r.modules, r.name), r.hash, r.kind))
      |> Set.ofList
    let hashes = rows |> List.map (fun r -> r.hash) |> List.distinct
    let stillHeld =
      Sql.query (
        Layers.view "@" Layers.locations
        + " SELECT owner, modules, name, item_hash, item_type FROM visible
            WHERE item_hash IN (SELECT value FROM json_each(@hashes))"
      )
      |> Sql.parameters (
        ("hashes", Sql.string (jsonStrings hashes)) :: Layers.sqlParams branch true
      )
      |> Sql.execute (fun read ->
        ((read.string "owner", read.string "modules", read.string "name"),
         read.string "item_hash",
         read.string "item_type"))
      |> Result.unwrap
      |> List.filter (fun held -> not (Set.contains held deprecatedAt))
      |> List.map (fun (_, h, k) -> (h, k))
      |> Set.ofList
    rows
    |> List.filter (fun r ->
      Option.exists isHarmful r.blob
      || not (Set.contains (r.hash, r.kind) stillHeld))
    |> List.map (fun r -> (r.hash, r.kind, r.blob))


/// The deprecated names and the content they leave deprecated (<fn contentOf>), cached per branch
/// and dropped with every other cache on a fold: dispatch asks once per trait, and a whole-view
/// query costs more to compile than to run.
let private deprecatedCached =
  Caching.withCache (fun (branch : PT.BranchId) ->
    uply {
      let names = deprecatedNamesSync branch
      return Some(names, contentOf branch names)
    })

let private cached (branch : PT.BranchId) =
  task {
    match! Ply.toTask (deprecatedCached branch) with
    | Some found -> return found
    | None -> return ([], [])
  }

/// The content <param branch> sees deprecated: (hash, kind, annotation). See <fn contentOf>.
let deprecatedIn
  (branch : PT.BranchId)
  : Task<List<string * string * Option<byte array>>> =
  task {
    let! (_, content) = cached branch
    return content
  }

/// Every name <param branch> sees deprecated.
let deprecatedNamesIn (branch : PT.BranchId) : Task<List<AtName>> =
  task {
    let! (names, _) = cached branch
    return names
  }
