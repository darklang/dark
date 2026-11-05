/// What deprecation STANDS for a piece of content, and the annotation encoding the `deprecations`
/// projection stores.
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

/// The deprecation standing for (<param itemHash>, <param itemKind>) as <param branch> sees it, or
/// None when the item is not deprecated there.
let standing
  (branch : PT.BranchId)
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let (Hash itemHashStr) = itemHash

    let! rows =
      Sql.query
        "SELECT branch_id, draft, state, annotation_blob FROM deprecations
         WHERE item_hash = @item_hash AND item_kind = @item_kind AND live = 1"
      |> Sql.parameters
        [ "item_hash", Sql.string itemHashStr
          "item_kind", Sql.string (itemKind.toString ()) ]
      |> Sql.executeAsync (fun read ->
        (read.string "branch_id",
         read.int64 "draft" <> 0L,
         (read.string "state", read.bytesOrNone "annotation_blob")))
    let row = Layers.pick (Layers.chainOf branch) rows

    match row with
    | Some("deprecated", Some blob) -> return deserializeAnnotation blob
    | _ -> return None
  }


/// Every item <param branch> sees deprecated: (hash, kind, annotation).
///
/// Cached per branch, dropped with every other cache on a fold: dispatch asks once per trait, and a
/// whole-view query costs more to compile than to run.
let private deprecatedInCached =
  Caching.withCache (fun (branch : PT.BranchId) ->
    uply {
      let! rows =
        Sql.query (
          Layers.view "@" Layers.deprecations
          + " SELECT item_hash, item_kind, annotation_blob FROM visible WHERE state = 'deprecated'"
        )
        |> Sql.parameters (Layers.sqlParams branch true)
        |> Sql.executeAsync (fun read ->
          (read.string "item_hash",
           read.string "item_kind",
           read.bytesOrNone "annotation_blob"))
      return Some rows
    })

let deprecatedIn
  (branch : PT.BranchId)
  : Task<List<string * string * Option<byte array>>> =
  task {
    match! Ply.toTask (deprecatedInCached branch) with
    | Some rows -> return rows
    | None -> return []
  }
