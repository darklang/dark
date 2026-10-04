/// What deprecation STANDS for a piece of content, and the annotation encoding the `deprecations`
/// projection stores.
///
/// One reader, for the same reason `Lww` is one rule: three places ask the question and they must
/// not drift. The fold writes the rows (`PackageOpPlayback`), authoring asks whether the op it is
/// about to drop as a duplicate says what already stands (`Inserts`, `Branches`), and the CLI and
/// LSP read it to display (`Queries`). Early in the compile order so all four can share it.
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

/// The deprecation standing for (<param itemHash>, <param itemKind>) in the projection, or None
/// when the item is not deprecated.
///
/// The ordering is the fold's: by the op's time, with arrival as the tie-break for rows folded
/// before `origin_ts` existed. `created_at` alone answered "whichever reached this machine last",
/// so two peers holding the same two ops could disagree about whether an item is deprecated.
let standing
  (itemHash : Hash)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let (Hash itemHashStr) = itemHash

    let! row =
      Sql.query
        """
        SELECT state, annotation_blob
        FROM deprecations
        WHERE item_hash = @item_hash
          AND item_kind = @item_kind
          AND unlisted_at IS NULL
        ORDER BY COALESCE(origin_ts, '') DESC, created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "item_hash", Sql.string itemHashStr
          "item_kind", Sql.string (itemKind.toString ()) ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.string "state", read.bytesOrNone "annotation_blob"))

    match row with
    | Some("deprecated", Some blob) -> return deserializeAnnotation blob
    | _ -> return None
  }
