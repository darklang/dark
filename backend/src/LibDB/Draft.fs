/// The REBUILD half of rewriting the draft: delete main's uncommitted ops and re-insert the ones the
/// caller kept, preserving their stamps, then re-fold.
///
/// The rest of the draft lives in Dark (`SCM.Draft`), which decides WHAT survives a rewrite and does
/// the surgical path itself. This is what Dark cannot do: it re-mints every surviving op's id, which
/// is hashing, and re-inserts through the fold.
///
/// The invariant it exists to hold: `Inserts.wholeMainDeletes` spares ops this build cannot decode, BY
/// ID. A synced store's draft holds a peer's ops on a newer format, stored and left unapplied on
/// purpose, and they are invisible to the Dark reader for exactly that reason. If the delete ever
/// stopped sparing them, authoring would silently eat a colleague's work with nothing to say so.
module LibDB.Draft

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// Delete every main op and re-insert the ones that survive. The fallback when the surgical path
/// cannot identify what a dropped op wrote.
///
/// `draftAsRead` is the draft the caller chose `keptIds` from. A draft op that is not in it arrived
/// after that choice and would be dropped without anyone deciding to, so then nothing is written and
/// the answer is `false`: the caller reads again and chooses again. Main moving between this read and
/// the write answers `false` the same way. `afterRead` runs in that gap, so a test can land a pull there.
let rebuildWith
  (afterRead : unit -> Task<unit>)
  (draftAsRead : Set<System.Guid>)
  (keptIds : Set<System.Guid>)
  : Task<bool> =
  task {
    let! rows = Queries.getMainOpsWithIds ()
    do! afterRead ()

    // What the caller's `mainDraftOps` would read from these rows: uncommitted and decodable.
    let draftNow =
      rows
      |> List.filter (fun r -> Option.isSome r.op && Option.isNone r.commitHash)
      |> List.map _.id
      |> Set.ofList

    if draftNow <> draftAsRead then
      return false
    else
      let preserveTs =
        rows
        |> List.choose (fun r -> r.originTs |> Option.map (fun ts -> (r.id, ts)))
        |> Map.ofList
      let preserveCommit =
        rows
        |> List.choose (fun r -> r.commitHash |> Option.map (fun c -> (r.id, c)))
        |> Map.ofList

      // An op survives if it was committed, or if the caller kept it.
      let surviving =
        rows
        |> List.choose (fun r ->
          match r.op with
          | Some op when Option.isSome r.commitHash || Set.contains r.id keptIds ->
            Some op
          | _ -> None)

      // `getMainOpsWithIds` cannot hand these back to re-insert, so the delete spares them.
      let unreadable =
        rows
        |> List.filter (fun r -> Option.isNone r.op)
        |> List.map _.id
        |> Set.ofList

      let! written =
        Inserts.rewriteMainIfUnchanged
          (rows |> List.map _.id)
          unreadable
          (fun opId ->
            match Map.tryFind opId preserveTs with
            | Some ts -> ts
            | None -> Inserts.nextOriginTs ())
          (fun opId -> Map.tryFind opId preserveCommit)
          surviving
      return Option.isSome written
  }

let rebuild
  (draftAsRead : Set<System.Guid>)
  (keptIds : Set<System.Guid>)
  : Task<bool> =
  rebuildWith (fun () -> Task.FromResult()) draftAsRead keptIds
