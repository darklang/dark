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
let rebuild (keptIds : Set<System.Guid>) : Task<unit> =
  task {
    let! ops = Queries.getWipOps ()
    let! preserveTs = Queries.getWipOpOriginTs ()
    let! preserveCommit = Queries.getWipOpCommits ()

    // An op survives if it was committed, or if the caller kept it.
    let surviving =
      ops
      |> List.filter (fun op ->
        let id = Inserts.computeOpHash op
        Map.containsKey id preserveCommit || Set.contains id keptIds)

    // One transaction for the delete and the re-insert; see `Inserts.rewriteOpsAtomically`. The ops
    // this build cannot decode are read by id first and kept, since `getWipOps` cannot re-insert them.
    let! unreadable = Inserts.unreadableMainOpIds ()
    let! _ =
      Inserts.rewriteOpsAtomically
        (Inserts.wholeMainDeletes unreadable)
        (fun opId ->
          match Map.tryFind opId preserveTs with
          | Some ts -> ts
          | None -> Inserts.nextOriginTs ())
        (fun opId -> Map.tryFind opId preserveCommit)
        // A whole-main rewrite re-folds bindings that were authored, propagated or resolved alike,
        // and the op alone does not say which. 'op' is the honest default.
        "op"
        surviving
    ()
  }
