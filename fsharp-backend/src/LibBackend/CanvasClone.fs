module LibBackend.CanvasClone

// Cloning canvases

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

let isOpThatCreatesToplevel (op : PT.Op) : bool =
  match op with
  | PT.SetHandler _
  | PT.CreateDB _
  | PT.SetFunction _
  | PT.CreateDBWithBlankOr _
  | PT.SetType _ -> true
  | PT.DeleteFunction _
  | PT.DeleteTLForever _
  | PT.DeleteFunctionForever _
  | PT.DeleteType _
  | PT.DeleteTypeForever _
  | PT.DeleteTL _
  | PT.DeleteDBCol _
  | PT.RenameDBname _
  | PT.ChangeDBColName _
  | PT.ChangeDBColType _
  | PT.SetExpr _
  | PT.AddDBCol _
  | PT.SetDBColName _
  | PT.SetDBColType _
  | PT.CreateDBMigration _
  | PT.AddDBColToDBMigration _
  | PT.SetDBColNameInDBMigration _
  | PT.SetDBColTypeInDBMigration _
  | PT.AbandonDBMigration _
  | PT.DeleteColInDBMigration _
  | PT.TLSavepoint _
  | PT.DeprecatedInitDBm _
  | PT.UndoTL _
  | PT.RedoTL _
  | PT.MoveTL _ -> false


// [onlyOpsSinceLastSavepoint ops] When we clone a canvas, we sometimes
// want to only copy over ops since the last toplevel definition (create) op - this erases
// history, which is invisible and we don't really know at clone-time what's
// in there, because we don't have any UI for inspecting history, nor do we
// store timestamps or edited-by-user for ops ("git blame").
let onlyOpsSinceLastSavepoint (ops : PT.Oplist) : PT.Oplist =
  let mutable encounteredCreateOp = false in

  List.reverse ops
  |> List.takeWhile (fun op ->
    if encounteredCreateOp then
      false
    else
      encounteredCreateOp <- isOpThatCreatesToplevel op
      true)
  |> List.reverse


// [updateHostsInOp op oldHost newHost] Given an [op], and an
// [oldHost] and a [newHost], update string literals from
// the old to the new host. Say your canvas contains a string literal that is
// (or contains) a url pointing to the [oldHost]
// ("://oldhost.builtwithdark.com/stuff", or its localhost equivalent), the
// [op] will be transformed to refer to the [newHost]
let updateHostsInOp
  (oldHost : CanvasName.T)
  (newHost : CanvasName.T)
  (op : PT.Op)
  : PT.Op =
  let replaceHost (str : string) : string =
    // This match covers string literals containing
    // ://oldHost.builtwithdark.com/stuff (or the localhost
    // equivalent), and replaces them with
    // ://newHost.builtwithdark.com. *)
    let host (canvas : CanvasName.T) =
      $"://{string canvas}.{Config.bwdServerContentHost}"
    String.replace (host oldHost) (host newHost) str
  let rec updateHostsInPattern (pattern : PT.Pattern) : PT.Pattern =
    PT.PatternTraversal.post
      (function
      | PT.PString (patternID, str) -> PT.PString(patternID, replaceHost str)
      | pat -> pat)
      pattern
  Op.astOf op
  |> Option.map (
    PT.ExprTraversal.pre (function
      | PT.EString (id, str) -> PT.EString(id, replaceHost str)
      | PT.EMatch (id, cond, branches) ->
        let newBranches =
          branches
          |> List.map (fun (pattern, expr) -> (updateHostsInPattern pattern, expr))
        PT.EMatch(id, cond, newBranches)
      | expr -> expr)
  )
  |> Option.map (fun newAst -> Op.withAST newAst op)
  |> Option.unwrap op


// Given two canvas names, clone TLs from one to the other.
//   - returns an error if fromCanvas doesn't exist, or if toCanvas does
//     ("don't clobber an existing canvas")
//   - optionally removes history - only copies ops since the last TLSavepoint (per TL)
//   - if there are string literals referring to the old canvas' url, rewrite them to
//     refer to the new one (see updateHostsInOp)
//   - runs in a DB transaction, so this should be all-or-nothing
// FSTODO tests
let cloneCanvas
  (fromCanvasName : CanvasName.T)
  (toCanvasName : CanvasName.T)
  (preserveHistory : bool)
  : Task<unit> =
  // Ensure we can copy from and to - fromCanvas must exist, toCanvas must
  // not. Yes, this is potentially racy, if toCanvas gets created by user
  // before we finish this function. I think this is unlikely enough as to be
  // an acceptable risk - users would have to get their welcome to dark email,
  // reset their password, and log in, before we finish running cloneCanvas.
  task {
    let! fromMeta = Canvas.getMeta fromCanvasName
    let! fromTLIDs = Serialize.fetchAllLiveTLIDs fromMeta.id
    let! fromOps = Canvas.loadOplists Canvas.LiveToplevels fromMeta.id fromTLIDs
    let! fromCanvas = Canvas.loadAll fromMeta

    let! toMeta = Canvas.getMeta toCanvasName
    let! toTLIDs = Serialize.fetchAllTLIDs toMeta.id
    if toTLIDs <> [] then Exception.raiseInternal "destination already exists" []

    // Transform the ops - remove pre-savepoint ops and update hosts (canvas names) in string literals
    let toOps =
      fromOps
      |> List.map (fun ((tlid, ops) : tlid * PT.Oplist) ->
        // We always "preserve history" for DBs because their ops are cumulative
        if preserveHistory
           || Map.includes tlid fromCanvas.dbs
           || Map.includes tlid fromCanvas.deletedDBs then
          (tlid, ops)
        else
          (tlid, onlyOpsSinceLastSavepoint ops))
      |> List.map (fun (tlid, ops) ->
        let newOps = List.map (updateHostsInOp fromCanvasName toCanvasName) ops
        let (isDeleted, toplevel) =
          Canvas.getToplevel tlid fromCanvas
          |> Exception.unwrapOptionInternal "gettoplevel" [ "tlid", tlid ]
        (tlid, newOps, toplevel, isDeleted))

    do! Canvas.saveTLIDs toMeta toOps
    return ()
  }
