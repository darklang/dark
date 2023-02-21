module LibBackend.Undo

// Support Undo in the oplists

open System.Runtime.InteropServices

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

type Oplist = List<int>


// Passthrough whether an op is new or not
type OpWithNewness = bool * PT.Op


let preprocess (ops : List<OpWithNewness>) : List<OpWithNewness> =
  // The client can add undopoints when it chooses. When we get an undo,
  // we go back to the previous undopoint for that TL.

  // When we get a redo, we ignore the undo immediately preceding it.
  // If there are multiple redos, they'll gradually eliminate the
  // previous undos.

  // undo algorithm:
  //   - Step 1: go through the list and remove all undo-redo pairs.
  //   After removing one pair, reprocess the list to remove others. *)
  //   - Step 2: A redo without an undo just before it is pointless, but
  //   the client might allow it. Error.
  //   - Step 3: there should now only be undos. Going from the front,
  //   each time there is an undo, drop the undo and all the ops going
  //   back to the previous savepoint, including the savepoint. Use the
  //   undos to go the the previous save point, dropping the ops between
  //   the undo and the
  ops
  // Step 1: remove undo-redo pairs. We do by processing from the back,
  // adding each element onto the front
  |> List.foldRight [] (fun ops op ->
    match op :: ops with
    | [] -> []
    | [ op ] -> [ op ]
    | (_, PT.UndoTL uid) :: (_, PT.RedoTL rid) :: rest when rid = uid -> rest
    // Step 2: error on solo redos
    | (_, PT.RedoTL id1) :: (_, PT.RedoTL id2) :: _rest when id1 = id2 -> op :: ops
    | _ :: (_, PT.RedoTL _) :: _rest ->
      Exception.raiseEditor "Already at latest redo"
    | ops -> ops)
  // Step 3: remove undos and all the ops up to the savepoint.
  // Go from the front and build the list up. If we hit an undo, drop
  // back until the last savepoint.
  |> List.fold [] (fun ops op ->
    match op with
    | _, PT.UndoTL tlid ->
      let notSavepoint o =
        match o with
        | _, PT.TLSavepoint sptlid when tlid = sptlid -> false
        | _ -> true

      let after = List.dropWhile notSavepoint ops
      let before = List.takeWhile notSavepoint ops
      // if the canvas is older than the new Savepoints, then its
      // possible to undo to a point with no Savepoints anymore
      let newBefore = List.filter (fun (_, o : PT.Op) -> Op.tlidOf o <> tlid) before

      let newAfter = after |> List.tail |> Option.defaultValue []
      // drop savepoint
      newBefore @ newAfter
    | _ -> op :: ops)
  // previous step leaves the list reversed
  |> List.reverse

let undoCount (ops : PT.Oplist) (tlid : tlid) : int =
  ops |> List.reverse |> List.takeWhile ((=) (PT.UndoTL tlid)) |> List.length
