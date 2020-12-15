open Core_kernel
open Libexecution
open Types
module Op = Libserialize.Op

(* ------------------------- *)
(* Undo *)
(* ------------------------- *)
(* Passthrough whether an op is new or not *)
type op_with_newness = bool * Types.op

let preprocess (ops : op_with_newness list) : op_with_newness list =
  (* The client can add undopoints when it chooses. When we get an undo,
   * we go back to the previous undopoint for that TL. *)

  (* When we get a redo, we ignore the undo immediately preceding it.
   * If there are multiple redos, they'll gradually eliminate the
   * previous undos. *)

  (* undo algorithm: *)

  (*   - Step 1: go through the list and remove all undo-redo pairs.
   *   After removing one pair, reprocess the list to remove others. *)

  (*   - Step 2: A redo without an undo just before it is pointless, but
   *   the client might allow it. Error. *)

  (*   - Step 3: there should now only be undos. Going from the front,
   *   each time there is an undo, drop the undo and all the ops going
   *   back to the previous savepoint, including the savepoint. Use the
   *   undos to go the the previous save point, dropping the ops between
   *   the undo and the *)
  ops
  (* Step 1: remove undo-redo pairs. We do by processing from the back,
   * adding each element onto the front *)
  |> List.fold_right ~init:[] ~f:(fun op ops ->
         match op :: ops with
         | [] ->
             []
         | [op] ->
             [op]
         | (_, UndoTL uid) :: (_, RedoTL rid) :: rest when rid = uid ->
             rest
         (* Step 2: error on solo redos *)
         | (_, RedoTL id1) :: (_, RedoTL id2) :: rest when id1 = id2 ->
             op :: ops
         | _ :: (_, RedoTL _) :: rest ->
             Exception.client "Already at latest redo"
         | ops ->
             ops)
  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop *)
  (* back until the last savepoint. *)
  |> List.fold_left ~init:[] ~f:(fun ops op ->
         match op with
         | _, UndoTL tlid ->
             let not_savepoint o =
               match o with
               | _, TLSavepoint sptlid when tlid = sptlid ->
                   false
               | _ ->
                   true
             in
             let after = List.drop_while ~f:not_savepoint ops in
             let before = List.take_while ~f:not_savepoint ops in
             (* if the canvas is older than the new Savepoints, then its
            * possible to undo to a point with no Savepoints anymore *)
             let new_before =
               List.filter before ~f:(fun (_, o) -> Op.tlidOf o <> tlid)
             in
             let new_after = after |> List.tl |> Option.value ~default:[] in
             (* drop savepoint *)
             new_before @ new_after
         | _ ->
             op :: ops)
  |> List.rev


(* previous step leaves the list reversed *)

let undo_count (ops : Types.oplist) (tlid : tlid) : int =
  ops |> List.rev |> List.take_while ~f:(( = ) (UndoTL tlid)) |> List.length
