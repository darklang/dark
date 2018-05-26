open Core
open Types

(* ------------------------- *)
(* Undo *)
(* ------------------------- *)

(* Undo's worked on the whole canvas before we realized that sucked. We
 * keep the old ones because our "list of ops" DB format relies on us
 * processing old ops correctly. *)
let preprocess_deprecated (ops: (Op.op * bool) list) : (Op.op * bool) list =
  (* - The client can add undopoints when it chooses. *)
  (* - When we get an undo, we go back to the previous undopoint. *)
  (* - When we get a redo, we ignore the undo immediately preceding it. If there *)
  (*   are multiple redos, they'll gradually eliminate the previous undos. *)
  (* undo algorithm: *)
  (*   - Step 1: go through the list and remove all undo-redo pairs. After *)
  (*   removing one pair, reprocess the list to remove others. *)
  (*   - Step 2: A redo without an undo just before it is pointless. Error if this *)
  (*   happens. *)
  (*   - Step 3: there should now only be undos. Going from the front, each time *)
  (*   there is an undo, drop the undo and all the ops going back to the previous *)
  (*   savepoint, including the savepoint. Use the undos to go the the *)
  (*   previous save point, dropping the ops between the undo and the *)
  ops
  (* Step 1: remove undo-redo pairs. We do by processing from the back, adding each *)
  (* element onto the front *)
  |> List.fold_right ~init:[] ~f:(fun op ops ->
    match (op :: ops) with
    | [] -> []
    | [op] -> [op]
    | (Op.DeprecatedUndo, _) :: (Op.DeprecatedRedo, _) :: rest -> rest
    | (Op.DeprecatedRedo, a) :: (Op.DeprecatedRedo, b) :: rest ->
      (Op.DeprecatedRedo, a) :: (Op.DeprecatedRedo, b) :: rest
    | _ :: (Op.DeprecatedRedo, _) :: rest -> (* Step 2: error on solo redos *)
        Exception.client "Already at latest redo"
    | ops -> ops)
  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop back until *)
  (* the last favepoint. *)
  |> List.fold_left ~init:[] ~f:(fun ops op ->
       if Tuple.T2.get1 op = Op.DeprecatedUndo
       then
         ops
         |> List.drop_while ~f:(fun (o, _) -> o <> Op.DeprecatedSavepoint)
         |> (fun ops -> List.drop ops 1)   (* also drop the savepoint *)
       else
         op :: ops)
  |> List.rev (* previous step leaves the list reversed *)


let preprocess (ops: (Op.op * bool) list) : (Op.op * bool) list =

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
  |> preprocess_deprecated (* Deal with old Redo/Undo/Savepoint format. *)

  (* Step 1: remove undo-redo pairs. We do by processing from the back,
   * adding each element onto the front *)

  |> List.fold_right ~init:[] ~f:(fun op ops ->
    match (op :: ops) with
    | [] -> []
    | [op] -> [op]
    | (Op.UndoTL uid, _) :: (Op.RedoTL rid, _) :: rest when rid = uid ->
      rest

    (* Step 2: error on solo redos *)
    | (Op.RedoTL id1, _) :: (Op.RedoTL id2, _) :: rest when id1 = id2 ->
      op :: ops
    | _ :: (Op.RedoTL _, _) :: rest ->
        Exception.client "Already at latest redo"
    | ops -> ops)

  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop *)
  (* back until the last favepoint. *)
  |> List.fold_left ~init:[]
     ~f:(fun ops op ->
         match op with
         | (Op.UndoTL tlid, _) ->
           let not_savepoint (o, _) =
             (match o with
             | Op.Savepoint tlids when List.mem tlids tlid ~equal:(=) ->
               false
             | _ -> true)
           in

           let after = List.drop_while ~f:not_savepoint ops in
           let before = List.take_while ~f:not_savepoint ops in
           (* if the canvas is older than the new Savepoints, then its
            * possible to undo to a point with no Savepoints anymore *)
           let (savepoint, sp_bool) = match after with
             | [] -> Exception.client "Cannot undo any more"
             | a :: _  -> a in

           let new_before = List.filter before
             ~f:(fun (o, _) -> Op.tlidsOf o <> [tlid]) in
           let new_savepoint =
             savepoint
             |> Op.tlidsOf
             |> List.filter ~f:((<>) tlid)
             |> fun tlids -> (Op.Savepoint tlids, sp_bool) in
           (* drop savepoint *)
           let new_after = after
                           |> List.tl
                           |> Option.value ~default:[] in

           new_before @ [new_savepoint] @ new_after
         | _ -> op :: ops
      )

  |> List.rev (* previous step leaves the list reversed *)


let undo_count (ops: Op.op list) (tlid: tlid) : int =
  ops
  |> List.rev
  |> List.take_while ~f:((=) (Op.UndoTL tlid))
  |> List.length

let is_undoable (ops: Op.op list) (tlid: tlid) : bool =
  ops
  |> List.map ~f:(fun op -> (op, false))
  |> preprocess
  |> List.exists ~f:(function | (Op.Savepoint tlids, false) ->
      List.mem ~equal:(=) tlids tlid
                              | _ -> false)

let is_redoable (ops: Op.op list) (tlid: tlid) : bool =
  ops |> List.last |> (=) (Some (Op.UndoTL tlid))


