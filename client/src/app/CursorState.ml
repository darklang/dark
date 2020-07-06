open Types

let rec unwrap (s : cursorState) : cursorState =
  match s with
  | DraggingTL (_, _, _, nested) | PanningCanvas {prevCursorState = nested; _}
    ->
      unwrap nested
  | _ ->
      s


let tlidOf (s : cursorState) : TLID.t option =
  match unwrap s with
  | Selecting (tlid, _) ->
      Some tlid
  | Entering (tlid, _) ->
      Some tlid
  | Omnibox _ ->
      None
  | Deselected ->
      None
  | FluidEntering tlid ->
      Some tlid
  (* NOTE: These have no id because unwrapCursorState should unwrap them *)
  | DraggingTL _ | PanningCanvas _ ->
      None


let idOf (s : cursorState) : ID.t option =
  match unwrap s with
  | Selecting (_, id) ->
      id
  | Omnibox _ ->
      None
  | Entering (_, id) ->
      Some id
  | Deselected ->
      None
  | FluidEntering _ ->
      None
  (* NOTE: These have no id because unwrapCursorState should unwrap them *)
  | DraggingTL _ | PanningCanvas _ ->
      None


let focusEntry (m : model) : msg Tea.Cmd.t =
  match unwrap m.cursorState with
  | Entering _ ->
      Tea_html_cmds.focus Defaults.entryID
  | _ ->
      Tea.Cmd.none


(* Based on Tea_html_cmds, applies offset after focus *)
let focusWithOffset id offset =
  Tea.Cmd.call (fun _ ->
      let ecb _ignored =
        match Js.Nullable.toOption (Web.Document.getElementById id) with
        | None ->
            (* Do not report this error, it's not a problem *)
            Js.log ("Attempted to focus a non-existant element of: ", id)
        | Some elem ->
            (* We have to focus after setting range, or the cursor will vanish when the offset is 0 *)
            elem##setSelectionRange offset offset ;
            Web.Node.focus elem ;
            ()
      in
      (* One to get out of the current render frame*)
      let cb _ignored = ignore (Web.Window.requestAnimationFrame ecb) in
      (* And another to properly focus *)
      ignore (Web.Window.requestAnimationFrame cb) ;
      ())


let focusEntryWithOffset (m : model) (offset : int) : msg Tea.Cmd.t =
  match unwrap m.cursorState with
  | Entering _ ->
      focusWithOffset Defaults.entryID offset
  | _ ->
      Tea.Cmd.none


let setCursorState (cursorState : cursorState) (m : model) :
    model * msg Tea.Cmd.t =
  let m = {m with cursorState} in
  (* TODO: Move the focusEntry part of this out, to happen
  * once all modifications have been applied. It's currently
  * too easy to call this only to have a later modification
  * change the model in such a way that the resulting view
  * no longer contains the thing we want to focus. *)
  (m, focusEntry m)
