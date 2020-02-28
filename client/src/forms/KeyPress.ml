open Prelude

(* Tea *)
module Cmd = Tea.Cmd
module Http = Tea.Http

(* Dark *)
module AC = Autocomplete
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Key = Keyboard
module Regex = Util.Regex

let undo_redo (m : model) (redo : bool) : modification =
  match tlidOf m.cursorState with
  | Some tlid ->
      let undo =
        if redo
        then AddOps ([RedoTL tlid], FocusSame)
        else AddOps ([UndoTL tlid], FocusSame)
      in
      ( match TL.get m tlid |> Option.andThen ~f:TL.asDB with
      | Some _ ->
          (* We could do it on the server but it's really hard
                 atm. To do it on the server, efficiently, we'd create
                 a canvas with almost all the ops, check if the tlid
                 is a DB, then recreate the canvas with all the ops
                 (such that preprocess with the DB works). That way we
                 load from disk/db once, but still check server side.
              *)
          if DB.isLocked m tlid
          then DisplayError "Cannot undo/redo in locked DBs"
          else undo
      | None ->
          undo )
  | None ->
      NoChange


let openOmnibox (m : model) : modification =
  match m.currentPage with
  | Architecture | FocusedHandler _ | FocusedDB _ | FocusedGroup _ ->
      Many [Deselect; Entry.openOmnibox ()]
  | FocusedFn _ | FocusedType _ ->
      Entry.openOmnibox ()


let defaultHandler (event : Keyboard.keyEvent) (m : model) : modification =
  let isFluidState =
    match m.cursorState with FluidEntering _ -> true | _ -> false
  in
  let isSettingViewFocused = m.settingsView.opened in
  let isMac = Entry.getBrowserPlatform () = Mac in
  let osCmdKeyHeld = if isMac then event.metaKey else event.ctrlKey in
  if isFluidState || isSettingViewFocused
  then NoChange
  else if osCmdKeyHeld && event.keyCode = Key.Z
  then
    undo_redo m event.shiftKey
    (* Note: CTRL+Y is Windows redo
      but CMD+Y on Mac is the history shortcut in Chrome (since CMD+H is taken for hide)
      See https://support.google.com/chrome/answer/157179?hl=en *)
  else if (not isMac) && event.ctrlKey && event.keyCode = Key.Y
  then undo_redo m true
  else if osCmdKeyHeld && event.keyCode = Key.S
  then ShowSaveToast
  else
    match m.cursorState with
    | Selecting (tlid, mId) ->
      ( match (event.keyCode, TL.get m tlid) with
      | Key.Escape, _ ->
        ( match mId with
        (* if we're selecting an expression,
                   go 'up' to selecting the toplevel only *)
        | Some _ ->
            Select (tlid, STTopLevelRoot)
        (* if we're selecting a toplevel only, deselect. *)
        | None ->
            Deselect )
      | Key.Enter, Some (TLGroup _) when event.shiftKey ->
          NoChange
      | Key.Enter, Some (TLTipe t as tl) when event.shiftKey ->
        ( match mId with
        | Some id ->
          ( match TL.find tl id with
          | Some (PTypeName _)
          | Some (PTypeFieldName _)
          | Some (PTypeFieldTipe _) ->
              let replacement = UserTypes.extend t in
              AddOps ([SetType replacement], FocusNext (tlid, Some id))
          | _ ->
              NoChange )
        | None ->
            NoChange )
      | Key.Enter, Some (TLDB _) when event.shiftKey ->
          let blankid = gid () in
          AddOps ([AddDBCol (tlid, blankid, gid ())], FocusExact (tlid, blankid))
      | Key.Enter, Some (TLFunc f as tl) when event.shiftKey ->
        ( match mId with
        | Some id ->
          ( match TL.find tl id with
          | Some (PParamTipe _) | Some (PParamName _) | Some (PFnName _) ->
              Refactor.addFunctionParameter m f id
          | _ ->
              NoChange )
        | _ ->
            NoChange )
      | Key.Enter, Some _ when event.shiftKey ->
          NoChange
      | Key.Enter, Some _ when not event.shiftKey ->
        ( match mId with
        | Some id ->
            Selection.enter m tlid id
        | None ->
            NoChange )
      | Key.Tab, _ ->
        (* NB: see `stopKeys` in ui.html *)
        ( match mId with
        | Some id ->
            if event.shiftKey
            then Selection.enterPrevBlank m tlid id
            else Selection.enterNextBlank m tlid id
        | None ->
            NoChange )
      | Key.O, Some _ ->
          if event.altKey then CenterCanvasOn tlid else NoChange
      | Key.K, Some _ ->
          if osCmdKeyHeld then openOmnibox m else NoChange
      | _ ->
          NoChange )
    | Entering cursor ->
        if event.ctrlKey
        then
          match event.keyCode with
          | Key.P ->
              AutocompleteMod ACSelectUp
          | Key.N ->
              AutocompleteMod ACSelectDown
          | _ ->
              NoChange
        else (
          match event.keyCode with
          | Key.Spacebar ->
            ( match cursor with
            | Creating _ ->
                if AC.isOmnibox m.complete
                then AutocompleteMod (ACSetQuery (m.complete.value ^ " "))
                else NoChange
            | _ ->
                NoChange )
          | Key.Enter ->
              Entry.submit m cursor Entry.StayHere
          | Key.Tab ->
            ( match cursor with
            | Filling (tlid, id) ->
                let content = AC.getValue m.complete in
                let hasContent = content <> "" in
                if event.shiftKey
                then
                  if hasContent
                  then NoChange
                  else Selection.enterPrevBlank m tlid id
                else if hasContent
                then Entry.submit m cursor Entry.GotoNext
                else Selection.enterNextBlank m tlid id
            | Creating _ ->
                NoChange )
          | Key.Unknown _ ->
              NoChange
          | Key.Escape ->
            ( match cursor with
            | Creating _ ->
                Many [Deselect; AutocompleteMod ACReset]
            | Filling (tlid, p) ->
                Many [Select (tlid, STID p); AutocompleteMod ACReset] )
          | Key.Up ->
              AutocompleteMod ACSelectUp (* NB: see `stopKeys` in ui.html *)
          | Key.Down ->
              AutocompleteMod ACSelectDown (* NB: see `stopKeys` in ui.html *)
          | Key.Backspace ->
              (* This was an old hack for strings in the AST of the old editor.
               * Unclear if it still is needed. *)
              let v = m.complete.value in
              Many
                [ AutocompleteMod (ACSetVisible true)
                ; AutocompleteMod (ACSetQuery v)
                ; AutocompleteMod (ACSetVisible true)
                ; MakeCmd (Entry.focusEntry m) ]
          | _ ->
              AutocompleteMod (ACSetVisible true) )
    | Deselected ->
      ( match m.currentPage with
      | Architecture ->
        ( match event.keyCode with
        | Key.Enter ->
            Entry.openOmnibox ()
        | Key.K ->
            if osCmdKeyHeld then Entry.openOmnibox () else NoChange
        | Key.A ->
            if event.ctrlKey then Viewport.pageLeft m else NoChange
        | Key.E ->
            if event.ctrlKey then Viewport.pageRight m else NoChange
        | Key.F ->
            if event.ctrlKey then Viewport.pageDown m else NoChange
        | Key.B ->
            if event.ctrlKey then Viewport.pageUp m else NoChange
        | Key.PageUp ->
            Viewport.pageUp m
        | Key.PageDown ->
            Viewport.pageDown m
        | Key.Up ->
            Viewport.moveUp m (* NB: see `stopKeys` in ui.html *)
        | Key.Down ->
            Viewport.moveDown m (* NB: see `stopKeys` in ui.html *)
        | Key.Left ->
            Viewport.moveLeft m
        | Key.Right ->
            Viewport.moveRight m
        | _ ->
            NoChange )
      | _ ->
          NoChange )
    | PanningCanvas _ ->
        NoChange
    | DraggingTL (_, _, _, _) ->
        NoChange
    | FluidEntering _ ->
        NoChange


let optionDefaultHandler (event : Keyboard.keyEvent) (m : model) :
    modification option =
  Some (defaultHandler event m)


(* process handlers until one has a result or we're done *)
(* this is sort of the opposite of >>= *)
(* NB: 'None' will allow subsequent handlers to run; 'Some NoChange' does not.
   (So if you wanted to _disable_ defaultHandler behavior for a given input,
   you could.) *)
let handler (event : Keyboard.keyEvent) (m : model) : modification =
  [optionDefaultHandler]
  |> List.foldl
       ~f:(fun h (acc : modification option) ->
         match acc with None -> h event m | Some _ -> acc)
       ~init:None
  |> Option.withDefault ~default:NoChange
