open Prelude

// Tea
module Cmd = Tea.Cmd
module Http = Tea.Http

// Dark
module AC = Autocomplete
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Key = Keyboard

type modification = AppTypes.modification
type model = AppTypes.model
module Mod = AppTypes.Modification
module Msg = AppTypes.Msg

let undo_redo = (m: AppTypes.model, redo: bool): modification =>
  switch CursorState.tlidOf(m.cursorState) {
  | Some(tlid) =>
    let undo = if redo {
      Mod.AddOps(list{RedoTL(tlid)}, FocusSame)
    } else {
      AddOps(list{UndoTL(tlid)}, FocusSame)
    }

    switch TL.get(m, tlid) |> Option.andThen(~f=TL.asDB) {
    | Some(_) =>
      /* We could do it on the server but it's really hard
                 atm. To do it on the server, efficiently, we'd create
                 a canvas with almost all the ops, check if the tlid
                 is a DB, then recreate the canvas with all the ops
                 (such that preprocess with the DB works). That way we
                 load from disk/db once, but still check server side.
 */
      if DB.isLocked(m, tlid) {
        Model.updateErrorMod(Error.set("Cannot undo/redo in locked DBs"))
      } else {
        undo
      }
    | None => undo
    }
  | None => NoChange
  }

let openOmnibox = (m: AppTypes.model): modification =>
  switch m.currentPage {
  | Architecture | FocusedHandler(_) | FocusedDB(_) => Many(list{Deselect, Entry.openOmnibox()})
  | FocusedFn(_) | FocusedType(_) | FocusedPackageManagerFn(_) => Entry.openOmnibox()
  | SettingsModal(_) =>
    // Shouldnt be able to open the omnibox from the settings modal
    NoChange
  }

let defaultHandler = (event: Keyboard.keyEvent, m: AppTypes.model): modification => {
  let isFluidState = switch m.cursorState {
  | FluidEntering(_) => true
  | _ => false
  }

  let isSettingViewFocused = m.settingsView.opened
  let isMac = Entry.getBrowserPlatform() == Mac
  let osCmdKeyHeld = if isMac {
    event.metaKey
  } else {
    event.ctrlKey
  }
  if isFluidState || isSettingViewFocused {
    NoChange
  } else if osCmdKeyHeld && event.keyCode == Key.Z {
    undo_redo(m, event.shiftKey)
  } else if (
    /* Note: CTRL+Y is Windows redo
      but CMD+Y on Mac is the history shortcut in Chrome (since CMD+H is taken for hide)
      See https://support.google.com/chrome/answer/157179?hl=en */
    !isMac && (event.ctrlKey && event.keyCode == Key.Y)
  ) {
    undo_redo(m, true)
  } else {
    switch m.cursorState {
    | Selecting(tlid, mId) =>
      switch (event.keyCode, TL.get(m, tlid)) {
      | (Key.Escape, _) =>
        switch mId {
        /* if we're selecting an expression,
         go 'up' to selecting the toplevel only */
        | Some(_) => Select(tlid, STTopLevelRoot)
        // if we're selecting a toplevel only, deselect.
        | None => Deselect
        }
      | (Key.Enter, Some(TLTipe(t) as tl)) if event.shiftKey =>
        switch mId {
        | Some(id) =>
          switch TL.find(tl, id) {
          | Some(PTypeName(_))
          | Some(PTypeFieldName(_))
          | Some(PTypeFieldTipe(_)) =>
            let replacement = UserTypes.extend(t)
            AddOps(list{SetType(replacement)}, FocusNext(tlid, Some(id)))
          | _ => NoChange
          }
        | None => NoChange
        }
      | (Key.Enter, Some(TLDB(_))) if event.shiftKey =>
        let blankid = gid()
        AddOps(list{AddDBCol(tlid, blankid, gid())}, FocusExact(tlid, blankid))
      | (Key.Enter, Some(TLFunc(f) as tl)) if event.shiftKey =>
        switch mId {
        | Some(id) =>
          switch TL.find(tl, id) {
          | Some(PParamTipe(_)) | Some(PParamName(_)) | Some(PFnName(_)) =>
            Refactor.addFunctionParameter(m, f, id)
          | _ => NoChange
          }
        | _ => NoChange
        }
      | (Key.Enter, Some(_)) if event.shiftKey => NoChange
      | (Key.Enter, Some(_)) if !event.shiftKey =>
        switch mId {
        | Some(id) => Selection.enter(m, tlid, id)
        | None => NoChange
        }
      | (Key.Tab, _) =>
        // NB: see `stopKeys` in ui.html
        switch mId {
        | Some(id) =>
          if event.shiftKey {
            Selection.enterPrevBlank(m, tlid, id)
          } else {
            Selection.enterNextBlank(m, tlid, id)
          }
        | None => NoChange
        }
      | (Key.O, Some(_)) =>
        if event.altKey {
          CenterCanvasOn(tlid)
        } else {
          NoChange
        }
      | (Key.K, Some(_)) =>
        if osCmdKeyHeld {
          openOmnibox(m)
        } else {
          NoChange
        }
      | _ => NoChange
      }
    | Entering(tlid, id) =>
      if event.ctrlKey {
        switch event.keyCode {
        | Key.P => AutocompleteMod(ACSelectUp)
        | Key.N => AutocompleteMod(ACSelectDown)
        | _ => NoChange
        }
      } else {
        switch event.keyCode {
        | Key.Enter => Entry.submit(m, tlid, id, Entry.StayHere)
        | Key.Tab =>
          let content = AC.getValue(m.complete)
          let hasContent = content != ""
          if event.shiftKey {
            if hasContent {
              NoChange
            } else {
              Selection.enterPrevBlank(m, tlid, id)
            }
          } else if hasContent {
            Entry.submit(m, tlid, id, Entry.GotoNext)
          } else {
            Selection.enterNextBlank(m, tlid, id)
          }
        | Key.Unknown(_) => NoChange
        | Key.Escape => Many(list{Select(tlid, STID(id)), AutocompleteMod(ACReset)})
        | Key.Up => AutocompleteMod(ACSelectUp) // NB: see `stopKeys` in ui.html
        | Key.Down => AutocompleteMod(ACSelectDown) // NB: see `stopKeys` in ui.html
        | Key.Backspace =>
          /* This was an old hack for strings in the AST of the old editor.
           * Unclear if it still is needed. */
          let v = m.complete.value
          Many(list{
            AutocompleteMod(ACSetVisible(true)),
            AutocompleteMod(ACSetQuery(v)),
            AutocompleteMod(ACSetVisible(true)),
            MakeCmd(CursorState.focusEntry(m)),
          })
        | _ => AutocompleteMod(ACSetVisible(true))
        }
      }
    | Omnibox(pos) =>
      if event.ctrlKey {
        switch event.keyCode {
        | Key.P => AutocompleteMod(ACSelectUp)
        | Key.N => AutocompleteMod(ACSelectDown)
        | _ => NoChange
        }
      } else {
        switch event.keyCode {
        | Key.Spacebar =>
          if AC.isOmnibox(m.complete) {
            AutocompleteMod(ACSetQuery(m.complete.value ++ " "))
          } else {
            NoChange
          }
        | Key.Enter =>
          let pos = Option.unwrap(~default=Viewport.findNewPos(m), pos)
          switch AC.highlighted(m.complete) {
          | Some(ACOmniAction(act)) => Entry.submitOmniAction(m, pos, act)
          // If empty, create an empty handler
          | None if m.complete.value == "" => Entry.submitOmniAction(m, pos, NewReplHandler(None))
          | _ => NoChange
          }
        | Key.Escape => Many(list{Deselect, AutocompleteMod(ACReset)})
        | Key.Up => AutocompleteMod(ACSelectUp) // NB: see `stopKeys` in ui.html
        | Key.Down => AutocompleteMod(ACSelectDown) // NB: see `stopKeys` in ui.html
        | Key.Backspace =>
          /* This was an old hack for strings in the AST of the old editor.
           * Unclear if it still is needed. */
          let v = m.complete.value
          Many(list{
            AutocompleteMod(ACSetVisible(true)),
            AutocompleteMod(ACSetQuery(v)),
            AutocompleteMod(ACSetVisible(true)),
            MakeCmd(CursorState.focusEntry(m)),
          })
        | _ => AutocompleteMod(ACSetVisible(true))
        }
      }
    | Deselected =>
      switch m.currentPage {
      | Architecture =>
        switch event.keyCode {
        | Key.Enter => Entry.openOmnibox()
        | Key.K =>
          if osCmdKeyHeld {
            Entry.openOmnibox()
          } else {
            NoChange
          }
        | Key.A =>
          if event.ctrlKey {
            Viewport.pageLeft(m)
          } else {
            NoChange
          }
        | Key.E =>
          if event.ctrlKey {
            Viewport.pageRight(m)
          } else {
            NoChange
          }
        | Key.F =>
          if event.ctrlKey {
            Viewport.pageDown(m)
          } else {
            NoChange
          }
        | Key.B =>
          if event.ctrlKey {
            Viewport.pageUp(m)
          } else {
            NoChange
          }
        | Key.PageUp => Viewport.pageUp(m)
        | Key.PageDown => Viewport.pageDown(m)
        | Key.Up => Viewport.moveUp(m) // NB: see `stopKeys` in ui.html
        | Key.Down => Viewport.moveDown(m) // NB: see `stopKeys` in ui.html
        | Key.Left => Viewport.moveLeft(m)
        | Key.Right => Viewport.moveRight(m)
        | _ => NoChange
        }
      | _ => NoChange
      }
    | PanningCanvas(_) => NoChange
    | DraggingTL(_, _, _, _) => NoChange
    | FluidEntering(_) => NoChange
    }
  }
}

let optionDefaultHandler = (event: Keyboard.keyEvent, m: AppTypes.model): option<
  modification,
> => Some(defaultHandler(event, m))

// process handlers until one has a result or we're done
// this is sort of the opposite of >>=
/* NB: 'None' will allow subsequent handlers to run; 'Some NoChange' does not.
   (So if you wanted to _disable_ defaultHandler behavior for a given input,
   you could.) */
let handler = (event: Keyboard.keyEvent, m: AppTypes.model): modification =>
  list{optionDefaultHandler} |> List.fold(~f=(acc: option<modification>, h) =>
    switch acc {
    | None => h(event, m)
    | Some(_) => acc
    }
  , ~initial=None) |> Option.unwrap(~default=Mod.NoChange)
