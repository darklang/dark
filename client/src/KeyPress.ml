open Tc
open Prelude
open Types

(* Tea *)
module Cmd = Tea.Cmd
module Http = Tea.Http

(* Dark *)
module AC = Autocomplete
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Key = Keyboard
module Regex = Util.Regex

(* Figure out from the string and the state whether this '.' means field
   access. *)
let isFieldAccessDot (m : model) (baseStr : string) : bool =
  (* We know from the fact that this function is called that there has
     been a '.' entered. However, it might not be in baseStr, so
     canonicalize it first. *)
  let str = Regex.replace ~re:(Regex.regex "\\.*$") ~repl:"" baseStr in
  let intOrString =
    String.startsWith ~prefix:"\"" str || Decoders.typeOfLiteral str = TInt
  in
  match m.cursorState with
  | Entering (Creating _) ->
      not intOrString
  | Entering (Filling (tlid, id)) ->
      TL.getPD m tlid id
      |> Option.map ~f:(fun pd ->
             (P.typeOf pd = Expr || P.typeOf pd = Field) && not intOrString )
      |> Option.withDefault ~default:false
  | _ ->
      false


let undo_redo (m : model) (redo : bool) : modification =
  match tlidOf m.cursorState with
  | Some tlid ->
      let undo =
        if redo
        then RPC ([RedoTL tlid], FocusSame)
        else RPC ([UndoTL tlid], FocusSame)
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
      Many [Deselect; Entry.openOmnibox m]
  | _ ->
      NoChange


let defaultHandler (event : Keyboard.keyEvent) (m : model) : modification =
  let osCmdKeyHeld =
    if Entry.getBrowserPlatform () = Mac then event.metaKey else event.ctrlKey
  in
  if osCmdKeyHeld && event.keyCode = Key.Z
  then undo_redo m event.shiftKey
  else if osCmdKeyHeld && event.keyCode = Key.Z
  then undo_redo m event.shiftKey
  else if osCmdKeyHeld && event.keyCode = Key.S
  then ShowSaveToast
  else
    match m.cursorState with
    | Selecting (tlid, mId) ->
      ( match (event.keyCode, TL.get m tlid) with
      | Key.Delete, _ ->
          Selection.delete m tlid mId
      | Key.Backspace, _ ->
          Selection.delete m tlid mId
      | Key.Escape, _ ->
        ( match mId with
        (* if we're selecting an expression,
                   go 'up' to selecting the toplevel only *)
        | Some _ ->
            Select (tlid, None)
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
              RPC ([SetType replacement], FocusNext (tlid, Some id))
          | _ ->
              NoChange )
        | None ->
            NoChange )
      | Key.Enter, Some (TLDB _) when event.shiftKey ->
          let blankid = gid () in
          RPC ([AddDBCol (tlid, blankid, gid ())], FocusExact (tlid, blankid))
      | Key.Enter, Some (TLHandler h as tl) when event.shiftKey ->
        ( match mId with
        | Some id ->
          ( match TL.find tl id with
          | Some (PExpr _) ->
              let blank = B.new_ () in
              let replacement = AST.addThreadBlank id blank h.ast in
              if h.ast = replacement
              then NoChange
              else
                RPC
                  ( [SetHandler (tlid, h.pos, {h with ast = replacement})]
                  , FocusExact (tlid, B.toID blank) )
          | Some (PVarBind _) ->
            ( match AST.findParentOfWithin_ id h.ast with
            | Some (F (_, Lambda (_, _))) ->
                let replacement = AST.addLambdaBlank id h.ast in
                RPC
                  ( [SetHandler (tlid, h.pos, {h with ast = replacement})]
                  , FocusNext (tlid, Some id) )
            | _ ->
                NoChange )
          | Some (PKey _) ->
              let nextid, _, replacement =
                AST.addObjectLiteralBlanks id h.ast
              in
              RPC
                ( [SetHandler (tlid, h.pos, {h with ast = replacement})]
                , FocusExact (tlid, nextid) )
          | Some (PPattern _) ->
              let nextid, _, replacement = AST.addPatternBlanks id h.ast in
              RPC
                ( [SetHandler (tlid, h.pos, {h with ast = replacement})]
                , FocusExact (tlid, nextid) )
          | _ ->
              NoChange )
        | _ ->
            NoChange )
      | Key.Enter, Some (TLFunc f as tl) when event.shiftKey ->
        ( match mId with
        | Some id ->
          ( match TL.find tl id with
          | Some (PExpr _) ->
              let blank = B.new_ () in
              let replacement = AST.addThreadBlank id blank f.ufAST in
              if f.ufAST = replacement
              then NoChange
              else
                RPC
                  ( [SetFunction {f with ufAST = replacement}]
                  , FocusExact (tlid, B.toID blank) )
          | Some (PVarBind _) ->
            ( match AST.findParentOfWithin_ id f.ufAST with
            | Some (F (_, Lambda (_, _))) ->
                let replacement = AST.addLambdaBlank id f.ufAST in
                RPC
                  ( [SetFunction {f with ufAST = replacement}]
                  , FocusNext (tlid, Some id) )
            | _ ->
                NoChange )
          | Some (PKey _) ->
              let nextid, _, replacement =
                AST.addObjectLiteralBlanks id f.ufAST
              in
              RPC
                ( [SetFunction {f with ufAST = replacement}]
                , FocusExact (tlid, nextid) )
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
            Selection.selectDownLevel m tlid mId )
      | Key.Up, _ ->
          (* NB: see `stopKeys` in ui.html *)
          if event.shiftKey
          then Selection.selectUpLevel m tlid mId
          else Selection.moveUp m tlid mId
      | Key.Down, _ ->
          (* NB: see `stopKeys` in ui.html *)
          if event.shiftKey
          then Selection.selectDownLevel m tlid mId
          else Selection.moveDown m tlid mId
      | Key.Right, _ ->
          if event.altKey
          then Selection.moveCursorBackInTime m tlid
          else Selection.moveRight m tlid mId
      | Key.Left, _ ->
          if event.altKey
          then Selection.moveCursorForwardInTime m tlid
          else Selection.moveLeft m tlid mId
      | Key.Tab, _ ->
          (* NB: see `stopKeys` in ui.html *)
          if event.shiftKey
          then Selection.selectPrevBlank m tlid mId
          else Selection.selectNextBlank m tlid mId
      (* Disabled to make room for Windows keyboards *)
      (* | Key.O -> *)
      (*   if event.ctrlKey *)
      (*   then Selection.selectUpLevel m tlid mId *)
      (*   else NoChange *)
      (* |  Key.I -> *)
      (*   if event.ctrlKey *)
      (*   then Selection.selectDownLevel m tlid mId *)
      (*   else NoChange *)
      | Key.C, Some tl ->
          if event.ctrlKey && event.altKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.wrap WIfCond m tl)
            |> Option.withDefault ~default:NoChange
          else NoChange
      | Key.F, Some tl ->
          if event.ctrlKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.extractFunction m tl)
            |> Option.withDefault ~default:NoChange
          else if event.altKey
          then FeatureFlags.start m
          else NoChange
      | Key.B, Some tl ->
          if event.ctrlKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.wrap WLetBody m tl)
            |> Option.withDefault ~default:NoChange
          else NoChange
      | Key.L, Some tl ->
          if event.ctrlKey && event.shiftKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.extractVariable m tl)
            |> Option.withDefault ~default:NoChange
          else if event.ctrlKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.wrap WLetRHS m tl)
            |> Option.withDefault ~default:NoChange
          else NoChange
      | Key.I, Some tl ->
          if event.ctrlKey && event.altKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.wrap WIfElse m tl)
            |> Option.withDefault ~default:NoChange
          else if event.ctrlKey
          then
            mId
            |> Option.andThen ~f:(TL.find tl)
            |> Option.map ~f:(Refactor.wrap WIfThen m tl)
            |> Option.withDefault ~default:NoChange
          else NoChange
      | Key.E, Some tl ->
          if event.altKey
          then
            if event.shiftKey
            then
              mId
              |> Option.andThen ~f:(TL.find tl)
              |> Option.map ~f:(Refactor.takeOffRail m tl)
              |> Option.withDefault ~default:NoChange
            else
              mId
              |> Option.andThen ~f:(TL.find tl)
              |> Option.map ~f:(Refactor.putOnRail m tl)
              |> Option.withDefault ~default:NoChange
          else NoChange
      | Key.O, Some _ ->
          if event.altKey then CenterCanvasOn tlid else NoChange
      | Key.K, Some _ ->
          if osCmdKeyHeld then openOmnibox m else NoChange
      | Key.Unknown _, Some _ ->
        ( (* colon *)
        match mId with
        | None ->
            NoChange
        | Some id ->
            if event.key = Some ":"
            then
              Many
                [ SelectCommand (tlid, id)
                ; AutocompleteMod (ACSetVisible true)
                ; AutocompleteMod (ACSetQuery ":") ]
            else NoChange )
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
          | Key.Enter ->
              if AC.isSmallStringEntry m.complete
              then
                Many
                  [ AutocompleteMod (ACAppendQuery "\n")
                  ; MakeCmd (Entry.focusEntry m) ]
              else if AC.isLargeStringEntry m.complete
              then Entry.submit m cursor Entry.StayHere
              else NoChange
          | _ ->
              NoChange
        else if event.shiftKey && event.keyCode = Key.Enter
        then
          match cursor with
          | Filling (tlid, _) ->
              let tl = TL.get m tlid in
              ( match tl with
              | Some (TLHandler _) ->
                  Entry.submit m cursor Entry.StartThread
              | Some (TLFunc _) ->
                  Entry.submit m cursor Entry.StartThread
              | _ ->
                  NoChange )
          | Creating _ ->
              Entry.submit m cursor Entry.StartThread
        else if event.altKey
        then
          match event.keyCode with
          | Key.E ->
            ( match cursor with
            | Creating _ ->
                NoChange
            | Filling (tlid, id) ->
              ( match TL.getTLAndPD m tlid id with
              | Some (tl, Some pd) ->
                  if event.shiftKey
                  then Refactor.takeOffRail m tl pd
                  else Refactor.putOnRail m tl pd
              | _ ->
                  NoChange ) )
          | _ ->
              NoChange
        else (
          match event.keyCode with
          | Key.Spacebar ->
            ( match cursor with
            | Creating _ ->
                NoChange
            | _ ->
                if m.complete.value = "=" || AC.isStringEntry m.complete
                then NoChange
                else Entry.submit m cursor Entry.GotoNext )
          | Key.Enter ->
              if AC.isLargeStringEntry m.complete
              then AutocompleteMod (ACSetQuery m.complete.value)
              else Entry.submit m cursor Entry.StayHere
          | Key.Tab ->
            ( match cursor with
            | Filling (tlid, p) ->
                if AC.isLargeStringEntry m.complete
                then
                  match event.targetSelectionStart with
                  | Some idx ->
                      let newQ =
                        String.insertAt
                          ~insert:"\t"
                          ~index:(idx + 1)
                          m.complete.value
                      in
                      AutocompleteMod (ACSetQuery newQ)
                  | None ->
                      NoChange
                else
                  let content = AC.getValue m.complete in
                  let hasContent = content |> String.length |> ( < ) 0 in
                  if event.shiftKey
                  then
                    if hasContent
                    then NoChange
                    else Selection.enterPrevBlank m tlid (Some p)
                  else if hasContent
                  then Entry.submit m cursor Entry.GotoNext
                  else Selection.enterNextBlank m tlid (Some p)
            | Creating _ ->
                NoChange )
          | Key.Unknown _ ->
              if event.key = Some "."
                 && isFieldAccessDot m m.complete.value
                 && not (VariantTesting.isFluid m.tests)
              then
                let c = m.complete in
                (* big hack to for Entry.submit to see field access *)
                let newC = {c with value = AC.getValue c ^ "."; index = -1} in
                let newM = {m with complete = newC} in
                Entry.submit newM cursor Entry.GotoNext
              else NoChange
          | Key.Escape ->
            ( match cursor with
            | Creating _ ->
                Many [Deselect; AutocompleteMod ACReset]
            | Filling (tlid, p) ->
                let tl = TL.get m tlid in
                ( match tl with
                | Some (TLHandler h) ->
                    let replacement = AST.closeBlanks h.ast in
                    if replacement = h.ast
                    then Many [Select (tlid, Some p); AutocompleteMod ACReset]
                    else
                      (* TODO: in this case, when filling a keyname on an
                           * object, nothing happens which is unexpected *)
                      RPC
                        ( [SetHandler (tlid, h.pos, {h with ast = replacement})]
                        , FocusNext (tlid, None) )
                | _ ->
                    Many [Select (tlid, Some p); AutocompleteMod ACReset] ) )
          | Key.Up ->
              AutocompleteMod ACSelectUp (* NB: see `stopKeys` in ui.html *)
          | Key.Down ->
              AutocompleteMod ACSelectDown (* NB: see `stopKeys` in ui.html *)
          | Key.Backspace ->
              (* This was the case in Elm, unclear about bucklescript  *)
              (* NB: when we backspace, we _almost_ always get an *)
              (* EntryInputMsg first. I believe the only time we don't *)
              (* get one when we backspace over '""'. That means that *)
              (* we'll get \""' if the previous value was '"a"' (cause *)
              (* EntryInputMsg will have run, and m.c.v will already be *)
              (* set to the new value '""') or the previous value was *)
              (* '""' (in which case EntryInputMsg will not have run so *)
              (* m.c.v will not have changed. *)
              (* The way we can tell the difference is based on *)
              (* m.c.prevValue. If m.c.pv is '""' or longer, that means *)
              (* EntryInputMsg was run and we are coming from a longer *)
              (* string. *)
              let v =
                if m.complete.value = "\"\""
                   && String.length m.complete.prevValue <= 2
                then ""
                else m.complete.value
              in
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
            Entry.openOmnibox m
        | Key.K ->
            if osCmdKeyHeld then Entry.openOmnibox m else NoChange
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
        | Key.Zero ->
            Viewport.moveToOrigin
        | Key.Tab ->
            Selection.selectNextToplevel m None
            (* NB: see `stopKeys` in ui.html *)
        | _ ->
            NoChange )
      | _ ->
          NoChange )
    | SelectingCommand (tlid, id) ->
      ( match event.keyCode with
      | Key.Escape ->
          Commands.endCommandExecution tlid id
      | Key.Enter ->
          Commands.executeCommand m tlid id (AC.highlighted m.complete)
      | Key.P ->
          if event.ctrlKey then AutocompleteMod ACSelectUp else NoChange
      | Key.N ->
          if event.ctrlKey then AutocompleteMod ACSelectDown else NoChange
      | Key.Up ->
          AutocompleteMod ACSelectUp (* NB: see `stopKeys` in ui.html *)
      | Key.Down ->
          AutocompleteMod ACSelectDown (* NB: see `stopKeys` in ui.html *)
      | _ ->
          NoChange )
    | Dragging (_, _, _, _) ->
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
         match acc with None -> h event m | Some _ -> acc )
       ~init:None
  |> fun modification ->
  match modification with Some m -> m | None -> NoChange
