open Prelude

(* Dark *)
module AC = Autocomplete
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Regex = Util.Regex
module Dom = Webapi.Dom

let openOmnibox ?(openAt : pos option = None) () : modification =
  Enter (Creating openAt)


(* --------------------- *)
(* Focus *)
(* --------------------- *)
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


(* selection *)
type range =
  < setStart : Web_node.t -> int -> unit [@bs.meth]
  ; setEnd : Web_node.t -> int -> unit [@bs.meth]
  ; startContainer : Web_node.t [@bs.get] >
  Js.t

external createRange : unit -> range = "createRange"
  [@@bs.val] [@@bs.scope "document"]

type selection =
  < toString : unit -> string [@bs.meth]
  ; removeAllRanges : unit -> unit [@bs.meth]
  ; addRange : range -> unit [@bs.meth]
  ; anchorOffset : int [@bs.get]
  ; focusOffset : int [@bs.get]
  ; anchorNode : Web_node.t [@bs.get]
  ; getRangeAt : int -> range [@bs.meth] >
  Js.t

(** [findFirstAncestorWithClass className node] returns the first ancestor of
  * [node] (including self) that has a class of [className] *)
let rec findFirstAncestorWithClass (className : string) (node : Dom.Node.t) :
    Dom.Node.t option =
  Dom.Element.ofNode node
  |> Option.andThen ~f:(fun el ->
         if el |> Dom.Element.classList |> Dom.DomTokenList.contains className
         then Some node
         else None)
  |> Option.orElseLazy (fun _ ->
         node
         |> Dom.Node.parentNode
         |> Option.andThen ~f:(findFirstAncestorWithClass className))


(** [preorderWalkUntil f node] recurses through all children of [node], calling
  * [f] with each Node before recursing. If [f] returns false, the walk stops. *)
let preorderWalkUntil ~(f : Dom.Node.t -> bool) (node : Dom.Node.t) : unit =
  let module Node = Dom.Node in
  let rec walk ~(f : Node.t -> bool) (n : Node.t) : bool =
    if f n
    then
      let continue =
        Node.firstChild n
        |> Option.map ~f:(walk ~f)
        |> Option.withDefault ~default:true
      in
      if continue
      then
        Node.nextSibling n
        |> Option.map ~f:(walk ~f)
        |> Option.withDefault ~default:true
      else false
    else false
  in
  Node.firstChild node
  |> Option.map ~f:(walk ~f)
  |> recoverOption "could not find child element"
  |> ignore


(** getFluidSelectionRange returns the [begin, end] indices of the last
  * selection/caret placement within a fluid editor.
  *
  * If there has been no selection/caret placement or if the selected nodes are
  * not part of a fluid editor, returns None.
  *
  * [begin] may be greater than, less than, or equal to [end], depending on the
  * selection direction. If there is no selection but rather a caret positior,
  * then [begin] == [end].
  *
  * This function works by first finding the fluid-editor div that the
  * selection starts in, then iterating through all it's children nodes until
  * it finds both the selection start and finish nodes (anchor and focus,
  * respectively). Each non-matching node that is passed by increments a
  * cursor, which is used to calculate the absolute 0-based index from the
  * beginning of the editor. *)
let getFluidSelectionRange () : (int * int) option =
  let module Node = Dom.Node in
  let module Window = Dom.Window in
  let module Selection = Dom.Selection in
  let sel = Dom.window |> Window.getSelection in
  Option.andThen2
    (Selection.anchorNode sel)
    (Selection.focusNode sel)
    ~f:(fun anchorNode focusNode ->
      findFirstAncestorWithClass "fluid-editor" anchorNode
      |> recoverOption "could not find fluid-editor"
      |> Option.andThen ~f:(fun editor ->
             let cursor = ref 0 in
             let anchorIdx, focusIdx = (ref None, ref None) in
             preorderWalkUntil editor ~f:(fun node ->
                 if Node.isSameNode anchorNode node
                 then anchorIdx := Some !cursor ;
                 if Node.isSameNode focusNode node then focusIdx := Some !cursor ;
                 (* If node is not a leaf, then advance cursor. This is
                  * probably a span or other container element. We'll see the
                  * actual text node later, and we don't want to double-count
                  * the textContent. *)
                 if not (Node.firstChild node |> Option.is_some)
                 then
                   cursor :=
                     !cursor + (node |> Node.textContent |> String.length) ;
                 let have_both =
                   Option.pair !anchorIdx !focusIdx |> Option.is_some
                 in
                 not have_both) ;
             let anchorOffset = sel |> Selection.anchorOffset in
             let focusOffset = sel |> Selection.focusOffset in
             Option.map2 !anchorIdx !focusIdx ~f:(fun anchor focus ->
                 (anchor + anchorOffset, focus + focusOffset))))


let getFluidCaretPos () : int option =
  match getFluidSelectionRange () with
  | Some (selStart, selEnd) ->
      if selStart = selEnd
      then Some selEnd
      else
        (* Should we change this to return None in selection cases? *)
        Some selEnd
  | None ->
      None


(** setFluidSelectionRange([beginIdx, endIdx]) attempts to select the passed
  * region in the currently selected fluid editor, if there is one.
  * If beginIdx == endIdx, it sets the caret position (0-width selection).
  *
  * This function assumes we never want to place the selection within a
  * nested DOM node (it crawls siblings).
  *
  * See getFluidSelectionRange for the counterpart. Note that it is not
  * strictly symmetrical with it, so there might be future edge-cases. *)
let setFluidSelectionRange (beginIdx : int) (endIdx : int) : unit =
  let module Node = Dom.Node in
  let module Element = Dom.Element in
  let module Window = Dom.Window in
  let module Document = Dom.Document in
  let module Selection = Dom.Selection in
  let module NodeList = Dom.NodeList in
  let clamp (min : int) (max : int) (n : int) =
    if n < min then min else if n > max then max else n
  in
  Dom.document
  |> Document.querySelector ".selected #active-editor"
  |> recoverOption
       "setFluidSelectionRange querySelector failed to find #active-editor"
  |> Option.andThen ~f:(fun editor ->
         let maxChars = editor |> Element.textContent |> String.length in
         let anchorBound = beginIdx |> clamp 0 maxChars in
         let focusBound = endIdx |> clamp 0 maxChars in
         let findNodeAndOffset (bound : int) : Node.t option * int =
           let offset = ref bound in
           Element.childNodes editor
           |> NodeList.toArray
           |> Array.find ~f:(fun child ->
                  let nodeLen = child |> Node.textContent |> String.length in
                  if !offset <= nodeLen
                  then true
                  else (
                    offset := !offset - nodeLen ;
                    false ))
           |> Option.andThen ~f:Node.firstChild
           |> fun n -> (n, !offset)
         in
         let maybeAnchor, anchorOffset = findNodeAndOffset anchorBound in
         let maybeFocus, focusOffset = findNodeAndOffset focusBound in
         Option.map2 maybeAnchor maybeFocus ~f:(fun anchorNode focusNode ->
             Dom.window
             |> Window.getSelection
             |> Selection.setBaseAndExtent
                  anchorNode
                  anchorOffset
                  focusNode
                  focusOffset)
         |> recoverOption
              ~debug:(maybeAnchor, maybeFocus)
              "setFluidSelectionRange failed to find selection nodes")
  |> ignore


let setFluidCaret (idx : int) : unit = setFluidSelectionRange idx idx

type browserPlatform =
  | Mac
  | Linux
  | Windows
  | UnknownPlatform

external jsValidateEmail : string -> bool = "validateEmail"
  [@@bs.val] [@@bs.scope "window"]

let validateEmail (email : string) : bool = jsValidateEmail email

external jsGetBrowserPlatform : unit -> browserPlatform Js.Nullable.t
  = "getBrowserPlatform"
  [@@bs.val] [@@bs.scope "window"]

let getBrowserPlatform () : browserPlatform =
  jsGetBrowserPlatform ()
  |> Js.Nullable.toOption
  |> Option.withDefault ~default:UnknownPlatform


external jsSendSegmentMessage : string -> unit = "sendSegmentMessage"
  [@@bs.val] [@@bs.scope "window"]

let string_of_segment_track (e : segmentTrack) : string =
  match e with
  | WelcomeModal ->
      "Welcome Modal"
  | OpenDocs ->
      "Open docs"
  | InviteUser ->
      "User shared dark"


let sendSegmentMessage (event : segmentTrack) : unit =
  string_of_segment_track event |> jsSendSegmentMessage |> ignore


external jsUnsupportedBrowser : unit -> bool Js.Nullable.t
  = "unsupportedBrowser"
  [@@bs.val] [@@bs.scope "window"]

let unsupportedBrowser () : bool =
  jsUnsupportedBrowser ()
  |> Js.Nullable.toOption
  |> Option.withDefault ~default:false


let focusEntry (m : model) : msg Tea.Cmd.t =
  match unwrapCursorState m.cursorState with
  | Entering _ ->
      Tea_html_cmds.focus Defaults.entryID
  | _ ->
      Tea.Cmd.none


let focusEntryWithOffset (m : model) (offset : int) : msg Tea.Cmd.t =
  match unwrapCursorState m.cursorState with
  | Entering _ ->
      focusWithOffset Defaults.entryID offset
  | _ ->
      Tea.Cmd.none


let newHandler m space name modifier pos =
  let tlid = gtlid () in
  let spaceid = gid () in
  let handler =
    { ast = EBlank (gid ())
    ; spec =
        { space = F (spaceid, space)
        ; name = B.ofOption name
        ; modifier = B.ofOption modifier }
    ; hTLID = tlid
    ; pos }
  in
  let idToEnter =
    (* TL.getNextBlank requires that there be a tl in the model to operate on;
     * here, we're setting an ID to focus before the model is updated, so we
     * generate our list of blankOrDatas here *)
    (* Fallback to ast if spec has no blanks *)
    handler.spec
    |> SpecHeaders.firstBlank
    |> Option.withDefault ~default:(handler.ast |> FluidExpression.toID)
  in
  let fluidMods =
    let s = m.fluidState in
    let newS = {s with newPos = 0} in
    let cursorState =
      if idToEnter = (handler.ast |> FluidExpression.toID)
      then FluidEntering tlid
      else Entering (Filling (tlid, idToEnter))
    in
    [ TweakModel (fun m -> {m with fluidState = newS})
    ; SetCursorState cursorState ]
  in
  let pageChanges = [SetPage (FocusedHandler (tlid, true))] in
  let rpc =
    AddOps ([SetHandler (tlid, pos, handler)], FocusNext (tlid, Some spaceid))
  in
  Many (rpc :: (pageChanges @ fluidMods))


let newDB (name : string) (pos : pos) : modification =
  let next = gid () in
  let tlid = gtlid () in
  let pageChanges = [SetPage (FocusedDB (tlid, true))] in
  let rpcCalls =
    [ CreateDBWithBlankOr (tlid, pos, Prelude.gid (), name)
    ; AddDBCol (tlid, next, Prelude.gid ()) ]
  in
  (* This is not _strictly_ correct, as there's no guarantee that the new DB
   * doesn't share a name with an old DB in a weird state that still has
   * data in the user_data table. But it's 99.999% correct, which of course
   * is the best type of correct *)
  Many
    ( AppendUnlockedDBs (StrSet.fromList [deTLID tlid])
    :: AddOps (rpcCalls, FocusExact (tlid, next))
    :: pageChanges )


let submitOmniAction (m : model) (pos : pos) (action : omniAction) :
    modification =
  let pos = {x = pos.x - 17; y = pos.y - 70} in
  let unused = Some "_" in
  match action with
  | NewDB maybeName ->
      let name =
        match maybeName with Some n -> n | None -> DB.generateDBName ()
      in
      newDB name pos
  | NewFunction name ->
      let blankfn = Refactor.generateEmptyFunction () in
      let newfn =
        match name with
        | Some n ->
            let metadata = blankfn.ufMetadata in
            let newMetadata = {metadata with ufmName = F (gid (), n)} in
            {blankfn with ufMetadata = newMetadata}
        | None ->
            blankfn
      in
      Many
        [ AddOps ([SetFunction newfn], FocusNothing)
        ; MakeCmd (Url.navigateTo (FocusedFn newfn.ufTLID)) ]
  | NewHTTPHandler route ->
      newHandler m "HTTP" route None pos
  | NewWorkerHandler name ->
      newHandler m "WORKER" name unused pos
  | NewCronHandler name ->
      newHandler m "CRON" name None pos
  | NewReplHandler name ->
      (* When creating a repl, dont ask the user for a name *)
      let name =
        Option.withDefault
          name
          ~default:(Util.Namer.generateAnimalWithPersonality ~space:"REPL" ())
      in
      newHandler m "REPL" (Some name) unused pos
  | NewGroup name ->
      Groups.createEmptyGroup name pos
  | Goto (page, tlid, _, _) ->
      Many [SetPage page; Select (tlid, STTopLevelRoot)]


type nextMove =
  | StartThread
  | StayHere
  | GotoNext

let validate (tl : toplevel) (pd : blankOrData) (value : string) : string option
    =
  let v pattern name =
    if Regex.exactly ~re:pattern value
    then None
    else Some (name ^ " must match /" ^ pattern ^ "/")
  in
  match pd with
  | PDBName _ ->
      v AC.dbNameValidator "DB name"
  | PDBColType _ ->
      v AC.dbColTypeValidator "DB type"
  | PDBColName _ ->
      if value = "id"
      then
        Some
          "The field name 'id' was reserved when IDs were implicit. We are transitioning to allowing it, but we're not there just yet. Sorry!"
      else v AC.dbColNameValidator "DB column name"
  | PEventName _ ->
      if TL.isHTTPHandler tl
      then
        (* Must satisfy both, else error *)
        v AC.httpNameValidator "route name"
        |> Option.or_else (AC.validateHttpNameValidVarnames value)
      else v AC.eventNameValidator "event name"
  | PEventModifier _ ->
      if TL.isHTTPHandler tl
      then v AC.httpVerbValidator "verb"
      else if TL.isCronHandler tl
      then AC.cronIntervalValidator value
      else v AC.eventModifierValidator "event modifier"
  | PEventSpace _ ->
      v AC.eventSpaceValidator "event space"
  | PFnName _ ->
      if String.startsWith ~prefix:"dark/" value
      then v AC.packageFnNameValidator "function name"
      else v AC.fnNameValidator "function name"
  | PParamName oldParam ->
      v AC.paramNameValidator "param name"
      |> Option.orElse (AC.validateFnParamNameFree tl oldParam value)
  | PParamTipe _ ->
      v AC.paramTypeValidator "param type"
  | PTypeName _ ->
      v AC.typeNameValidator "type name"
  | PTypeFieldName _ ->
      v AC.fieldNameValidator "type field name"
  | PTypeFieldTipe _ ->
      v AC.paramTypeValidator "type field type"
  | PGroupName _ ->
      v AC.groupNameValidator "group name"


let submitACItem
    (m : model)
    (cursor : entryCursor)
    (item : autocompleteItem)
    (move : nextMove) : modification =
  let stringValue = AC.getValue m.complete in
  match cursor with
  | Creating _ ->
      NoChange
  | Filling (tlid, id) ->
    ( match TL.getTLAndPD m tlid id with
    | Some (tl, Some pd) ->
      ( match validate tl pd stringValue with
      | Some error ->
          (* We submit when users click away from an input, but they might not have typed anything! We
           * don't want to adjust the validators to allow empty strings where they are not allowed, but we
           * also don't want to display an error when they were not responsible for it! *)
          if stringValue = "" then NoChange else DisplayError error
      | None ->
          let wrap ops next =
            let wasEditing = P.isBlank pd |> not in
            let focus =
              if wasEditing && move = StayHere
              then
                match next with
                | None ->
                    FocusSame
                | Some nextID ->
                    FocusExact (tlid, nextID)
              else FocusNext (tlid, next)
            in
            AddOps (ops, focus)
          in
          let wrapID ops = wrap ops (Some id) in
          let wrapNew ops new_ = wrap ops (Some (P.toID new_)) in
          let save newtl next =
            if newtl = tl
            then NoChange
            else
              match newtl with
              | TLHandler h ->
                  wrapNew [SetHandler (tlid, h.pos, h)] next
              | TLFunc f ->
                  wrapNew [SetFunction f] next
              | TLTipe t ->
                  wrapNew [SetType t] next
              | TLGroup g ->
                  AddGroup g
              | TLDB _ ->
                  recover "no vars in DBs" ~debug:tl NoChange
          in
          let saveH h next = save (TLHandler h) next in
          let replace new_ =
            tl |> TL.replace pd new_ |> fun tl_ -> save tl_ new_
          in
          ( match (pd, item, tl) with
          | PDBName (F (id, oldName)), ACDBName value, TLDB _ ->
              if AC.assertValid AC.dbNameValidator value <> value
              then
                DisplayError
                  ("DB name must match " ^ AC.dbNameValidator ^ " pattern")
              else if oldName = value (* leave as is *)
              then
                (* TODO(JULIAN): I think this should actually be STCaret with a target indicating the end of the ac item? *)
                Select (tlid, STID id)
              else if List.member ~value (TL.allDBNames m.dbs)
              then DisplayError ("There is already a DB named " ^ value)
              else
                let varrefs = Refactor.renameDBReferences m oldName value in
                AddOps (RenameDBname (tlid, value) :: varrefs, FocusNothing)
          | PDBColType ct, ACDBColType value, TLDB db ->
              if B.toOption ct = Some value
              then
                (* TODO(JULIAN): I think this should actually be STCaret with a target indicating the end of the ac item? *)
                Select (tlid, STID id)
              else if DB.isMigrationCol db id
              then
                wrapID
                  [ SetDBColTypeInDBMigration (tlid, id, value)
                  ; AddDBColToDBMigration (tlid, gid (), gid ()) ]
              else if B.isBlank ct
              then
                wrapID
                  [ SetDBColType (tlid, id, value)
                  ; AddDBCol (tlid, gid (), gid ()) ]
              else wrapID [ChangeDBColType (tlid, id, value)]
          | PDBColName cn, ACDBColName value, TLDB db ->
              if B.toOption cn = Some value
              then
                (* TODO(JULIAN): I think this should actually be STCaret with a target indicating the end of the ac item? *)
                Select (tlid, STID id)
              else if DB.isMigrationCol db id
              then wrapID [SetDBColNameInDBMigration (tlid, id, value)]
              else if DB.hasCol db value
              then
                DisplayError
                  ("Can't have two DB fields with the same name: " ^ value)
              else if B.isBlank cn
              then wrapID [SetDBColName (tlid, id, value)]
              else wrapID [ChangeDBColName (tlid, id, value)]
          | PEventName _, ACCronName value, _
          | PEventName _, ACReplName value, _
          | PEventName _, ACWorkerName value, _ ->
              replace (PEventName (B.newF value))
          | PEventName _, ACHTTPRoute value, TLHandler h ->
              (* Check if the ACHTTPRoute value is a 404 path *)
              let f404s =
                m.f404s |> List.find ~f:(fun f404 -> f404.path = value)
              in
              ( match f404s with
              | Some f404 ->
                  let new_ = B.newF value in
                  let modifier =
                    if B.isBlank h.spec.modifier
                    then B.newF f404.modifier
                    else h.spec.modifier
                  in
                  let specInfo : handlerSpec =
                    {space = h.spec.space; name = B.newF f404.path; modifier}
                  in
                  (* We do not delete the 404 on the server because the list of 404s is  *)
                  (* generated by filtering through the unused HTTP handlers *)
                  Many
                    [ saveH {h with spec = specInfo} (PEventName new_)
                    ; Delete404 f404 ]
              | None ->
                  replace (PEventName (B.newF value)) )
          (* allow arbitrary HTTP modifiers *)
          | PEventModifier _, ACHTTPModifier value, _
          | PEventModifier _, ACCronTiming value, _
          | PEventModifier _, ACEventModifier value, _ ->
              replace (PEventModifier (B.newF value))
          (* allow arbitrary eventspaces *)
          | PEventSpace space, ACEventSpace value, TLHandler h ->
              let new_ = B.newF value in
              let replacement = SpecHeaders.replaceEventSpace id new_ h.spec in
              let replacedModifier =
                match (replacement.space, space) with
                | F (_, newSpace), F (_, oldSpace) when newSpace == oldSpace ->
                    replacement
                (*
                 * If becoming a WORKER or REPL, set modifier to "_" as it's invalid otherwise *)
                | F (_, "REPL"), _ | F (_, "WORKER"), _ ->
                    SpecHeaders.replaceEventModifier
                      (B.toID h.spec.modifier)
                      (B.newF "_")
                      replacement
                (*
                 * Remove modifier when switching between any other types *)
                | _, _ ->
                    SpecHeaders.replaceEventModifier
                      (B.toID h.spec.modifier)
                      (B.new_ ())
                      replacement
              in
              let replacedName =
                match (replacedModifier.space, h.spec.name) with
                (*
                 * If from a REPL, drop repl_ prefix and lowercase *)
                | F (_, newSpace), F (_, name)
                  when newSpace <> "REPL"
                       && String.startsWith
                            ~prefix:"repl_"
                            (String.toLower name) ->
                    SpecHeaders.replaceEventName
                      (B.toID h.spec.name)
                      (B.new_ ())
                      replacedModifier
                (*
                 * If from an HTTP, strip leading slash and any colons *)
                | F (_, newSpace), F (_, name)
                  when newSpace <> "HTTP" && String.startsWith ~prefix:"/" name
                  ->
                    SpecHeaders.replaceEventName
                      (B.toID h.spec.name)
                      (B.newF
                         ( String.dropLeft ~count:1 name
                         |> String.split ~on:":"
                         |> String.join ~sep:"" ))
                      replacedModifier
                (*
                 * If becoming an HTTP, add a slash at beginning *)
                | F (_, "HTTP"), F (_, name)
                  when not (String.startsWith ~prefix:"/" name) ->
                    SpecHeaders.replaceEventName
                      (B.toID h.spec.name)
                      (B.newF ("/" ^ name))
                      replacedModifier
                | _, _ ->
                    replacedModifier
              in
              saveH {h with spec = replacedName} (PEventSpace new_)
          | PFnName _, ACFnName value, TLFunc old ->
              if B.isFilledValue old.ufMetadata.ufmName value
              then NoChange
              else if List.member
                        ~value
                        (UserFunctions.allNames m.userFunctions)
              then DisplayError ("There is already a Function named " ^ value)
              else
                let newPD = PFnName (B.newF value) in
                let new_ =
                  { old with
                    ufMetadata = {old.ufMetadata with ufmName = B.newF value} }
                in
                let changedNames = Refactor.renameFunction m old value in
                wrapNew (SetFunction new_ :: changedNames) newPD
          | PParamName _, ACParamName value, _ ->
              replace (PParamName (B.newF value))
          | PParamTipe _, ACParamTipe tipe, _ ->
              replace (PParamTipe (B.newF tipe))
          | PTypeName _, ACTypeName value, TLTipe old ->
              if List.member ~value (UserTypes.allNames m.userTipes)
              then DisplayError ("There is already a Type named " ^ value)
              else
                let newPD = PTypeName (B.newF value) in
                let new_ = UserTypes.replace pd newPD old in
                let changedNames = Refactor.renameUserTipe m old new_ in
                wrapNew (SetType new_ :: changedNames) newPD
          | PTypeFieldName _, ACTypeFieldName value, _ ->
              replace (PTypeFieldName (B.newF value))
          | PTypeFieldTipe _, ACTypeFieldTipe tipe, _ ->
              replace (PTypeFieldTipe (B.newF tipe))
          | PGroupName _, ACGroupName name, _ ->
              replace (PGroupName (B.newF name))
          | pd, item, _ ->
              DisplayAndReportError
                ( "Invalid autocomplete option"
                , None
                , Some
                    ( Types.show_blankOrData pd
                    ^ ", "
                    ^ Types.show_autocompleteItem item ) ) ) )
    | _ ->
        recover "Missing tl/pd" ~debug:cursor NoChange )


let submit (m : model) (cursor : entryCursor) (move : nextMove) : modification =
  match cursor with
  | Creating p ->
      let pos =
        match p with Some pos -> pos | None -> Viewport.findNewPos m
      in
      ( match AC.highlighted m.complete with
      | Some (ACOmniAction act) ->
          submitOmniAction m pos act
      (* If empty, create an empty handler *)
      | None when m.complete.value = "" ->
          submitOmniAction m pos (NewReplHandler None)
      | _ ->
          NoChange )
  | Filling _ ->
    ( match AC.highlighted m.complete with
    | Some (ACOmniAction _) ->
        recover "Shouldnt allow omniactions here" ~debug:cursor NoChange
    | Some item ->
        submitACItem m cursor item move
    | None ->
        (* We removed ACExtra to define more specific autocomplete items.*)
        (* These are all autocomplete items who's target accepts and handles a free form value *)
        let item =
          let value = m.complete.value in
          match m.complete.target with
          | Some (_, p) ->
            ( match P.typeOf p with
            | DBColName ->
                Some (ACDBColName value)
            | FnName ->
                Some (ACFnName value)
            | ParamName ->
                Some (ACParamName value)
            | TypeName ->
                Some (ACTypeName value)
            | TypeFieldName ->
                Some (ACTypeFieldName value)
            | GroupName ->
                Some (ACGroupName value)
            | EventModifier ->
                (* Does not accept freeform inputs, but goes to validation call for more specific error message displayed to user *)
                Some (ACEventModifier value)
            | _ ->
                None )
          | None ->
              None
        in
        ( match item with
        | Some acItem ->
            submitACItem m cursor acItem move
        | None ->
            (* There's no good error message when the user submits an empty string, but just not doing anything
             * shows that it's not a valid input *)
            if m.complete.value = ""
            then NoChange
            else DisplayError "Invalid input" ) )


(* Submit, but don't move the cursor
 *
 * This was added to to cleanly express "commit the state of an input box when I click away",
 * but is more generally intended to express "commit the state and I'll handle the cursor"
 * *)
let commit (m : model) (cursor : entryCursor) = submit m cursor StayHere
