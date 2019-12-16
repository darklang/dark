open Tc
open Prelude
open Types

(* Dark *)
module AC = Autocomplete
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Regex = Util.Regex

let openOmnibox (vPos : vPos option) : modification =
  let vp =
    match vPos with
    | Some p ->
        p
    | None ->
        (* Let's try to get this closer to center top *)
        Viewport.centerTop
  in
  let pos = Viewport.toAbsolute vp in
  Enter (Creating pos)


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
      () )


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

external getSelection : unit -> selection = "getSelection"
  [@@bs.val] [@@bs.scope "window"]

external jsGetFluidSelectionRange :
  unit -> int array Js.Nullable.t
  = "getFluidSelectionRange"
  [@@bs.val] [@@bs.scope "window"]

external jsSetFluidSelectionRange :
  int array -> unit
  = "setFluidSelectionRange"
  [@@bs.val] [@@bs.scope "window"]

let getFluidSelectionRange () : (int * int) option =
  match Js.Nullable.toOption (jsGetFluidSelectionRange ()) with
  | Some [|beginIdx; endIdx|] ->
      Some (beginIdx, endIdx)
  | _ ->
      (* We know the array either has 2 values or is undefined *)
      None


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


let setFluidSelectionRange ((beginIdx, endIdx) : int * int) : unit =
  jsSetFluidSelectionRange [|beginIdx; endIdx|]


let setFluidCaret (idx : int) : unit = jsSetFluidSelectionRange [|idx; idx|]

external querySelector : string -> Web_node.t Js.Nullable.t = "querySelector"
  [@@bs.val] [@@bs.scope "document"]

type browserPlatform =
  | Mac
  | Linux
  | Windows
  | UnknownPlatform

external jsGetBrowserPlatform :
  unit -> browserPlatform Js.Nullable.t
  = "getBrowserPlatform"
  [@@bs.val] [@@bs.scope "window"]

let getBrowserPlatform () : browserPlatform =
  jsGetBrowserPlatform ()
  |> Js.Nullable.toOption
  |> Option.withDefault ~default:UnknownPlatform


let focusEntry (m : model) : msg Tea.Cmd.t =
  match unwrapCursorState m.cursorState with
  | Entering _ | SelectingCommand (_, _) ->
      Tea_html_cmds.focus Defaults.entryID
  | _ ->
      Tea.Cmd.none


let focusEntryWithOffset (m : model) (offset : int) : msg Tea.Cmd.t =
  match unwrapCursorState m.cursorState with
  | Entering _ | SelectingCommand (_, _) ->
      focusWithOffset Defaults.entryID offset
  | _ ->
      Tea.Cmd.none


let createFunction (fn : function_) : expr =
  let blanks count = List.initialize count (fun _ -> B.new_ ()) in
  let r =
    if List.member ~value:fn.fnReturnTipe Runtime.errorRailTypes
    then Rail
    else NoRail
  in
  let (ID id) = gid () in
  F
    ( ID id
    , FnCall
        ( F (ID (id ^ "_name"), fn.fnName)
        , blanks (List.length fn.fnParameters)
        , r ) )


let newHandler m space name modifier pos =
  let tlid =
    if VariantTesting.variantIsActive m GridLayout
    then gtlidDT ()
    else gtlid ()
  in
  let spaceid = gid () in
  let handler =
    { ast = B.new_ ()
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
     * generate our list of pointerDatas here *)
    (* Fallback to ast if spec has no blanks *)
    handler.spec
    |> SpecHeaders.firstBlank
    |> Option.withDefault ~default:(handler.ast |> Blank.toID)
  in
  let fluidMods =
    if VariantTesting.isFluid m.tests
    then
      let s = m.fluidState in
      let newS = {s with newPos = 0} in
      let cursorState =
        if idToEnter = (handler.ast |> Blank.toID)
        then FluidEntering tlid
        else Entering (Filling (tlid, idToEnter))
      in
      [ TweakModel (fun m -> {m with fluidState = newS})
      ; SetCursorState cursorState ]
    else []
  in
  let pageChanges =
    match m.currentPage with
    | FocusedFn _ | FocusedType _ ->
        [SetPage (FocusedHandler (tlid, true))]
    | _ ->
        []
  in
  let rpc =
    RPC ([SetHandler (tlid, pos, handler)], FocusNext (tlid, Some spaceid))
  in
  Many (rpc :: (pageChanges @ fluidMods))


let newDB (name : string) (pos : pos) (m : model) : modification =
  let next = gid () in
  let tlid =
    if List.member ~value:GridLayout m.tests then gtlidDT () else gtlid ()
  in
  let pageChanges =
    match m.currentPage with
    | FocusedFn _ | FocusedType _ ->
        [SetPage (FocusedDB (tlid, true))]
    | _ ->
        []
  in
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
    :: RPC (rpcCalls, FocusExact (tlid, next))
    :: pageChanges )


let submitOmniAction (m : model) (pos : pos) (action : omniAction) :
    modification =
  let pos = {x = pos.x; y = pos.y - 100} in
  let unused = Some "_" in
  match action with
  | NewDB maybeName ->
      let name =
        match maybeName with Some n -> n | None -> DB.generateDBName ()
      in
      newDB name pos m
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
        [ RPC ([SetFunction newfn], FocusNothing)
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

let parseAst
    (ast : expr)
    (complete : autocomplete)
    (item : autocompleteItem)
    (str : string) : expr option =
  let eid = gid () in
  let b1 = B.new_ () in
  let b2 = B.new_ () in
  let b3 = B.new_ () in
  match item with
  | ACConstructorName "Just" ->
      Some (F (eid, Constructor (B.newF "Just", [b1])))
  | ACConstructorName "Nothing" ->
      Some (F (eid, Constructor (B.newF "Nothing", [])))
  | ACConstructorName "Ok" ->
      Some (F (eid, Constructor (B.newF "Ok", [b1])))
  | ACConstructorName "Error" ->
      Some (F (eid, Constructor (B.newF "Error", [b1])))
  | ACKeyword KIf ->
      Some (F (eid, If (b1, b2, b3)))
  | ACKeyword KLet ->
      Some (F (eid, Let (b1, b2, b3)))
  | ACKeyword KLambda ->
    ( match (complete.target, ast) with
    | _, Blank _ | None, _ ->
        Some (F (eid, Lambda ([B.newF "var"], b2)))
    | Some (_, pd), _ ->
        let parent = ast |> AST.findParentOfWithin (P.toID pd) in
        (* Function name and param index *)
        let fnname, index =
          match parent with
          | F (_, FnCall (name, exprs, _)) ->
              let paramIndex =
                List.findIndex ~f:(fun e -> B.toID e = P.toID pd) exprs
                |> Option.withDefault ~default:(-1)
              in
              (B.toMaybe name, paramIndex)
          | _ ->
              (None, -1)
        in
        let lambdaArgs =
          complete.functions
          |> List.find ~f:(fun f -> Some f.fnName = fnname)
          |> Option.andThen ~f:(fun fn -> List.getAt ~index fn.fnParameters)
          |> (function
               | None | Some {paramBlock_args = []} ->
                   (* add default value if empty or not found*)
                   ["var"]
               | Some {paramBlock_args} ->
                   paramBlock_args)
          |> List.map ~f:(fun str -> B.newF str)
        in
        Some (F (eid, Lambda (lambdaArgs, b2))) )
  | ACKeyword KMatch ->
      Some (F (eid, Match (b1, [(b2, b3)])))
  | ACLiteral str ->
      Some (F (eid, Value str))
  | ACFunction fn ->
      Some (createFunction fn)
  | ACVariable (varname, _) ->
      Some (B.newF (Variable varname))
  | _ ->
      (* TODO: remove all these cases, replacing them with autocomplete options *)
      let firstWord = String.split ~on:" " str in
      ( match firstWord with
      | [""] ->
          Some b1
      | ["[]"] ->
          Some (F (eid, ListLiteral [B.new_ ()]))
      | ["["] ->
          Some (F (eid, ListLiteral [B.new_ ()]))
      | ["{}"] ->
          Some (F (eid, ObjectLiteral [(B.new_ (), B.new_ ())]))
      | ["{"] ->
          Some (F (eid, ObjectLiteral [(B.new_ (), B.new_ ())]))
      | _ ->
          None )


(* Assumes PD is within AST. Returns (new AST, new Expr) *)
let replaceExpr
    (m : model)
    (ast : expr)
    (old_ : expr)
    (move : nextMove)
    (item : autocompleteItem) : expr * expr =
  let value = AC.getValue m.complete in
  let id = B.toID old_ in
  let old, new_ =
    (* assign thread to variable *)
    if Regex.exactly ~re:"=[a-zA-Z].*" value
    then
      match AST.threadAncestors id ast with
      (* turn the current thread into a let-assignment to this *)
      (* name, and close the thread *)
      | (F (_, Thread _) as thread) :: _ ->
          let bindName = value |> String.dropLeft ~count:1 |> String.trim in
          ( thread
          , B.newF (Let (B.newF bindName, AST.closeThreads thread, B.new_ ()))
          )
      | _ ->
          (old_, old_)
      (* field access *)
    else if String.endsWith ~suffix:"." value
    then
      ( old_
      , B.newF
          (FieldAccess
             (B.newF (Variable (String.dropRight ~count:1 value)), B.new_ ()))
      )
    else
      ( old_
      , parseAst ast m.complete item value |> Option.withDefault ~default:old_
      )
  in
  let newAst =
    match move with
    | StartThread ->
        ast
        |> AST.replace (PExpr old) (PExpr new_)
        |> AST.wrapInThread (B.toID new_)
    | _ ->
        ast
        |> AST.replace (PExpr old) (PExpr new_)
        |> AST.maybeExtendThreadAt (B.toID new_) (B.new_ ())
        |> AST.maybeExtendListLiteralAt (PExpr new_)
  in
  (newAst, new_)


let parsePattern (str : string) : pattern option =
  match str with
  | "Nothing" ->
      Some (B.newF (PConstructor ("Nothing", [])))
  | "Just" ->
      Some (B.newF (PConstructor ("Just", [B.new_ ()])))
  | "Ok" ->
      Some (B.newF (PConstructor ("Ok", [B.new_ ()])))
  | "Error" ->
      Some (B.newF (PConstructor ("Error", [B.new_ ()])))
  | _ ->
      let variablePattern = "[a-z_][a-zA-Z0-9_]*" in
      if Runtime.isStringLiteral str
      then
        if Runtime.isValidDisplayString str
        then
          Some (B.newF (PLiteral (Runtime.convertDisplayStringToLiteral str)))
        else None
      else if Decoders.isLiteralRepr str
      then Some (B.newF (PLiteral str))
      else if Regex.exactly ~re:variablePattern str
      then Some (B.newF (PVariable str))
      else None


let getAstFromTopLevel tl : expr =
  match tl with
  | TLHandler h ->
      h.ast
  | TLFunc f ->
      f.ufAST
  | TLGroup _ ->
      recover "No ASTs in Groups" ~debug:tl (B.new_ ())
  | TLDB _ ->
      recover "No ASTs in DBs" ~debug:tl (B.new_ ())
  | TLTipe _ ->
      recover "No ASTs in Types" ~debug:tl (B.new_ ())


let validate (tl : toplevel) (pd : pointerData) (value : string) :
    string option =
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
  | PVarBind _ ->
      v AC.varnameValidator "variable name"
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
  | PField _ ->
      v AC.fieldNameValidator "fieldname"
  | PKey _ ->
      v AC.keynameValidator "key"
  | PExpr _ ->
      None (* Done elsewhere *)
  | PFFMsg _ ->
      None
  | PFnName _ ->
      v AC.fnNameValidator "function name"
  | PFnCallName _ ->
      None
  | PConstructorName _ ->
      v AC.constructorNameValidator "constructor name"
  | PParamName _ ->
      v AC.paramNameValidator "param name"
      |> Option.orElse (AC.validateFnParamNameFree tl value)
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
  | PPattern currentPattern ->
      let validPattern value =
        Decoders.isLiteralRepr value
        || v AC.varnamePatternValidator "variable pattern" = None
        || v AC.constructorPatternValidator "constructor pattern" = None
      in
      let body =
        let ast = getAstFromTopLevel tl in
        let parent =
          if B.toID ast = P.toID pd
          then ast
          else AST.findParentOfWithin (P.toID pd) ast
        in
        ( match parent with
        | F (_, Match (_, cases)) ->
            cases
            |> List.find ~f:(fun (p, _) ->
                   Pattern.extractById p (B.toID currentPattern)
                   |> Option.isSome )
        | _ ->
            None )
        |> Option.map ~f:Tuple2.second
        |> AST.recoverBlank "validate: recover! pattern without parent match"
      in
      ( match parsePattern value with
      | Some newPattern ->
        ( match (currentPattern, newPattern) with
        | Blank _, _ | F (_, PLiteral _), _ ->
            if validPattern value
            then None
            else Some "pattern must be literal or variable or constructor"
        | F (_, PVariable _), F (_, PVariable _) ->
            None
        | _ ->
            let noUses =
              Pattern.variableNames currentPattern
              |> List.map ~f:(fun v -> AST.uses v body)
              |> List.all ~f:List.isEmpty
            in
            if not noUses
            then
              Some "Unsafe pattern replacement, remove RHS variable uses first"
            else if not (validPattern value)
            then Some "pattern must be literal or variable or constructor"
            else None )
      | _ ->
          Some "Invalid Pattern" )


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
            RPC (ops, focus)
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
          let saveAst ast next =
            match tl with
            | TLHandler h ->
                saveH {h with ast} next
            | TLFunc f ->
                save (TLFunc {f with ufAST = ast}) next
            | TLDB _ ->
                recover "no ASTs in DBs" ~debug:tl NoChange
            | TLTipe _ ->
                recover "no ASTs in Tipes" ~debug:tl NoChange
            | TLGroup _ ->
                recover "no ASTs in Groups" ~debug:tl NoChange
          in
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
                RPC (RenameDBname (tlid, value) :: varrefs, FocusNothing)
          | PDBColType ct, ACDBColType value, TLDB db ->
              if B.asF ct = Some value
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
              if B.asF cn = Some value
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
          | PVarBind _, ACVarBind varName, _ ->
              replace (PVarBind (B.newF varName))
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
          | PField _, ACField fieldname, _ ->
              let fieldname =
                if String.startsWith ~prefix:"." fieldname
                then String.dropLeft ~count:1 fieldname
                else fieldname
              in
              let fieldname =
                if String.endsWith ~suffix:"." fieldname
                then String.dropRight ~count:1 fieldname
                else fieldname
              in
              let ast = getAstFromTopLevel tl in
              let parent = AST.findParentOfWithin id ast in
              (* Nested field? *)
              if String.endsWith ~suffix:"." m.complete.value
              then
                (* wrap the field access with another field access *)
                (* get the parent ID from the old AST, cause it has the blank.
                 Then get the parent structure from the new ID *)
                let wrapped =
                  match parent with
                  | F (id_, FieldAccess (lhs, _)) ->
                      B.newF
                        (FieldAccess
                           ( F (id_, FieldAccess (lhs, B.newF fieldname))
                           , B.new_ () ))
                  | _ ->
                      recover "should be a field" ~debug:parent parent
                in
                let new_ = PExpr wrapped in
                let replacement = TL.replace (PExpr parent) new_ tl in
                save replacement new_
              else if move = StartThread
              then
                (* Starting a new thread from the field *)
                let replacement =
                  AST.replace pd (PField (B.newF fieldname)) ast
                in
                let newAst = AST.wrapInThread (B.toID parent) replacement in
                saveAst newAst (PExpr parent)
              else (* Changing a field *)
                replace (PField (B.newF fieldname))
          | PKey _, ACKey value, _ ->
              let new_ = PKey (B.newF value) in
              getAstFromTopLevel tl
              |> AST.replace pd new_
              |> AST.maybeExtendObjectLiteralAt new_
              |> fun ast_ -> saveAst ast_ new_
          | PExpr e, ACExpr _, _
          | PExpr e, ACFunction _, _
          | PExpr e, ACLiteral _, _
          | PExpr e, ACKeyword _, _
          | PExpr e, ACConstructorName _, _
          | PExpr e, ACVariable _, _ ->
              let ast = getAstFromTopLevel tl in
              let newast, newexpr = replaceExpr m ast e move item in
              saveAst newast (PExpr newexpr)
          | PFFMsg _, ACFFMsg value, _ ->
              replace (PFFMsg (B.newF value))
          | PFnName _, ACFnName value, TLFunc old ->
              if List.member ~value (Functions.allNames m.userFunctions)
              then DisplayError ("There is already a Function named " ^ value)
              else
                let newPD = PFnName (B.newF value) in
                let new_ =
                  { old with
                    ufMetadata = {old.ufMetadata with ufmName = B.newF value}
                  }
                in
                let changedNames = Refactor.renameFunction m old new_ in
                wrapNew (SetFunction new_ :: changedNames) newPD
          | PConstructorName _, ACConstructorName value, _ ->
              replace (PConstructorName (B.newF value))
          | PParamName _, ACParamName value, _ ->
              replace (PParamName (B.newF value))
          | PParamTipe _, ACParamTipe tipe, _ ->
              replace (PParamTipe (B.newF tipe))
          | PPattern _, ACConstructorName value, _ ->
            ( match parsePattern value with
            | None ->
                DisplayError "not a pattern"
            | Some p ->
                let new_ = PPattern p in
                getAstFromTopLevel tl
                |> AST.replace pd new_
                |> AST.maybeExtendPatternAt new_
                |. saveAst new_ )
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
                    ( Types.show_pointerData pd
                    ^ ", "
                    ^ Types.show_autocompleteItem item ) ) ) )
    | _ ->
        recover "Missing tl/pd" ~debug:cursor NoChange )


let submit (m : model) (cursor : entryCursor) (move : nextMove) : modification
    =
  match cursor with
  | Creating pos ->
    ( match AC.highlighted m.complete with
    | Some (ACOmniAction act) ->
        submitOmniAction m pos act
    (* If empty, create an empty handler *)
    | None when m.complete.value = "" ->
        submitOmniAction m pos (NewReplHandler None)
    | _ ->
        NoChange )
  | _ ->
    ( match AC.highlighted m.complete with
    | Some (ACOmniAction _) ->
        recover "Shouldnt allow omniactions here" ~debug:cursor NoChange
    | Some item ->
        submitACItem m cursor item move
    | _ ->
        (* We removed ACExtra to define more specific autocomplete items.*)
        (* These are all autocomplete items who's target accepts and handles a free form value *)
        let item =
          let value = m.complete.value in
          match m.complete.target with
          | Some (_, p) ->
            ( match P.typeOf p with
            | Expr ->
                (* We need ACExpr so that PExpr still works until we switch to fluid *)
                Some (ACExpr value)
            | DBColName ->
                Some (ACDBColName value)
            | VarBind ->
                Some (ACVarBind value)
            | Field ->
                Some (ACField value)
            | Key ->
                Some (ACKey value)
            | FFMsg ->
                Some (ACFFMsg value)
            | FnName ->
                Some (ACFnName value)
            | ParamName ->
                Some (ACParamName value)
            | Pattern ->
                Some (ACConstructorName value)
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
