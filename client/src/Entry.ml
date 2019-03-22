open Tc
open Prelude
open Types

(* Dark *)
module AC = Autocomplete
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel

let createFindSpace (m : model) : modification =
  Enter (Creating (Viewport.toAbsolute m Defaults.initialVPos))


(* --------------------- *)
(* Focus *)
(* --------------------- *)
(* Based on Tea_html_cmds, applies offset after focus *)
let focusWithOffset id offset =
  Tea.Cmd.call (fun _ ->
      let ecb _ignored =
        match Js.Nullable.toOption (Web.Document.getElementById id) with
        | None ->
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


let newHandlerSpec (_ : unit) : handlerSpec =
  {module_ = B.new_ (); name = B.new_ (); modifier = B.new_ ()}


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


let submitOmniAction (pos : pos) (action : omniAction) : modification =
  let pos = {x = pos.x - 17; y = pos.y - 70} in
  match action with
  | NewDB maybeName ->
      let name =
        match maybeName with Some n -> n | None -> DB.generateDBName ()
      in
      DB.createDB name pos
  | NewHandler name ->
      let next = gid () in
      let tlid = gtlid () in
      let spec = newHandlerSpec () in
      let spec = {spec with name = B.ofOption name} in
      let handler = {ast = Blank next; spec; tlid} in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))
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
      let next = gid () in
      let tlid = gtlid () in
      let handler =
        { ast = B.new_ ()
        ; spec =
            { name = B.ofOption route
            ; module_ = B.newF "HTTP"
            ; modifier = Blank next }
        ; tlid }
      in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))
  | NewEventSpace name ->
      let next = gid () in
      let tlid = gtlid () in
      let spec = newHandlerSpec () in
      let handler =
        { ast = B.new_ ()
        ; spec = {spec with module_ = B.newF name; name = Blank next}
        ; tlid }
      in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))
  | Goto (page, tlid, _) ->
      Many [SetPage page; Select (tlid, None)]


type nextMove =
  | StartThread
  | StayHere
  | GotoNext

let parseAst (item : autocompleteItem) (str : string) : expr option =
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
      Some (F (eid, Lambda ([B.newF "var"], b2)))
  | ACKeyword KMatch ->
      Some (F (eid, Match (b1, [(b2, b3)])))
  | ACLiteral litstr ->
      Some (F (eid, Value litstr))
  | ACFunction fn ->
      Some (createFunction fn)
  | ACVariable varname ->
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
    if Util.reExactly "=[a-zA-Z].*" value
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
    else (old_, parseAst item value |> Option.withDefault ~default:old_)
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
      if Decoders.isLiteralString str
      then Some (B.newF (PLiteral str))
      else if Util.reExactly variablePattern str
      then Some (B.newF (PVariable str))
      else None


let getAstFromTopLevel tl =
  match tl.data with
  | TLHandler h ->
      h.ast
  | TLFunc f ->
      f.ufAST
  | TLDB _ ->
      impossible ("No ASTs in DBs", tl.data)
  | TLTipe _ ->
      impossible ("No ASTs in Types", tl.data)


let validate (tl : toplevel) (pd : pointerData) (value : string) :
    string option =
  let v pattern name =
    if Util.reExactly pattern value
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
      then v AC.httpNameValidator "route name"
      else v AC.eventNameValidator "event name"
  | PEventModifier _ ->
      if TL.isHTTPHandler tl
      then v AC.httpVerbValidator "verb"
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
      None
  | PParamTipe _ ->
      v AC.paramTypeValidator "param type"
  | PPattern currentPattern ->
      let validPattern value =
        Decoders.isLiteralString value
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
        |> deOption "validate: impossible! pattern without parent match"
        |> Tuple2.second
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
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      let result = validate tl pd stringValue in
      if result <> None
      then DisplayAndReportError (deOption "checked above" result)
      else
        let maybeH = TL.asHandler tl in
        let db = TL.asDB tl in
        let wrap ops next =
          let wasEditing = P.isBlank pd |> not in
          let focus =
            if wasEditing && move = StayHere
            then
              match next with
              | None ->
                  FocusSame
              | Some nextID ->
                  FocusExact (tl.id, nextID)
            else FocusNext (tl.id, next)
          in
          RPC (ops, focus)
        in
        let wrapID ops = wrap ops (Some id) in
        let wrapNew ops new_ = wrap ops (Some (P.toID new_)) in
        let save newtl next =
          if newtl = tl
          then NoChange
          else
            match newtl.data with
            | TLHandler h ->
                wrapNew [SetHandler (tlid, tl.pos, h)] next
            | TLFunc f ->
                wrapNew [SetFunction f] next
            | TLTipe t ->
                wrapNew [SetType t] next
            | TLDB _ ->
                impossible ("no vars in DBs", tl.data)
        in
        let saveH h next = save {tl with data = TLHandler h} next in
        let saveAst ast next =
          match tl.data with
          | TLHandler h ->
              saveH {h with ast} next
          | TLFunc f ->
              save {tl with data = TLFunc {f with ufAST = ast}} next
          | TLDB _ ->
              impossible ("no ASTs in DBs", tl.data)
          | TLTipe _ ->
              impossible ("no ASTs in Tipes", tl.data)
        in
        let replace new_ =
          tl |> TL.replace pd new_ |> fun tl_ -> save tl_ new_
        in
        ( match (pd, item) with
        | PDBName (F (id, oldName)), ACDBName value ->
            if AC.assertValid AC.dbNameValidator value <> value
            then
              DisplayError
                ("DB name must match " ^ AC.dbNameValidator ^ " pattern")
            else if oldName = value (* leave as is *)
            then Select (tlid, Some id)
            else if List.member ~value (TL.allDBNames m.toplevels)
            then DisplayError ("There is already a DB named " ^ value)
            else
              let varrefs = Refactor.renameDBReferences m oldName value in
              RPC (RenameDBname (tlid, value) :: varrefs, FocusNothing)
        | PDBColType ct, ACDBColType value ->
            let db1 = deOption "db" db in
            if B.asF ct = Some value
            then Select (tlid, Some id)
            else if DB.isMigrationCol db1 id
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
        | PDBColName cn, ACExtra value ->
            let db1 = deOption "db" db in
            if B.asF cn = Some value
            then Select (tlid, Some id)
            else if DB.isMigrationCol db1 id
            then wrapID [SetDBColNameInDBMigration (tlid, id, value)]
            else if DB.hasCol db1 value
            then
              DisplayError
                ("Can't have two DB fields with the same name: " ^ value)
            else if B.isBlank cn
            then wrapID [SetDBColName (tlid, id, value)]
            else wrapID [ChangeDBColName (tlid, id, value)]
        | PVarBind _, ACExtra varName ->
            replace (PVarBind (B.newF varName))
        | PEventName _, ACEventName value ->
            replace (PEventName (B.newF value))
        (* allow arbitrary HTTP modifiers *)
        | PEventModifier _, ACHTTPModifier value
        | PEventModifier _, ACCronTiming value
        | PEventModifier _, ACExtra value ->
            replace (PEventModifier (B.newF value))
        (* allow arbitrary eventspaces *)
        | PEventSpace _, ACEventSpace value ->
            let h = deOption "maybeH - eventspace" maybeH in
            let new_ = B.newF value in
            let replacement = SpecHeaders.replaceEventSpace id new_ h.spec in
            let replacement2 =
              if SpecHeaders.visibleModifier replacement
              then replacement
              else
                SpecHeaders.replaceEventModifier
                  (B.toID h.spec.modifier)
                  (B.newF "_")
                  replacement
            in
            saveH {h with spec = replacement2} (PEventSpace new_)
        | PField _, ACExtra fieldname | PField _, ACField fieldname ->
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
                    impossible ("should be a field", parent)
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
        | PKey _, ACExtra value ->
            let new_ = PKey (B.newF value) in
            getAstFromTopLevel tl
            |> AST.replace pd new_
            |> AST.maybeExtendObjectLiteralAt new_
            |> fun ast_ -> saveAst ast_ new_
        | PExpr e, item ->
          ( match tl.data with
          | TLHandler h ->
              let newast, newexpr = replaceExpr m h.ast e move item in
              saveAst newast (PExpr newexpr)
          | TLFunc f ->
              let newast, newexpr = replaceExpr m f.ufAST e move item in
              saveAst newast (PExpr newexpr)
          | TLTipe _ ->
              (* TODO(types): error here? *)
              NoChange
          | TLDB db ->
            ( match db.activeMigration with
            | None ->
                NoChange
            | Some am ->
                if List.member ~value:pd (AST.allData am.rollback)
                then
                  let newast, newexpr =
                    replaceExpr m am.rollback e move item
                  in
                  wrapNew [SetExpr (tl.id, id, newast)] (PExpr newexpr)
                else if List.member ~value:pd (AST.allData am.rollforward)
                then
                  let newast, newexpr =
                    replaceExpr m am.rollforward e move item
                  in
                  wrapNew [SetExpr (tl.id, id, newast)] (PExpr newexpr)
                else NoChange ) )
        | PFFMsg _, ACExtra value ->
            replace (PFFMsg (B.newF value))
        | PFnName _, ACExtra value ->
            let newPD = PFnName (B.newF value) in
            let newTL = TL.replace pd newPD tl in
            let changedNames =
              let old = TL.asUserFunction tl |> deOption "old userFn" in
              let new_ = TL.asUserFunction newTL |> deOption "new userFn" in
              Refactor.renameFunction m old new_
            in
            wrapNew
              ( SetFunction
                  (TL.asUserFunction newTL |> deOption "must be function")
              :: changedNames )
              newPD
        | PConstructorName _, ACConstructorName value ->
            replace (PConstructorName (B.newF value))
        | PParamName _, ACExtra value ->
            replace (PParamName (B.newF value))
        | PParamTipe _, ACParamTipe value ->
            replace (PParamTipe (B.newF (RT.str2tipe value)))
        | PPattern _, ACExtra value ->
          ( match parsePattern value with
          | None ->
              DisplayError "not a pattern"
          | Some p ->
              let new_ = PPattern p in
              getAstFromTopLevel tl
              |> AST.replace pd new_
              |> AST.maybeExtendPatternAt new_
              |. saveAst new_ )
        | pd, item ->
            DisplayAndReportError
              ( "Invalid autocomplete option: ("
              ^ Types.show_pointerData pd
              ^ ", "
              ^ Types.show_autocompleteItem item ) )


let submit (m : model) (cursor : entryCursor) (move : nextMove) : modification
    =
  match cursor with
  | Creating pos ->
    ( match AC.highlighted m.complete with
    | Some (ACOmniAction act) ->
        submitOmniAction pos act
    (* If empty, create an empty handler *)
    | None when m.complete.value = "" ->
        submitOmniAction pos (NewHandler None)
    | _ ->
        NoChange )
  | _ ->
    ( match AC.highlighted m.complete with
    | Some (ACOmniAction _) ->
        impossible "Shouldnt allow omniactions here"
    | Some item ->
        submitACItem m cursor item move
    | _ ->
        (* TODO: remove this. This is a transitional step to get to fully
                typed. *)
        submitACItem m cursor (ACExtra m.complete.value) move )
