open Tea
open! Porting
module AC = Autocomplete
module B = Blank
module P = Pointer
open Prelude
module RT = Runtime
module TL = Toplevel
open Types

let createFindSpace (m : model) : modification =
  Enter (Creating (Viewport.toAbsolute m Defaults.initialVPos))

(* fixed from Tea_html_cmds *)
let focusIdTask id =
  Tea_cmd.call
    (fun _ ->
      let ecb _ignored =
        match Js.Nullable.toOption (Web.Document.getElementById id) with
        | None -> Js.log ("Attempted to focus a non-existant element of: ", id)
        | Some elem -> Web.Node.focus elem
      in
      (* One to get out of the current render frame*)
      let cb _ignored = ignore (Web.Window.requestAnimationFrame ecb); in
      (* And another to properly focus *)
      ignore (Web.Window.requestAnimationFrame cb);
      ())

let focusEntry (m : model) : msg Cmd.t =
  match unwrapCursorState m.cursorState with
  | Entering _ | SelectingCommand (_, _) -> focusIdTask Defaults.entryID
  | _ -> Cmd.none

let newHandlerSpec (_ : unit) : handlerSpec =
  {module_= B.new_ (); name= B.new_ (); modifier= B.new_ ()}

let createFunction (m : model) (name : fnName) : expr option =
  let blanks count = List.initialize count (fun _ -> B.new_ ()) in
  let fn =
    m.complete.functions
    |> List.filter (fun fn_ -> fn_.fnName = name)
    |> List.head
  in
  match fn with
  | Some function_ ->
      let r = if function_.fnReturnTipe = TOption then Rail else NoRail in
      Some
      (B.newF (FnCall (name, blanks (List.length function_.fnParameters), r)))
  | None -> None

let submitOmniAction (m : model) (pos : pos) (action : omniAction) :
    modification =
  match action with
  | NewDB dbname ->
      let next = gid () in
      let tlid = gtlid () in
      RPC
        ( [CreateDB (tlid, pos, dbname); AddDBCol (tlid, next, gid ())]
        , FocusExact (tlid, next) )
  | NewHandler ->
      let next = gid () in
      let tlid = gtlid () in
      let spec = newHandlerSpec () in
      let handler = {ast= Blank next; spec; tlid} in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))
  | NewFunction name ->
      let blankfn = Refactor.generateEmptyFunction () in
      let newfn =
        match name with
        | Some n ->
            let metadata = blankfn.ufMetadata in
            let newMetadata = {metadata with ufmName= F (gid (), n)} in
            {blankfn with ufMetadata= newMetadata}
        | None -> blankfn
      in
      Many
        [ RPC ([SetFunction newfn], FocusNothing)
        ; MakeCmd (Url.navigateTo (Fn (newfn.ufTLID, Defaults.centerPos))) ]
  | NewHTTPHandler ->
      let next = gid () in
      let tlid = gtlid () in
      let spec = newHandlerSpec () in
      let handler =
        { ast= B.new_ ()
        ; spec= {spec with module_= B.newF "HTTP"; name= Blank next}
        ; tlid }
      in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))
  | NewEventSpace name ->
      let next = gid () in
      let tlid = gtlid () in
      let spec = newHandlerSpec () in
      let handler =
        { ast= B.new_ ()
        ; spec= {spec with module_= B.newF name; name= Blank next}
        ; tlid }
      in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))
  | NewHTTPRoute route ->
      let next = gid () in
      let tlid = gtlid () in
      let spec = newHandlerSpec () in
      let handler =
        { ast= B.new_ ()
        ; spec=
            { name= B.newF route; module_= B.newF "HTTP"; modifier= Blank next
            }
        ; tlid }
      in
      RPC ([SetHandler (tlid, pos, handler)], FocusExact (tlid, next))

type nextAction = StartThread | StayHere | GotoNext

let parseAst (m : model) (str : string) : expr option =
  let eid = gid () in
  let b1 = B.new_ () in
  let b2 = B.new_ () in
  let b3 = B.new_ () in
  let firstWord = String.split " " str in
  match firstWord with
  | ["if"] -> Some (F (eid, If (b1, b2, b3)))
  | ["let"] -> Some (F (eid, Let (b1, b2, b3)))
  | ["lambda"] -> Some (F (eid, Lambda ([B.newF "var"], b2)))
  | [""] -> Some b1
  | ["[]"] -> Some (F (eid, ListLiteral [B.new_ ()]))
  | ["["] -> Some (F (eid, ListLiteral [B.new_ ()]))
  | ["{}"] -> Some (F (eid, ObjectLiteral [(B.new_ (), B.new_ ())]))
  | ["{"] -> Some (F (eid, ObjectLiteral [(B.new_ (), B.new_ ())]))
  | _ ->
      if Decoders.isLiteralString str then Some (F (eid, Value str))
      else createFunction m str

let replaceExpr (m : model) (tlid : tlid) (ast : expr) (old_ : expr)
    (action : nextAction) (value : string) : expr * expr =
  let id = B.toID old_ in
  let target = Some (tlid, PExpr old_) in
  let old, new_ =
    if List.member value (Analysis.currentVarnamesFor m target) then
      (old_, B.newF (Variable value))
    else if String.endsWith "." value then
      ( old_
      , B.newF
          (FieldAccess (B.newF (Variable (String.dropRight 1 value)), B.new_ ()))
      )
    else if Util.reExactly "=[a-zA-Z].*" value then
      match AST.threadAncestors id ast with
      | (F (_, Thread _) as thread) :: _ ->
          let bindName = value |> String.dropLeft 1 |> String.trim in
          ( thread
          , B.newF (Let (B.newF bindName, AST.closeThreads thread, B.new_ ()))
          )
      | _ -> (old_, old_)
    else (old_, parseAst m value |> Option.withDefault old_)
  in
  let newAst =
    match action with
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

let validate (tl : toplevel) (pd : pointerData) (value : string) :
    string option =
  let v pattern name =
    if Util.reExactly pattern value then None
    else Some (name ^ " must match /" ^ pattern ^ "/")
  in
  match pd with
  | PDBColType ct -> v "\\[?[A-Z]\\w+\\]?" "DB type"
  | PDBColName cn ->
      if value = "id" then
        Some
          "The field name 'id' was reserved when IDs were implicit. We are \
           transitioning to allowing it, but we're not there just yet. Sorry!"
      else v "\\w+" "DB column name"
  | PVarBind _ -> v "[a-zA-Z_][a-zA-Z0-9_]*" "variable name"
  | PEventName _ ->
      let urlSafeCharacters = "[-a-zA-Z0-9@:%_+.~#?&/=]" in
      let http = "/(" ^ urlSafeCharacters ^ "*)" in
      let _ = "comment" in
      let event = urlSafeCharacters ^ "+" in
      let _ = "comment" in
      if TL.isHTTPHandler tl then v http "route name" else v event "event name"
  | PEventModifier _ ->
      if TL.isHTTPHandler tl then v "[A-Z]+" "verb"
      else v "[a-zA-Z_][\\sa-zA-Z0-9_]*" "event modifier"
  | PEventSpace _ -> v "[A-Z_]+" "event space"
  | PField _ -> v ".+" "fieldname"
  | PKey _ -> v ".+" "key"
  | PExpr e -> None
  | PFFMsg _ -> None
  | PFnName _ -> None
  | PParamName _ -> None
  | PParamTipe _ -> v "[A-Z][a-z]*" "param type"

let submit (m : model) (cursor : entryCursor) (action : nextAction) :
    modification =
  let value = AC.getValue m.complete in
  match cursor with
  | Creating pos -> (
      let tlid = gtlid () in
      let threadIt expr =
        match action with
        | StartThread -> B.newF (Thread [expr; B.new_ ()])
        | GotoNext -> expr
        | StayHere -> expr
      in
      let wrapExpr expr =
        let newAst = threadIt expr in
        let focus =
          newAst |> AST.allData |> List.filter P.isBlank |> List.head
          |> Option.map P.toID
          |> Option.map (fun x -> FocusExact (tlid, x))
          |> Option.withDefault (FocusNext (tlid, None))
        in
        let _ = "comment" in
        let _ = "comment" in
        let op =
          SetHandler
            ( tlid
            , {x= pos.x - 17; y= pos.y - 70}
            , {ast= newAst; spec= newHandlerSpec (); tlid} )
        in
        RPC ([op], focus)
      in
      if List.member value (Analysis.currentVarnamesFor m None) then
        wrapExpr <| B.newF (Variable value)
      else if String.endsWith "." value then
        wrapExpr
        <| B.newF
             (FieldAccess
                (B.newF (Variable (String.dropRight 1 value)), B.new_ ()))
      else
        match parseAst m value with None -> NoChange | Some v -> wrapExpr v )
  | Filling (tlid, id) -> (
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      let result = validate tl pd value in
      if result <> None then
        DisplayAndReportError (deOption "checked above" result)
      else if String.length value < 1 then NoChange
      else
        let maybeH = TL.asHandler tl in
        let db = TL.asDB tl in
        let wrap ops next =
          let wasEditing = P.isBlank pd |> not in
          let focus =
            if wasEditing && action = StayHere then
              match next with
              | None -> FocusSame
              | Some nextID -> FocusExact (tl.id, nextID)
            else FocusNext (tl.id, next)
          in
          RPC (ops, focus)
        in
        let wrapID ops = wrap ops (Some id) in
        let wrapNew ops new_ = wrap ops (Some (P.toID new_)) in
        let save newtl next =
          if newtl = tl then NoChange
          else
            match newtl.data with
            | TLHandler h -> wrapNew [SetHandler (tlid, tl.pos, h)] next
            | TLFunc f -> wrapNew [SetFunction f] next
            | TLDB _ -> impossible ("no vars in DBs", tl.data)
        in
        let saveH h next = save {tl with data= TLHandler h} next in
        let saveAst ast next =
          match tl.data with
          | TLHandler h -> saveH {h with ast} next
          | TLFunc f -> save {tl with data= TLFunc {f with ufAST= ast}} next
          | TLDB _ -> impossible ("no vars in DBs", tl.data)
        in
        let replace new_ =
          tl |> TL.replace pd new_ |> fun tl_ -> save tl_ new_
        in
        match pd with
        | PDBColType ct ->
            let db1 = deOption "db" db in
            if B.isBlank ct then
              wrapID
                [ SetDBColType (tlid, id, value)
                ; AddDBCol (tlid, gid (), gid ()) ]
            else if DB.isMigrationCol db1 id then
              wrapID
                [ SetDBColTypeInDBMigration (tlid, id, value)
                ; AddDBColToDBMigration (tlid, gid (), gid ()) ]
            else if B.asF ct = Some value then Select (tlid, Some id)
            else wrapID [ChangeDBColType (tlid, id, value)]
        | PDBColName cn ->
            let db1 = deOption "db" db in
            if B.isBlank cn then wrapID [SetDBColName (tlid, id, value)]
            else if DB.hasCol db1 value then
              DisplayError
                ("Can't have two DB fields with the same name: " ^ value)
            else if DB.isMigrationCol db1 id then
              wrapID [SetDBColNameInDBMigration (tlid, id, value)]
            else if B.asF cn = Some value then Select (tlid, Some id)
            else wrapID [ChangeDBColName (tlid, id, value)]
        | PVarBind _ -> replace (PVarBind (B.newF value))
        | PEventName _ -> replace (PEventName (B.newF value))
        | PEventModifier _ -> replace (PEventModifier (B.newF value))
        | PEventSpace _ ->
            let h = deOption "maybeH - eventspace" maybeH in
            let new_ = B.newF value in
            let replacement = SpecHeaders.replaceEventSpace id new_ h.spec in
            let replacement2 =
              if SpecHeaders.visibleModifier replacement then replacement
              else
                SpecHeaders.replaceEventModifier (B.toID h.spec.modifier)
                  (B.newF "_") replacement
            in
            saveH {h with spec= replacement2} (PEventSpace new_)
        | PField _ ->
            let ast =
              match tl.data with
              | TLHandler h -> h.ast
              | TLFunc f -> f.ufAST
              | TLDB _ -> impossible ("No fields in DBs", tl.data)
            in
            let parent = AST.parentOf id ast in
            if action = StartThread then
              let replacement = AST.replace pd (PField (B.newF value)) ast in
              let newAst = AST.wrapInThread (B.toID parent) replacement in
              saveAst newAst (PExpr parent)
            else if String.endsWith "." value then
              let fieldname = String.dropRight 1 value in
              let _ = "comment" in
              let _ = "comment" in
              let _ = "comment" in
              let wrapped =
                match parent with
                | F (id_, FieldAccess (lhs, rhs)) ->
                    B.newF
                      (FieldAccess
                         ( F (id_, FieldAccess (lhs, B.newF fieldname))
                         , B.new_ () ))
                | _ -> impossible ("should be a field", parent)
              in
              let new_ = PExpr wrapped in
              let replacement = TL.replace (PExpr parent) new_ tl in
              save replacement new_
            else replace (PField (B.newF value))
        | PKey k ->
            let new_ = PKey (B.newF value) in
            let ast =
              match tl.data with
              | TLHandler h -> h.ast
              | TLFunc f -> f.ufAST
              | TLDB _ -> impossible ("No fields in DBs", tl.data)
            in
            ast |> AST.replace pd new_
            |> AST.maybeExtendObjectLiteralAt new_
            |> fun ast_ -> saveAst ast_ new_
        | PExpr e -> (
          match tl.data with
          | TLHandler h ->
              let newast, newexpr = replaceExpr m tl.id h.ast e action value in
              saveAst newast (PExpr newexpr)
          | TLFunc f ->
              let newast, newexpr =
                replaceExpr m tl.id f.ufAST e action value
              in
              saveAst newast (PExpr newexpr)
          | TLDB db -> (
            match db.activeMigration with
            | None -> NoChange
            | Some am ->
                if List.member pd (AST.allData am.rollforward) then
                  let newast, newexpr =
                    replaceExpr m tl.id am.rollforward e action value
                  in
                  wrapNew [SetExpr (tl.id, id, newast)] (PExpr newexpr)
                else if List.member pd (AST.allData am.rollback) then
                  let newast, newexpr =
                    replaceExpr m tl.id am.rollback e action value
                  in
                  wrapNew [SetExpr (tl.id, id, newast)] (PExpr newexpr)
                else NoChange ) )
        | PFFMsg _ -> replace (PFFMsg (B.newF value))
        | PFnName _ ->
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
        | PParamName _ -> replace (PParamName (B.newF value))
        | PParamTipe _ -> replace (PParamTipe (B.newF (RT.str2tipe value))) )


