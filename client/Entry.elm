module Entry exposing (..)

-- builtins
import Task
-- import Result exposing (Result)
-- import Set

-- lib
import Dom
-- import Result.Extra as RE
-- import Maybe.Extra as ME
-- import List.Extra as LE

-- dark
-- import Util
import Defaults
import Types exposing (..)
-- import Autocomplete
import Viewport
import Util exposing (deMaybe)
import AST
import Toplevel as TL
import Runtime as RT
import Pointer as P


createFindSpace : Model -> Modification
createFindSpace m = Enter (Creating (Viewport.toAbsolute m Defaults.initialPos))

---------------------
-- Focus
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusEntry


newHandlerSpec : () -> HandlerSpec
newHandlerSpec _ = { name = Blank (gid ())
                   , modifier = Blank (gid ())
                   , types = { input = Blank (gid ())
                             , output = Blank (gid ())
                             }
                   }

createFunction : Model -> FnName -> Bool -> Maybe Expr
createFunction m name hasImplicitParam =
  let holeModifier = if hasImplicitParam then -1 else 0
      holes count = List.map (\_ -> Hole (gid ())) (List.range 1 count)
      fn = m.complete.functions
           |> List.filter (\fn -> fn.name == name)
           |> List.head
  in
    case fn of
      Just function ->
        Just <|
          FnCall
            (gid ())
            name
            (holes ((List.length function.parameters) + holeModifier))
      Nothing -> Nothing

type ThreadAction = StartThread | ContinueThread
submit : Model -> EntryCursor -> ThreadAction -> String -> Modification
submit m cursor action value =
  let
      parseAst str hasImplicit =
        let eid = gid ()
            hid1 = gid ()
            hid2 = gid ()
            hid3 = gid ()
            firstWord = String.split " " str in
        case firstWord of
          ["if"] ->
            Just (If eid (Hole hid1) (Hole hid2) (Hole hid3))
          ["let"] ->
            Just (Let eid (Blank hid1) (Hole hid2) (Hole hid3))
          ["lambda"] ->
            Just (Lambda eid ["var"] (Hole hid1))
          [""] ->
            Just (Hole eid)
          ["[]"] ->
            Just (Value eid "[]")
          ["{}"] ->
            Just (Value eid "{}")
          ["null"] ->
            Just (Value eid "null")
          _ ->
            if not (RT.isLiteral str)
            then createFunction m value hasImplicit
            else Just <| Value eid str

  in
  case cursor of
    Creating pos ->
      let tlid = gtlid ()
          wrap op = RPC ([op], FocusFirstAST tlid)
          threadIt expr =
            case action of
              ContinueThread ->
                expr
              StartThread ->
                let hid = gid ()
                in Thread (gid ()) [expr, Hole hid]
      in

      -- DB creation
      if String.startsWith "DB" value && (value |> String.contains "::" |> not)
      then
        let dbName = value
                     |> String.dropLeft 2
                     |> String.trim
        in wrap <| CreateDB tlid pos dbName

      -- field access
      else if String.endsWith "." value
      then
        let access = FieldAccess (gid ())
                                 (Variable (gid ()) (String.dropRight 1 value))
                                 (Blank (gid ()))
            ast = threadIt access
            handler = { ast = ast, spec = newHandlerSpec () }
        in wrap <| SetHandler tlid pos handler

      -- varnames
      else if List.member value m.complete.varnames
      then
        let var = Variable (gid ()) value
            ast = threadIt var
            handler = { ast = ast, spec = newHandlerSpec () }
        in wrap <| SetHandler tlid pos handler

      -- start new AST
      else
        case parseAst value False of
          Nothing -> NoChange
          Just v ->
            let ast = threadIt v
                handler = { ast = ast, spec = newHandlerSpec () }
            in wrap <| SetHandler tlid pos handler

    Filling tlid p ->
      let tl = TL.getTL m tlid
          predecessor = TL.getPrevBlank tl p
          wrap op = RPC ([op], FocusNext tlid predecessor)

          replaceExpr m h tlid p value =
            let id = P.idOf p
                old_ = AST.subtree id h.ast
                (old, new) =
                  -- assign thread to variable
                  if String.startsWith "= " value
                  then
                    case AST.threadAncestors id h.ast of
                      -- turn the current thread into a let-assignment to this
                      -- name, and close the thread
                      (Thread tid _ as thread) :: _ ->
                        let bindName = value
                                       |> String.dropLeft 2
                                       |> String.trim
                        in
                          ( AST.toPD thread
                          , AST.toPD <|
                              Let (gid ())
                                (Filled (gid ()) bindName)
                                (AST.closeThread thread)
                                (Hole (gid ())))

                      _ ->
                        (old_, old_)

                  -- field access
                  else if String.endsWith "." value
                  then
                    ( old_
                    , AST.toPD <|
                        FieldAccess
                          (gid ())
                          (Variable (gid ()) (String.dropRight 1 value))
                          (Blank (gid ())))

                  -- variables
                  else if List.member value m.complete.varnames
                  then
                    (old_, AST.toPD <| Variable (gid ()) value)

                  -- parsed exprs
                  else
                    ( old_
                    , parseAst
                        value
                          (action == ContinueThread
                          && TL.isThread h p)
                      |> Maybe.map AST.toPD
                      |> Maybe.withDefault old_)


                ast1 = case action of
                  ContinueThread -> h.ast
                  StartThread ->
                    AST.wrapInThread id h.ast

                ast2 = AST.replace (P.pdToP old) new ast1
                ast3 = AST.maybeExtendThreadAt (P.idOfD new) ast2
            in
                if old == new
                then NoChange
                else wrap <| SetHandler tlid tl.pos { h | ast = ast3 }

      in
      if String.length value < 1
      then NoChange
      else
      let id = P.idOf p
          maybeH = TL.asHandler tl
          db = TL.asDB tl
          validate pattern name success =
            if Util.rematch pattern value
            then success
            else Error (name ++ " must match /" ++ pattern ++ "/")
      in
      case P.typeOf p of
        DBColType ->
          validate "[A-Z]\\w+" "DB type"
            <| wrap <| SetDBColType tlid id value
        DBColName ->
          validate "\\w+" "DB name"
            <| wrap <| SetDBColName tlid id value
        VarBind ->
          validate "[a-zA-Z_][a-zA-Z0-9_]*" "variable name"
            <|
            let h = deMaybe "maybeH - varbind" maybeH
                replacement = AST.replaceVarBind p value h.ast in
            wrap <| SetHandler tlid tl.pos { h | ast = replacement }
        HTTPRoute ->
          validate "/([-a-zA-Z0-9@:%_+.~#?&/=]*)" "http route"
            <|
          let h = deMaybe "maybeH - httproute" maybeH
              replacement = TL.replaceHTTPRouteBlank id value h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        HTTPVerb ->
          validate "[A-Z]+" "HTTP Verb"
            <|
          let h = deMaybe "maybeH - httpverb" maybeH
              replacement = TL.replaceHTTPVerbBlank id value h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        Field ->
          validate ".+" "fieldname"
            <|
          let h = deMaybe "maybeH - field" maybeH
              parent = AST.parentOf id h.ast
              newAst =
                if String.endsWith "." value
                then
                  let fieldname = String.dropRight 1 value
                  -- wrap the field access with another field access
                  -- get the parent ID from the old AST, cause it has the
                  -- hole. Then get the parent structure from the new I
                      wrapped =
                        case parent of
                          FieldAccess id lhs rhs ->
                            FieldAccess (gid ())
                            (FieldAccess id lhs (Filled (gid()) fieldname))
                            (Blank (gid()))
                          _ -> Debug.crash "should be a field"
                  in
                      AST.replace (AST.toP parent) (AST.toPD wrapped) h.ast
                else
                  let replacement = AST.replaceField p value h.ast in
                  case action of
                    ContinueThread -> replacement
                    StartThread ->
                      -- id is not in the replacement, so search for the
                      -- parent in the old ast
                      let parentID = AST.parentOf id h.ast |> AST.toID in
                      AST.wrapInThread parentID replacement
          in
              wrap <| SetHandler tlid tl.pos { h | ast = newAst }
        Expr ->
          let h = deMaybe "maybeH - expr" maybeH in
          replaceExpr m h tlid p value
        DarkType ->
          Debug.crash "TODO entry darktype"
        DarkTypeField ->
          Debug.crash "TODO entry darkfield"



  -- let pt = EntryParser.parseFully value
  -- in case pt of
  --   Ok pt -> execute m <| EntryParser.pt2ast m cursor pt
  --   Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
