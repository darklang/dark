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
import Analysis
import Viewport
import Util exposing (deMaybe)
import AST
import Toplevel as TL
import Runtime as RT
import Refactor
import Pointer as P
import SpecTypes
import SpecHeaders
import Blank as B


createFindSpace : Model -> Modification
createFindSpace m = Enter (Creating (Viewport.toAbsolute m Defaults.initialPos))

---------------------
-- Focus
---------------------

focusEntry : Model -> Cmd Msg
focusEntry m =
  case unwrapCursorState m.cursorState of
    Entering _ ->
      Dom.focus Defaults.entryID |> Task.attempt FocusEntry
    _ ->
      Cmd.none


newHandlerSpec : () -> HandlerSpec
newHandlerSpec _ = { module_ = B.new ()
                   , name = B.new ()
                   , modifier = B.new ()
                   , types = { input = F (gid ()) DTAny
                             , output = F (gid ()) DTAny
                             }
                   }

createFunction : Model -> FnName -> Bool -> Maybe Expr
createFunction m name hasImplicitParam =
  let blankModifier = if hasImplicitParam then -1 else 0
      blanks count = List.map (\_ -> B.new ()) (List.range 1 count)
      fn = m.complete.functions
           |> List.filter (\fn -> fn.name == name)
           |> List.head
  in
    case fn of
      Just function ->
        Just <|
          B.newF
            (FnCall
              name
              (blanks ((List.length function.parameters) + blankModifier)))
      Nothing -> Nothing

type ThreadAction = StartThread | ContinueThread
submit : Model -> EntryCursor -> ThreadAction -> String -> Modification
submit m cursor action value =
  let
      parseAst str hasImplicit =
        let eid = gid ()
            b1 = B.new ()
            b2 = B.new ()
            b3 = B.new ()
            firstWord = String.split " " str in
        case firstWord of
          ["if"] ->
            Just <| F eid (If b1 b2 b3)
          ["let"] ->
            Just <| F eid (Let (B.new()) b2 b3)
          ["lambda"] ->
            Just <| F eid (Lambda ["var"] b2)
          [""] ->
            Just b1
          ["[]"] ->
            Just <| F eid (Value "[]")
          ["{}"] ->
            Just <| F eid (Value "{}")
          ["null"] ->
            Just <| F eid (Value "null")
          _ ->
            if not (RT.isLiteral str)
            then createFunction m value hasImplicit
            else Just <| F eid (Value str)

  in
  case cursor of
    Creating pos ->
      let tlid = gtlid ()
          threadIt expr =
            case action of
              ContinueThread ->
                expr
              StartThread ->
                B.newF (Thread [expr, B.new ()])
          wrapExpr expr =
            let newAst = threadIt expr
                focus = newAst
                        |> AST.allData
                        |> List.filter P.isBlank
                        |> List.head
                        |> Maybe.map P.toID
                        |> Maybe.map (FocusExact tlid)
                        |> Maybe.withDefault (FocusNext tlid Nothing)
                op = SetHandler tlid pos { ast = newAst
                                         , spec = newHandlerSpec ()
                                         }
            in RPC ([op], focus)
      in

      -- DB creation
      if String.startsWith "DB" value && (value |> String.contains "::" |> not)
      then
        let dbName = value
                     |> String.dropLeft 2
                     |> String.trim
            next = gid ()
        in RPC ([ CreateDB tlid pos dbName
                , AddDBCol tlid next (gid ())
                ], FocusExact tlid next)

      -- field access
      else if String.endsWith "." value
      then
        wrapExpr <|
          B.newF
            (FieldAccess
               (B.newF (Variable (String.dropRight 1 value)))
               (B.new ()))

      -- varnames
      else if List.member value (Analysis.varnamesFor m Nothing)
      then wrapExpr <| B.newF (Variable value)

      -- start new AST
      else
        case parseAst value False of
          Nothing -> NoChange
          Just v -> wrapExpr v

    Filling tlid id ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id

          replaceExpr : Model -> Toplevel -> PointerData -> String -> Modification
          replaceExpr m tl p value =
            let id = P.toID p
                old_ = TL.findExn tl id
                target = Just (tl.id, p)
                ast =
                  case tl.data of
                    TLHandler h -> h.ast
                    TLFunc f -> f.ast
                    TLDB _ -> Debug.crash "replaceExpr: expected toplevel with AST component, got TLDB"
                (old, new) =
                  -- assign thread to variable
                  if String.startsWith "= " value
                  then
                    case AST.threadAncestors id ast of
                      -- turn the current thread into a let-assignment to this
                      -- name, and close the thread
                      (F _ (Thread _) as thread) :: _ ->
                        let bindName = value
                                       |> String.dropLeft 2
                                       |> String.trim
                        in
                          ( PExpr thread
                          , PExpr <|
                              B.newF (
                                Let
                                  (B.newF bindName)
                                  (AST.closeThread thread)
                                  (B.new ())))

                      _ ->
                        (old_, old_)

                  -- field access
                  else if String.endsWith "." value
                  then
                    ( old_
                    , PExpr <|
                        B.newF
                          (FieldAccess
                            (B.newF (Variable (String.dropRight 1 value)))
                            (B.new ())))

                  -- variables
                  else if List.member value (Analysis.varnamesFor m target)
                  then
                    (old_, PExpr <| B.newF (Variable value))

                  -- parsed exprs
                  else
                    ( old_
                    , parseAst
                        value
                          (action == ContinueThread
                          && AST.isThreadBlank ast (P.toID p))
                      |> Maybe.map PExpr
                      |> Maybe.withDefault old_)


                ast1 = case action of
                  ContinueThread -> ast
                  StartThread ->
                    AST.wrapInThread id ast

                ast2 = AST.replace old new ast1
                ast3 = AST.maybeExtendThreadAt (P.toID new) ast2
            in
            if old == new
            then NoChange
            else
              let focus = FocusNext tlid (Just (P.toID new)) in
              case tl.data of
                TLHandler h ->
                 RPC ([SetHandler tlid tl.pos { h | ast = ast3 }] , focus)
                TLFunc f ->
                 RPC ([SetFunction { f | ast = ast3 }], focus)
                TLDB _ -> Debug.crash "replaceExpr: expected toplevel with AST component, got TLDB"

      in
      if String.length value < 1
      then NoChange
      else
      let maybeH = TL.asHandler tl
          db = TL.asDB tl
          predecessor = TL.getPrevBlank tl (Just pd) |> Maybe.map P.toID
          wrapPred op = RPC ([op], FocusNext tlid predecessor)
          wrap op new = RPC ([op], FocusNext tlid (Just new))
          validate pattern name success =
            if Util.rematch pattern value
            then success
            else Error (name ++ " must match /" ++ pattern ++ "/")
      in
      case pd of
        PDBColType _ ->
          validate "[A-Z]\\w+" "DB type"
            <| wrap (SetDBColType tlid id value) id
        PDBColName _ ->
          validate "\\w+" "DB column name"
            <| wrap (SetDBColName tlid id value) id
        PVarBind _ ->
          validate "[a-zA-Z_][a-zA-Z0-9_]*" "variable name"
            <|
              case tl.data of
                TLHandler h ->
                  let replacement = AST.replaceVarBind pd value h.ast in
                  wrapPred <|
                    SetHandler tlid tl.pos { h | ast = replacement }
                TLFunc f ->
                  let replacement = AST.replaceVarBind pd value f.ast in
                  wrapPred <| SetFunction { f | ast = replacement }
                TLDB _ -> Debug.crash "not handler or func - varbind"

        PEventName _ ->
          let eventNameValidation =
                if TL.isHTTPHandler tl
                then "/([-a-zA-Z0-9@:%_+.~#?&/=]*)"
                else "[a-zA-Z_][a-zA-Z0-9_]*"
          in
          validate eventNameValidation "event name"
          <|
          let h = deMaybe "maybeH - eventname" maybeH
              new = B.newF value
              replacement = SpecHeaders.replaceEventName id new h.spec
          in
          wrap
            (SetHandler tlid tl.pos { h | spec = replacement })
            (B.toID new)
        PEventModifier _ ->
          let eventModifierValidation =
                if TL.isHTTPHandler tl
                then "[A-Z]+"
                else "[a-zA-Z_][a-zA-Z0-9_]*"
          in
          validate eventModifierValidation "event modifier"
          <|
          let h = deMaybe "maybeH - eventmodifier" maybeH
              new = B.newF value
              replacement = SpecHeaders.replaceEventModifier id new h.spec in
          wrap
            (SetHandler tlid tl.pos { h | spec = replacement })
            (B.toID new)
        PEventSpace _ ->
          validate "[A-Z]+" "event space"
            <|
          let h = deMaybe "maybeH - eventspace" maybeH
              new = B.newF value
              replacement = SpecHeaders.replaceEventSpace id new h.spec
          in
          wrap
            (SetHandler tlid tl.pos { h | spec = replacement })
            (B.toID new)
        PField _ ->
          validate ".+" "fieldname"
            <|
          let ast =
                case tl.data of
                  TLHandler h -> h.ast
                  TLFunc f -> f.ast
                  TLDB _ -> Debug.crash "not handler or func - field"
              parent = AST.parentOf id ast
              newAst =
                if String.endsWith "." value
                then
                  let fieldname = String.dropRight 1 value
                  -- wrap the field access with another field access
                  -- get the parent ID from the old AST, cause it has the
                  -- blank. Then get the parent structure from the new I
                      wrapped =
                        case parent of
                          F id (FieldAccess lhs rhs) ->
                            B.newF (
                              FieldAccess
                                (F id (FieldAccess lhs (B.newF fieldname)))
                                (B.new ()))
                          _ -> Debug.crash "should be a field"
                  in
                  AST.replace (PExpr parent) (PExpr wrapped) ast
                else
                  let replacement = AST.replaceField pd value ast in
                  case action of
                    ContinueThread -> replacement
                    StartThread ->
                      -- id is not in the replacement, so search for the
                      -- parent in the old ast
                      let parentID = AST.parentOf id ast |> B.toID in
                      AST.wrapInThread parentID replacement
          in
              case tl.data of
                TLHandler h ->
                  wrapPred <| SetHandler tlid tl.pos { h | ast = newAst }
                TLFunc f ->
                  wrapPred <| SetFunction { f | ast = newAst }
                TLDB _ -> Debug.crash "not handler or func - field"

        PExpr _ ->
          replaceExpr m tl pd value
        PDarkType _ ->
          validate "(String|Int|Any|Empty|{)" "type"
            <|
          let specType =
                case value of
                  "String" -> DTString
                  "Any" -> DTAny
                  "Int" -> DTInt
                  "Empty" -> DTEmpty
                  "{" -> DTObj [(B.new (), B.new ())]
                  _ -> Debug.crash "disallowed value"
              h = deMaybe "maybeH - httpverb" maybeH
              newPD = PDarkType (B.newF specType)
              replacement = SpecTypes.replace pd newPD h.spec
          in
          wrap
            (SetHandler tlid tl.pos { h | spec = replacement })
            (P.toID newPD)

        PDarkTypeField _ ->
          let h = deMaybe "maybeH - expr" maybeH
              newPD = PDarkTypeField (B.newF value)
              replacement = SpecTypes.replace pd newPD h.spec
          in
          wrap
            (SetHandler tlid tl.pos { h | spec = replacement })
            (P.toID newPD)
        PFFMsg _ ->
          let newPD = PFFMsg (B.newF value)
              newTL = TL.replace pd newPD tl
              h = TL.asHandler newTL |> deMaybe "must be handler"
          in
          wrap (SetHandler tlid tl.pos h) (P.toID newPD)
        PFnName _ ->
          let newPD = PFnName (B.newF value)
              newTL = TL.replace pd newPD tl
              changedNames =
                let old = TL.asUserFunction tl |> deMaybe "old userFn"
                    new = TL.asUserFunction newTL |> deMaybe "new userFn"
                in Refactor.renameFunction m old new
          in
          RPC (SetFunction (TL.asUserFunction newTL |> deMaybe "must be function")
               :: changedNames, FocusNext tlid (Just (P.toID newPD)))
        PParamName _ ->
          let newPD = PParamName (B.newF value)
              newTL = TL.replace pd newPD tl
              newFn = TL.asUserFunction newTL |> deMaybe "param fn"
          in
          wrap
            (SetFunction newFn)
            (P.toID newPD)
        PParamTipe _ ->
          validate "[A-Z][a-z]*" "param tipe"
          <|
          let newPD = PParamTipe (B.newF (RT.str2tipe value))
              newTL = TL.replace pd newPD tl
              newFn = TL.asUserFunction newTL |> deMaybe "tipe fn"
          in
          wrap (SetFunction newFn) (P.toID newPD)



