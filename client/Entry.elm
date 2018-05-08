module Entry exposing (..)

-- builtins
import Task
-- import Result exposing (Result)
-- import Set

-- lib
import Dom
-- import Result.Extra as RE
-- import Maybe.Extra as ME
import List.Extra as LE

-- dark
import Types exposing (..)
import Prelude  exposing (..)
import Util
import DB
import Defaults
import Analysis
import Viewport
import AST
import Toplevel as TL
import Runtime as RT
import Refactor
import Pointer as P
import SpecTypes
import SpecHeaders
import Blank as B
import Autocomplete as AC


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

createFunction : Model -> FnName -> Maybe Expr
createFunction m name =
  let blanks count = LE.initialize count (\_ -> B.new ())
      fn = m.complete.functions
           |> List.filter (\fn -> fn.name == name)
           |> List.head
  in
    case fn of
      Just function ->
        Just <|
          B.newF (FnCall name (blanks (List.length function.parameters)))
      Nothing -> Nothing

submitOmniAction : Model -> Pos -> OmniAction -> Modification
submitOmniAction m pos action =
  case action of
    NewDB dbname ->
      let next = gid ()
          tlid = gtlid ()
      in
          RPC ([ CreateDB tlid pos dbname
               , AddDBCol tlid next (gid ())
               ] , FocusExact tlid next)
    NewHTTPHandler ->
      let next = gid ()
          tlid = gtlid ()
          spec = newHandlerSpec ()
          handler = { ast = B.new()
                    , spec = { spec | module_ = B.newF "HTTP"
                                    , name = Blank next}
                    }
      in
          RPC ([ SetHandler tlid pos handler
               ] , FocusExact tlid next)
    NewEventSpace name ->
      let next = gid ()
          tlid = gtlid ()
          spec = newHandlerSpec ()
          handler = { ast = B.new()
                    , spec = { spec | module_ = B.newF name
                                    , name = Blank next}
                    }
      in
          RPC ([ SetHandler tlid pos handler
               ] , FocusExact tlid next)
    NewHTTPRoute route ->
      let next = gid ()
          tlid = gtlid ()
          spec = newHandlerSpec ()
          handler = { ast = B.new()
                    , spec = { spec | name = B.newF route
                                    , module_ = B.newF "HTTP"
                                    , modifier = Blank next}
                    }
      in
          RPC ([ SetHandler tlid pos handler
               ] , FocusExact tlid next)

replaceExpr : Model -> Toplevel -> PointerData -> ThreadAction -> String -> Modification
replaceExpr m tl p action value =
  let id = P.toID p
      old_ = TL.findExn tl id
      target = Just (tl.id, p)
      ast =
        case tl.data of
          TLHandler h -> h.ast
          TLFunc f -> f.ast
          TLDB _ -> impossible ("No expressions in DBs", tl.data)

      (old, new) =
        -- assign thread to variable
        if Util.reExactly "=[a-zA-Z].*" value
        then
          case AST.threadAncestors id ast of
            -- turn the current thread into a let-assignment to this
            -- name, and close the thread
            (F _ (Thread _) as thread) :: _ ->
              let bindName = value
                             |> String.dropLeft 1
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
        then (old_, PExpr <| B.newF (Variable value))

        -- parsed exprs
        else
          ( old_
          , parseAst m value
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
    let focus = FocusNext tl.id (Just (P.toID new)) in
    case tl.data of
      TLHandler h ->
       RPC ([SetHandler tl.id tl.pos { h | ast = ast3 }] , focus)
      TLFunc f ->
       RPC ([SetFunction { f | ast = ast3 }], focus)
      TLDB _ -> impossible ("No expres in DBs", tl.data)

parseAst : Model -> String -> Maybe Expr
parseAst m str =
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
      then createFunction m str
      else Just <| F eid (Value str)

type ThreadAction = StartThread | ContinueThread
submit : Model -> EntryCursor -> ThreadAction -> Modification
submit m cursor action =
  let value = AC.getValue m.complete
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
                -- NB: these pos magic numbers position the tl body where the click was
                op = SetHandler tlid { pos | x = pos.x - 17, y = pos.y - 70 } { ast = newAst
                                         , spec = newHandlerSpec ()
                                         }
            in RPC ([op], focus)
      in

      -- field access
      if String.endsWith "." value
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
        case parseAst m value of
          Nothing -> NoChange
          Just v -> wrapExpr v

    Filling tlid id ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id

      in
      if String.length value < 1
      then NoChange
      else
      let maybeH = TL.asHandler tl
          db = TL.asDB tl
          predecessor = TL.getPrevBlank tl (Just pd) |> Maybe.map P.toID
          wrapPred ops = RPC (ops, FocusNext tlid predecessor)
          wrap ops new = RPC (ops, FocusNext tlid (Just new))
          validate pattern name success =
            if Util.reExactly pattern value
            then success
            else Error (name ++ " must match /" ++ pattern ++ "/")
      in
      case pd of
        PDBColType ct ->
          if B.asF ct == Just value
          then Select tlid (Just id)
          else if B.isBlank ct
          then
            validate "\\[?[A-Z]\\w+\\]?" "DB type"
              <| wrap [ SetDBColType tlid id value
                      , AddDBCol tlid (gid ()) (gid ())]
                      id
          else
            validate "\\[?[A-Z]\\w+\\]?" "DB type"
              <| wrap [ ChangeDBColType tlid id value] id

        PDBColName cn ->
          if value == "id"
          then Error ("id's are automatic and implicit, no need to add them")
          else if B.asF cn == Just value
          then Select tlid (Just id)
          else if DB.hasCol (db |> deMaybe "db") value
          then Error ("Can't have two DB fields with the same name: " ++ value)
          else if B.isBlank cn
          then
            validate "\\w+" "DB column name"
              <| wrap [SetDBColName tlid id value] id
          else
            validate "\\w+" "DB column name"
              <| wrap [ChangeDBColName tlid id value] id
        PVarBind _ ->
          validate "[a-zA-Z_][a-zA-Z0-9_]*" "variable name"
            <|
              case tl.data of
                TLHandler h ->
                  let replacement = AST.replaceVarBind pd value h.ast in
                  wrapPred
                    [SetHandler tlid tl.pos { h | ast = replacement }]
                TLFunc f ->
                  let replacement = AST.replaceVarBind pd value f.ast in
                  wrapPred [SetFunction { f | ast = replacement }]
                TLDB _ -> impossible ("no vars in DBs", tl.data)

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
            [SetHandler tlid tl.pos { h | spec = replacement }]
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
            [SetHandler tlid tl.pos { h | spec = replacement }]
            (B.toID new)
        PEventSpace _ ->
          validate "[A-Z_]+" "event space"
            <|
          let h = deMaybe "maybeH - eventspace" maybeH
              new = B.newF value
              replacement = SpecHeaders.replaceEventSpace id new h.spec
              replacement2 =
                if SpecHeaders.isHTTP replacement
                then replacement
                else
                  SpecHeaders.replaceEventModifier
                   (B.toID h.spec.modifier)
                   (B.newF "_")
                   replacement
          in
          wrap
            [SetHandler tlid tl.pos { h | spec = replacement2 }]
            (B.toID new)
        PField _ ->
          validate ".+" "fieldname"
            <|
          let ast =
                case tl.data of
                  TLHandler h -> h.ast
                  TLFunc f -> f.ast
                  TLDB _ -> impossible ("No fields in DBs", tl.data)
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
                          _ -> impossible ("should be a field", parent)
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
                  wrapPred [SetHandler tlid tl.pos { h | ast = newAst }]
                TLFunc f ->
                  wrapPred [SetFunction { f | ast = newAst }]
                TLDB _ -> impossible ("no fields in DBs", tl.data)

        PExpr _ ->
          replaceExpr m tl pd action value
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
                  _ -> todo "disallowed value"
              h = deMaybe "maybeH - httpverb" maybeH
              newPD = PDarkType (B.newF specType)
              replacement = SpecTypes.replace pd newPD h.spec
          in
          wrap
            [SetHandler tlid tl.pos { h | spec = replacement }]
            (P.toID newPD)

        PDarkTypeField _ ->
          let h = deMaybe "maybeH - expr" maybeH
              newPD = PDarkTypeField (B.newF value)
              replacement = SpecTypes.replace pd newPD h.spec
          in
          wrap
            [SetHandler tlid tl.pos { h | spec = replacement }]
            (P.toID newPD)
        PFFMsg _ ->
          let newPD = PFFMsg (B.newF value)
              newTL = TL.replace pd newPD tl
              h = TL.asHandler newTL |> deMaybe "must be handler"
          in
          wrap [SetHandler tlid tl.pos h] (P.toID newPD)
        PFnName _ ->
          let newPD = PFnName (B.newF value)
              newTL = TL.replace pd newPD tl
              changedNames =
                let old = TL.asUserFunction tl |> deMaybe "old userFn"
                    new = TL.asUserFunction newTL |> deMaybe "new userFn"
                in Refactor.renameFunction m old new
          in
          wrap
            (SetFunction (TL.asUserFunction newTL |> deMaybe "must be function")
            :: changedNames)
            (P.toID newPD)
        PParamName _ ->
          let newPD = PParamName (B.newF value)
              newTL = TL.replace pd newPD tl
              newFn = TL.asUserFunction newTL |> deMaybe "param fn"
          in
          wrap
            [SetFunction newFn]
            (P.toID newPD)
        PParamTipe _ ->
          validate "[A-Z][a-z]*" "param tipe"
          <|
          let newPD = PParamTipe (B.newF (RT.str2tipe value))
              newTL = TL.replace pd newPD tl
              newFn = TL.asUserFunction newTL |> deMaybe "tipe fn"
          in
          wrap [SetFunction newFn] (P.toID newPD)



