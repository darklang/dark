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
import Pointer as P
import SpecTypes
import SpecHeaders
import Blank


createFindSpace : Model -> Modification
createFindSpace m = Enter (Creating (Viewport.toAbsolute m Defaults.initialPos))

---------------------
-- Focus
---------------------

focusEntry : Model -> Cmd Msg
focusEntry m =
  case unwrapState m.state of
    Entering _ ->
      Dom.focus Defaults.entryID |> Task.attempt FocusEntry
    _ ->
      Cmd.none


newHandlerSpec : () -> HandlerSpec
newHandlerSpec _ = { module_ = Blank.new ()
                   , name = Blank.new ()
                   , modifier = Blank.new ()
                   , types = { input = F (gid ()) DTAny
                             , output = F (gid ()) DTAny
                             }
                   }

createFunction : Model -> FnName -> Bool -> Maybe BExpr
createFunction m name hasImplicitParam =
  let blankModifier = if hasImplicitParam then -1 else 0
      blanks count = List.map (\_ -> Blank.new ()) (List.range 1 count)
      fn = m.complete.functions
           |> List.filter (\fn -> fn.name == name)
           |> List.head
  in
    case fn of
      Just function ->
        Just <|
          Blank.newF
            (NFnCall
              name
              (blanks ((List.length function.parameters) + blankModifier)))
      Nothing -> Nothing

type ThreadAction = StartThread | ContinueThread
submit : Model -> EntryCursor -> ThreadAction -> String -> Modification
submit m cursor action value =
  let
      parseAst str hasImplicit =
        let eid = gid ()
            b1 = Blank.new ()
            b2 = Blank.new ()
            b3 = Blank.new ()
            firstWord = String.split " " str in
        case firstWord of
          ["if"] ->
            Just <| F eid (NIf b1 b2 b3)
          ["let"] ->
            Just <| F eid (NLet (Blank.new()) b2 b3)
          ["lambda"] ->
            Just <| F eid (NLambda ["var"] b2)
          [""] ->
            Just b1
          ["[]"] ->
            Just <| F eid (NValue "[]")
          ["{}"] ->
            Just <| F eid (NValue "{}")
          ["null"] ->
            Just <| F eid (NValue "null")
          _ ->
            if not (RT.isLiteral str)
            then createFunction m value hasImplicit
            else Just <| F eid (NValue str)

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
                Blank.newF (NThread [expr, Blank.new ()])
          wrapExpr expr =
            wrap <| SetHandler tlid pos { ast = threadIt expr
                                        , spec = newHandlerSpec ()
                                        }
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
        wrapExpr <|
          Blank.newF
            (NFieldAccess
               (Blank.newF (NVariable (String.dropRight 1 value)))
               (Blank.new ()))

      -- varnames
      else if List.member value (Analysis.varnamesFor m Nothing)
      then wrapExpr <| Blank.newF (NVariable value)

      -- start new AST
      else
        case parseAst value False of
          Nothing -> NoChange
          Just v -> wrapExpr v

    Filling tlid p ->
      let tl = TL.getTL m tlid
          predecessor = TL.getPrevBlank tl (Just p)
          wrap op = RPC ([op], FocusNext tlid predecessor)

          replaceExpr : Model -> Handler -> TLID -> Pointer -> String -> Modification
          replaceExpr m h tlid p value =
            let id = P.idOf p
                old_ = AST.subtree id h.ast
                target = Just (tlid, p)
                (old, new) =
                  -- assign thread to variable
                  if String.startsWith "= " value
                  then
                    case AST.threadAncestors id h.ast of
                      -- turn the current thread into a let-assignment to this
                      -- name, and close the thread
                      (F _ (NThread _) as thread) :: _ ->
                        let bindName = value
                                       |> String.dropLeft 2
                                       |> String.trim
                        in
                          ( AST.toPD thread
                          , AST.toPD <|
                              Blank.newF (
                                NLet
                                  (Blank.newF bindName)
                                  (AST.closeThread thread)
                                  (Blank.new ())))

                      _ ->
                        (old_, old_)

                  -- field access
                  else if String.endsWith "." value
                  then
                    ( old_
                    , AST.toPD <|
                        Blank.newF
                          (NFieldAccess
                            (Blank.newF (NVariable (String.dropRight 1 value)))
                            (Blank.new ())))

                  -- variables
                  else if List.member value (Analysis.varnamesFor m target)
                  then
                    (old_, AST.toPD <| Blank.newF (NVariable value))

                  -- parsed exprs
                  else
                    ( old_
                    , parseAst
                        value
                          (action == ContinueThread
                          && AST.isThread h.ast p)
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
        EventName ->
          let eventNameValidation =
                if TL.isHTTPHandler tl
                then
                  "/([-a-zA-Z0-9@:%_+.~#?&/=]*)"
                else
                  "[a-zA-Z_][a-zA-Z0-9_]*"
          in
              validate eventNameValidation "event name"
              <|
              let h = deMaybe "maybeH - eventname" maybeH
                  replacement = SpecHeaders.replaceEventNameBlank id value h.spec in
              wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        EventModifier ->
          let eventModifierValidation =
                if TL.isHTTPHandler tl
                then
                  "[A-Z]+"
                else
                  "[a-zA-Z_][a-zA-Z0-9_]*"
          in
          validate eventModifierValidation "event modifier"
            <|
          let h = deMaybe "maybeH - eventmodifier" maybeH
              replacement = SpecHeaders.replaceEventModifierBlank id value h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        EventSpace ->
          validate "[A-Z]+" "event space"
            <|
          let h = deMaybe "maybeH - eventspace" maybeH
              replacement = SpecHeaders.replaceEventSpaceBlank id value h.spec in
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
                          F id (NFieldAccess lhs rhs) ->
                            Blank.newF (
                              NFieldAccess
                                (F id (NFieldAccess lhs (Blank.newF fieldname)))
                                (Blank.new ()))
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
                      let parentID = AST.parentOf id h.ast |> Blank.toID in
                      AST.wrapInThread parentID replacement
          in
              wrap <| SetHandler tlid tl.pos { h | ast = newAst }
        Expr ->
          let h = deMaybe "maybeH - expr" maybeH in
          replaceExpr m h tlid p value
        DarkType ->
          validate "(String|Int|Any|Empty|{)" "type"
            <|
          let specType =
                case value of
                  "String" -> DTString
                  "Any" -> DTAny
                  "Int" -> DTInt
                  "Empty" -> DTEmpty
                  "{" -> DTObj [(Blank.new (), Blank.new ())]
                  _ -> Debug.crash "disallowed value"
              h = deMaybe "maybeH - httpverb" maybeH
              pd = PDarkType (Blank.newF specType)
              replacement = SpecTypes.replace p pd h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        DarkTypeField ->
          let h = deMaybe "maybeH - expr" maybeH
              pd = PDarkTypeField (Blank.newF value)
              replacement = SpecTypes.replace p pd h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }

