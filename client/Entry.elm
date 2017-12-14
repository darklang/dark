module Entry exposing (..)

-- builtins
import Task
-- import Result exposing (Result)
import Dict
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
-- import EntryParser exposing (AST(..), ACreating(..), AExpr(..), AFillParam(..), AFillResult(..), ARef(..))
import Util
import AST
import Toplevel as TL
import Runtime as RT
import Analysis


---------------------
-- Nodes
---------------------
updateValue : String -> Modification
updateValue target =
  Many [ AutocompleteMod <| ACSetQuery target ]

createFindSpace : Model -> Modification
createFindSpace m = Enter False (Creating (Viewport.toAbsolute m Defaults.initialPos))
---------------------
-- Focus
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusEntry


---------------------
-- Submitting the entry form to the server
---------------------
-- refocus : Bool -> Focus -> Focus
-- refocus re default =
--   case default of
--     FocusNext id -> if re then Refocus id else default
--     FocusExact id -> if re then Refocus id else default
--     f -> f
--

tlid : () -> TLID
tlid unit = TLID (Util.random unit)

gid : () -> ID -- Generate ID
gid unit = ID (Util.random unit)

emptyHS : () -> HandlerSpec
emptyHS _ = { name = Empty (gid ())
            , module_ = Empty (gid ())
            , modifier = Empty (gid ())
            }

createFunction : Model -> FnName -> Bool ->Maybe Expr
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

objectSubmit : Model -> Bool -> EntryCursor -> String -> Modification
objectSubmit m re cursor value =
  let access = FieldAccess (gid ()) (Variable (gid ()) value) (Empty (gid ())) in
  case cursor of
    Creating pos ->
      let handler = { ast = access, spec = emptyHS () }
          id = tlid ()
      in
      RPC ([SetHandler id pos handler], FocusNext id Nothing)
    Filling tlid id ->
      let tl = TL.getTL m tlid
          predecessor = TL.getPrevHole tl id
          focus = FocusNext tlid predecessor
          wrap op = RPC ([op], focus)
      in
          case TL.holeType tl id of
            ExprHole h ->
              let replacement = AST.replaceExpr id access h.ast in
                  wrap <| SetHandler tlid tl.pos { h | ast = replacement }
            _ -> submit m re cursor value

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let id = tlid ()
      eid = gid ()
      tid1 = gid ()
      tid2 = gid ()
      tid3 = gid ()
      hid1 = gid ()
      hid2 = gid ()
      hid3 = gid ()
      parseAst str hasImplicitParam =
        let firstWord = String.split " " str in
        case firstWord of
          ["if"] ->
            Just (If eid (Hole hid1) (Hole hid2) (Hole hid3))
          ["let"] ->
            Just (Let eid (Empty hid1) (Hole hid2) (Hole hid3))
          ["lambda"] ->
            Just (Lambda eid ["var"] (Hole hid1))
          [""] ->
            Just (Hole eid)
          _ ->
            if RT.tipeOf str == TIncomplete || AST.isInfix str
            then createFunction m value hasImplicitParam
            else Just <| Value eid str

  in
  case cursor of
    Creating pos ->
      if String.startsWith "DB" value
      then
        let dbName = value
                     |> String.dropLeft 2
                     |> String.trim in
          RPC ([CreateDB id pos dbName], FocusNext id Nothing)
      else
        case parseAst value False of
          Nothing -> NoChange
          Just v ->
            let handler = { ast = v, spec = emptyHS () } in
            RPC ([SetHandler id pos handler], FocusNext id Nothing)
    Filling tlid id ->
      let tl = TL.getTL m tlid
          predecessor = TL.getPrevHole tl id
          focus = FocusNext tlid predecessor
          wrap op = RPC ([op], focus)
          replaceExpr m h tlid id value =
            if String.startsWith "= " value
            then
              case AST.threadAncestors id h.ast |> List.reverse of
                [] -> NoChange
                thread :: _ ->
                  -- turn the current thread into a let-assignment to this
                  -- name, and close the thread
                  case thread of
                    Thread tid _ ->
                      let bindName = value
                                     |> String.dropLeft 2
                                     |> String.trim
                          newLet = Let (gid ())
                                       (Full bindName)
                                       (AST.closeThread thread)
                                       (Hole (gid ()))
                          replacement = AST.replaceExpr tid newLet h.ast
                      in wrap <| SetHandler tlid tl.pos { h | ast = replacement }
                    _ -> NoChange
            else
              -- check if value is in model.varnames
              let (ID rid) = id
                  availableVars =
                    let avd = Analysis.getAvailableVarnames m tlid
                    in Dict.get rid avd |> Maybe.withDefault []
                  holeReplacement =
                    if List.member value availableVars
                    then
                      Just (Variable (gid ()) value)
                    else
                      -- TODO: we expect something to go wrong the first
                      -- time, maybe we'll have too many holes
                      parseAst value (TL.isThreadHole h id)
              in
              case holeReplacement of
                Nothing ->
                  NoChange
                Just v ->
                  let replacement = AST.replaceExpr id v h.ast in
                  wrap <| SetHandler tlid tl.pos { h | ast = replacement }
      in
      if String.length value < 1
      then NoChange
      else
      case TL.holeType tl id of
        DBColTypeHole _ ->
          wrap <| SetDBColType tlid id value
        DBColNameHole _ ->
          wrap <| SetDBColName tlid id value
        BindHole h ->
          let replacement = AST.replaceBindHole id value h.ast in
          wrap <| SetHandler tlid tl.pos { h | ast = replacement }
        SpecHole h ->
          let replacement = TL.replaceSpecHole id value h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        FieldHole h ->
          let replacement = AST.replaceFieldHole id value h.ast in
          wrap <| SetHandler tlid tl.pos { h | ast = replacement }
        ExprHole h ->
          replaceExpr m h tlid id value
        NotAHole ->
          case tl.data of
            TLHandler h ->
              replaceExpr m h tlid id value
            _ -> NoChange

  -- let pt = EntryParser.parseFully value
  -- in case pt of
  --   Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt
  --   Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
