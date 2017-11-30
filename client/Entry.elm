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
import List.Extra as LE

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
createFindSpace m = Enter False (Creating (Viewport.toAbsolute m Defaults.initialPos)) Nothing
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

createFunction : Model -> FnName -> Maybe Expr
createFunction m name =
  let holes count = List.map (\_ -> Hole (gid ())) (List.range 1 count)
      fn = m.complete.functions
           |> List.filter (\fn -> fn.name == name)
           |> List.head
  in
    case fn of
      Just function ->
        Just <| FnCall (gid ()) name (holes (List.length function.parameters))
      Nothing -> Nothing

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
      parseAst str =
        let firstWord = String.split " " str in
        case firstWord of
          ["if"] ->
            Just (If eid (Hole hid1) (Hole hid2) (Hole hid3))
          ["let"] ->
              Just (Let eid [(Empty hid1, Hole hid2)] (Hole hid3))
          ["lambda"] ->
            Just (Lambda eid ["var"] (Hole hid1))
          _ ->
            if RT.tipeOf str == TIncomplete || AST.isInfix str
            then createFunction m value
            else Just <| Value eid str

  in
  case cursor of
    Creating pos ->
      let emptyHS = { name = Empty (gid ())
                    , module_ = Empty (gid ())
                    , modifier = Empty (gid ())} in
      if String.startsWith "DB" value
      then
        let dbName = value
                     |> String.dropLeft 2
                     |> String.trim in
          RPC ([CreateDB id pos dbName], FocusNext id Nothing)
      else
        case parseAst value of
          Nothing -> NoChange
          Just v ->
            let handler = { ast = v, spec = emptyHS } in
            RPC ([SetHandler id pos handler], FocusNext id Nothing)
    Filling tlid id ->
      let tl = TL.getTL m tlid in
      case tl.data of
        TLDB db ->
          if TL.isDBRowNameHole db id
          then
            RPC ([ SetDBRowName tl.id id value ]
                 , FocusNext tl.id Nothing)
          else if TL.isDBRowTypeHole db id
          then
            RPC ([ SetDBRowType tl.id id value ]
                 , FocusNext tl.id Nothing)

          else
            NoChange
        TLHandler h ->
          if TL.isBindHole h id
          then
            let replacement = AST.replaceBindHole id value h.ast in
            RPC ([SetHandler tl.id tl.pos { h | ast = replacement}]
                , FocusNext tl.id Nothing)

          else if TL.isSpecHole h id
          then
            let replacement = TL.replaceSpecHole id value h.spec in
            RPC ([ SetHandler tl.id tl.pos { h | spec = replacement }]
                 , FocusNext tl.id Nothing)


          else
            -- check if value is in model.varnames
            let (ID rid) = id
                availableVars =
                  let avd = Analysis.getAvailableVarnames m tlid
                  in Dict.get rid avd |> Maybe.withDefault []
                holeReplacement =
                  if List.member value availableVars
                  then Just (Variable (gid ()) value)
                  else parseAst value
            in
            case holeReplacement of
              Nothing -> NoChange
              Just v ->
                let
                    replacement = AST.replaceHole id v h.ast
                    holes = TL.allHoles tl
                    predecessor =
                      LE.elemIndex id holes
                      |> Maybe.map (\i -> i - 1)
                      |> Maybe.map (max 0)
                      |> Maybe.andThen (\i -> LE.getAt i holes)
                in
                RPC ([SetHandler tl.id tl.pos { h | ast = replacement}]
              , FocusNext tl.id predecessor)

  -- let pt = EntryParser.parseFully value
  -- in case pt of
  --   Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt
  --   Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
