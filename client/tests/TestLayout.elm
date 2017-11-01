module TestLayout exposing (..)

-- tests
import Test exposing (..)
import Expect exposing (Expectation, pass, fail)

-- builtins
import Json.Decode as JSD
import Dict

-- libs

-- dark
import Autocomplete
import DarkTestCode
import DarkTestData
import Defaults
import Graph as G
import GraphValidator as GV
import RPC
import Util exposing (deMaybe)
import Entry
import Selection
import Types exposing (..)

center : Pos
center = {x=475, y=325}

expectResult : Result a b -> Expectation
expectResult r =
  case r of
    Ok ok -> Expect.pass
    Err err -> Expect.fail (toString err)

firstNode : Model -> Node
firstNode m = firstNodeWhere m (always True)

firstNodeWhere : Model -> (Node -> Bool) -> Node
firstNodeWhere m cond = m.nodes |> Dict.values |> List.filter cond |> List.head |> deMaybe

opNamesAndPoses : List RPC -> List (String, MPos)
opNamesAndPoses rpcs =
  List.map
    (\rpc ->
      case rpc of
        AddFunctionCall id name pos -> (name, pos)
        AddBlock _ pos _ args -> ("Block: " ++ String.join ", " args, pos)
        AddValue _ val pos -> (val, pos)
    -- | SetConstant Name (ID, ParamName)
    -- | SetEdge ID (ID, ParamName)
    -- | DeleteNode ID
    -- | UpdateNodeCursor ID Cursor
    -- | UpdateNodePosition ID MPos
        x -> ("other: " ++ toString x, NoPos Nothing)
    ) rpcs

modelFrom : (String, String) -> Model
modelFrom (name, json) =
  let result = JSD.decodeString RPC.decodeGraph json
      m = Defaults.defaultModel Defaults.defaultEditor
      m2 = { m | complete = Autocomplete.init DarkTestCode.functions}
  in case result of
      Err msg -> Debug.crash <| "Error while reading test file " ++ name ++ ": " ++ msg
      Ok nodes ->
        G.reposition { m2 | nodes = nodes }

testAllLayouts : Test
testAllLayouts =
  describe "Test All Layouts"
    (List.map
      (\(name, json) ->
        let m = modelFrom (name, json) in
        describe name
          [ test "nodes positioned" (\_ -> GV.validate m |> expectResult)])
       DarkTestData.tests)


testIfAlone : Test
testIfAlone =
  test "layout of if alone"
  (\_ ->
    let m = modelFrom DarkTestData.empty
        cursor = Creating center
        mod = Entry.submit m False cursor "if" in
    case mod of
      RPC (rpcs, _) ->
        let ops = opNamesAndPoses rpcs
            expected = [ ("if", Root center)
                       , ("Block: then", NoPos Nothing)
                       , ("Block: else", NoPos Nothing)]
        in if (Util.containsOrdered expected ops)
           then Expect.pass
           else Expect.equal ops expected
      _ -> Expect.fail "wrong type of mod"
   )


testDeleteIf : Test
testDeleteIf =
  test "layout_DeleteIf"
  (\_ ->
    let m = modelFrom DarkTestData.simple_if
        if_ = firstNodeWhere m (\n -> n.name == "if")
        m2 = { m | state = (Selecting if_.id) }
        mod = Selection.deleteSelected m if_.id in
    case mod of
     -- this might be a bit brittle but I dont feel like making a framework for this now
     Many [_, Select _] -> Expect.fail "selected something but shouldnt have"
     Many [_, Deselect] -> Expect.pass
     _ -> Expect.fail "Something completely unexpected"
   )
