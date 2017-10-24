module TestLayout exposing (..)

-- tests
import Test exposing (..)
import Expect exposing (Expectation, pass, fail)

-- builtins
import Json.Decode as JSD

-- libs

-- dark
import Autocomplete
import DarkTestCode
import DarkTestData
import Defaults
import Graph as G
import Overlap as O
import RPC
import Util exposing (deMaybe)
import Entry
import Types exposing (..)

center : Pos
center = {x=475, y=325}

expectResult : Result a b -> Expectation
expectResult r =
  case r of
    Ok ok -> Expect.pass
    Err err -> Expect.fail (toString err)

expectNoOverlap : List (Node, Node) -> Expectation
expectNoOverlap overlap =
  case overlap of
    [] -> Expect.pass
    _  -> Expect.fail <| "Nodes overlap " ++ (O.ppNodePairs overlap)

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
          [ test "overlap" (\_ -> O.overlappingNodes m |> expectNoOverlap)
          , test "nodes positioned" (\_ -> G.validate m |> expectResult)
          ])
       DarkTestData.tests)


testIfAlone : Test
testIfAlone =
  test "layout_ifAlone"
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
