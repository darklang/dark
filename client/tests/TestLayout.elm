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
import DarkTestData exposing (..)
import Defaults
import Graph as G
import RPC
import Util exposing (deMaybe)
import Entry
import Types exposing (..)

center : Pos
center = {x=475, y=325}

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

testIfAlone : Test
testIfAlone =
  test "layout_ifAlone"
  (\_ ->
    let m = Defaults.defaultModel Defaults.defaultEditor
        m2 = { m | complete = Autocomplete.init DarkTestCode.functions}
        cursor = Creating center
        mod = Entry.submit m2 False cursor "if" in
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

expectResult : Result a b -> Expectation
expectResult r =
  case r of
    Ok ok -> Expect.pass
    Err err -> Expect.fail (toString err)


testAddBlockAsNewRoot : Test
testAddBlockAsNewRoot =
  test "layout_if_as_arg"
  (\_ ->
    let json = DarkTestData.layout_if_as_arg
        result = JSD.decodeString RPC.decodeGraph json
        m = Defaults.defaultModel Defaults.defaultEditor
        m2 = { m | complete = Autocomplete.init DarkTestCode.functions}
    in case result of
        Err msg -> Expect.fail msg
        Ok nodes ->
          let m3 = G.reposition { m2 | nodes = nodes }

          in expectResult (G.validate m3)
   )
