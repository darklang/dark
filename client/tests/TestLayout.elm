module TestLayout exposing (all)

-- tests
import ElmTest.Extra exposing (Test, describe, test, skip, todo)
import Expect exposing (Expectation)

-- builtins
import Json.Decode as JSD
import Dict

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

all : Test
all =
  describe "layout test"
  [test "layout_equals_ifarg"
  (\_ ->
    let json = DarkTestData.simple_equals
        result = JSD.decodeString RPC.decodeGraph json
        m = Defaults.defaultModel Defaults.defaultEditor
        m2 = { m | complete = Autocomplete.init DarkTestCode.functions}
    in case result of
        Err msg -> Expect.fail msg
        Ok nodes ->
          let m3 = { m2 | nodes = nodes }
              node = nodes |> Dict.values |> List.head |> deMaybe
              cursor = Filling node (ParamHole node (node.arguments |> List.head |> deMaybe |> Tuple.first) 0)
              mod = Entry.submit m3 False cursor "if"
          in Expect.equal mod (RPC ([], FocusSame))
   )]
