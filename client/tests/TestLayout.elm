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
import DarkTestData exposing (..)
import Defaults
import RPC
import Util exposing (deMaybe)
import Entry
import Types exposing (..)

all : Test
all =
  test "layout_equals_ifarg"
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
          in case mod of
               RPC (rpcs, _) ->
                 let updates = List.filter
                                 (\r -> case r of
                                          UpdateNodePosition _ _ -> True
                                          _ -> False) rpcs
                 in case updates of
                      [ UpdateNodePosition (ID 95509132) (Dependent (Just _))
                      , UpdateNodePosition (ID _) (Root _)] -> Expect.pass
                      _ -> Expect.fail "bad shape of output"
               _ -> Expect.fail "wrong type of mod"
   )
