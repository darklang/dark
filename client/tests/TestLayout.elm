module TestLayout exposing (all)

-- tests
import ElmTest.Extra exposing (Test, describe, test, skip, todo)
import Expect exposing (Expectation)

-- builtins
import Json.Decode as JSD

-- libs

-- dark
import DarkTestData exposing (..)
import Defaults
import Graph as G
import RPC
import Util

all : Test
all =
  describe "layout test"
  [test "layout_equals_ifarg"
  (\_ ->
    let json = DarkTestData.layout_equals_ifarg
        result = JSD.decodeString RPC.decodeGraph json
        m = Defaults.defaultModel Defaults.defaultEditor
    in case result of
        Err msg -> Expect.fail msg
        Ok nodes ->
          { m | nodes = nodes }
          |> G.reposition
          |> G.validate
          |> (\r -> Expect.true "true" (Util.resultIsOk r)
                    |> Expect.onFail (toString r))
   )]

--       node = m.nodes |> List.head |> deMaybe
--       m2 = Entry.enterExact node
--       m3 = Enter.submit m2 False "if"
--   in expect m3.positioningIsCorrect

