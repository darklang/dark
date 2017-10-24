module TestOverlapTriple exposing (..)

-- tests
import Test exposing (..)
import Expect exposing (Expectation, pass, fail)

-- builtins
import Json.Decode as JSD
import List

-- dark
import Autocomplete
import DarkTestCode
import DarkTestData exposing (..)
import Defaults
import RPC
import Graph as G
import Overlap as O

triple : Test
triple =
  test "overlap_test_triple"
  (\_ ->
    let json = DarkTestData.overlaptriple
        result = JSD.decodeString RPC.decodeGraph json
        m = Defaults.defaultModel Defaults.defaultEditor
        m2 = { m | complete = Autocomplete.init DarkTestCode.functions}
    in case result of
        Err msg -> Expect.fail msg
        Ok nodes ->
          let m3 = { m2 | nodes = nodes }
              m4 = G.reposition m3
              overlap = O.overlappingNodes m4
          in case overlap of
                 [] -> Expect.pass
                 _  -> Expect.fail <| "Nodes overlap " ++ (O.ppNodePairs overlap)
   )
