module TestRPC exposing (..)

-- tests
import Test exposing (describe)
import Expect exposing (Expectation)

-- builtins
import Json.Decode as JSD
import Json.Encode as JSE
import Dict

-- libs

-- dark
import Types exposing (..)
import AST
import Blank as B
import UtilsForTests exposing (..)
import RPC exposing (..)

id1 = ID 5
id2 = ID 10

testRoundtrip : JSD.Decoder a -> (a -> JSE.Value) -> String -> a -> Test.Test
testRoundtrip decoder encoder name v =
  test ("roundtrip " ++ name) <|
    (Expect.equal (Ok v)
       (v
        |> encoder
        |> JSD.decodeValue decoder))

rtDval = testRoundtrip decodeDval encodeDval



serverCompatible : Test.Test
serverCompatible =
  Test.describe "compatible with server JSON encoding"
    [ test "obj uses list" <|
        (Expect.equal "[\"DObj\",{\"foo\":[\"DInt\",5]}]"
          (DObj (Dict.fromList [("foo", DInt 5)])
           |> encodeDval
           |> JSE.encode 0))
    , test "dresp shape" <|
        (Expect.equal "[\"DResp\",[[\"Response\",401,[]],[\"DNull\"]]]"
          (DResp (Response 401 []) DNull
           |> encodeDval
           |> JSE.encode 0))

    ]


roundtrips : Test.Test
roundtrips =
  Test.describe "roundtrips"
    [ rtDval "int" (DInt 5)
    , rtDval "obj" (DObj (Dict.fromList [("foo", DInt 5)]))
    , rtDval "date" (DDate "can be anything atm")
    , rtDval "incomplete" DIncomplete
    , rtDval "float" (DFloat 7.2)
    , rtDval "true" (DBool True)
    , rtDval "false" (DBool False)
    , rtDval "string" (DStr "incredibly this was broken")
    , rtDval "null" DNull
    , rtDval "id" (DID "1232345346456")
    , rtDval "title" (DTitle "some title")
    , rtDval "errorrail" (DErrorRail (DInt 5))
    , rtDval "db" (DDB "Visitors")
    , rtDval "list" (DList [DDB "Visitors", DInt 4])
    , rtDval "redirect" (DResp (Redirect "/home") DNull)
    , rtDval "httpresponse" (DResp (Response 200 []) (DStr "success"))
    ]
