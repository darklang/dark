open Belt
open Porting
module B = Blank
module JSD = Json.Decode
module JSE = Json.Encode
open RPC
open Types
open UtilsForTests

let id1 = ID 5

let id2 = ID 10

let testRoundtrip decoder encoder name v =
  test ("roundtrip " ^ name)
  <| Expect.equal (Ok v) (v |> encoder |> JSD.decodeValue decoder)

let rtDval = testRoundtrip decodeDval encodeDval

let serverCompatible =
  Test.describe "compatible with server JSON encoding"
    [ test "obj uses list"
      <| Expect.equal "[\"DObj\",{\"foo\":[\"DInt\",5]}]"
           ( DObj (Dict.fromList [("foo", DInt 5)])
           |> encodeDval |> JSE.encode 0 )
    ; test "dresp shape"
      <| Expect.equal "[\"DResp\",[[\"Response\",401,[]],[\"DNull\"]]]"
           (DResp (Response (401, []), DNull) |> encodeDval |> JSE.encode 0) ]

let roundtrips =
  Test.describe "roundtrips"
    [ rtDval "int" (DInt 5)
    ; rtDval "obj" (DObj (Dict.fromList [("foo", DInt 5)]))
    ; rtDval "date" (DDate "can be anything atm")
    ; rtDval "incomplete" DIncomplete
    ; rtDval "float" (DFloat 7.2)
    ; rtDval "true" (DBool true)
    ; rtDval "false" (DBool false)
    ; rtDval "string" (DStr "incredibly this was broken")
    ; rtDval "null" DNull
    ; rtDval "id" (DID "1232345346456")
    ; rtDval "title" (DTitle "some title")
    ; rtDval "errorrail" (DErrororRail (DInt 5))
    ; rtDval "db" (DDB "Visitors")
    ; rtDval "list" (DList [DDB "Visitors"; DInt 4])
    ; rtDval "redirect" (DResp (Redirect "/home", DNull))
    ; rtDval "httpresponse" (DResp (Response (200, []), DStr "success")) ]
