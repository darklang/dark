open Tc
open Types
open Jest

let testRoundtrip decoder encoder (name : string) (v : 'a) =
  test ("roundtrip " ^ name) (fun () ->
      Expect.expect v |> Expect.toEqual (v |> encoder |> decoder) )


let rtDval = testRoundtrip Decoders.dval Encoders.dval

let () =
  describe "compatible with server JSON encoding" (fun () ->
      test "obj uses list" (fun () ->
          Expect.expect "[\"DObj\",{\"foo\":[\"DInt\",5]}]"
          |> Expect.toEqual
               ( DObj (StrDict.fromList [("foo", DInt 5)])
               |> Encoders.dval
               |> Js.Json.stringify ) ) ;
      test "dresp shape" (fun () ->
          Expect.expect "[\"DResp\",[[\"Response\",401,[]],[\"DNull\"]]]"
          |> Expect.toEqual
               ( DResp (Response (401, []), DNull)
               |> Encoders.dval
               |> Js.Json.stringify ) ) ;
      describe "roundtrips" (fun () ->
          rtDval "int" (DInt 5) ;
          rtDval "obj" (DObj (StrDict.fromList [("foo", DInt 5)])) ;
          rtDval "date" (DDate "can be anything atm") ;
          rtDval "incomplete" DIncomplete ;
          rtDval "float" (DFloat 7.2) ;
          rtDval "true" (DBool true) ;
          rtDval "false" (DBool false) ;
          rtDval "string" (DStr "incredibly this was broken") ;
          rtDval "null" DNull ;
          rtDval "id" (DID "1232345346456") ;
          rtDval "errorrail" (DErrorRail (DInt 5)) ;
          rtDval "db" (DDB "Visitors") ;
          rtDval "list" (DList [DDB "Visitors"; DInt 4]) ;
          rtDval "redirect" (DResp (Redirect "/home", DNull)) ;
          rtDval "httpresponse" (DResp (Response (200, []), DStr "success")) ;
          () ) ;
      () )
