open Prelude
open Tester

let testRoundtrip decoder encoder (name : string) (v : 'a) =
  test ("roundtrip " ^ name) (fun () ->
      expect v |> toEqual (v |> encoder |> decoder))


let rtDval = testRoundtrip Decoders.dval Encoders.dval

let run () =
  describe "compatible with server JSON encoding" (fun () ->
      test "obj uses list" (fun () ->
          expect "[\"DObj\",{\"foo\":[\"DInt\",5]}]"
          |> toEqual
               ( DObj (StrDict.fromList [("foo", DInt 5)])
               |> Encoders.dval
               |> Js.Json.stringify )) ;
      test "dresp shape" (fun () ->
          expect "[\"DResp\",[[\"Response\",401,[]],[\"DNull\"]]]"
          |> toEqual
               ( DResp (Response (401, []), DNull)
               |> Encoders.dval
               |> Js.Json.stringify )) ;
      describe "roundtrips" (fun () ->
          rtDval "int" (DInt 5) ;
          rtDval "obj" (DObj (StrDict.fromList [("foo", DInt 5)])) ;
          rtDval "date" (DDate "can be anything atm") ;
          rtDval "incomplete" (DIncomplete SourceNone) ;
          rtDval "float" (DFloat 7.2) ;
          rtDval "true" (DBool true) ;
          rtDval "false" (DBool false) ;
          rtDval "string" (DStr "incredibly this was broken") ;
          rtDval "null" DNull ;
          rtDval "errorrail" (DErrorRail (DInt 5)) ;
          rtDval "db" (DDB "Visitors") ;
          rtDval "list" (DList [|DDB "Visitors"; DInt 4|]) ;
          rtDval "redirect" (DResp (Redirect "/home", DNull)) ;
          rtDval "httpresponse" (DResp (Response (200, []), DStr "success")) ;
          ()) ;
      ()) ;
  ()
