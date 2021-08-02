open Prelude
open Tester
open CurlCommand
module B = BlankOr

let defaultTLID = TLID.fromString "7"

let http ~(path : string) ?(meth = "GET") () : handler =
  { ast = FluidAST.ofExpr (EBlank (gid ()))
  ; hTLID = defaultTLID
  ; pos = {x = 0; y = 0}
  ; spec = {space = B.newF "HTTP"; name = B.newF path; modifier = B.newF meth}
  }


(* Sets the model with the appropriate toplevels *)
let makeModel ?(handlers = []) ?(traces = Map.String.empty) ~cursorState () :
    model =
  let default = Defaults.defaultModel in
  { default with
    handlers = Handlers.fromList handlers
  ; canvasName = "test-curl"
  ; cursorState
  ; traces }


let run () =
  describe "strAsBodyCurl" (fun () ->
      test "returns jsonfied curl flag" (fun () ->
          expect (strAsBodyCurl (DStr "{\"a\":1,\"b\":false}"))
          |> toEqual (Some "-d '{\"a\":1,\"b\":false}'")) ;
      test "returns None if input dval is not DObj" (fun () ->
          expect (strAsBodyCurl DNull) |> toEqual None)) ;
  describe "objAsHeaderCurl" (fun () ->
      test "returns header curl flag string" (fun () ->
          let obj =
            Dval.obj
              [ ("Content-Type", DStr "application/json")
              ; ("Authorization", DStr "Bearer abc123") ]
          in
          expect (objAsHeaderCurl obj)
          |> toEqual
               (Some
                  "-H 'Authorization:Bearer abc123' -H 'Content-Type:application/json'")) ;
      test "returns None if input dval is not DObj" (fun () ->
          expect (objAsHeaderCurl DNull) |> toEqual None)) ;
  describe "curlFromSpec" (fun () ->
      let m =
        makeModel
          ~handlers:[http ~path:"/test" ()]
          ~cursorState:(Selecting (defaultTLID, None))
          ()
      in
      test "returns command for /test GET" (fun () ->
          expect (curlFromSpec m defaultTLID)
          |> toEqual (Some "curl http://test-curl.builtwithdark.com/test")) ;
      test "returns command in https if env=prod" (fun () ->
          let m1 = {m with environment = "production"} in
          expect (curlFromSpec m1 defaultTLID)
          |> toEqual (Some "curl https://test-curl.builtwithdark.com/test")) ;
      test "returns None if tlid not found" (fun () ->
          expect (curlFromSpec m (TLID.fromString "1")) |> toEqual None) ;
      test "returns None for non-HTTP handlers" (fun () ->
          let cronTLID = TLID.fromString "2" in
          let cron =
            { ast = FluidAST.ofExpr (EBlank (gid ()))
            ; hTLID = cronTLID
            ; pos = {x = 0; y = 0}
            ; spec =
                { space = B.newF "CRON"
                ; name = B.newF "cleanKitchen"
                ; modifier = B.newF "Fortnightly" } }
          in
          let m1 = {m with handlers = Handlers.fromList [cron]} in
          expect (curlFromSpec m1 cronTLID) |> toEqual None)) ;
  describe "curlFromCurrentTrace" (fun () ->
      let traces input =
        Map.String.empty
        |> Map.add
             ~key:"7"
             ~value:
               [ ( "123"
                 , Ok
                     { input
                     ; timestamp = "2019-09-17T12:00:30Z"
                     ; functionResults = [] } ) ]
      in
      test "returns command for /test GET with headers" (fun () ->
          let headers =
            Dval.obj
              [ ("Content-Type", DStr "application/json")
              ; ("Authorization", DStr "Bearer abc123") ]
          in
          let input =
            Belt.Map.String.empty
            |. Belt.Map.String.set
                 "request"
                 (Dval.obj
                    [ ("body", DNull)
                    ; ("headers", headers)
                    ; ("url", DStr "http://test-curl.builtwithdark.com/test") ])
          in
          let m =
            makeModel
              ~handlers:[http ~path:"/test" ()]
              ~traces:(traces input)
              ~cursorState:(Selecting (defaultTLID, None))
              ()
          in
          expect (curlFromCurrentTrace m defaultTLID)
          |> toEqual
               (Some
                  "curl -H 'Authorization:Bearer abc123' -H 'Content-Type:application/json' -X GET 'http://test-curl.builtwithdark.com/test'")) ;
      test "returns command for /test POST with body" (fun () ->
          let input =
            Belt.Map.String.set
              Belt.Map.String.empty
              "request"
              (Dval.obj
                 [ ("fullBody", DStr "{\"a\":1,\"b\":false}")
                 ; ("headers", DNull)
                 ; ("url", DStr "http://test-curl.builtwithdark.com/test") ])
          in
          let m =
            makeModel
              ~handlers:[http ~path:"/test" ~meth:"POST" ()]
              ~traces:(traces input)
              ~cursorState:(Selecting (defaultTLID, None))
              ()
          in
          expect (curlFromCurrentTrace m defaultTLID)
          |> toEqual
               (Some
                  "curl -d '{\"a\":1,\"b\":false}' -X POST 'http://test-curl.builtwithdark.com/test'"))) ;
  ()
