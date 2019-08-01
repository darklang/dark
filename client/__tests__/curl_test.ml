open! Tc
open Types
open Jest
open Expect
open Curl
module B = Blank

let defaultTLID = TLID "7"

let http ~(path : string) ?(meth = "GET") () : handler =
  { ast = B.new_ ()
  ; hTLID = defaultTLID
  ; pos = {x = 0; y = 0}
  ; spec =
      { space = Blank.newF "HTTP"
      ; name = Blank.newF path
      ; modifier = Blank.newF meth } }


(* Sets the model with the appropriate toplevels *)
let makeModel ?(handlers = []) ?(traces = StrDict.empty) ~cursorState () :
    model =
  let default = Defaults.defaultModel in
  { default with
    handlers = Handlers.fromList handlers
  ; canvasName = "test-curl"
  ; cursorState
  ; traces }


let () =
  describe "objAsJsonCurl" (fun () ->
      test "returns jsonfied curl flag" (fun () ->
          let dict =
            StrDict.empty
            |> StrDict.insert ~key:"a" ~value:(DInt 1)
            |> StrDict.insert ~key:"b" ~value:(DBool false)
          in
          expect (objAsJsonCurl (DObj dict))
          |> toEqual (Some "-d '{\"a\":1,\"b\":false}'") ) ;
      test "returns None if input dval is not DObj" (fun () ->
          expect (objAsJsonCurl DNull) |> toEqual None ) ) ;
  describe "objAsHeaderCurl" (fun () ->
      test "returns header curl flag string" (fun () ->
          let dict =
            StrDict.empty
            |> StrDict.insert
                 ~key:"Content-Type"
                 ~value:(DStr "application/json")
            |> StrDict.insert
                 ~key:"Authorization"
                 ~value:(DStr "Bearer abc123")
          in
          expect (objAsHeaderCurl (DObj dict))
          |> toEqual
               (Some
                  "-H 'Authorization:Bearer abc123' -H 'Content-Type:application/json'")
      ) ;
      test "returns None if input dval is not DObj" (fun () ->
          expect (objAsHeaderCurl DNull) |> toEqual None ) ) ;
  describe "curlFromSpec" (fun () ->
      let m =
        makeModel
          ~handlers:[http ~path:"/test" ()]
          ~cursorState:(Selecting (defaultTLID, None))
          ()
      in
      test "returns command for /test GET" (fun () ->
          expect (curlFromSpec m defaultTLID)
          |> toEqual
               (Some "curl -X GET http://test-curl.builtwithdark.com/test") ) ;
      test "returns command in https if env=prod" (fun () ->
          let m1 = {m with environment = "production"} in
          expect (curlFromSpec m1 defaultTLID)
          |> toEqual
               (Some "curl -X GET https://test-curl.builtwithdark.com/test") ) ;
      test "returns None if tlid not found" (fun () ->
          expect (curlFromSpec m (TLID "1")) |> toEqual None ) ;
      test "returns None for non-HTTP handlers" (fun () ->
          let cronTLID = TLID "2" in
          let cron =
            { ast = B.new_ ()
            ; hTLID = cronTLID
            ; pos = {x = 0; y = 0}
            ; spec =
                { space = Blank.newF "CRON"
                ; name = Blank.newF "cleanKitchen"
                ; modifier = Blank.newF "Fortnightly" } }
          in
          let m1 = {m with handlers = Handlers.fromList [cron]} in
          expect (curlFromSpec m1 cronTLID) |> toEqual None ) ) ;
  describe "curlFromCurrentTrace" (fun () ->
      let traces input =
        StrDict.empty
        |> StrDict.insert
             ~key:"7"
             ~value:
               [ ( "123"
                 , Some
                     { input
                     ; timestamp = "2019-09-17T12:00:30Z"
                     ; functionResults = [] } ) ]
      in
      test "returns command for /test GET with headers" (fun () ->
          let headers =
            StrDict.empty
            |> StrDict.insert
                 ~key:"Content-Type"
                 ~value:(DStr "application/json")
            |> StrDict.insert
                 ~key:"Authorization"
                 ~value:(DStr "Bearer abc123")
          in
          let input =
            StrDict.empty
            |> StrDict.insert
                 ~key:"request"
                 ~value:
                   (DObj
                      ( StrDict.empty
                      |> StrDict.insert ~key:"body" ~value:DNull
                      |> StrDict.insert ~key:"headers" ~value:(DObj headers)
                      |> StrDict.insert
                           ~key:"url"
                           ~value:
                             (DStr "http://test-curl.builtwithdark.com/test")
                      ))
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
                  "curl -H 'Authorization:Bearer abc123' -H 'Content-Type:application/json' -X GET http://test-curl.builtwithdark.com/test")
      ) ;
      test "returns command for /test POST with body" (fun () ->
          let body =
            StrDict.empty
            |> StrDict.insert ~key:"a" ~value:(DInt 1)
            |> StrDict.insert ~key:"b" ~value:(DBool false)
          in
          let input =
            StrDict.empty
            |> StrDict.insert
                 ~key:"request"
                 ~value:
                   (DObj
                      ( StrDict.empty
                      |> StrDict.insert ~key:"body" ~value:(DObj body)
                      |> StrDict.insert ~key:"headers" ~value:DNull
                      |> StrDict.insert
                           ~key:"url"
                           ~value:
                             (DStr "http://test-curl.builtwithdark.com/test")
                      ))
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
                  "curl -d '{\"a\":1,\"b\":false}' -X POST http://test-curl.builtwithdark.com/test")
      ) ) ;
  ()
