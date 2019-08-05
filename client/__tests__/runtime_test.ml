open! Tc
open Jest
open Expect
open Runtime

let () =
  describe "validStringLiteral" (fun () ->
      let t name subject expected =
        test name (fun () ->
            expect (isValidDisplayString subject) |> toEqual expected )
      in
      t "newline" "\\n" true ;
      t "carriage return" "\\r" true ;
      t "tab" "\\t" true ;
      t "escaped backslash" "\\\\" true ;
      t "escaped quote" "\\\"" true ;
      t "naked backslash" "\\" false ;
      t "invalid backslashed char" "\\6" false ;
      t "many" "\\n\\t\\r\\\\\\\"" true ;
      () ) ;
  describe "convertLiteralStringToDisplay" (fun () ->
      let t name subject expected =
        test name (fun () ->
            expect (convertLiteralToDisplayString subject) |> toEqual expected
        )
      in
      t "newline" "\n" "\\n" ;
      t "newline2" "asd\nqwe" "asd\\nqwe" ;
      t "carriage return" "\r" "\\r" ;
      t "carriage return2" "asd\rqwe" "asd\\rqwe" ;
      t "tab" "\t" "\\t" ;
      t "tab2" "asd\tqwe" "asd\\tqwe" ;
      t "escaped backslash" "\\" "\\\\" ;
      t "escaped backslash2" "asd\\qwe" "asd\\\\qwe" ;
      t "escaped quote" "\"" "\\\"" ;
      t "escaped quote2" "asd\"qwe" "asd\\\"qwe" ;
      t
        "many"
        "asd\n\t\r\n\t\r\n\t\r\"\"\"qwe"
        "asd\\n\\t\\r\\n\\t\\r\\n\\t\\r\\\"\\\"\\\"qwe" ;
      () ) ;
  describe "convertDisplayStringToLiteral " (fun () ->
      let t name subject expected =
        test name (fun () ->
            expect (convertDisplayStringToLiteral subject) |> toEqual expected
        )
      in
      t "newline" "\\n" "\n" ;
      t "newline2" "asd\\nqwe" "asd\nqwe" ;
      t "carriage return" "\\r" "\r" ;
      t "carriage return2" "asd\\rqwe" "asd\rqwe" ;
      t "tab2" "\\t" "\t" ;
      t "tab2 " "asd\\tqwe" "asd\tqwe" ;
      t "escaped backslash" "\\\\" "\\" ;
      t "escaped backslash2" "asd\\\\qwe" "asd\\qwe" ;
      t "escaped quote" "\\\"" "\"" ;
      t "escaped quote2" "asd\\\"qwe" "asd\"qwe" ;
      () ) ;
  describe "pathFromInputVars" (fun () ->
      let noRequest = StrDict.empty in
      let noURL = StrDict.fromList [("request", Types.DObj StrDict.empty)] in
      let generate url =
        StrDict.fromList
          [("request", Types.DObj (StrDict.fromList [("url", Types.DStr url)]))]
      in
      test "returns None if no request object" (fun () ->
          expect (pathFromInputVars noRequest) |> toEqual None ) ;
      test "returns None if no url object" (fun () ->
          expect (pathFromInputVars noURL) |> toEqual None ) ;
      test "returns None if no url is parseable - numbers" (fun () ->
          expect (pathFromInputVars (generate "123456")) |> toEqual None ) ;
      test "returns None if no url is parseable - no slashes" (fun () ->
          expect (pathFromInputVars (generate "localhost")) |> toEqual None ) ;
      test "returns None if no url is parseable - no scheme" (fun () ->
          expect (pathFromInputVars (generate "//foobar.builwithdark.com"))
          |> toEqual None ) ;
      test "returns path with no query string" (fun () ->
          expect
            (pathFromInputVars
               (generate "https://foobar.builwithdark.com/hello"))
          |> toEqual (Some "/hello") ) ;
      test "returns path with query string" (fun () ->
          expect
            (pathFromInputVars
               (generate
                  "https://foobar.builwithdark.com/hello?foo=bar&baz=quux"))
          |> toEqual (Some "/hello?foo=bar&baz=quux") ) ) ;
  ()
