open Tester
open Util
open ViewUtils.PrettyDocs

let run () =
  describe "Regex" (fun () ->
      test "exactly" (fun () ->
          expect (Regex.exactly ~re:"ok" "ok") |> toEqual true) ;
      test "captures has no matches" (fun () ->
          expect (Regex.captures ~re:(Regex.regex tagEx) "Hello") |> toEqual []) ;
      test "captures has matches" (fun () ->
          expect (Regex.captures ~re:(Regex.regex tagEx) "<type Option>")
          |> toEqual ["<type Option>"; ""; "type"; "Option"; ""]) ;
      test "captures {code block}" (fun () ->
          expect
            (Regex.captures ~re:(Regex.regex codeEx) "for example: {let a = 1}")
          |> toEqual
               ["for example: {let a = 1}"; "for example: "; "let a = 1"; ""]) ;
      ()) ;
  describe "PrettyDocs" (fun () ->
      test "converts tagged string" (fun () ->
          expect (convert "takes in <type Option>")
          |> toEqual [txt "takes in "; tag "type" [txt "Option"]]) ;
      test "converts normal string" (fun () ->
          expect (convert "Bye") |> toEqual [txt "Bye"]) ;
      test "converts constructors" (fun () ->
          expect (convert "{Ok <var value>}")
          |> toEqual [tag "code" [txt "Ok "; tag "var" [txt "value"]]]) ;
      test "converts string with multiple tags and a constructor" (fun () ->
          expect
            (convert
               "<return Returns> an <type Result>. If it is {Error <var message>}, then it will go to error rail")
          |> toEqual
               [ tag "return" [txt "Returns"]
               ; txt " an "
               ; tag "type" [txt "Result"]
               ; txt ". If it is "
               ; tag "code" [txt "Error "; tag "var" [txt "message"]]
               ; txt ", then it will go to error rail" ]) ;
      test
        "captures will not accept the tag-name code, and render it as plain text"
        (fun () -> expect (convert "<code cookies>") |> toEqual [txt "cookies"]) ;
      test "validates against nested tags" (fun () ->
          expect (validate "<type some <var a>>") |> toEqual false) ;
      test "validates against nested code blocks" (fun () ->
          expect (validate "{some code {more code}}") |> toEqual false) ;
      ()) ;
  ()
