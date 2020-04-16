open Tester
open PrettyDocs

let run () =
  describe "Regex" (fun () ->
      test "exactly" (fun () ->
          expect (Regex.exactly ~re:"ok" "ok") |> toEqual true) ;
      test "captures has no matches" (fun () ->
          expect (Regex.captures ~re:(Regex.regex tagEx) "Hello") |> toEqual []) ;
      test "captures has matches" (fun () ->
          expect (Regex.captures ~re:(Regex.regex tagEx) "<type Option>")
          |> toEqual ["<type Option>"; ""; "type"; "Option"; ""]) ;
      test "captures code block" (fun () ->
          expect
            (Regex.captures
               ~re:(Regex.regex codeEx)
               "for example: {{let a = 1}}")
          |> toEqual
               ["for example: {{let a = 1}}"; "for example: "; "let a = 1"; ""]) ;
      ()) ;
  describe "PrettyDocs" (fun () ->
      test "convert_ catches invalid tags" (fun () ->
          expect (convert_ "<div contenteditable>")
          |> toEqual
               (ParseFail
                  [("<div contenteditable>", "'div' is not a valid tag type")])) ;
      test "convert_ catches nested tags" (fun () ->
          expect (convert_ "<type <var bad bunny>>")
          |> toEqual
               (ParseFail [("<type <var bad bunny>>", "contains nested tags")])) ;
      test "convert_ catches nested code blocks" (fun () ->
          expect (convert_ "{{Just {{ok}} }}")
          |> toEqual
               (ParseFail [("{{Just {{ok}} }}", "contains nested code blocks")])) ;
      test "converts tagged string" (fun () ->
          expect (convert "takes in <type Option>")
          |> toEqual [txt "takes in "; tag "type" [txt "Option"]]) ;
      test "converts normal string" (fun () ->
          expect (convert "Bye") |> toEqual [txt "Bye"]) ;
      test "converts code blocks" (fun () ->
          expect (convert "{{Ok <var value>}}")
          |> toEqual [tag "code" [txt "Ok "; tag "var" [txt "value"]]]) ;
      test "converts link tag" (fun () ->
          expect (convert "Into the [dark](http://www.darklang.com)")
          |> toEqual [txt "Into the "; link "dark" "http://www.darklang.com"]) ;
      test
        "converts string with multiple tags, a link, and a code block"
        (fun () ->
          expect
            (convert
               "Returns an <type Result>.\n It will got to [error rail](https://darklang.github.io/docs/error-handling#error-rail), if it is {{Error <var message>}}")
          |> toEqual
               [ txt "Returns an "
               ; tag "type" [txt "Result"]
               ; txt ".\n It will got to "
               ; link
                   "error rail"
                   "https://darklang.github.io/docs/error-handling#error-rail"
               ; txt ", if it is "
               ; tag "code" [txt "Error "; tag "var" [txt "message"]] ]) ;
      ()) ;
  ()
