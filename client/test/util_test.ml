open Tester
open Util
open ViewUtils.PrettyDocs

let run () =
  describe "Regex" (fun () ->
      let tagEx = "^\\<(\\w+)\\s(.+)\\>$" in
      test "exactly" (fun () ->
          expect (Regex.exactly ~re:"ok" "ok") |> toEqual true) ;
      test "captures has no matches" (fun () ->
          expect (Regex.captures ~re:(Regex.regex tagEx) "Hello") |> toEqual []) ;
      test "captures has matches" (fun () ->
          expect (Regex.captures ~re:(Regex.regex tagEx) "<type Option>")
          |> toEqual ["<type Option>"; "type"; "Option"]) ;
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
      ()) ;
  ()
