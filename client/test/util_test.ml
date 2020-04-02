open Tester
open Prelude
open ViewUtils

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
      test "captures nested matches" (fun () -> 
        expect (Regex.captures ~re:(Regex.regex tagEx) "<cons Ok <var value>>")
        |> toEqual ["<cons Ok <var value>>"; "cons"; "Ok <var value>"]
      );
      ()) ;

  (* TODO(alice) mv to it's own test file *)
 describe "PrettyDocs" (fun () ->
    let span c t = Html.span [Html.class' c] [Html.text t] in
    test "converts tagged string" (fun ()->
      expect (PrettyDocs.convert "takes in <type Option>") |> toEqual 
      [Html.text "takes in "; Html.span [Html.class' "type"][Html.text "Option"]]
    );
    test "converts normal string" (fun ()->
      expect (PrettyDocs.convert "Bye") |> toEqual [Html.text "Bye"]
    );
    test "converts constructors" (fun () ->
      expect (PrettyDocs.convert "{Ok <var value>}")
      |> toEqual [Html.span [Html.class' "cons"] [
          Html.text "Ok "
        ; Html.span [Html.class' "var"] [Html.text "value"]
      ]]
    );
    test "converts string with multiple tags and a constructor" (fun () ->
      expect (PrettyDocs.convert "<return Returns> an <type Result>. If it is {Error <var message>}, then it will go to error rail")
      |> toEqual 
        [ span "return" "Returns"
        ; Html.text " an "
        ; span "type" "Result"
        ; Html.text ". If it is "
        ; Html.span [Html.class' "cons"] [Html.text "Error "; span "var" "message"]
        ; Html.text ", then it will go to error rail"
        ]
    );
    test "Dark Internal" (fun () ->
      let str = "<return Return> a <type list> of all user email addresses for non-admins and not in @darklang.com or @example.com" in
      expect(PrettyDocs.convert str) |> toEqual
        [ span "return" "Return"
        ; Html.text " a "
        ; span "type" "list"
        ; Html.text " of all user email addresses for non-admins and not in @darklang.com or @example.com"
        ]
    );
    ());
  ()
