open Tester
open! Tc
open Util

let run () =
  describe "Regex" (fun () ->
      let tagEx = Regex.regex "^\\((\\w+)\\s(.+)\\)$" in
      test "exactly" (fun () ->
          expect (Regex.exactly ~re:"ok" "ok") |> toEqual true) ;
      test "captures has no matches" (fun () ->
          expect (Regex.captures ~re:tagEx "Hello") |> toEqual []) ;
      test "captures has matches" (fun () ->
          expect (Regex.captures ~re:tagEx "(type Option)")
          |> toEqual ["(type Option)"; "type"; "Option"]) ;
      ()) ;
  ()
