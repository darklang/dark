open Tester
open Util

let run () =
  describe "obscureString" (fun () ->
      test "string len < n" (fun () ->
          expect (obscureString ~n:4 "ace") |> toEqual "XXX") ;
      test "string len > n" (fun () ->
          expect (obscureString ~n:4 "abc-123-def-456-ghi-789-xyz")
          |> toEqual "XXXXXXXXXXXXXXXXXXXXXXX-xyz") ;
      ()) ;
  ()
