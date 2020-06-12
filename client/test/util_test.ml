open Tester
open Util

let run () =
  describe "obscureString" (fun () ->
      test "string is empty" (fun () -> expect (obscureString "") |> toEqual "") ;
      test "string len = 3" (fun () ->
          expect (obscureString "ace") |> toEqual "XXX") ;
      test "string len = 5" (fun () ->
          expect (obscureString "alice") |> toEqual "XXXXe") ;
      test "string len = 8" (fun () ->
          expect (obscureString "abcd1234") |> toEqual "XXXXXX34") ;
      test "string len = 10" (fun () ->
          expect (obscureString "a1b2c3d4e5") |> toEqual "XXXXXXXXe5") ;
      test "string len = 16" (fun () ->
          expect (obscureString "abcd1234efgh0987")
          |> toEqual "XXXXXXXXXXXX0987") ;
      test "string len > 16" (fun () ->
          expect (obscureString "abc-123-def-456-ghi-789-xyz")
          |> toEqual "XXXXXXXXXXXXXXXXXXXXXXX-xyz") ;
      ()) ;
  describe "hideSecrets" (fun () ->
      let secrets = ["abc[123]-\\bunnies"; "XSUYD-JFKJWD-NKFADS"] in
      test "replaces a secret" (fun () ->
          expect (hideSecrets secrets "XSUYD-JFKJWD-NKFADS")
          |> toEqual "XXXXXXXXXXXXXXXFADS") ;
      test "replaces a in substring" (fun () ->
          expect (hideSecrets secrets "Bearer XSUYD-JFKJWD-NKFADS")
          |> toEqual "Bearer XXXXXXXXXXXXXXXFADS") ;
      test "replaces secret with regex like characters" (fun () ->
          expect (hideSecrets secrets "abc[123]-\\bunnies")
          |> toEqual "XXXXXXXXXXXXXnies") ;
      test "replaces all of the same secret" (fun () ->
          expect
            (hideSecrets
               secrets
               "{ \"token\" : \"XSUYD-JFKJWD-NKFADS\", \"auth\" : \"XSUYD-JFKJWD-NKFADS\" }")
          |> toEqual
               "{ \"token\" : \"XXXXXXXXXXXXXXXFADS\", \"auth\" : \"XXXXXXXXXXXXXXXFADS\" }") ;
      test "replaces multiple different secrets" (fun () ->
          expect
            (hideSecrets
               secrets
               "{ \"token\" : \"XSUYD-JFKJWD-NKFADS\", \"secret\" : \"abc[123]-\\bunnies\" }")
          |> toEqual
               "{ \"token\" : \"XXXXXXXXXXXXXXXFADS\", \"secret\" : \"XXXXXXXXXXXXXnies\" }")) ;
  ()
