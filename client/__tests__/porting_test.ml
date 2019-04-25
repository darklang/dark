open Tc
open Jest
open Expect

let () =
  describe "toOption" (fun () ->
      test "it returns None when the value equals a sentinel" (fun () ->
          expect (Option.toOption ~sentinel:3 3) |> toEqual None ) ;
      test
        "it returns (Some value) when the value does not equal the sentinel"
        (fun () ->
          expect (Option.toOption ~sentinel:(-1) 4) |> toEqual (Some 4) ) ) ;
  describe "String.dropRight" (fun () ->
      test
        "it returns the empty string when passed the empty string"
        (fun () -> expect (String.dropRight ~count:50 "") |> toEqual "" ) ;
      test "it returns the passed string when told to drop 0" (fun () ->
          expect (String.dropRight ~count:0 "foo") |> toEqual "foo" ) ;
      test
        "it returns the passed string when told to drop a negative number"
        (fun () -> expect (String.dropRight ~count:(-2) "foo") |> toEqual "foo"
      ) ;
      test
        "it drops the correct number of items when the number < length"
        (fun () -> expect (String.dropRight ~count:2 "foo") |> toEqual "f" ) ;
      test
        "it returns the empty string when told to drop a number == length"
        (fun () -> expect (String.dropRight ~count:3 "foo") |> toEqual "" ) ;
      test
        "it returns the empty string when told to drop a number > length"
        (fun () -> expect (String.dropRight ~count:5555 "foo") |> toEqual "" ) ;
      () ) ;
  ()
