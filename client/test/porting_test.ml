open Prelude
open Tester

let run () =
  describe "String.dropRight" (fun () ->
      test "it returns the empty string when passed the empty string" (fun () ->
          expect (String.dropRight ~count:50 "") |> toEqual "") ;
      test "it returns the passed string when told to drop 0" (fun () ->
          expect (String.dropRight ~count:0 "foo") |> toEqual "foo") ;
      test
        "it returns the passed string when told to drop a negative number"
        (fun () -> expect (String.dropRight ~count:(-2) "foo") |> toEqual "foo") ;
      test
        "it drops the correct number of items when the number < length"
        (fun () -> expect (String.dropRight ~count:2 "foo") |> toEqual "f") ;
      test
        "it returns the empty string when told to drop a number == length"
        (fun () -> expect (String.dropRight ~count:3 "foo") |> toEqual "") ;
      test
        "it returns the empty string when told to drop a number > length"
        (fun () -> expect (String.dropRight ~count:5555 "foo") |> toEqual "") ;
      ()) ;
  ()
