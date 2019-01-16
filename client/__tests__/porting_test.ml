open! Porting
open Jest
open Expect

let () =
  describe "toOption" (fun () ->
      test "it returns None when the value equals a sentinel" (fun () ->
          expect (toOption ~sentinel:3 3) |> toEqual None ) ;
      test
        "it returns (Some value) when the value does not equal the sentinel"
        (fun () -> expect (toOption ~sentinel:(-1) 4) |> toEqual (Some 4) ) ) ;
  describe "String.dropRight" (fun () ->
      test
        "it returns the empty string when passed the empty string"
        (fun () -> expect (String.dropRight 50 "") |> toEqual "" ) ;
      test "it returns the passed string when told to drop 0" (fun () ->
          expect (String.dropRight 0 "foo") |> toEqual "foo" ) ;
      test
        "it returns the passed string when told to drop a negative number"
        (fun () -> expect (String.dropRight (-2) "foo") |> toEqual "foo" ) ;
      test
        "it drops the correct number of items when the number < length"
        (fun () -> expect (String.dropRight 2 "foo") |> toEqual "f" ) ;
      test
        "it returns the empty string when told to drop a number == length"
        (fun () -> expect (String.dropRight 3 "foo") |> toEqual "" ) ;
      test
        "it returns the empty string when told to drop a number > length"
        (fun () -> expect (String.dropRight 5555 "foo") |> toEqual "" ) ) ;
  describe "List.replace" (fun () ->
      let initList = [0 ; 1 ; 1 ; 2 ; 3 ; 5 ; 8 ; 13] in
      test
        "it returns original list if seach conditions is not satisfied"
        (fun () ->
          let finderFn = (fun x -> x = 7)
          and replaceFn = (fun x -> x*2) in
          expect(List.replace finderFn replaceFn initList) |> toEqual initList
        );
      test
        "It returns modified list if search search conditions are satisfied"
        (fun () ->
          let finderFn = (fun x -> x = 3)
          and replaceFn = (fun x -> x*2)
          and expectedResult = [0 ; 1 ; 1 ; 2 ; 6 ; 5 ; 8 ; 13] in
          expect(List.replace finderFn replaceFn initList) |> toEqual expectedResult
        );
  );
  ()
