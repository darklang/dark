open! Porting
open Types
open Autocomplete
open Prelude
open Jest
open Expect

let () =
  describe "toOption" (fun () ->
    test "it returns None when the value equals a sentinel" (fun () ->
      expect (toOption ~sentinel:3 3) |> toEqual (None)
    );
    test "it returns (Some value) when the value does not equal the sentinel" (fun () ->
      expect (toOption ~sentinel:(-1) 4) |> toEqual (Some 4)
    );
  );
  ()
