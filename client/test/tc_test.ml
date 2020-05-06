open Tester
open! Tc

let run () =
  describe "List" (fun () ->
      test "moveInto" (fun () ->
          expect ([1; 2; 3; 4] |> List.moveInto ~oldPos:3 ~newPos:1)
          |> toEqual [1; 4; 2; 3])) ;
  ()
