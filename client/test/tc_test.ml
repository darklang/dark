open Tester
open! Tc

let run () =
  describe "List" (fun () ->
      test "moveInto moves an element between two elements" (fun () ->
          expect ([0; 1; 2; 3; 4] |> List.moveInto ~oldPos:4 ~newPos:1)
          |> toEqual [0; 4; 1; 2; 3]) ;
      test "moveInto moves an element further down the list" (fun () ->
          expect
            ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.moveInto ~oldPos:1 ~newPos:6)
          |> toEqual [0; 2; 3; 4; 5; 1; 6; 7; 8; 9]) ;
      test "moveInto moves an element further up the list" (fun () ->
          expect
            ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.moveInto ~oldPos:6 ~newPos:1)
          |> toEqual [0; 6; 1; 2; 3; 4; 5; 7; 8; 9]) ;
      test "moveInto moves element to beginning of the list" (fun () ->
          expect ([0; 1; 2; 3; 4] |> List.moveInto ~oldPos:3 ~newPos:0)
          |> toEqual [3; 0; 1; 2; 4]) ;
      test "moveInto moves element to end of the list" (fun () ->
          expect ([0; 1; 2; 3; 4] |> List.moveInto ~oldPos:3 ~newPos:5)
          |> toEqual [0; 1; 2; 4; 3]) ;
      test "moveTo caps overflow" (fun () ->
          expect ([0; 1; 2; 3; 4] |> List.moveInto ~oldPos:2 ~newPos:7)
          |> toEqual [0; 1; 3; 4; 2])) ;
  ()
