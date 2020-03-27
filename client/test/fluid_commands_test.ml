open Tester
open Prelude
open Fluid_test_data

let run () =
  describe "commandsFor" (fun () ->
      let hasCmd name expr =
        FluidCommands.commandsFor defaultTestModel expr
        |> List.find ~f:(fun (c : Types.command) -> c.commandName = name)
        |> Option.isSome
      in
      let expectCmd name expr = expect (hasCmd name expr) |> toEqual true in
      let expectNoCmd name expr = expect (hasCmd name expr) |> toEqual false in
      test "has put on rail for railable NoRail function" (fun () ->
          expectCmd "put-function-on-rail" aRailableFnCall) ;
      test "no put on rail for non-railable NoRail function" (fun () ->
          expectNoCmd "put-function-on-rail" aFnCall) ;
      test "has take off rail for Rail function" (fun () ->
          expectCmd "take-function-off-rail" aOnRailFnCall) ;
      test "no put on rail for Rail function" (fun () ->
          expectNoCmd "put-function-on-rail" aOnRailFnCall) ;
      test "no take off rail for NoRail function" (fun () ->
          expectNoCmd "take-function-off-rail" aFnCall) ;
      test "no copy as curl for normal function" (fun () ->
          expectNoCmd "copy-request-as-curl" aFnCall) ;
      test "has copy as curl for Http function" (fun () ->
          let expr =
            FluidExpression.EFnCall
              (gid (), "HttpClient::get", [aStr; emptyRecord; emptyRecord], Rail)
          in
          expectCmd "copy-request-as-curl" expr) ;
      ())
