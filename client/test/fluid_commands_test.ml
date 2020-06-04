open Tester
open Prelude
open Fluid_test_data

let makeTL ast =
  TLHandler
    { ast
    ; spec =
        { space = Blank (gid ())
        ; name = Blank (gid ())
        ; modifier = Blank (gid ()) }
    ; hTLID = TLID.fromString "7"
    ; pos = Defaults.origin }


let run () =
  describe "commandsFor" (fun () ->
      let hasCmd (name : string) (expr : FluidExpression.t) (ast : FluidAST.t) =
        let tl = makeTL ast in
        FluidCommands.commandsFor defaultTestModel tl expr
        |> List.find ~f:(fun (c : Types.command) -> c.commandName = name)
        |> Option.isSome
      in
      let expectCmd name expr =
        expect (hasCmd name expr (FluidAST.ofExpr expr)) |> toEqual true
      in
      let expectNoCmd name expr =
        expect (hasCmd name expr (FluidAST.ofExpr expr)) |> toEqual false
      in
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
      test "has add flag for AST without flag" (fun () ->
          expectCmd "add-feature-flag" aFnCall) ;
      test "no add flag for AST with existing flag" (fun () ->
          expectNoCmd "add-feature-flag" letWithflagBody) ;
      test "has discard+commit flag for target expr inside flag" (fun () ->
          let targetExpr = oneCharStr in
          let ast =
            FluidExpression.ELet (gid (), "a", aShortInt, flagOld targetExpr)
            |> FluidAST.ofExpr
          in
          let hasDiscard = hasCmd "discard-feature-flag" targetExpr ast in
          let hasCommit = hasCmd "commit-feature-flag" targetExpr ast in
          expect (hasDiscard, hasCommit) |> toEqual (true, true)) ;
      test "no discard or commit flag for target expr outside flag" (fun () ->
          let targetExpr = aShortInt in
          let ast =
            FluidExpression.ELet (gid (), "a", targetExpr, flagOld oneCharStr)
            |> FluidAST.ofExpr
          in
          let hasDiscard = hasCmd "discard-feature-flag" targetExpr ast in
          let hasCommit = hasCmd "commit-feature-flag" targetExpr ast in
          expect (hasDiscard, hasCommit) |> toEqual (false, false)) ;
      test "has convert to if for if expression" (fun () ->
          expectCmd "convert-if-to-match" emptyIf) ;
      test "no convert to if for match expression" (fun () ->
          expectNoCmd "convert-if-to-match" emptyMatch) ;
      ())
