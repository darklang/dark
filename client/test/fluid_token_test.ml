open Prelude
open Tester
open FluidToken
open FluidShortcuts

let toString = FluidPrinter.eToTestString

let run () =
  describe "tokenization of unexpected exprs" (fun () ->
      test "let in a binop lhs" (fun () ->
          let ast = binop "+" (let' "x" (int 5) (int 1)) (int 2) in
          expect (toString ast) |> toEqual "let x = 5\n1\n+ 2") ;
      ()) ;
  describe "analysisID of token" (fun () ->
      test "returns id of varBind if token is TLetVarName" (fun () ->
          let leftLetToken = TLetVarName (ID "1", ID "2", "a") in
          expect (analysisID leftLetToken) |> toEqual (ID "2")) ;
      test
        "returns id of record field name if token is TRecordFieldname "
        (fun () ->
          let leftLetToken =
            TRecordFieldname
              {recordID = ID "1"; exprID = ID "2"; index = 1; fieldName = "name"}
          in
          expect (analysisID leftLetToken) |> toEqual (ID "2")) ;
      test "return ids of" (fun () ->
          let lambdaVar = TLambdaVar (ID "1", ID "2", 1, "var") in
          expect (analysisID lambdaVar) |> toEqual (ID "2")) ;
      ()) ;
  ()
