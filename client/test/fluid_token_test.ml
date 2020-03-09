open Prelude
open Tester
open FluidToken

let run () =
  describe "analysisID of token" (fun () ->
      test "returns id of varBind if token is TLetVarName" (fun () ->
          let leftLetToken = TLetVarName (ID "1", ID "2", "a") in
          expect (analysisID leftLetToken |> ID.toString) |> toEqual "2") ;
      test
        "returns id of record field name if token is TRecordFieldname "
        (fun () ->
          let leftLetToken =
            TRecordFieldname
              {recordID = ID "1"; exprID = ID "2"; index = 1; fieldName = "name"}
          in
          expect (analysisID leftLetToken |> ID.toString) |> toEqual "2") ;
      test "return ids of" (fun () ->
          let lambdaVar =
            TLambdaVar (ID.fromString "1", ID.fromString "2", 1, "var")
          in
          expect (analysisID lambdaVar |> ID.toString) |> toEqual "2") ;
      ()) ;
  ()
