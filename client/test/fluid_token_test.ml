open Tester
open! Tc
open Types
open FluidToken

let run () =
  describe "analysisID of token" (fun () ->
      test "returns id of varBind if token is TLetLHS" (fun () ->
          let leftLetToken = TLetLHS (ID "1", ID "2", "a") in
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
