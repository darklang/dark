open! Tc
open Types
open Jest
open Expect
module Ds = DarkStorage

let d2 =
  { deployHash = "abc123"
  ; url = ""
  ; lastUpdate = Js.Date.fromString "2019-01-02"
  ; status = Deployed }


let d1 =
  { deployHash = "def456"
  ; url = ""
  ; lastUpdate = Js.Date.fromString "2019-01-01"
  ; status = Deployed }


let d3 =
  { deployHash = "xyz789"
  ; url = ""
  ; lastUpdate = Js.Date.fromString "2019-01-03"
  ; status = Deploying }


let originalList : staticDeploy list = [d2; d1; d3]

let () =
  describe "appendDeploy" (fun () ->
      test "sort by lastUpdate timestamp" (fun () ->
          let sortedList = [d3; d2; d1] in
          expect (Ds.appendDeploy [] originalList) |> toEqual sortedList ) ;
      test
        "de-dup by deployHash and choose the one with most recent lastUpdate"
        (fun () ->
          let newDeploy =
            { deployHash = "xyz789"
            ; url = ""
            ; lastUpdate = Js.Date.fromString "2019-01-04"
            ; status = Deploying }
          in
          let mergedList = [newDeploy; d2; d1] in
          expect (Ds.appendDeploy [newDeploy] originalList)
          |> toEqual mergedList ) ) ;
  ()
