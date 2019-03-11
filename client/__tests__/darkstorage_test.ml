open! Tc
open Types
open Jest
open Expect
module Ds = DarkStorage

let originalList : staticDeploy list =
  [ { deployHash = "abc123"
    ; url = ""
    ; lastUpdate = "2019-01-02"
    ; status = Deployed }
  ; { deployHash = "def456"
    ; url = ""
    ; lastUpdate = "2019-01-01"
    ; status = Deployed }
  ; { deployHash = "xyz789"
    ; url = ""
    ; lastUpdate = "2019-01-03"
    ; status = Deploying } ]


let () =
  describe "appendDeploy" (fun () ->
      test "sort by lastUpdate timestamp" (fun () ->
          let sortedList =
            [ { deployHash = "xyz789"
              ; url = ""
              ; lastUpdate = "2019-01-03"
              ; status = Deploying }
            ; { deployHash = "abc123"
              ; url = ""
              ; lastUpdate = "2019-01-02"
              ; status = Deployed }
            ; { deployHash = "def456"
              ; url = ""
              ; lastUpdate = "2019-01-01"
              ; status = Deployed } ]
          in
          expect (Ds.appendDeploy [] originalList) |> toEqual sortedList ) ;
      test
        "de-dup by deployHash and choose the one with most recent lastUpdate"
        (fun () ->
          let newDeploy =
            { deployHash = "xyz789"
            ; url = ""
            ; lastUpdate = "2019-01-04"
            ; status = Deploying }
          in
          let mergedList =
            [ newDeploy
            ; { deployHash = "abc123"
              ; url = ""
              ; lastUpdate = "2019-01-02"
              ; status = Deployed }
            ; { deployHash = "def456"
              ; url = ""
              ; lastUpdate = "2019-01-01"
              ; status = Deployed } ]
          in
          expect (Ds.appendDeploy [newDeploy] originalList)
          |> toEqual mergedList ) ) ;
  ()
