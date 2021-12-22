module TestUtils.FSI

// This isn't really for tests, it's for utilities to tests things in FSI. I
// didn't have a better place for this.


open System.Threading.Tasks
open FSharp.Control.Tasks

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module DvalRepr = LibExecution.DvalRepr
module OCamlInterop = LibBackend.OCamlInterop
module Exe = LibExecution.Execution

open Prelude

let execute (code : string) : RT.Dval =
  let t =
    task {
      let! owner = TestUtils.testOwner.Force()
      let! state = TestUtils.executionStateFor owner "fsi" Map.empty Map.empty
      let prog = FSharpToExpr.parseRTExpr code
      return! Exe.executeExpr state Map.empty prog
    }

  Task.WaitAll [| t :> Task |]
  t.Result

let executeOCaml (code : string) : RT.Dval =
  let t =
    task {
      let! owner = TestUtils.testOwner.Force()
      let prog = FSharpToExpr.parsePTExpr code
      let! state = TestUtils.executionStateFor owner "fsi" Map.empty Map.empty
      let accountID = state.program.accountID
      let canvasID = state.program.canvasID
      return! OCamlInterop.execute accountID canvasID prog Map.empty [] []
    }

  Task.WaitAll [| t :> Task |]
  t.Result


let toBytes (dv : RT.Dval) : string =
  dv |> string |> UTF8.toBytes |> System.BitConverter.ToString
