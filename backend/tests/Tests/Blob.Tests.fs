/// Tests for the Blob Dval — see thinking/blobs-and-streams/.
///
/// This file grows across Phase 1 chunks. Chunk 1.2 covers the
/// ephemeral-blob byte-store on ExecutionState; later chunks extend it
/// with serialization roundtrips, promotion, and memory-bound
/// assertions.
module Tests.Blob

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes


/// Minimal ExecutionState suitable for exercising blob helpers
/// without needing a full test canvas.
let private freshState () : RT.ExecutionState =
  let builtins = localBuiltIns pmPT
  Exe.createState
    builtins
    pmRT
    Exe.noTracing
    (fun _ _ _ _ -> uply { return () })
    (fun _ _ _ _ -> uply { return () })
    PT.mainBranchId
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      dbs = Map.empty
      secrets = [] }


let ephemeralRoundtrip =
  testTask "ephemeral blob roundtrips bytes through the store" {
    let state = freshState ()
    let payload = [| 1uy; 2uy; 3uy; 4uy; 5uy |]

    let dv = Dval.newEphemeralBlob state payload

    match dv with
    | RT.DBlob(RT.Ephemeral _) -> ()
    | _ -> failtest $"expected DBlob(Ephemeral _), got {dv}"

    let ref =
      match dv with
      | RT.DBlob ref -> ref
      | _ -> failtest "unreachable"

    let! bytes = Dval.readBlobBytes state ref |> Ply.toTask
    Expect.equal bytes payload "roundtripped bytes match original"
  }


let twoEphemeralsAreDistinct =
  testTask "two ephemeral blobs with same bytes have distinct handles" {
    let state = freshState ()

    let dv1 = Dval.newEphemeralBlob state [| 7uy; 7uy; 7uy |]
    let dv2 = Dval.newEphemeralBlob state [| 7uy; 7uy; 7uy |]

    let ref1, ref2 =
      match dv1, dv2 with
      | RT.DBlob r1, RT.DBlob r2 -> r1, r2
      | _ -> failtest "expected DBlob for both"

    match ref1, ref2 with
    | RT.Ephemeral id1, RT.Ephemeral id2 ->
      Expect.notEqual id1 id2 "each mint gets a fresh uuid"
    | _ -> failtest "expected Ephemeral for both"

    let! b1 = Dval.readBlobBytes state ref1 |> Ply.toTask
    let! b2 = Dval.readBlobBytes state ref2 |> Ply.toTask
    Expect.equal b1 [| 7uy; 7uy; 7uy |] "first blob reads its bytes"
    Expect.equal b2 [| 7uy; 7uy; 7uy |] "second blob reads its bytes"
  }


let missingEphemeralRaises =
  testTask "reading an unknown ephemeral id raises" {
    let state = freshState ()
    let bogusRef = RT.Ephemeral(System.Guid.NewGuid())

    let mutable raised = false
    try
      let! _ = Dval.readBlobBytes state bogusRef |> Ply.toTask
      ()
    with _ ->
      raised <- true

    Expect.isTrue raised "expected an exception on missing ephemeral id"
  }


let tests =
  testList
    "blob"
    [ ephemeralRoundtrip; twoEphemeralsAreDistinct; missingEphemeralRaises ]
