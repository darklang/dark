/// Tests for the Stream Dval — see thinking/blobs-and-streams/.
///
/// Chunk 2.2: pull-only FromIO streams. Later chunks add transforms
/// (Mapped/Filtered/Take/Concat), drain helpers (toList/toBlob), and
/// disposal.
module Tests.Stream

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType


/// Build a stream backed by an in-memory list. Each `next` call pulls
/// the head off a mutable ref cell. Simple scaffolding for the pull
/// semantics; real IO-backed streams arrive in chunk 2.8 (HttpClient).
let private streamOfList
  (items : List<RT.Dval>)
  (elemType : RT.ValueType)
  : RT.Dval =
  let remaining = ref items
  let next () : Ply<Option<RT.Dval>> =
    uply {
      match remaining.Value with
      | head :: tail ->
        remaining.Value <- tail
        return Some head
      | [] -> return None
    }
  Dval.newStream elemType next


let pullsElementsInOrder =
  testTask "stream: pull returns elements in order then None" {
    let items = [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ]
    let s = streamOfList items VT.int64

    let! first = Dval.readStreamNext s |> Ply.toTask
    let! second = Dval.readStreamNext s |> Ply.toTask
    let! third = Dval.readStreamNext s |> Ply.toTask
    let! fourth = Dval.readStreamNext s |> Ply.toTask

    Expect.equal first (Some(RT.DInt64 1L)) "first pull = 1"
    Expect.equal second (Some(RT.DInt64 2L)) "second pull = 2"
    Expect.equal third (Some(RT.DInt64 3L)) "third pull = 3"
    Expect.equal fourth None "fourth pull = None (exhausted)"
  }


let staysDrainedAfterExhaustion =
  testTask "stream: subsequent pulls after exhaustion keep returning None" {
    let s = streamOfList [ RT.DInt64 42L ] VT.int64

    let! first = Dval.readStreamNext s |> Ply.toTask
    let! afterExhaust = Dval.readStreamNext s |> Ply.toTask
    let! stillDone = Dval.readStreamNext s |> Ply.toTask

    Expect.equal first (Some(RT.DInt64 42L)) "first pull = 42"
    Expect.equal afterExhaust None "pull past end = None"
    Expect.equal stillDone None "still None on further pulls"
  }


let emptyStreamImmediatelyDone =
  testTask "stream: empty FromIO returns None on first pull" {
    let s = streamOfList [] VT.int64
    let! result = Dval.readStreamNext s |> Ply.toTask
    Expect.equal result None "empty stream = None"
  }


let toValueTypeReflectsElemType =
  test "stream: Dval.toValueType returns KTStream of the element type" {
    let s = streamOfList [ RT.DString "hi" ] VT.string
    let vt = RT.Dval.toValueType s
    Expect.equal
      vt
      (RT.ValueType.Known(RT.KTStream VT.string))
      "toValueType reflects the elem type"
  }


let tests =
  testList
    "stream"
    [ pullsElementsInOrder
      staysDrainedAfterExhaustion
      emptyStreamImmediatelyDone
      toValueTypeReflectsElemType ]
