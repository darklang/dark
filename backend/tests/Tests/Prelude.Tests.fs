module Tests.Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils


let asyncTests =

  // slow it down so later items might be run first
  let delay (f : unit -> 'a) (i : int) : Ply<'a> =
    uply {
      do! Task.Delay(100 - (i * 10))
      return (f ())
    }

  testList
    "sequential"
    [ testTask "mapSequentially" {
        let fn (i : int) = delay (fun () -> i + 1) i
        let! result = Ply.List.mapSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result [ 2; 3; 4; 5 ] ""
      }
      testTask "filterSequentially" {
        let fn (i : int) = uply { return (i % 2) = 0 }
        let! result = Ply.List.filterSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result [ 2; 4 ] ""
      }
      testTask "findSequentially" {
        let fn (i : int) = delay (fun () -> i = 3) i
        let! result = Ply.List.findSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result (Some 3) ""
      }
      testTask "iterSequentially" {
        let mutable state = []
        let fn (i : int) = delay (fun () -> state <- i + 1 :: state) i
        do! Ply.List.iterSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal state [ 5; 4; 3; 2 ] ""
      } ]

let mapTests =
  testList
    "map"
    [ testMany2
        "Map.mergeFavoringRight"
        Map.mergeFavoringRight
        [ Map.empty, Map.empty, Map.empty
          (Map.ofList [ (1, 1); (2, 2); (3, 3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ]) ]
      testMany2
        "Map.mergeFavoringLeft"
        Map.mergeFavoringLeft
        [ Map.empty, Map.empty, Map.empty
          (Map.ofList [ (1, 1); (2, 2); (3, 3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ],
           Map.ofList [ (1, 1); (2, 2); (3, 3) ]) ] ]

let floatTests =
  testList
    "Float"
    [ testMany
        "readFloat"
        readFloat
        [ -0.0, (Negative, "0", "0")
          0.0, (Positive, "0", "0")
          82.10, (Positive, "82", "099999999999994315658113919198513031005859375")
          -180.0, (Negative, "180", "0") ] ]

let dateTests =
  testList
    "DateTime"
    [ testMany
        "toIsoString"
        (fun (i : NodaTime.Instant) -> i.toIsoString ())
        [ NodaTime.Instant.ofUtcInstant (2000, 10, 1, 16, 1, 1),
          "2000-10-01T16:01:01Z" ]
      testMany
        "ofIsoString"
        NodaTime.Instant.ofIsoString
        [ "2000-10-01T16:01:01Z",
          NodaTime.Instant.ofUtcInstant (2000, 10, 1, 16, 1, 1) ] ]

let assertions =
  testList
    "Assertions"
    [ test "assertFn" { assertFn "msg" System.Double.IsFinite 6.0 }
      test "assertFn2" { assertFn2 "msg" String.contains "x" "xxx" }
      test "assertEq" { assertEq "msg" "x" "x" }
      test "assertIn" { assertIn "msg" [ "x" ] "x" }
      test "assert_" { assert_ "_" [] true } ]


/// Every `Ply.List` sequential helper, over a list far longer than anything a fold-based version survived.
///
/// `flatten` and `foldSequentially` were the ones that actually went down: a fold there builds one nested
/// continuation per element and unwinds only at the end, and a .NET stack overflow cannot be caught, so the
/// process disappears. That is how the relay died decoding ~10,800 ops.
///
/// Worth being precise about what this pins and what it does not. It asserts these helpers are CORRECT and
/// order-preserving at 100k. It does NOT reproduce the crash: reverting `iterSequentially` to its fold form
/// still passes here, because awaiting an already-completed Ply does not deepen the stack the way building
/// a result chain does. The uniform iterative shape is the invariant; the crash is only reachable by some
/// of them.
let deepRecursion =
  let big = List.init 100_000 (fun i -> i)

  testList
    "Ply.List handles lists too deep for a fold"
    [ testTask "mapSequentially" {
        let! r = Ply.List.mapSequentially (fun i -> Ply(i + 1)) big |> Ply.toTask
        Expect.equal (List.length r) 100_000 "mapped every element"
      }
      testTask "filterSequentially" {
        let! r = Ply.List.filterSequentially (fun i -> Ply(i % 2 = 0)) big |> Ply.toTask
        Expect.equal (List.length r) 50_000 "kept the evens, in order"
        Expect.equal (Seq.head r) 0 "order preserved"
      }
      testTask "filterMapSequentially" {
        let! r =
          Ply.List.filterMapSequentially
            (fun i -> Ply(if i % 10 = 0 then Some i else None))
            big
          |> Ply.toTask
        Expect.equal (List.length r) 10_000 "kept every tenth, in order"
        Expect.equal (Seq.head r) 0 "order preserved"
      }
      testTask "iterSequentially" {
        let mutable n = 0
        do! Ply.List.iterSequentially (fun _ -> uply { n <- n + 1 }) big |> Ply.toTask
        Expect.equal n 100_000 "visited every element"
      }
      testTask "findSequentially" {
        let! r = Ply.List.findSequentially (fun i -> Ply((i = 99_999))) big |> Ply.toTask
        Expect.equal r (Some 99_999) "found the last element"
      }
      testTask "flatten" {
        let! r = Ply.List.flatten (big |> List.map Ply) |> Ply.toTask
        Expect.equal (List.length r) 100_000 "flattened every element"
      }
      testTask "foldSequentially" {
        let! r =
          Ply.List.foldSequentially (fun acc i -> Ply(acc + int64 i)) 0L big |> Ply.toTask
        Expect.equal r 4_999_950_000L "summed every element"
      } ]


let tests =
  testList
    "prelude"
    [ asyncTests; mapTests; floatTests; dateTests; assertions; deepRecursion ]
