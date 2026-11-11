/// How a string joined by `+` holds its text (`RT.DarkString.append`).
///
/// A joined string must be the string `String.append` would have made: the same text, length,
/// equality, hashes, order, serialized bytes and printing. So each property joins random pieces
/// in random shapes and compares the result with the same pieces appended eagerly.
module Tests.Strings

open Expecto

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module BS = LibSerialization.Binary.Serialization

type DarkString = RT.DarkString

/// Pieces that change across a join: combining marks after a base, Hangul jamo, CR then LF, a
/// surrogate pair split in two, flags and skin tones; and plain text long enough to join.
let private alphabet =
  [| "a"
     "hello world, "
     "e"
     "\u0301"
     "\u0327"
     "\u1100"
     "\u1161"
     "\u11A8"
     "\r"
     "\n"
     "\u00E9"
     "日本"
     "\u00E9\u0301"
     "\uD83D"
     "\uDE00"
     "\uD83D\uDE00"
     "\uD83C\uDDFA"
     "\uD83C\uDDF8"
     "\uD83C\uDFFD"
     "|" |]

let private piece (rng : System.Random) : string =
  if rng.Next 6 = 0 then
    String.replicate (rng.Next(1, 400)) "xyz "
  else
    String.concat
      ""
      (List.init (rng.Next(1, 6)) (fun _ -> alphabet[rng.Next alphabet.Length]))

/// The pieces joined in a random shape, and the same shape appended eagerly.
let rec private build
  (rng : System.Random)
  (pieces : string[])
  : DarkString * string =
  if pieces.Length = 1 then
    DarkString pieces[0], String.normalize pieces[0]
  else
    let k = rng.Next(1, pieces.Length)
    let a, ea = build rng pieces[.. k - 1]
    let b, eb = build rng pieces[k..]
    DarkString.append a b, String.append ea eb

let private sameEverywhere (expected : string) (s : DarkString) =
  let d = RT.DStr s
  let e = RT.DString expected
  Expect.equal s.Value expected "the text"
  Expect.equal s.EgcCount (String.lengthInEgcs expected) "the length"
  Expect.isTrue (d = e) "F# equality"
  Expect.isTrue (Dval.equals d e) "Dark equality"
  Expect.equal (d.GetHashCode()) (e.GetHashCode()) "GetHashCode"
  Expect.equal (Dval.hash d) (Dval.hash e) "Dval.hash"
  Expect.equal (RT.DictKey.hash d) (RT.DictKey.hash e) "DictKey.hash"
  Expect.equal (RT.DvalOrdering.compareForSort d e) 0 "ordering"
  Expect.equal
    (BS.RT.Dval.serialize "a" d)
    (BS.RT.Dval.serialize "b" e)
    "serialized bytes"
  Expect.equal (Dval.toDebugString d) (Dval.toDebugString e) "printed"

let tests =
  testList
    "Strings"
    [ test "a joined string is the string appending would have made" {
        let rng = System.Random 6421
        for round in 1..400 do
          let n = if round % 4 = 0 then rng.Next(1, 400) else rng.Next(1, 30)
          let pieces = Array.init n (fun _ -> piece rng)
          let joined, eager = build rng pieces
          // Normalizing once at the end is normalizing after every append.
          Expect.equal
            eager
            (Array.fold String.append "" pieces)
            "any shape appends alike"
          sameEverywhere eager joined
          // Read once, it answers from what it kept.
          sameEverywhere eager joined
      }

      test "joined strings that differ anywhere are unequal and order as before" {
        let rng = System.Random 6422
        for _ in 1..200 do
          let pieces =
            Array.init (rng.Next(2, 60)) (fun _ -> String.replicate 40 "ab")
          let i = rng.Next pieces.Length
          let changed = Array.updateAt i (String.replicate 40 "ac") pieces
          let a, ea = build rng pieces
          let b, eb = build rng changed
          let da, db = RT.DStr a, RT.DStr b
          Expect.isFalse (da = db) "F# equality"
          Expect.isFalse (Dval.equals da db) "Dark equality"
          Expect.equal
            (RT.DvalOrdering.compareForSort da db)
            (RT.DvalOrdering.compareForSort (RT.DString ea) (RT.DString eb))
            "ordering"
      }

      test "a join that has been read, joined again, reads the same" {
        let rng = System.Random 6423
        let s, eager =
          [ 1..2000 ]
          |> List.fold
            (fun (s, eager) i ->
              let p = piece rng
              let s = DarkString.append s (DarkString p)
              let eager = String.append eager p
              if i % 7 = 0 then Expect.equal s.Value eager "midway"
              (s, eager))
            (DarkString "", "")
        sameEverywhere eager s
      }

      test "a hundred thousand appends either way read without running out of stack" {
        let pieces = Array.init 100_000 (fun i -> if i % 3 = 0 then "é" else "ab")
        let left =
          pieces
          |> Array.fold
            (fun acc p -> DarkString.append acc (DarkString p))
            (DarkString "")
        let right =
          Array.foldBack
            (fun p acc -> DarkString.append (DarkString p) acc)
            pieces
            (DarkString "")
        let expected = String.normalize (String.concat "" pieces)
        Expect.equal left.Value expected "appended"
        Expect.equal right.Value expected "prepended"
      }

      test "threads reading one joined string all read its text" {
        let pieces = Array.init 5000 (fun i -> $"piece {i} ")
        let s =
          pieces
          |> Array.fold
            (fun acc p -> DarkString.append acc (DarkString p))
            (DarkString "")
        let expected = String.concat "" pieces
        let read = Array.Parallel.init 32 (fun _ -> s.Value)
        Expect.all read (fun r -> r = expected) "every thread"
      } ]
