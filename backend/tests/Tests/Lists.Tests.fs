/// How a list holds its values (`LibExecution.Rrb`, `RT.DvalList`).
///
/// The same values must make the same list however it was built: equal, hashed alike, serialized
/// to the same bytes and printed the same. So each property here builds its lists by random
/// sequences of prepend, append, concatenation and slicing, and compares them with the list built
/// in one go.
module Tests.Lists

open Expecto

open Prelude

module RT = LibExecution.RuntimeTypes
type Rrb = RT.Rrb
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module BS = LibSerialization.Binary.Serialization

type DvalList = RT.DvalList


/// A vector and the values it should hold, built by `steps` random operations.
let rec private randomVec
  (rng : System.Random)
  (next : unit -> RT.Dval)
  (steps : int)
  : RT.RrbVec * RT.Dval[] =
  let fresh n = Array.init n (fun _ -> next ())
  match (if steps = 0 then 0 else rng.Next 8) with
  | 0 ->
    let a = fresh (rng.Next(0, (if rng.Next 3 = 0 then 3000 else 80)))
    Rrb.ofArray a, a
  | 1 ->
    let v, m = randomVec rng next (steps - 1)
    let a = fresh (rng.Next(0, 200))
    Array.fold (fun v x -> Rrb.push x v) v a, Array.append m a
  | 2 ->
    let v, m = randomVec rng next (steps - 1)
    let chunks = List.init (rng.Next(0, 80)) (fun _ -> fresh (rng.Next(1, 33)))
    List.fold (fun v c -> Rrb.prependArray c v) v chunks,
    Array.concat (List.rev chunks @ [ m ])
  | 3
  | 4 ->
    let v1, m1 = randomVec rng next (steps - 1)
    let v2, m2 = randomVec rng next (steps - 1)
    Rrb.concat v1 v2, Array.append m1 m2
  | 5 ->
    let v, m = randomVec rng next (steps - 1)
    let k = rng.Next(0, m.Length + 2)
    Rrb.drop k v, Array.skip (min k m.Length) m
  | 6 ->
    let v, m = randomVec rng next (steps - 1)
    let k = rng.Next(0, m.Length + 2)
    Rrb.take k v, Array.truncate k m
  | _ ->
    let v, m = randomVec rng next (steps - 1)
    if m.Length = 0 then
      v, m
    else
      let i = rng.Next m.Length
      Rrb.set i (RT.DInt64 -1L) v, Array.updateAt i (RT.DInt64 -1L) m

let private counter () =
  let n = ref 0L
  fun () ->
    n.Value <- n.Value + 1L
    RT.DInt64 n.Value

let private vecContents (v : RT.RrbVec) : RT.Dval[] =
  let viaGet = Array.init v.Count (fun i -> Rrb.get i v)
  let viaCopy = Array.zeroCreate v.Count
  Rrb.copyTo v 0 viaCopy 0 v.Count
  let viaFold =
    Rrb.foldFrom (fun acc x -> x :: acc) [] 0 v |> List.rev |> Array.ofList
  Expect.equal viaCopy viaGet "copyTo reads what get does"
  Expect.equal viaFold viaGet "foldFrom reads what get does"
  viaGet

let private vecTests =
  testList
    "Rrb"
    [ test
        "random pushes, prepends, concatenations, slices and sets keep the values and the shape" {
        let rng = System.Random 6144
        let next = counter ()
        for _ in 1..2000 do
          let v, model = randomVec rng next 5
          Expect.equal (Rrb.problems v) [] "a well-formed tree"
          Expect.equal (vecContents v) model "the values"
      }

      test "a list split anywhere and joined again is the same list" {
        let rng = System.Random 6145
        let next = counter ()
        for _ in 1..300 do
          let v, model = randomVec rng next 4
          let k = rng.Next(0, model.Length + 1)
          let joined = Rrb.concat (Rrb.take k v) (Rrb.drop k v)
          Expect.equal (Rrb.problems joined) [] "a well-formed tree"
          Expect.equal (vecContents joined) model "the values"
      }

      test "thousands of small concatenations at either end stay shallow" {
        let rng = System.Random 6146
        let next = counter ()
        let v, model =
          List.fold
            (fun (v, m) _ ->
              let a = Array.init (rng.Next(1, 40)) (fun _ -> next ())
              if rng.Next 2 = 0 then
                Rrb.concat v (Rrb.ofArray a), Array.append m a
              else
                Rrb.concat (Rrb.ofArray a) v, Array.append a m)
            (Rrb.empty, [||])
            [ 1..5000 ]
        Expect.equal (Rrb.problems v) [] "a well-formed tree"
        Expect.equal (vecContents v) model "the values"
        // ~100k values fit in four levels of 32; RRB's slack allows one more.
        Expect.isLessThanOrEqual v.shift 20 "the height"
      }

      test "a million values prepended a leaf at a time stay shallow" {
        let next = counter ()
        let chunks = List.init 31250 (fun _ -> Array.init 32 (fun _ -> next ()))
        let v = List.fold (fun v c -> Rrb.prependArray c v) Rrb.empty chunks
        Expect.equal (Rrb.problems v) [] "a well-formed tree"
        Expect.equal v.Count 1_000_000 "the count"
        Expect.isLessThanOrEqual v.shift 15 "the height"
        let firstChunk = List.reduce (fun _ c -> c) chunks
        Expect.equal (Rrb.get 0 v) firstChunk[0] "the first value"
      } ]


/// A list with these values, built by a random sequence of list operations.
let rec private build (rng : System.Random) (items : RT.Dval[]) : DvalList =
  let whole () = DvalList.ofArray items
  let split () = rng.Next(0, items.Length + 1)
  match rng.Next 11 with
  | 0 -> whole ()
  | 9 -> DvalList.init items.Length (fun i -> items[i])
  | 10 -> DvalList.ofRevList (List.rev (List.ofArray items))
  | 1 -> DvalList.ofList (List.ofArray items)
  // Prepended one at a time, from the back.
  | 2 -> Array.foldBack DvalList.cons items DvalList.empty
  // Appended one at a time.
  | 3 -> Array.fold DvalList.snoc DvalList.empty items
  | 4 ->
    let k = split ()
    DvalList.append (build rng items[.. k - 1]) (build rng items[k..])
  // Sliced out of a longer list.
  | 5 ->
    let before = Array.init (rng.Next 70) (fun i -> RT.DInt64(int64 -i))
    let after = Array.init (rng.Next 70) (fun i -> RT.DInt64(int64 (1000 + i)))
    let longer = build rng (Array.concat [ before; items; after ])
    longer |> DvalList.drop before.Length |> DvalList.take items.Length
  // What is left after taking values off the front.
  | 6 ->
    let before = Array.init (rng.Next 70) (fun i -> RT.DInt64(int64 -i))
    let rec unconsN (n : int) (list : DvalList) =
      if n = 0 then
        list
      else
        match DvalList.uncons list with
        | ValueSome(struct (_, rest)) -> unconsN (n - 1) rest
        | ValueNone -> list
    unconsN before.Length (build rng (Array.append before items))
  | 7 -> DvalList.rev (build rng (Array.rev items))
  | _ ->
    let k = split ()
    let front = build rng items[.. k - 1]
    let back = build rng items[k..]
    DvalList.fold (fun acc item -> DvalList.snoc acc item) front back

let private sameEverywhere (items : RT.Dval[]) (a : DvalList) (b : DvalList) =
  let la = RT.DList(VT.int64, a)
  let lb = RT.DList(VT.int64, b)
  Expect.equal (DvalList.toArray a) items "the values"
  Expect.equal (DvalList.length a) items.Length "the length"
  Expect.isTrue (la = lb) "F# equality"
  Expect.isTrue (a.Equals b) "DvalList equality"
  Expect.isTrue (Dval.equals la lb) "Dark equality"
  Expect.equal (la.GetHashCode()) (lb.GetHashCode()) "GetHashCode"
  Expect.equal (Dval.hash la) (Dval.hash lb) "Dval.hash"
  Expect.equal (RT.DictKey.hash la) (RT.DictKey.hash lb) "DictKey.hash"
  Expect.equal (RT.DvalOrdering.compareForSort la lb) 0 "ordering"
  Expect.equal
    (BS.RT.Dval.serialize "a" la)
    (BS.RT.Dval.serialize "b" lb)
    "serialized bytes"
  Expect.equal (Dval.toDebugString la) (Dval.toDebugString lb) "printed"
  for i in 0 .. items.Length - 1 do
    Expect.equal (DvalList.tryItem (int64 i) a) (Some items[i]) "getAt"

let private listTests =
  testList
    "DvalList"
    [ test "the same values make the same list however it was built" {
        let rng = System.Random 6386
        for round in 1..400 do
          let n =
            match round % 4 with
            | 0 -> rng.Next 5
            | 1 -> rng.Next 40
            | 2 -> rng.Next 300
            | _ -> rng.Next 3000
          let items = Array.init n (fun i -> RT.DInt64(int64 (i * 7)))
          let reference = DvalList.ofArray items
          sameEverywhere items reference (build rng items)
          sameEverywhere items (build rng items) (build rng items)
      }

      test "lists that differ anywhere are unequal and order as before" {
        let rng = System.Random 6387
        for _ in 1..200 do
          let n = rng.Next(1, 500)
          let items = Array.init n (fun i -> RT.DInt64(int64 i))
          let i = rng.Next n
          let changed = Array.updateAt i (RT.DInt64 -1L) items
          let a = RT.DList(VT.int64, build rng items)
          let b = RT.DList(VT.int64, build rng changed)
          Expect.isFalse (a = b) "F# equality"
          Expect.isFalse (Dval.equals a b) "Dark equality"
          Expect.equal (RT.DvalOrdering.compareForSort a b) 1 "ordering"
          let shorter = RT.DList(VT.int64, build rng items[.. n - 2])
          Expect.isFalse (a = shorter) "a prefix"
          Expect.equal
            (RT.DvalOrdering.compareForSort shorter a)
            -1
            "a prefix sorts first"
      }

      test "NaN stays unequal to itself inside a list" {
        let nan = RT.DFloat nan
        let a = RT.DList(VT.known RT.KTFloat, DvalList.ofArray (Array.create 40 nan))
        let b = RT.DList(VT.known RT.KTFloat, DvalList.ofArray (Array.create 40 nan))
        // As when a list was an F# list: the comparer a union's `Equals` passes in reaches the values.
        Expect.isFalse (a = b) "F# ="
        Expect.isFalse (a.Equals b) "Equals"
        Expect.isFalse (Dval.equals a b) "Dark equality"
      }

      test
        "a million-value list folds, compares and copies without running out of stack" {
        let items = Array.init 1_000_000 (fun i -> RT.DInt64(int64 i))
        let a = DvalList.ofArray items
        let b = Array.foldBack DvalList.cons items DvalList.empty
        Expect.isTrue
          (Dval.equals (RT.DList(VT.int64, a)) (RT.DList(VT.int64, b)))
          "equal"
        Expect.equal
          (RT.DvalOrdering.compareForSort
            (RT.DList(VT.int64, a))
            (RT.DList(VT.int64, b)))
          0
          "ordered alike"
        Expect.equal (DvalList.fold (fun n _ -> n + 1) 0 b) 1_000_000 "fold"
        Expect.equal (DvalList.toArray b) items "toArray"
      }

      test "a pattern walk visits every value once, in order" {
        let items = Array.init 5000 (fun i -> RT.DInt64(int64 i))
        let rng = System.Random 6388
        let rec walk (list : DvalList) (acc : List<RT.Dval>) =
          match DvalList.uncons list with
          | ValueSome(struct (head, rest)) -> walk rest (head :: acc)
          | ValueNone -> List.rev acc
        for _ in 1..20 do
          Expect.equal (walk (build rng items) []) (List.ofArray items) "the values"
      } ]


let tests = testList "Lists" [ vecTests; listTests ]
