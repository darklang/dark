/// Small scalars shared between values (`RT.Dval.dint8`, `duint8`, `dchar`).
///
/// A shared value must be the value a fresh one is: equal, hashed alike, ordered alike, and
/// serialized and printed the same.
module Tests.Scalars

open Expecto

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module BS = LibSerialization.Binary.Serialization

let private same (shared : RT.Dval) (fresh : RT.Dval) =
  Expect.isTrue (shared = fresh) "F# equality"
  Expect.isTrue (Dval.equals shared fresh) "Dark equality"
  Expect.equal (shared.GetHashCode()) (fresh.GetHashCode()) "GetHashCode"
  Expect.equal (Dval.hash shared) (Dval.hash fresh) "Dval.hash"
  Expect.equal (RT.DictKey.hash shared) (RT.DictKey.hash fresh) "DictKey.hash"
  Expect.equal (RT.DvalOrdering.compareForSort shared fresh) 0 "ordering"
  Expect.equal
    (BS.RT.Dval.serialize "a" shared)
    (BS.RT.Dval.serialize "b" fresh)
    "serialized bytes"
  Expect.equal (Dval.toDebugString shared) (Dval.toDebugString fresh) "printed"

let tests =
  testList
    "Scalars"
    [ test "every Int8, UInt8 and ASCII character is the value a fresh one is" {
        for i in -128 .. 127 do
          same (RT.Dval.dint8 (int8 i)) (RT.DInt8(int8 i))
        for i in 0..255 do
          same (RT.Dval.duint8 (uint8 i)) (RT.DUInt8(uint8 i))
        for i in 0..127 do
          let c = string (char i)
          same (RT.Dval.dchar c) (RT.DChar c)
        for c in [ "é"; "日"; "😀"; "é"; "" ] do
          same (RT.Dval.dchar c) (RT.DChar c)
      }

      test "bytes from a byte array or from storage are the shared ones" {
        let bytes = Array.init 1000 (fun i -> byte (i % 256))
        match Dval.byteArrayToDvalList bytes with
        | RT.DList(_, items) ->
          let values = RT.DvalList.toArray items
          Expect.equal values (Array.map RT.DUInt8 bytes) "the values"
          Expect.isTrue
            (Array.forall2
              (fun v b -> obj.ReferenceEquals(v, RT.Dval.duint8 b))
              values
              bytes)
            "shared"
        | _ -> failtest "a list"
        let read =
          BS.RT.Dval.deserialize "x" (BS.RT.Dval.serialize "x" (RT.DUInt8 7uy))
        Expect.isTrue (obj.ReferenceEquals(read, RT.Dval.duint8 7uy)) "read back"
      } ]
