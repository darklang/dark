module FuzzTests.BytesToString

open System.Threading.Tasks

open Expecto
open FsCheck

open TestUtils.TestUtils
open FuzzTests.Utils

module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop

type Generator =
  static member SafeString() : Arbitrary<string> = Arb.fromGen (Generators.string ())

let toStringTest (bytes : byte []) : bool =
  let t =
    task {
      let! meta = initializeTestCanvas "bytes-to-string"

      let ast = $"toString_v0 myValue" |> FSharpToExpr.parsePTExpr
      let symtable = Map [ "myvalue", RT.DBytes bytes ]

      let! expected = OCamlInterop.execute meta.owner meta.id ast symtable [] []

      let! state = executionStateFor meta Map.empty Map.empty
      let! actual =
        LibExecution.Execution.executeExpr state symtable (ast.toRuntimeType ())

      if Expect.dvalEquality actual expected then return true else return false
    }
  t.Result

let tests =
  testList
    "bytesToString"
    [ testPropertyWithGenerator
        typeof<Generator>
        "comparing bytesToString"
        toStringTest ]
