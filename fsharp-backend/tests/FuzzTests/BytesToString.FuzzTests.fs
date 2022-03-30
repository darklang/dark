/// Generators and FuzzTests ensuring `toString` yields
/// consistent results for `byte`s across OCaml and F# backends
module FuzzTests.BytesToString

open System.Threading.Tasks

open Expecto
open FsCheck

open TestUtils.TestUtils
open FuzzTests.Utils

module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

type Generator =
  static member String() : Arbitrary<string> =
    Arb.fromGen (Generators.ocamlSafeString)

/// Checks that `toString` on a `byte[]` produces
/// the same string for both OCaml and F# runtimes
let toStringTest (bytes : byte []) : bool =
  task {
    let! meta = initializeTestCanvas "bytes-to-string"

    let ast = $"toString_v0 myValue" |> FSharpToExpr.parsePTExpr
    let symtable = Map [ "myvalue", RT.DBytes bytes ]

    let! expected = OCamlInterop.execute meta.owner meta.id ast symtable [] []

    let! state = executionStateFor meta Map.empty Map.empty
    let! actual =
      LibExecution.Execution.executeExpr state symtable (PT2RT.Expr.toRT ast)

    if Expect.dvalEquality actual expected then return true else return false
  }
  |> result

let tests =
  testList
    "bytesToString"
    [ testProperty typeof<Generator> "comparing bytesToString" toStringTest ]
