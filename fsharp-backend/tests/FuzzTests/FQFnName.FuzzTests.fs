/// Generators and FuzzTests around Fully-Qualified Function Names
module FuzzTests.FQFnName

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module G = FuzzTests.Generators

/// Helper function to generate allowed function name parts
let nameGenerator (first : char list) (other : char list) : Gen<string> =
  gen {
    let! tailLength = Gen.choose (0, 20)
    let! head = Gen.elements first
    let! tail = Gen.arrayOfLength tailLength (Gen.elements other)
    return System.String(Array.append [| head |] tail)
  }

let ownerName : Gen<string> =
  nameGenerator [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

let packageName = ownerName
let modName : Gen<string> = nameGenerator [ 'A' .. 'Z' ] G.alphaNumericString
let fnName : Gen<string> = nameGenerator [ 'a' .. 'z' ] G.alphaNumericString

type Generator =
  static member SafeString() : Arbitrary<string> =
    Arb.fromGenShrink (Generators.ocamlSafeString, Arb.shrink<string>)

  static member PTFQFnName() : Arbitrary<PT.FQFnName.T> =
    { new Arbitrary<PT.FQFnName.T>() with
        member _.Generator =
          let stdlib =
            gen {
              let! module_ = modName
              let! function_ = fnName
              let! version = G.nonNegativeInt
              return PTParser.FQFnName.stdlibFqName module_ function_ version
            }

          let user = Gen.map PTParser.FQFnName.userFqName fnName

          let package =
            gen {
              let! owner = ownerName
              let! package = packageName
              let! module_ = modName
              let! function_ = fnName
              let! version = G.nonNegativeInt

              return
                PTParser.FQFnName.packageFqName
                  owner
                  package
                  module_
                  function_
                  version
            }

          Gen.oneof [ stdlib; user; package ] }

  static member RTFQFnName() : Arbitrary<RT.FQFnName.T> =
    { new Arbitrary<RT.FQFnName.T>() with
        member _.Generator =
          Generator.PTFQFnName().Generator |> Gen.map PT2RT.FQFnName.toRT }

/// ProgramType can roundtrip cleanly to/from RuntimeType
let ptRoundtrip (a : PT.FQFnName.T) : bool =
  a |> PT2RT.FQFnName.toRT |> RT.FQFnName.toString |> PTParser.FQFnName.parse
  .=. a

let tests =
  testList
    "PT.FQFnName"
    [ testPropertyWithGenerator typeof<Generator> "roundtripping" ptRoundtrip ]
