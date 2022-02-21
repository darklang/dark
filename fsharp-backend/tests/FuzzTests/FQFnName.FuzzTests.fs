module FuzzTests.FQFnName

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module G = FuzzTests.Utils.Generators

let tpwg = testPropertyWithGenerator

let nameGenerator (first : char list) (other : char list) : Gen<string> =
  gen {
    let! length = Gen.choose (0, 20)
    let! head = Gen.elements first
    let! tail = Gen.arrayOfLength length (Gen.elements other)
    return System.String(Array.append [| head |] tail)
  }

let ownerName : Gen<string> =
  nameGenerator [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

let packageName = ownerName
let modName : Gen<string> = nameGenerator [ 'A' .. 'Z' ] G.alphaNumericString
let fnName : Gen<string> = nameGenerator [ 'a' .. 'z' ] G.alphaNumericString

type Generator =
  static member SafeString() : Arbitrary<string> =
    Arb.fromGenShrink (G.string (), Arb.shrink<string>)

  static member PTFQFnName() : Arbitrary<PT.FQFnName.T> =
    { new Arbitrary<PT.FQFnName.T>() with
        member x.Generator =
          let stdlib =
            gen {
              let! module_ = modName
              let! function_ = fnName
              let! version = G.nonNegativeInt ()
              return PT.FQFnName.stdlibFqName module_ function_ version
            }

          let user = Gen.map PT.FQFnName.userFqName fnName

          let package =
            gen {
              let! owner = ownerName
              let! package = packageName
              let! module_ = modName
              let! function_ = fnName
              let! version = G.nonNegativeInt ()

              return
                PT.FQFnName.packageFqName owner package module_ function_ version
            }

          Gen.oneof [ stdlib; user; package ] }

  static member RTFQFnName() : Arbitrary<RT.FQFnName.T> =
    { new Arbitrary<RT.FQFnName.T>() with
        member x.Generator = Generator.PTFQFnName().Generator }

let ptRoundtrip (a : PT.FQFnName.T) : bool = string a |> PT.FQFnName.parse .=. a

let tests =
  testList "PT.FQFnName" [ tpwg typeof<Generator> "roundtripping" ptRoundtrip ]
