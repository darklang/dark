/// FuzzTests around Passwords
module FuzzTests.Passwords

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module G = Generators

type Generator =
  static member String() : Arbitrary<string> = G.SafeUnicodeString

/// We should be able to successfully 'check' a
/// password against a hash of the same password
let hashCheckRoundtrip (rawPassword : string) : bool =
  let t =
    task {
      let! meta = initializeTestCanvas "executePure"
      let! state = executionStateFor meta Map.empty Map.empty

      let ast =
        $"Password.check_v0 (Password.hash_v0 \"{rawPassword}\") \"{rawPassword}\""
        |> Parser.Parser.parseRTExpr

      match! LibExecution.Execution.executeExpr state Map.empty ast with
      | RT.DBool true -> return true
      | _ -> return false
    }
  t.Result

let tests config =
  testList
    "password"
    [ testProperty config typeof<Generator> "hash/check roundtrip" hashCheckRoundtrip ]
