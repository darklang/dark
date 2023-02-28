/// Utilities useful for writing and running FuzzTests
module FuzzTests.Utils

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

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable


let (.=.) actual expected : bool =
  if actual = expected then
    Expect.equal actual expected ""
    true
  else
    let o = string actual |> UTF8.toBytes
    let e = string expected |> UTF8.toBytes
    Expect.equal (actual, o) (expected, e) ""
    false

type FuzzTestConfig = { MaxTests : int }

let baseConfigWithGenerator config (typ : System.Type) : FsCheckConfig =
  { FsCheckConfig.defaultConfig with maxTest = config.MaxTests; arbitrary = [ typ ] }

let testProperty config (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (baseConfigWithGenerator config typ) name x
