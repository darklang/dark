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
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal

/// Extracts the result from a task
let result (t : Task<'a>) : 'a = t.Result

let (.=.) actual expected : bool =
  if actual = expected then
    Expect.equal actual expected ""
    true
  else
    let o = string actual |> UTF8.toBytes
    let e = string expected |> UTF8.toBytes
    Expect.equal (actual, o) (expected, e) ""
    false

let baseConfigWithGenerator (typ : System.Type) : FsCheckConfig =
  { FsCheckConfig.defaultConfig with maxTest = 10; arbitrary = [ typ ] }

let testProperty (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (baseConfigWithGenerator typ) name x
