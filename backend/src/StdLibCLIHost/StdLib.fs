/// StdLib functions that are part of running the CLI
///
/// Aggregates functions in other modules
module StdLibCLIHost.StdLib

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module StdLib = LibExecution.StdLib


let fnRenames : StdLib.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames : StdLib.TypeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

let contents (extraStdlibForUserPrograms : StdLib.Contents) =
  StdLib.combine
    [ Libs.CLI.contents extraStdlibForUserPrograms ]
    fnRenames
    typeRenames
