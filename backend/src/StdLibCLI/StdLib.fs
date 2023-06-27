module StdLibCLI.StdLib

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

let contents =
  StdLib.combine
    [ Libs.Directory.contents
      Libs.Environment.contents
      Libs.File.contents
      Libs.Process.contents
      Libs.Output.contents ]
    fnRenames
    typeRenames
