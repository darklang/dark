module StdLibCli.StdLib

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames : Builtin.TypeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

let contents =
  Builtin.combine
    [ Libs.Directory.contents
      Libs.Environment.contents
      Libs.File.contents
      Libs.Process.contents
      Libs.Output.contents
      Libs.Time.contents ]
    fnRenames
    typeRenames
