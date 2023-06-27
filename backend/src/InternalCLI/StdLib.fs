/// Include libraries here that we would not want to even be possible from other environments
module InternalCLI.StdLib

open Prelude
module StdLib = LibExecution.StdLib

let fnRenames : StdLib.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames : StdLib.TypeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

let contents : StdLib.Contents =
  StdLib.combine [ Libs.Packages.contents ] fnRenames typeRenames
