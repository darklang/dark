/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibCloudExecution.StdLib

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
  Builtin.combine [ Libs.DB.contents; Libs.Event.contents ] fnRenames typeRenames
