/// Builtin functions that can only be run on the backend
///
/// Aggregates functions in other modules
module BuiltinDarkInternal.Builtin

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins () : Builtins =
  Builtin.combine
    [ Libs.Canvases.builtins ()
      Libs.DBs.builtins ()
      Libs.Infra.builtins ()
      Libs.Users.builtins () ]
    fnRenames
