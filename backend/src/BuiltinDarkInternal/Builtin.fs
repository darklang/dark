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
  // The `darkInternal*` fns are no longer gated. The CLI is a
  // trusted-user context (one process per user), so the
  // `internalFnsAllowed` per-program flag was never load-bearing
  // there. Names retained for now — callers in
  // `packages/darklang/cli/packages/db.dark` etc still address them
  // by their existing names.
  Builtin.combine
    [ Libs.Canvases.builtins ()
      Libs.DBs.builtins ()
      Libs.Infra.builtins ()
      Libs.Users.builtins () ]
    fnRenames
