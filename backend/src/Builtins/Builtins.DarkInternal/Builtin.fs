/// Builtin functions that can only be run on the backend
///
/// Aggregates functions in other modules
module Builtins.DarkInternal.Builtin

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

// `darkInternal*` builtins used to be gated behind a per-canvas allowlist.
// CLI is single-app; all builtins are intended to be exposed. The gate is
// gone; this module is now just a builtin grouping for dark-internal/admin
// surface.
let builtins () : Builtins =
  Builtin.combine
    [ Libs.Canvases.builtins ()
      Libs.DBs.builtins ()
      Libs.Domains.builtins ()
      Libs.Infra.builtins ()
      Libs.Users.builtins () ]
    fnRenames
