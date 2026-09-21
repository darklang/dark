module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let ptPM = LibExecution.ProgramTypes.PackageManager.empty

/// For parsing packages, which may reference _any_ builtin, plus the test-only library the
/// `.dark` testfiles call.
///
/// `LibTest` is not a platform: it ships in no executable, and giving it a platform record would
/// put it in the catalog where `dark platforms` would offer to install it.
let all () : RT.Builtins =
  LibExecution.Builtin.combine
    [ (Platforms.Sets.everythingFor ptPM).builtins; TestUtils.LibTest.builtins () ]
    []
