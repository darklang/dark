module Builtins.CliHost.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins () = Builtin.combine [ Libs.Cli.builtins () ] fnRenames


/// Darklang running Darklang: `eval`, script parsing and execution, the authoring path, the policy
/// commands. The self-hosting surface, and the one platform whose builtins call back into the
/// interpreter rather than out to the world.
///
/// It does not require `Lang` or `Store`, which surprised me. Parsing happens in `LibParser` and
/// resolution through the package manager on the execution state; neither reaches another
/// platform's BUILTINS, which is what `requires` means. It does not need `Host` either: reading the
/// script off disk is the caller's job, not this platform's.
let platform : LibExecution.Platform.Platform =
  { name = "Darklang"
    version = 0
    description = "eval, script execution, authoring, policy commands."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }
