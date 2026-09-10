module Builtins.Random.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () =
  Builtin.combine [ Libs.Uuid.builtins (); Libs.Random.builtins () ] fnRenames


/// Entropy: random numbers and v4 UUIDs. Separate from `Clock` because the two are refused for
/// different reasons, and separate from `Core` because neither is a pure function.
let platform : LibExecution.Platform.Platform =
  { name = "Random"
    version = 0
    description = "Random numbers and generated UUIDs."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }
