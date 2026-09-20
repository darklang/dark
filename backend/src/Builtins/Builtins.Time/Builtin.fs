module Builtins.Time.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () =
  Builtin.combine [ Libs.Time.builtins (); Libs.DateTime.builtins () ] fnRenames


/// Reading the wall clock. Split out from `Core` because "what time is it" is the smallest
/// non-determinism a program can have, and a build that wants reproducible output wants to say no
/// to exactly this and nothing else.
let platform : LibExecution.Platform.Platform =
  { name = "Clock"
    version = 0
    description = "The wall clock."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }
