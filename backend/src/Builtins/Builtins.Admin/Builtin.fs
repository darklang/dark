/// Administering the instance: exporting a seed, backing the store up and restoring it, reading and
/// writing config and the relay secret, and managing capability policy.
///
/// Carved out of `Builtins.Store` because the effect contract said so. `Store` is the platform every
/// program needs in order to resolve a name, so it is the one worth keeping narrow, and one builtin
/// here (`pmSeedExport`) was giving it `file-write` and `native`. Nothing that only wants to call
/// `Stdlib.List.map` should be linking a builtin that can write anywhere on the disk.
///
/// The split is also a real audience split. These are the operations a person runs deliberately, at
/// a prompt, about their own installation. They are not operations a program performs while doing
/// its job, which is what everything left in `Store` is.
///
/// Three platforms, not one, for the reason `Builtins.Cli` and `Builtins.Data` ship several: a
/// platform is a value and the assembly is not the unit. `dark config` reads two config keys and a
/// path, and under one `Admin` platform that meant granting `Native` as well, because thirteen
/// policy builtins were sitting next to it.
module Builtins.Admin.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames = []


/// Config, the relay secret, the store path, and backing the store up or restoring it. Reaches the
/// package store and nothing else, which is what `dark config` and `dark backups` actually want.
let instancePlatform : LibExecution.Platform.Platform =
  { name = "Instance"
    version = 0
    description = "Config, the relay secret, store backup and restore."
    builtins = Builtin.combine [ Libs.Store.builtins () ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


/// Exporting a seed: reads the store and writes a file you name. `file-write` lives here and
/// nowhere else in this assembly.
let seedPlatform : LibExecution.Platform.Platform =
  { name = "Seed"
    version = 0
    description = "Exporting a package seed to a file."
    builtins = Builtin.combine [ Libs.Seed.builtins ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


/// The capability policy: the rules that gate every other platform.
///
/// Every builtin here is `Native`, and that is the honest declaration rather than a lazy one.
/// Policy is what decides what everything else may do, so the ability to edit it is the ability to
/// do anything, and no narrower effect would describe that. The value of it being its own platform
/// is that nothing else has to carry the claim.
let policyPlatform : LibExecution.Platform.Platform =
  { name = "Policy"
    version = 0
    description = "Reading and editing the capability policy."
    builtins = Builtin.combine [ Libs.Permissions.builtins ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


let platforms : List<LibExecution.Platform.Platform> =
  [ instancePlatform; seedPlatform; policyPlatform ]


let builtins () : Builtins =
  Builtin.combine
    [ Libs.Seed.builtins; Libs.Permissions.builtins; Libs.Store.builtins () ]
    fnRenames
