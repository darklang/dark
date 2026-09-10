/// The package store: packages, ops, branches, the at-rest type checker, and the policy surface
/// over package approvals.
///
/// Carved out of the former `Builtins.Matter`, which fused this with the user database, raw SQLite,
/// traces and accounts. They are separated because they are wanted separately: reading and writing
/// Darklang code is what the language needs to run at all, while a user database is something a
/// particular program wants. Keeping them together meant every executable that could resolve a name
/// also linked `LibCloud` and the raw SQLite floor.
///
/// Store ADMINISTRATION was then carved out again into `Builtins.Admin` (seed export, backup and
/// restore, config, policy). This platform is the one every program needs to resolve a name, so
/// every effect it carries is one every program carries, and `pmSeedExport` alone was giving it
/// `file-write`.
module Builtins.Store.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes


let fnRenames : Builtin.FnRenames = []


let private allFns (pm : PT.PackageManager) : List<BuiltInFn> =
  Libs.Packages.fns pm @ Libs.AtRestTypeChecker.fns pm @ Libs.PackageOps.fns pm


/// The split here is on DECLARED EFFECTS rather than on file layout, which is different from how
/// `Builtins.Cli`, `Builtins.Data` and `Builtins.Admin` divide up.
///
/// It has to be. Reading and writing the store are interleaved inside `Libs/Packages.fs` and
/// `Libs/PackageOps.fs`, so no arrangement of files separates them, and the effect declaration is
/// the exact criterion anyway: a builtin belongs on the write side precisely when it says it may
/// write.
///
/// The consequence is worth stating: adding `PackageWrite` to a builtin MOVES it between platforms.
/// That is the behaviour I want. It changes the manifest fingerprint, which forces a re-review, and
/// `Tests.Platform.declaredEffectsMatchReality` fails if either platform's surface drifts. A
/// hand-maintained list of names would have gone stale silently instead.
let private writes (fn : BuiltInFn) : bool =
  Set.contains LibExecution.Effects.Effect.PackageWrite fn.callEffects


/// The builtin VALUES go with the reads. A value is a constant (`scmMainBranchId` is the main
/// branch's uuid), so there is nothing for the write half to claim, and dropping them on the floor
/// is the mistake this comment exists to stop being made twice: `Builtin.make []` compiles fine and
/// the failure arrives much later, as `Builtin.scmMainBranchId not found` during the package
/// reload.
let readBuiltins (pm : PT.PackageManager) : Builtins =
  Builtin.make (Libs.PackageOps.values ()) (allFns pm |> List.filter (writes >> not))

let writeBuiltins (pm : PT.PackageManager) : Builtins =
  Builtin.make [] (allFns pm |> List.filter writes)


/// Every builtin in this assembly. The cost report and the seed exporter want the assembly.
let builtins (pm : PT.PackageManager) : Builtins =
  Builtin.combine [ readBuiltins pm; writeBuiltins pm ] fnRenames


/// Reading Darklang code itself.
///
/// `Lang` requires this, and so does anything that resolves a package name, which is nearly
/// everything: it is the platform that makes the language usable rather than merely computable.
/// Because EVERY program links it, every effect it carries is one every program carries, which is
/// why it is worth this much care. It has now shed `file-write` (to `Seed`), `native` (to
/// `Darklang`), a stale `random`, and finally `package-write`.
///
/// What forced the last one: `dark tree`, `dark search`, `dark ls`, `dark view` and half a dozen
/// others read the store and never write it, and every one of them was being granted the ability to
/// rewrite package code in order to list it.
let platform (pm : PT.PackageManager) : LibExecution.Platform.Platform =
  { name = "Store"
    version = 0
    description = "Reading the package store: items, ops, branches, approvals."
    builtins = readBuiltins pm
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


/// Writing Darklang code: naming things, recording ops, propagating, merging branches.
///
/// A real audience split rather than a mechanical one. Everything here is part of AUTHORING, which
/// is a thing a person or an agent does deliberately, and not something a program does in the
/// course of resolving a name and running.
let authoringPlatform (pm : PT.PackageManager) : LibExecution.Platform.Platform =
  { name = "Authoring"
    version = 0
    description = "Writing the package store: names, ops, propagation, merges."
    builtins = writeBuiltins pm
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }
