/// Composing installed external platforms with the ones this build links.
///
/// Lives here rather than beside the rest of the install machinery because it is the one part that
/// needs the CATALOG: `fnRenames` is a property of the composed set, and the catalog is what this
/// assembly is for. Everything else about installing is in `LibDB`, below the builtins, so a
/// builtin can reach it.
module Platforms.Compose

open Prelude

module PT = LibExecution.ProgramTypes
module Platform = LibExecution.Platform

/// Everything this instance links, plus every external platform it has installed for this machine.
///
/// Skipped installs come back rather than being swallowed, so a caller can say why a platform the
/// person installed is not in the list. Silence there is the failure mode worth avoiding: an
/// instance that boots fine and quietly has fewer platforms than you think.
let composedWith
  (pm : PT.PackageManager)
  (linked : Platform.PlatformSet)
  : Ply.Ply<Platform.PlatformSet * List<string * string>> =
  uply {
    let! (external_, skipped) = LibDB.InstalledPlatforms.platforms pm (LibDB.InstalledPlatforms.currentRid ())
    if List.isEmpty external_ then
      return (linked, skipped)
    else
      // `PlatformSet.make` refuses a name collision, which is what should happen if an installed
      // platform claims a name this build already ships. Better a loud start than a silent
      // shadowing of a builtin somebody trusts.
      return (Platform.PlatformSet.make (linked.platforms @ external_) Sets.fnRenames, skipped)
  }
