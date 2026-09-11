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
      // A platform claiming a name something already provides is SKIPPED, one at a time, with the
      // name and the platform that has it. Never shadowed, because a builtin quietly meaning
      // somebody else's code is the worst outcome here; and never fatal, because an install is
      // undone by a command, and a command that cannot start cannot undo anything. The install
      // path refuses this case up front, so reaching it means the build changed underneath an
      // install that was fine when it was made.
      let mutable accepted = linked.platforms
      let mutable problems = skipped
      for candidate in external_ do
        match Platform.PlatformSet.claimsTaken accepted candidate with
        | [] -> accepted <- accepted @ [ candidate ]
        | taken ->
          let rendered =
            taken
            |> List.map (fun (key, owner) -> $"{key} is already provided by {owner}")
            |> String.concat "; "
          problems <- problems @ [ (candidate.name, rendered) ]
      return (Platform.PlatformSet.make accepted Sets.fnRenames, problems)
  }
