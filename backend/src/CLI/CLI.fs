module Cli.Main

open Prelude
open Tablecloth

// ---------------------
// Version information
// ---------------------

type VersionInfo = { hash : string; buildDate : string; inDevelopment : bool }

#if DEBUG
let inDevelopment : bool = true
#else
let inDevelopment : bool = false
#endif

open System.Reflection

let info () =
  let buildAttributes =
    Assembly.GetEntryAssembly().GetCustomAttribute<AssemblyMetadataAttribute>()
  // This reads values created during the build in CLI.fsproj
  // It doesn't feel like this is how it's supposed to be used, but it works. But
  // what if we wanted more than two parameters?
  let buildDate = buildAttributes.Key
  let gitHash = buildAttributes.Value
  { hash = gitHash; buildDate = buildDate; inDevelopment = inDevelopment }


// ---------------------
// Execution
// ---------------------
[<EntryPoint>]
let main (args : string[]) =
  let config : LibCLI.Main.DarkCLIConfig =
    { name = "Darklang CLI"
      extraStdlibForUserPrograms = [], []
      allowInternalDarkFunctions = false }

  LibCLI.Main.main config args
