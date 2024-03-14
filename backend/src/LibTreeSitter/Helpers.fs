[<AutoOpen>]
module LibTreeSitter.Helpers

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices

let baseTempPath = Path.Combine(Path.GetTempPath(), "darklang")

let resourceExtensionForOS =
  if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".dll"
  elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then ".so"
  elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then ".dylib"
  else raise (PlatformNotSupportedException())
