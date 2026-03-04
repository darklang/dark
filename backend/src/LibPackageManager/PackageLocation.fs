module LibPackageManager.PackageLocation

module PT = LibExecution.ProgramTypes

/// Dot-separated FQN string for use as map keys and debug output.
let toFQN (loc : PT.PackageLocation) : string =
  match loc.modules with
  | [] -> $"{loc.owner}.{loc.name}"
  | modules ->
    let modulesStr = modules |> String.concat "."
    $"{loc.owner}.{modulesStr}.{loc.name}"
