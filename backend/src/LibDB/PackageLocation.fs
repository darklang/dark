module LibDB.PackageLocation

module PT = LibExecution.ProgramTypes

/// Dot-separated FQN string for use as map keys and debug output.
let toFQN (loc : PT.PackageLocation) : string =
  match loc.modules with
  | [] -> $"{loc.owner}.{loc.name}"
  | modules ->
    let modulesStr = modules |> String.concat "."
    $"{loc.owner}.{modulesStr}.{loc.name}"


/// Deterministic per-location stand-in for a content hash, for use before an
/// item's references are resolved. A content hash computed at that point is
/// lossy: an unresolved reference carries no name, so two items differing only
/// in which unresolved name they mention hash identically, and any hash-keyed
/// registry silently drops one of them. One placeholder per location cannot
/// collide. The real content hash replaces it after resolution.
let placeholderHash (loc : PT.PackageLocation) : PT.Hash =
  let bytes =
    System.Security.Cryptography.SHA256.HashData(
      System.Text.Encoding.UTF8.GetBytes(toFQN loc)
    )
  PT.Hash(System.Convert.ToHexString(bytes).ToLowerInvariant())
