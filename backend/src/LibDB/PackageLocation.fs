module LibDB.PackageLocation

module PT = LibExecution.ProgramTypes

/// Dot-separated FQN string for use as map keys, the conflict/sync wire, and debug output.
let toFQN (loc : PT.PackageLocation) : string =
  match loc.modules with
  | [] -> $"{loc.owner}.{loc.name}"
  | modules ->
    let modulesStr = modules |> String.concat "."
    $"{loc.owner}.{modulesStr}.{loc.name}"

/// Parse a dot-FQN "owner[.modules].name" back into a PackageLocation (owner = head, name = last,
/// modules = the middle). `None` if there's no name segment. The inverse of `toFQN`.
let fromFQN (fqn : string) : Option<PT.PackageLocation> =
  match fqn.Split('.') |> List.ofArray with
  | owner :: rest ->
    match List.rev rest with
    | name :: revModules ->
      Some { owner = owner; modules = List.rev revModules; name = name }
    | [] -> None
  | _ -> None

/// The `modules` column is stored dot-joined; split it back to a list ("" → []). Inverse of the
/// `String.concat "."` the writers use.
let modulesOfString (modules : string) : List<string> =
  if modules = "" then [] else modules.Split('.') |> Array.toList

/// The portable timestamp-LWW comparison both the op fold (`applySetName`) and the resolution overlay
/// (`Resolutions.applyToLocations`) use to order two DIFFERENT bindings of one name: is the NEW binding
/// (content `newHash`, creation/resolver stamp `newTs`) stale vs the CURRENT (`curHash`/`curTs`)?
/// Older-by-stamp loses; an exact same-stamp tie is broken by the HIGHER content hash — a tie-break over
/// CONTENT (not arrival), so every instance and a from-scratch refold converge on the same winner.
/// (Callers handle the same-content case — a re-bind to what's already there — separately.)
let bindingIsStale
  (curHash : string, curTs : string)
  (newHash : string, newTs : string)
  : bool =
  newTs < curTs || (newTs = curTs && newHash < curHash)
