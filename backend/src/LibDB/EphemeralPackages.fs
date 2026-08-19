/// Declarations that exist in this process but were never written to the store:
/// the types, values and fns a CLI script declares for itself.
///
/// A runtime error carries content hashes, and it is rendered well after the
/// executor that raised it is gone. The pretty-printer turns a hash back into a
/// name by asking the package manager which locations that hash is bound to, so
/// a declaration the store has never seen renders as a 64-character hash, and a
/// message about the wrong function reads as an ordinary type mismatch.
///
/// Registering the declarations here puts them in reach of that lookup without
/// threading a second package manager through every pretty-printer entry point.
///
/// Registrations accumulate rather than replace, so an error can still name a
/// declaration from an enclosing script after a nested one has run. Growth is
/// bounded by content addressing: a hash is registered once however many times
/// its declaration is parsed, and a REPL session's worth of declarations is
/// small. Two names for one hash mean two names for one declaration, which is
/// the situation `pickLocation` already exists to resolve.
module LibDB.EphemeralPackages

open Prelude

module PT = LibExecution.ProgramTypes

/// Hash to every location it is known by.
type private Registry =
  { types : Map<PT.Hash, List<PT.PackageLocation>>
    values : Map<PT.Hash, List<PT.PackageLocation>>
    fns : Map<PT.Hash, List<PT.PackageLocation>> }

let private empty = { types = Map.empty; values = Map.empty; fns = Map.empty }

let mutable private registry = empty
let private writeLock = obj ()

let private add
  (entries : List<PT.Hash * PT.PackageLocation>)
  (m : Map<PT.Hash, List<PT.PackageLocation>>)
  : Map<PT.Hash, List<PT.PackageLocation>> =
  entries
  |> List.fold
    (fun acc (hash, loc) ->
      let existing = Map.tryFind hash acc |> Option.defaultValue []
      if List.contains loc existing then
        acc
      else
        Map.add hash (existing @ [ loc ]) acc)
    m

/// Make these declarations nameable by hash for the rest of the process.
///
/// Locked because this is a read-modify-write: two lowerings racing would
/// silently lose one of them. Reads stay lock-free, each seeing whichever
/// immutable snapshot is current.
let register
  (types : List<PT.Hash * PT.PackageLocation>)
  (values : List<PT.Hash * PT.PackageLocation>)
  (fns : List<PT.Hash * PT.PackageLocation>)
  : unit =
  lock writeLock (fun () ->
    registry <-
      { types = add types registry.types
        values = add values registry.values
        fns = add fns registry.fns })

/// Reverse lookups only. Resolution reads the store directly, and adding a layer
/// to `findType`/`getType` would put this on the parser's hot path for the sake
/// of an error message.
let typeLocations (hash : PT.Hash) : List<PT.PackageLocation> =
  Map.tryFind hash registry.types |> Option.defaultValue []

let valueLocations (hash : PT.Hash) : List<PT.PackageLocation> =
  Map.tryFind hash registry.values |> Option.defaultValue []

let fnLocations (hash : PT.Hash) : List<PT.PackageLocation> =
  Map.tryFind hash registry.fns |> Option.defaultValue []
