/// Shared name resolution utilities used by both the parser (NameResolver)
/// and the runtime deferred resolver (DeferredResolver).
module LibPackageManager.NameLookup

open Prelude

open LibExecution.ProgramTypes
module PT = LibExecution.ProgramTypes


type GenericName = { modules : List<string>; name : string; version : int }

/// Generate candidate fully-qualified names to try, from most specific to
/// least specific.
///
/// Given the name `Option.Option` while in module `Darklang.Stdlib`, tries:
///   Darklang.Stdlib.Option.Option, Darklang.Option.Option, Option.Option
let namesToTry
  (currentModule : List<string>)
  (given : GenericName)
  : List<GenericName> =
  let rec loop (modulesToPrepend : List<string>) : List<GenericName> =
    match List.splitLast modulesToPrepend with
    | None -> [ given ]
    | Some(allButLast, _) ->
      let newNameToTry = { given with modules = modulesToPrepend @ given.modules }
      newNameToTry :: loop allButLast

  // handle explicit Stdlib.etc shortcut
  let addl =
    match given.modules with
    | "Stdlib" :: _ -> [ { given with modules = "Darklang" :: given.modules } ]
    | _ -> []

  (loop currentModule) @ addl


/// Walk the `namesToTry` candidates against `findInPM`, returning the
/// first matching `(hash, location)` pair — or `None` if no candidate
/// resolved. Used by both `LibParser/NameResolver` (initial resolve)
/// and `DeferredResolver` (re-resolve against current PM state) so the
/// candidate-fold logic isn't duplicated.
let findFirstPackageMatch
  (currentModule : List<string>)
  (genericName : GenericName)
  (findInPM : PT.PackageLocation -> Ply<Option<Hash>>)
  : Ply<Option<Hash * PT.PackageLocation>> =
  Ply.List.foldSequentially
    (fun acc (candidate : GenericName) ->
      match acc with
      | Some _ -> Ply acc
      | None ->
        uply {
          match candidate.modules with
          | [] -> return None
          | owner :: mods ->
            let loc : PT.PackageLocation =
              { owner = owner; modules = mods; name = candidate.name }
            match! findInPM loc with
            | Some hash -> return Some(hash, loc)
            | None -> return None
        })
    None
    (namesToTry currentModule genericName)
