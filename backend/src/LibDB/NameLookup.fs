/// Shared name resolution utilities used by both the parser (NameResolver)
/// and the runtime deferred resolver (DeferredResolver).
module LibDB.NameLookup

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

  // Handle explicit Stdlib.etc shortcut, plus a final stdlib fallback for
  // Result/Option. Their constructors (Ok/Error, Some/None) already resolve
  // unqualified from any module, so their unqualified type names should resolve the
  // same way while still allowing local definitions to win first.
  let addl =
    match given.modules, given.name with
    | "Stdlib" :: _, _ -> [ { given with modules = "Darklang" :: given.modules } ]
    | ([] | [ "Result" ]), "Result"
    | ([] | [ "Option" ]), "Option" ->
      [ { given with modules = [ "Darklang"; "Stdlib"; given.name ] } ]
    | _, _ -> []

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
