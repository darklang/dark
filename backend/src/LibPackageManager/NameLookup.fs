/// Shared name resolution utilities used by both the parser (NameResolver)
/// and the runtime deferred resolver (DeferredResolver).
module LibPackageManager.NameLookup

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
