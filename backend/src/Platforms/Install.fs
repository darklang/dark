/// Resolving a written manifest against THIS instance.
///
/// `Written.resolve` takes a lookup because `LibExecution.Platform` sits below `ProgramTypes` and
/// cannot see a package manager. This is where the lookup comes from: the store the consumer
/// actually has.
///
/// That direction is the whole reason a manifest names types instead of carrying hashes. The same
/// manifest resolves differently on two instances, and failing to resolve is information rather
/// than an error in the manifest: this platform wants a type you do not have.
module Platforms.Install

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Platform = LibExecution.Platform

/// Split a manifest's type name into a location.
///
/// Fully qualified, always: `Darklang.Stdlib.Result` rather than `Stdlib.Result`. Dark source has
/// an implicit owner and a manifest deliberately does not, because a third party's manifest should
/// not depend on whose resolution shortcuts are in play. A name with no module part is refused
/// rather than guessed at.
let location (name : string) : Option<PT.PackageLocation> =
  match name.Split('.') |> List.ofArray with
  | owner :: rest when rest <> [] ->
    let modules = List.take (List.length rest - 1) rest
    match List.last rest with
    | Some typeName -> Some { owner = owner; modules = modules; name = typeName }
    | None -> None
  | _ -> None

/// Every package type a manifest names, in order of first appearance.
let private namedTypes (m : Platform.Written.Manifest) : List<string> =
  let rec names (t : Platform.External.NamedType) : List<string> =
    match t with
    | Platform.External.NBuiltin _ -> []
    | Platform.External.NList inner -> names inner
    | Platform.External.NDict(k, v) -> names k @ names v
    | Platform.External.NTuple items -> List.collect names items
    | Platform.External.NCustom(name, args) -> name :: List.collect names args

  m.fns
  |> List.collect (fun fn ->
    (fn.parameters |> List.collect (fun (_, t) -> names t)) @ names fn.returnType)
  |> List.distinct

/// Resolve a written manifest against a package manager.
///
/// Looks every named type up ONCE and up front, rather than resolving lazily as the tree is walked.
/// Two reasons: the lookup is I/O and the walk is not, so batching keeps `NamedType.resolve` pure;
/// and a manifest naming three types this instance lacks should say all three, not the first.
let resolve
  (pm : PT.PackageManager)
  (written : Platform.Written.Manifest)
  : Ply<Result<Platform.External.Manifest, Platform.External.Rejection>> =
  uply {
    let mutable found = Map.empty

    for name in namedTypes written do
      match location name with
      | None ->
        // Left absent, so `NamedType.resolve` reports it with everything else rather than this
        // raising a different kind of error for a different kind of bad name.
        ()
      | Some location ->
        let! resolved = pm.findType location
        match resolved with
        | Some hash ->
          // `PT.Hash` and `RT.Hash` are distinct types over the same string, so the hash is
          // carried across rather than passed. Same content, two vocabularies.
          let (PT.Hash h) = hash
          found <- Map.add name (RT.FQTypeName.fqPackage h) found
        | None -> ()

    return Platform.Written.resolve (fun name -> Map.tryFind name found) written
  }


/// Read a platform manifest that lives in the package store as a `val`.
///
/// Manifests ride sync this way rather than through `package_blobs`, which does not sync: a
/// persistent blob serializes as a hash and a length, and the bytes never leave the machine that
/// wrote them. A package value is text, is content-addressed, is approvable and pinnable like any
/// other package item, and is authored the way everything else is.
///
/// The ARTIFACTS do not ride this. They are large, per target, and lazily needed, so they are
/// fetched by hash and verified (`Platforms.Artifacts`). A manifest is the small reviewable half
/// and it is the half that should follow you between machines.
let manifestFrom
  (pm : PT.PackageManager)
  (location : PT.PackageLocation)
  : Ply<Result<Platform.External.Manifest, Platform.External.Rejection>> =
  uply {
    let notFound (why : string) =
      let coordinate =
        String.concat "." (location.owner :: location.modules @ [ location.name ])
      Error
        ({ manifest = coordinate; problems = [ why ] } : Platform.External.Rejection)

    match! pm.findValue location with
    | None -> return notFound "no such package value"
    | Some hash ->
      match! pm.getValue hash with
      | None -> return notFound "the value resolved to a hash the store does not have"
      | Some value ->
        // A manifest is a string LITERAL, not an expression that computes one. Anything else would
        // mean running package code to find out what a platform claims, which is the wrong order:
        // the manifest is what you read BEFORE deciding to trust it.
        match value.body with
        | PT.EString(_, [ PT.StringText text ]) ->
          match Platform.Written.parse text with
          | Error problems ->
            let coordinate =
              String.concat "." (location.owner :: location.modules @ [ location.name ])
            return
              Error
                ({ manifest = coordinate; problems = problems } : Platform.External.Rejection)
          | Ok written -> return! resolve pm written
        | _ -> return notFound "a manifest must be a plain string literal"
  }
