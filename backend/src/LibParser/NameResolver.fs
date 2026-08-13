/// Resolves WrittenTypes names against builtins and branch-aware package lookups.
module LibParser.NameResolver

open Prelude
open LibExecution.ProgramTypes

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
type NRE = PT.NameResolutionError

// Package fn/value names are `name` or `name_vN`; type names are `Name` or
// `Name_v0`.
let private parseFnNameString (fnName : string) : Result<string * int, string> =
  match fnName with
  | Regex.Regex "^([a-z][a-z0-9A-Z_]*[']?)_v(\d+)$" [ name; version ] ->
    // TryParse (not `int`) so an absurdly long version string is a format error, not
    // an OverflowException raised from a function that otherwise returns Result
    match System.Int32.TryParse version with
    | true, v -> Ok(name, v)
    | false, _ -> Error "Bad format in fn name"
  | Regex.Regex "^([a-z][a-z0-9A-Z_]*[']?)$" [ name ] -> Ok(name, 0)
  | _ -> Error "Bad format in fn name"

let private parseTypeNameString (typeName : string) : Result<string, string> =
  match typeName with
  | Regex.Regex "^([A-Z][a-z0-9A-Z_]*[']?)_v0$" [ name ] -> Ok name
  | Regex.Regex "^([A-Z][a-z0-9A-Z_]*[']?)$" [ name ] -> Ok name
  | _ -> Error "Bad format in type name"


/// Controls whether unresolved names are allowed.
///
/// Package loading uses `Allow` while the package manager is being filled, since
/// sibling package items may not exist yet. Expression lowering also uses
/// `Allow` for ambiguous names so it can fall back to local variables.
[<RequireQualifiedAccess>]
type OnMissing =
  | ThrowError
  | Allow

// CLEANUP: return the Result directly and let callers decide how to handle it.
let throwIfRelevant
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (given : NEList<string>)
  (nr : PT.NameResolution<'a>)
  : PT.NameResolution<'a> =
  { nr with
      resolved =
        nr.resolved
        |> Result.mapError (fun err ->
          match onMissing with
          | OnMissing.ThrowError ->
            Exception.raiseInternal
              "Unresolved name when not allowed"
              [ "error", err; "given", given; "currentModule", currentModule ]
          | OnMissing.Allow -> err) }


type GenericName = LibDB.NameLookup.GenericName

let namesToTry = LibDB.NameLookup.namesToTry


/// Shared resolver for type, value, and function names.
/// Returns the matched package location alongside the resolved name. Builtins and
/// unresolved names have no package location.
///
/// The location comes from the winning `namesToTry` candidate, so dependency
/// edges can record it directly.
///
/// `branchId` selects which branch's package view the lookups see (WIP included).
/// Package loading and tests pass `mainBranchId`; CLI-script parsing passes the
/// run's branch so intra-branch WIP resolves.
let resolveGenericName<'FQName, 'Builtin when 'Builtin : comparison>
  (builtins : Option<Set<'Builtin>>)
  (onMissing : OnMissing)
  (branchId : PT.BranchId)
  (currentModule : List<string>)
  (given : NEList<string>)
  (parseName : string -> Result<string * int, string>)
  (findInPM : (PT.BranchId * PT.PackageLocation) -> Ply<Option<Hash>>)
  (makePackageFQName : Hash -> 'FQName)
  (makeBuiltinFQName : string * int -> 'FQName)
  (builtinToRT : string * int -> 'Builtin)
  : Ply<PT.NameResolution<'FQName>> =
  uply {
    let originalName = NEList.toList given
    let notFoundError = Error NRE.NotFound
    let (modules, name) = NEList.splitLast given

    match parseName name with
    | Error _ ->
      // Invalid names should obey `OnMissing` too. In `ThrowError` mode, fail
      // hard just like `NotFound`.
      return
        throwIfRelevant
          onMissing
          currentModule
          given
          { originalName = originalName; resolved = Error NRE.InvalidName }
    | Ok(name, version) ->
      let genericName : GenericName =
        { modules = modules; name = name; version = version }

      // Try a candidate. Returns the resolved FQName paired with the
      // matched PackageLocation (None for builtins).
      let tryResolve
        (nameToTry : GenericName)
        : Ply<Result<'FQName * Option<PT.PackageLocation>, unit>> =
        uply {
          match nameToTry.modules with
          | [] -> return Error()
          | owner :: modules ->
            // Check builtins if applicable (values/fns only, not types)
            match builtins with
            | Some builtinSet when owner = "Builtin" && modules = [] ->
              let builtInRT = builtinToRT (nameToTry.name, nameToTry.version)
              if Set.contains builtInRT builtinSet then
                let fqName = makeBuiltinFQName (nameToTry.name, nameToTry.version)
                return Ok(fqName, None)
              else
                return Error()
            | _ ->
              // Try package manager lookup
              let location : PT.PackageLocation =
                { owner = owner; modules = modules; name = nameToTry.name }
              match! findInPM (branchId, location) with
              | Some id -> return Ok(makePackageFQName id, Some location)
              | None -> return Error()
        }

      let! (result, location) =
        Ply.List.foldSequentially
          (fun (currentResult, currentLoc) nameToTry ->
            match currentResult with
            | Ok _ -> Ply((currentResult, currentLoc))
            | Error _ ->
              uply {
                match! tryResolve nameToTry with
                | Error() -> return (currentResult, currentLoc)
                | Ok(success, loc) -> return (Ok success, loc)
              })
          (notFoundError, None)
          (namesToTry currentModule genericName)

      let resolved =
        result |> Result.map (fun name -> { name = name; location = location })
      return
        throwIfRelevant
          onMissing
          currentModule
          given
          { originalName = originalName; resolved = resolved }
  }


let resolveTypeName
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (branchId : PT.BranchId)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
  let warning = "Builtin types don't exist"
  let emptyBuiltins = None // irrelevant for types

  match name with
  // TODO remodel things appropriately so this is not needed
  | WT.KnownBuiltin(_name, _version) -> Exception.raiseInternal warning []
  | WT.Unresolved given ->
    // Types don't have builtins, so pass None
    // parseTypeName returns just name (version always 0 for types)
    let parseTypeName name = parseTypeNameString name |> Result.map (fun n -> (n, 0))

    resolveGenericName
      emptyBuiltins
      onMissing
      branchId
      currentModule
      given
      parseTypeName
      packageManager.findType
      PT.FQTypeName.FQTypeName.Package
      (fun _ -> Exception.raiseInternal warning [])
      (fun _ -> Exception.raiseInternal warning [])



let resolveValueName
  (builtins : Set<RT.FQValueName.Builtin>)
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (branchId : PT.BranchId)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQValueName.FQValueName>> =
  match name with
  | WT.KnownBuiltin(name, version) ->
    Ply(
      { originalName = [ name ]
        resolved =
          Ok { name = PT.FQValueName.fqBuiltIn name version; location = None } }
      : PT.NameResolution<_>
    )
  | WT.Unresolved given ->
    resolveGenericName
      (Some builtins)
      onMissing
      branchId
      currentModule
      given
      parseFnNameString
      packageManager.findValue
      PT.FQValueName.FQValueName.Package
      (fun (n, v) -> PT.FQValueName.Builtin { name = n; version = v })
      (fun (n, v) -> { RT.FQValueName.Builtin.name = n; version = v })


let resolveFnName
  (builtinFns : Set<RT.FQFnName.Builtin>)
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (branchId : PT.BranchId)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
  match name with
  | WT.KnownBuiltin(n, v) ->
    Ply(
      { originalName = [ n ]
        resolved = Ok { name = PT.FQFnName.fqBuiltIn n v; location = None } }
      : PT.NameResolution<_>
    )
  | WT.Unresolved given ->
    resolveGenericName
      (Some builtinFns)
      onMissing
      branchId
      currentModule
      given
      parseFnNameString
      packageManager.findFn
      PT.FQFnName.FQFnName.Package
      (fun (n, v) -> PT.FQFnName.Builtin { name = n; version = v })
      (fun (n, v) -> { RT.FQFnName.Builtin.name = n; version = v })
