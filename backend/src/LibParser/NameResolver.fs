/// CLEANUP still feels like this can be tidied/shortened a bit.
module LibParser.NameResolver

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes
type NRE = PT.NameResolutionError


/// If a name is not found, should we raise an exception?
///
/// - when the local package DB is fully empty, and we're filling it in for the
///   first time, we want to allow all names to be NotFound -- other package
///   items won't be there yet
/// - sometimes when parsing, we're not sure whether something is:
///   - a variable
///   - or something else, like a value or fn.
///   During these times, we _also_ want to allow errors as well, so we can
///   parse it as a variable as a fallback if nothing is found under that name.
[<RequireQualifiedAccess>]
type OnMissing =
  | ThrowError
  | Allow

// TODO: we should probably just return the Result, and let the caller
// handle the error if they want to...
let throwIfRelevant
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (given : NEList<string>)
  (result : PT.NameResolution<'a>)
  : PT.NameResolution<'a> =
  result
  |> Result.mapError (fun err ->
    match onMissing with
    | OnMissing.ThrowError ->
      Exception.raiseInternal
        "Unresolved name when not allowed"
        [ "error", err; "given", given; "currentModule", currentModule ]
    | OnMissing.Allow -> err)


type GenericName = { modules : List<string>; name : string; version : int }

/// If we're 'given' the name `Option.Option`
/// and we're parsing in `Darklang.Stdlib`,
///
/// We should look for the thing in the following places:
/// - Darklang.Stdlib.Option.Option
/// - Darklang.Option.Option
/// - Option.Option
/// , in that order (most specific first).
///
/// TODO?: accept an Option<string> of the _owner_ as well.
/// I think that'll be useful in many contexts to help resolve names...
let namesToTry
  (currentModule : List<string>)
  (given : GenericName)
  : List<GenericName> =
  let rec loop (modulesToPrepend : List<string>) : List<GenericName> =
    match List.splitLast modulesToPrepend with
    | None -> [ given ]

    | Some(allButLast, _last) ->
      let newNameToTry = { given with modules = modulesToPrepend @ given.modules }
      newNameToTry :: loop allButLast

  // handle explicit Stdlib.etc shortcut
  let addl =
    match given.modules with
    | "Stdlib" :: _ -> [ { given with modules = "Darklang" :: given.modules } ]
    // TODO: additional shortcuts for Option and Result, at least?
    | _ -> []

  (loop currentModule) @ addl


/// Generic name resolution that handles the common pattern across Type/Value/Fn resolution
let resolveGenericName<'FQName, 'Builtin when 'Builtin : comparison>
  (builtins : Option<Set<'Builtin>>)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (given : NEList<string>)
  (parseName : string -> Result<string * int, string>)
  (findInPM : PT.PackageLocation -> Ply<Option<System.Guid>>)
  (makePackageFQName : System.Guid -> 'FQName)
  (makeBuiltinFQName : string * int -> 'FQName)
  (builtinToRT : string * int -> 'Builtin)
  : Ply<PT.NameResolution<'FQName>> =
  uply {
    let notFoundError = Error(NRE.NotFound(NEList.toList given))
    let (modules, name) = NEList.splitLast given

    match parseName name with
    | Error _ -> return Error(NRE.InvalidName(NEList.toList given))
    | Ok(name, version) ->
      let genericName = { modules = modules; name = name; version = version }

      let tryResolve (nameToTry : GenericName) : Ply<Result<'FQName, unit>> =
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
                return Ok fqName
              else
                return Error()
            | _ ->
              // Try package manager lookup
              let location : PT.PackageLocation =
                { owner = owner; modules = modules; name = nameToTry.name }
              match! findInPM location with
              | Some id -> return Ok(makePackageFQName id)
              | None -> return Error()
        }

      let! result =
        Ply.List.foldSequentially
          (fun currentResult nameToTry ->
            match currentResult with
            | Ok _ -> Ply currentResult
            | Error _ ->
              uply {
                match! tryResolve nameToTry with
                | Error() -> return currentResult
                | Ok success -> return Ok success
              })
          notFoundError
          (namesToTry currentModule genericName)

      return throwIfRelevant onMissing currentModule given result
  }


let resolveTypeName
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
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
    let parseTypeName name =
      FS2WT.Expr.parseTypeName name |> Result.map (fun n -> (n, 0))

    resolveGenericName
      emptyBuiltins
      onMissing
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
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQValueName.FQValueName>> =
  match name with
  | WT.KnownBuiltin(name, version) ->
    Ok(PT.FQValueName.fqBuiltIn name version) |> Ply
  | WT.Unresolved given ->
    resolveGenericName
      (Some builtins)
      onMissing
      currentModule
      given
      FS2WT.Expr.parseFnName
      packageManager.findValue
      PT.FQValueName.FQValueName.Package
      (fun (n, v) -> PT.FQValueName.Builtin { name = n; version = v })
      (fun (n, v) -> { RT.FQValueName.Builtin.name = n; version = v })


let resolveFnName
  (builtinFns : Set<RT.FQFnName.Builtin>)
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
  match name with
  | WT.KnownBuiltin(n, v) -> Ok(PT.FQFnName.fqBuiltIn n v) |> Ply
  | WT.Unresolved given ->
    resolveGenericName
      (Some builtinFns)
      onMissing
      currentModule
      given
      FS2WT.Expr.parseFnName
      packageManager.findFn
      PT.FQFnName.FQFnName.Package
      (fun (n, v) -> PT.FQFnName.Builtin { name = n; version = v })
      (fun (n, v) -> { RT.FQFnName.Builtin.name = n; version = v })
