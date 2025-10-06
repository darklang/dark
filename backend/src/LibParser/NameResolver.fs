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
    | _ -> []

  (loop currentModule) @ addl


let resolveTypeName
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
  match name with
  // TODO remodel things appropriately so this is not needed
  | WT.KnownBuiltin(_name, _version) ->
    Exception.raiseInternal "Builtin types don't exist" []
  | WT.Unresolved given ->
    let notFoundError = Error(NRE.NotFound(NEList.toList given))

    let tryPackageName
      (name : PT.PackageType.Name)
      : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
      // TODO: error if type version is somehow non-0 (here and other package stuff in this file)
      // TODO: also do this in the Dark equivalent
      uply {
        match! packageManager.findType name with
        | Some hash -> return Ok(PT.FQTypeName.FQTypeName.Package hash)
        | None -> return notFoundError
      }

    let tryResolve
      (name : GenericName)
      : Ply<Result<PT.FQTypeName.FQTypeName, unit>> =
      uply {
        match name.modules with
        | [] -> return Error()
        | owner :: modules ->
          let name = PT.PackageType.name owner modules name.name
          let! packageName = tryPackageName name
          return packageName |> Result.mapError (fun _ -> ())
      }

    uply {
      let (modules, name) = NEList.splitLast given

      // parses `TypeName_v2` into `(TypeName, 2)`, or just `TypeName` into `(TypeName, 0)`.
      // TODO: ensure we're validating fully and reasonably (e.g. include module)
      match FS2WT.Expr.parseTypeName name with
      | Error _ -> return Error(NRE.InvalidName(NEList.toList given))
      | Ok name ->
        let genericName = { modules = modules; name = name; version = 0 }

        let! (result : PT.NameResolution<PT.FQTypeName.FQTypeName>) =
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



let resolveValueName
  (builtinValues : Set<RT.FQValueName.Builtin>)
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQValueName.FQValueName>> =
  match name with
  | WT.KnownBuiltin(name, version) ->
    Ok(PT.FQValueName.fqBuiltIn name version) |> Ply
  | WT.Unresolved given ->
    let notFoundError = Error(NRE.NotFound(NEList.toList given))

    let tryPackageName
      (name : PT.PackageValue.Name)
      : Ply<PT.NameResolution<PT.FQValueName.FQValueName>> =
      uply {
        match! packageManager.findValue name with
        | Some hash -> return Ok(PT.FQValueName.FQValueName.Package hash)
        | None -> return notFoundError
      }

    let tryResolve
      (name : GenericName)
      : Ply<Result<PT.FQValueName.FQValueName, unit>> =
      uply {
        match name.modules with
        | [] -> return Error()
        | owner :: modules ->
          if owner = "Builtin" && modules = [] then
            let (builtInRT : RT.FQValueName.Builtin) =
              { name = name.name; version = name.version }

            if Set.contains builtInRT builtinValues then
              let (builtInPT : PT.FQValueName.Builtin) =
                { name = name.name; version = name.version }
              return Ok(PT.FQValueName.Builtin builtInPT)
            else
              return Error()
          else
            let name = PT.PackageValue.name owner modules name.name
            let! packageName = tryPackageName name
            return packageName |> Result.mapError (fun _ -> ())
      }

    uply {
      let (modules, name) = NEList.splitLast given

      match FS2WT.Expr.parseFnName name with
      | Error _ -> return Error(NRE.InvalidName(NEList.toList given))
      | Ok(name, version) ->
        let genericName = { modules = modules; name = name; version = version }

        let! (result : PT.NameResolution<PT.FQValueName.FQValueName>) =
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


let resolveFnName
  (builtinFns : Set<RT.FQFnName.Builtin>)
  (packageManager : PT.PackageManager)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
  match name with
  | WT.KnownBuiltin(name, version) -> Ok(PT.FQFnName.fqBuiltIn name version) |> Ply
  | WT.Unresolved given ->
    let notFoundError = Error(NRE.NotFound(NEList.toList given))

    let tryPackageName
      (name : PT.PackageFn.Name)
      : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
      uply {
        match! packageManager.findFn name with
        | Some hash -> return Ok(PT.FQFnName.FQFnName.Package hash)
        | None -> return notFoundError
      }

    let tryResolve (name : GenericName) : Ply<Result<PT.FQFnName.FQFnName, unit>> =
      uply {
        match name.modules with
        | [] -> return Error()
        | owner :: modules ->
          if owner = "Builtin" && modules = [] then
            let (builtInRT : RT.FQFnName.Builtin) =
              { name = name.name; version = name.version }

            if Set.contains builtInRT builtinFns then
              let (builtInPT : PT.FQFnName.Builtin) =
                { name = name.name; version = name.version }
              return Ok(PT.FQFnName.Builtin builtInPT)
            else
              return Error()

          else
            let name = PT.PackageFn.name owner modules name.name
            let! packageName = tryPackageName name
            return packageName |> Result.mapError (fun _ -> ())
      }

    uply {
      let (modules, name) = NEList.splitLast given

      match FS2WT.Expr.parseFnName name with
      | Error _ -> return Error(NRE.InvalidName(NEList.toList given))
      | Ok(name, version) ->
        let genericName = { modules = modules; name = name; version = version }

        let! (result : PT.NameResolution<PT.FQFnName.FQFnName>) =
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
