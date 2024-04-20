module LibParser.NameResolver

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes
module NRE = LibExecution.NameResolutionError


type UserStuff =
  { types : Set<PT.FQTypeName.UserProgram>
    constants : Set<PT.FQConstantName.UserProgram>
    fns : Set<PT.FQFnName.UserProgram> }

  static member empty = { types = Set.empty; constants = Set.empty; fns = Set.empty }
  static member fromProgram(program : RT.Program) : UserStuff =
    let map fn items = items |> Map.keys |> List.map fn |> Set
    { types = program.types |> map PT2RT.FQTypeName.UserProgram.fromRT
      constants = program.constants |> map PT2RT.FQConstantName.UserProgram.fromRT
      fns = program.fns |> map PT2RT.FQFnName.UserProgram.fromRT }


/// If a name is not found, should we raise an exception?
///
/// - when the local package DB is fully empty, and we're filling it in for the
///   first time, we want to allow all names to be NotFound -- other package
///   items won't be there yet
/// - sometimes when parsing, we're not sure whether something is:
///   - a variable
///   - or something else, like a constant or fn.
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

  // handle explicit PACKAGE.etc and Stdlib.etc shortcut
  let addl =
    match given.modules with
    | "Stdlib" :: _ -> [ { given with modules = "Darklang" :: given.modules } ]
    | "PACKAGE" :: owner :: modules -> [ { given with modules = owner :: modules } ]
    | _ -> []

  (loop currentModule) @ addl


let resolveTypeName
  (packageManager : RT.PackageManager)
  (userTypes : Set<PT.FQTypeName.UserProgram>)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
  let err errType names : PT.NameResolution<PT.FQTypeName.FQTypeName> =
    Error { nameType = NRE.Type; errorType = errType; names = names }

  match name with
  // TODO remodel things appropriately so this is not needed
  | WT.KnownBuiltin(_name, _version) ->
    Exception.raiseInternal "Builtin types don't exist" []
  | WT.Unresolved given ->
    let notFoundError = err NRE.NotFound (NEList.toList given)

    let tryPackageName
      (name : PT.FQTypeName.Package)
      : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
      let rtName = PT2RT.FQTypeName.Package.toRT name
      uply {
        match! packageManager.getType rtName with
        | Some _found -> return Ok(PT.FQTypeName.FQTypeName.Package name)
        | None -> return notFoundError
      }

    let tryResolve
      (name : GenericName)
      : Ply<Result<PT.FQTypeName.FQTypeName, unit>> =
      uply {
        let (userProgram : PT.FQTypeName.UserProgram) =
          { modules = name.modules; name = name.name; version = name.version }

        if Set.contains userProgram userTypes then
          return Ok(PT.FQTypeName.UserProgram userProgram)
        else
          match name.modules with
          | [] -> return Error()
          | owner :: modules ->
            let name = PT.FQTypeName.package owner modules name.name name.version
            let! packageName = tryPackageName name
            return packageName |> Result.mapError (fun _ -> ())
      }

    uply {
      let (modules, name) = NEList.splitLast given

      // parses `TypeName_v2` into `(TypeName, 2)`, or just `TypeName` into `(TypeName, 0)`.
      // TODO: ensure we're validating fully and reasonably (e.g. include module)
      match FS2WT.Expr.parseTypeName name with
      | Error _ -> return err NRE.InvalidPackageName (NEList.toList given)
      | Ok(name, version) ->
        let genericName = { modules = modules; name = name; version = version }

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



let resolveConstantName
  (builtinConstants : Set<RT.FQConstantName.Builtin>)
  (packageManager : RT.PackageManager)
  (userConstants : Set<PT.FQConstantName.UserProgram>)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =
  let err errType names : PT.NameResolution<PT.FQConstantName.FQConstantName> =
    Error { nameType = NRE.Constant; errorType = errType; names = names }

  match name with
  | WT.KnownBuiltin(name, version) ->
    Ok(PT.FQConstantName.fqBuiltIn name version) |> Ply
  | WT.Unresolved given ->
    let notFoundError = err NRE.NotFound (NEList.toList given)

    let tryPackageName
      (name : PT.FQConstantName.Package)
      : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =
      let rtName = PT2RT.FQConstantName.Package.toRT name
      uply {
        match! packageManager.getConstant rtName with
        | Some _found -> return Ok(PT.FQConstantName.FQConstantName.Package name)
        | None -> return notFoundError
      }

    let tryResolve
      (name : GenericName)
      : Ply<Result<PT.FQConstantName.FQConstantName, unit>> =
      uply {
        let (userProgram : PT.FQConstantName.UserProgram) =
          { modules = name.modules; name = name.name; version = name.version }

        if name.modules = [ "Builtin" ] then
          let (builtInRT : RT.FQConstantName.Builtin) =
            { name = name.name; version = name.version }

          if Set.contains builtInRT builtinConstants then
            let (builtInPT : PT.FQConstantName.Builtin) =
              { name = name.name; version = name.version }
            return Ok(PT.FQConstantName.Builtin builtInPT)
          else
            return Error()
        elif Set.contains userProgram userConstants then
          return Ok(PT.FQConstantName.UserProgram userProgram)
        else
          match name.modules with
          | [] -> return Error()
          | owner :: modules ->
            let name = PT.FQConstantName.package owner modules name.name name.version
            let! packageName = tryPackageName name
            return packageName |> Result.mapError (fun _ -> ())
      }

    uply {
      let (modules, name) = NEList.splitLast given

      match FS2WT.Expr.parseFnName name with
      | Error _ -> return err NRE.InvalidPackageName (NEList.toList given)
      | Ok(name, version) ->

        let genericName = { modules = modules; name = name; version = version }

        let! (result : PT.NameResolution<PT.FQConstantName.FQConstantName>) =
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
  (packageManager : RT.PackageManager)
  (userFns : Set<PT.FQFnName.UserProgram>)
  (onMissing : OnMissing)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
  let err errType names : PT.NameResolution<PT.FQFnName.FQFnName> =
    Error { nameType = NRE.Function; errorType = errType; names = names }

  match name with
  | WT.KnownBuiltin(name, version) -> Ok(PT.FQFnName.fqBuiltIn name version) |> Ply
  | WT.Unresolved given ->
    let notFoundError = err NRE.NotFound (NEList.toList given)

    let tryPackageName
      (name : PT.FQFnName.Package)
      : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
      let rtName = PT2RT.FQFnName.Package.toRT name
      uply {
        match! packageManager.getFn rtName with
        | Some _found -> return Ok(PT.FQFnName.FQFnName.Package name)
        | None -> return notFoundError
      }

    let tryResolve (name : GenericName) : Ply<Result<PT.FQFnName.FQFnName, unit>> =
      uply {
        let (userProgram : PT.FQFnName.UserProgram) =
          { modules = name.modules; name = name.name; version = name.version }

        if name.modules = [ "Builtin" ] then
          let (builtInRT : RT.FQFnName.Builtin) =
            { name = name.name; version = name.version }

          if Set.contains builtInRT builtinFns then
            let (builtInPT : PT.FQFnName.Builtin) =
              { name = name.name; version = name.version }
            return Ok(PT.FQFnName.Builtin builtInPT)
          else
            return Error()

        elif Set.contains userProgram userFns then
          return Ok(PT.FQFnName.UserProgram userProgram)
        else
          match name.modules with
          | [] -> return Error()
          | owner :: modules ->
            let name = PT.FQFnName.package owner modules name.name name.version
            let! packageName = tryPackageName name
            return packageName |> Result.mapError (fun _ -> ())
      }

    uply {
      let (modules, name) = NEList.splitLast given

      match FS2WT.Expr.parseFnName name with
      | Error _ -> return err NRE.InvalidPackageName (NEList.toList given)
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
