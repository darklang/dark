// CLEANUP : there is room for refactoring here to reduce duplicated code
/// Conversion functions from WrittenTypes to ProgramTypes
module LibParser.NameResolver

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes
module NRE = LibExecution.NameResolutionError

type NameResolver =
  {
    builtinFns : Set<PT.FQFnName.Builtin>
    builtinConstants : Set<PT.FQConstantName.Builtin>

    userTypes : Set<PT.FQTypeName.UserProgram>
    userFns : Set<PT.FQFnName.UserProgram>
    userConstants : Set<PT.FQConstantName.UserProgram>

    /// If a name is not found, should we raise an exception?
    ///
    /// - when the local package DB is fully empty, and we're filling it in for the
    ///   first time, we want to allow all names to be NotFound -- other package
    ///   items won't be there yet
    /// - sometimes when parsing, we're not sure whether something is:
    ///   - a variable
    ///   - or something else, like a constant or fn.
    ///   During these times, we want to allow errors as well, so we can
    ///   parse it as a variable as a fallback if nothing is found under that name.
    allowError : bool

    // Since packages often use things that are defined in the same package, and
    // those package items won't be in the package manager yet when we try to
    // resolve them, we need another approach here.
    hackLocallyDefinedPackageTypes : Set<RT.FQTypeName.Package>
    hackLocallyDefinedPackageFns : Set<RT.FQFnName.Package>
    hackLocallyDefinedPackageConstants : Set<RT.FQConstantName.Package>

    packageManager : RT.PackageManager
  }


let empty : NameResolver =
  { builtinFns = Set.empty
    builtinConstants = Set.empty

    userTypes = Set.empty
    userFns = Set.empty
    userConstants = Set.empty

    // CLEANUP this should probably default to `false`
    allowError = true

    hackLocallyDefinedPackageTypes = Set.empty
    hackLocallyDefinedPackageFns = Set.empty
    hackLocallyDefinedPackageConstants = Set.empty

    packageManager = RT.PackageManager.Empty }


let create
  (builtinFns : List<PT.FQFnName.Builtin>)
  (builtinConstants : List<PT.FQConstantName.Builtin>)
  (userTypes : List<PT.FQTypeName.UserProgram>)
  (userFns : List<PT.FQFnName.UserProgram>)
  (userConstants : List<PT.FQConstantName.UserProgram>)
  (allowError : bool)
  (packageManager : RT.PackageManager)
  : NameResolver =
  { builtinFns = Set.ofList builtinFns
    builtinConstants = Set.ofList builtinConstants

    userTypes = Set.ofList userTypes
    userFns = Set.ofList userFns
    userConstants = Set.ofList userConstants

    allowError = allowError

    hackLocallyDefinedPackageTypes = Set.empty
    hackLocallyDefinedPackageFns = Set.empty
    hackLocallyDefinedPackageConstants = Set.empty

    packageManager = packageManager }


let merge
  (a : NameResolver)
  (b : NameResolver)
  (packageManager : RT.PackageManager)
  : NameResolver =
  { builtinFns = Set.union a.builtinFns b.builtinFns
    builtinConstants = Set.union a.builtinConstants b.builtinConstants

    userTypes = Set.union a.userTypes b.userTypes
    userFns = Set.union a.userFns b.userFns
    userConstants = Set.union a.userConstants b.userConstants

    allowError = a.allowError && b.allowError

    hackLocallyDefinedPackageTypes =
      Set.union a.hackLocallyDefinedPackageTypes b.hackLocallyDefinedPackageTypes
    hackLocallyDefinedPackageFns =
      Set.union a.hackLocallyDefinedPackageFns b.hackLocallyDefinedPackageFns
    hackLocallyDefinedPackageConstants =
      Set.union
        a.hackLocallyDefinedPackageConstants
        b.hackLocallyDefinedPackageConstants

    packageManager = packageManager }


let fromBuiltins (builtins : RT.Builtins) : NameResolver =
  { builtinFns =
      builtins.fns
      |> Map.keys
      |> List.map PT2RT.FQFnName.Builtin.fromRT
      |> Set.ofList

    builtinConstants =
      builtins.constants
      |> Map.keys
      |> List.map PT2RT.FQConstantName.Builtin.fromRT
      |> Set.ofList

    userTypes = Set.empty
    userFns = Set.empty
    userConstants = Set.empty

    allowError = true

    hackLocallyDefinedPackageTypes = Set.empty
    hackLocallyDefinedPackageFns = Set.empty
    hackLocallyDefinedPackageConstants = Set.empty

    packageManager = RT.PackageManager.Empty }


let fromExecutionState (state : RT.ExecutionState) : NameResolver =
  { builtinFns =
      state.builtins.fns
      |> Map.keys
      |> List.map PT2RT.FQFnName.Builtin.fromRT
      |> Set.ofList
    builtinConstants =
      state.builtins.constants
      |> Map.keys
      |> List.map PT2RT.FQConstantName.Builtin.fromRT
      |> Set.ofList

    userTypes =
      state.program.types
      |> Map.keys
      |> List.map PT2RT.FQTypeName.UserProgram.fromRT
      |> Set.ofList
    userFns =
      state.program.fns
      |> Map.keys
      |> List.map PT2RT.FQFnName.UserProgram.fromRT
      |> Set.ofList
    userConstants =
      state.program.constants
      |> Map.keys
      |> List.map PT2RT.FQConstantName.UserProgram.fromRT
      |> Set.ofList

    allowError = true

    hackLocallyDefinedPackageFns = Set.empty
    hackLocallyDefinedPackageTypes = Set.empty
    hackLocallyDefinedPackageConstants = Set.empty

    packageManager = state.packageManager }


// TODO: there's a lot going on here to resolve the outer portion of the name and to
// avoid repeat work for package/modules, but it's also adding a lot of code and
// complexity. Perhaps this changes when packages become identified by ID


let resolveTypeName
  (nameErrorType : NRE.NameType)
  (parser : string -> Result<string * int, string>)
  (userTypes : Set<PT.FQTypeName.UserProgram>)
  (packageTypeExists : RT.FQTypeName.Package -> Ply<bool>)
  (packageNameMapper : PT.FQTypeName.Package -> RT.FQTypeName.Package)
  (allowError : bool)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
  uply {
    match name with
    | WT.KnownBuiltin(_name, _version) ->
      // todo this is not supposed to happen as we don't allow builtin types
      return
        Error({ nameType = nameErrorType; errorType = NRE.NotFound; names = [] })
    | WT.Unresolved given ->
      let resolve
        (names : NEList<string>)
        : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
        uply {
          let (modules, name) = NEList.splitLast names
          match parser name with
          | Error _msg ->
            return
              Error(
                { nameType = nameErrorType
                  errorType = NRE.InvalidPackageName
                  names = NEList.toList names }
              )
          | Ok(name, version) ->
            match modules with
            | "Stdlib" :: otherModules ->
              let name =
                PT.FQTypeName.package
                  "Darklang"
                  ("Stdlib" :: otherModules)
                  name
                  version

              let! packageTypeExists = packageTypeExists (packageNameMapper name)

              if packageTypeExists then
                return Ok(PT.FQTypeName.FQTypeName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )

            | "PACKAGE" :: owner :: modules ->
              let name = PT.FQTypeName.package owner modules name version

              let! packageTypeExists = packageTypeExists (packageNameMapper name)

              if packageTypeExists then
                return Ok(PT.FQTypeName.FQTypeName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )


            | _ ->
              // 2. Name exactly matches something in the UserProgram space
              let (userProgram : PT.FQTypeName.UserProgram) =
                { modules = modules; name = name; version = version }

              if Set.contains userProgram userTypes then
                return Ok(PT.FQTypeName.UserProgram userProgram)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
        }

      // 4. Check name in
      //   - a. exact name
      //   - b. current module
      //   - c. parent module(s)
      //   - d. darklang.stdlib package space // NOT IMPLEMENTED

      // Look in the current module and all parent modules
      // for X.Y, and current module A.B.C, try in the following order
      // A.B.C.X.Y
      // A.B.X.Y
      // A.X.Y
      // X.Y

      // List of locations to try and find the name
      // i.e. PACKAGE.[owner], PACKAGE.[owner].[module1]
      let namesToTry : List<NEList<string>> =
        let rec loop (modules : List<string>) : List<NEList<string>> =
          match modules with
          | [] -> [ given ]
          | _ ->
            let rest = List.initial modules
            (NEList.prependList modules given) :: loop rest
        loop currentModule

      let! (result : PT.NameResolution<PT.FQTypeName.FQTypeName>) =
        Ply.List.foldSequentially
          (fun currentResult (pathToTry : NEList<string>) ->
            match currentResult with
            | Ok _ -> Ply currentResult
            | Error _ ->
              uply {
                let! newResult = resolve pathToTry
                match newResult with
                | Ok _ -> return newResult
                | Error _ -> return currentResult // keep the first error message
              })
          (Error
            { nameType = nameErrorType
              errorType = NRE.NotFound
              names = NEList.toList given })
          namesToTry

      match result with
      | Ok result -> return Ok result
      | Error err ->
        if not allowError then
          return
            Exception.raiseInternal
              "Unresolved name when not allowed"
              [ "namesToTry", namesToTry
                "error", err
                "given", given
                "currentModule", currentModule ]
        else
          return Error err
  }



let resolveConstantName
  (nameErrorType : NRE.NameType)
  (parser : string -> Result<string * int, string>)
  (builtinConstants : Set<PT.FQConstantName.Builtin>)
  (userConstants : Set<PT.FQConstantName.UserProgram>)
  (packageConstantExists : RT.FQConstantName.Package -> Ply<bool>)
  (packageNameMapper : PT.FQConstantName.Package -> RT.FQConstantName.Package)
  (allowError : bool)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =

  uply {
    // These are named exactly during parsing
    match name with
    | WT.KnownBuiltin(name, version) ->
      return Ok(PT.FQConstantName.fqBuiltIn name version)

    | WT.Unresolved given ->
      let resolve
        (names : NEList<string>)
        : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =
        uply {
          let (modules, name) = NEList.splitLast names
          match parser name with
          | Error _msg ->
            return
              Error(
                { nameType = nameErrorType
                  errorType = NRE.InvalidPackageName
                  names = NEList.toList names }
              )
          | Ok(name, version) ->
            match modules with

            // TODO: reuse some code between this and the next match case
            | "Stdlib" :: otherModules ->
              let name =
                PT.FQConstantName.package
                  "Darklang"
                  ("Stdlib" :: otherModules)
                  name
                  version

              let! packageConstantExists =
                packageConstantExists (packageNameMapper name)

              if packageConstantExists then
                return Ok(PT.FQConstantName.FQConstantName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )

            | "PACKAGE" :: owner :: modules ->
              let name = PT.FQConstantName.package owner modules name version

              let! packageConstantExists =
                packageConstantExists (packageNameMapper name)

              if packageConstantExists then
                return Ok(PT.FQConstantName.FQConstantName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )

            | [ "Builtin" ] ->
              let (builtIn : PT.FQConstantName.Builtin) =
                { name = name; version = version }

              if Set.contains builtIn builtinConstants then
                return Ok(PT.FQConstantName.Builtin builtIn)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )

            | _ ->
              // 2. Name exactly matches something in the UserProgram space
              let (userProgram : PT.FQConstantName.UserProgram) =
                { modules = modules; name = name; version = version }

              if Set.contains userProgram userConstants then
                return Ok(PT.FQConstantName.UserProgram userProgram)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
        }

      // 4. Check name in
      //   - a. exact name
      //   - b. current module
      //   - c. parent module(s)
      //   - d. darklang.stdlib package space // NOT IMPLEMENTED

      // Look in the current module and all parent modules
      // for X.Y, and current module A.B.C, try in the following order
      // A.B.C.X.Y
      // A.B.X.Y
      // A.X.Y
      // X.Y

      // List of locations to try and find the name
      // i.e. PACKAGE.[owner], PACKAGE.[owner].[module1]
      let namesToTry : List<NEList<string>> =
        let rec loop (modules : List<string>) : List<NEList<string>> =
          match modules with
          | [] -> [ given ]
          | _ ->
            let rest = List.initial modules
            (NEList.prependList modules given) :: loop rest
        loop currentModule

      let! (result : PT.NameResolution<PT.FQConstantName.FQConstantName>) =
        Ply.List.foldSequentially
          (fun currentResult (pathToTry : NEList<string>) ->
            match currentResult with
            | Ok _ -> Ply currentResult
            | Error _ ->
              uply {
                let! newResult = resolve pathToTry
                match newResult with
                | Ok _ -> return newResult
                | Error _ -> return currentResult // keep the first error message
              })
          (Error
            { nameType = nameErrorType
              errorType = NRE.NotFound
              names = NEList.toList given })
          namesToTry

      match result with
      | Ok result -> return Ok result
      | Error err ->
        if not allowError then
          return
            Exception.raiseInternal
              "Unresolved name when not allowed"
              [ "namesToTry", namesToTry
                "error", err
                "given", given
                "currentModule", currentModule ]
        else
          return Error err
  }

let resolveFnName
  (nameErrorType : NRE.NameType)
  (parser : string -> Result<string * int, string>)
  (builtinFns : Set<PT.FQFnName.Builtin>)
  (userFns : Set<PT.FQFnName.UserProgram>)
  (packageFnExists : RT.FQFnName.Package -> Ply<bool>)
  (packageNameMapper : PT.FQFnName.Package -> RT.FQFnName.Package)
  (allowError : bool)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =

  uply {
    // These are named exactly during parsing
    match name with
    | WT.KnownBuiltin(name, version) -> return Ok(PT.FQFnName.fqBuiltIn name version)

    | WT.Unresolved given ->
      let resolve
        (names : NEList<string>)
        : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
        uply {
          let (modules, name) = NEList.splitLast names
          match parser name with
          | Error _msg ->
            return
              Error(
                { nameType = nameErrorType
                  errorType = NRE.InvalidPackageName
                  names = NEList.toList names }
              )
          | Ok(name, version) ->
            match modules with
            | "Stdlib" :: otherModules ->
              let name =
                PT.FQFnName.package
                  "Darklang"
                  ("Stdlib" :: otherModules)
                  name
                  version

              let! packageFnExists = packageFnExists (packageNameMapper name)

              if packageFnExists then
                return Ok(PT.FQFnName.FQFnName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
            | "PACKAGE" :: owner :: modules ->
              let name = PT.FQFnName.package owner modules name version

              let! packageFnExists = packageFnExists (packageNameMapper name)

              if packageFnExists then
                return Ok(PT.FQFnName.FQFnName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
            | [ "Builtin" ] ->
              let (builtIn : PT.FQFnName.Builtin) =
                { name = name; version = version }

              if Set.contains builtIn builtinFns then
                return Ok(PT.FQFnName.Builtin builtIn)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
            | _ ->
              // 2. Name exactly matches something in the UserProgram space
              let (userProgram : PT.FQFnName.UserProgram) =
                { modules = modules; name = name; version = version }

              if Set.contains userProgram userFns then
                return Ok(PT.FQFnName.UserProgram userProgram)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
        }

      // 4. Check name in
      //   - a. exact name
      //   - b. current module
      //   - c. parent module(s)
      //   - d. darklang.stdlib package space // NOT IMPLEMENTED

      // Look in the current module and all parent modules
      // for X.Y, and current module A.B.C, try in the following order
      // A.B.C.X.Y
      // A.B.X.Y
      // A.X.Y
      // X.Y

      // List of locations to try and find the name
      // i.e. PACKAGE.[owner], PACKAGE.[owner].[module1]
      let namesToTry : List<NEList<string>> =
        let rec loop (modules : List<string>) : List<NEList<string>> =
          match modules with
          | [] -> [ given ]
          | _ ->
            let rest = List.initial modules
            (NEList.prependList modules given) :: loop rest
        loop currentModule

      let! (result : PT.NameResolution<PT.FQFnName.FQFnName>) =
        Ply.List.foldSequentially
          (fun currentResult (pathToTry : NEList<string>) ->
            match currentResult with
            | Ok _ -> Ply currentResult
            | Error _ ->
              uply {
                let! newResult = resolve pathToTry
                match newResult with
                | Ok _ -> return newResult
                | Error _ -> return currentResult // keep the first error message
              })
          (Error
            { nameType = nameErrorType
              errorType = NRE.NotFound
              names = NEList.toList given })
          namesToTry

      match result with
      | Ok result -> return Ok result
      | Error err ->
        if not allowError then
          return
            Exception.raiseInternal
              "Unresolved name when not allowed"
              [ "namesToTry", namesToTry
                "error", err
                "given", given
                "currentModule", currentModule ]
        else
          return Error err
  }




module TypeName =
  let packageTypeExists
    (pm : RT.PackageManager)
    (hackLocallyDefinedPackageTypes : Set<RT.FQTypeName.Package>)
    (typeName : RT.FQTypeName.Package)
    : Ply<bool> =
    uply {
      match! pm.getType typeName with
      | None -> return Set.contains typeName hackLocallyDefinedPackageTypes
      | Some _ -> return true
    }

  let maybeResolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
    resolveTypeName
      NRE.Type
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseTypeName
      resolver.userTypes
      (packageTypeExists
        resolver.packageManager
        resolver.hackLocallyDefinedPackageTypes)
      PT2RT.FQTypeName.Package.toRT
      true
      currentModule
      name

  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
    resolveTypeName
      NRE.Type
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseTypeName
      resolver.userTypes
      (packageTypeExists
        resolver.packageManager
        resolver.hackLocallyDefinedPackageTypes)
      PT2RT.FQTypeName.Package.toRT
      resolver.allowError
      currentModule
      name


module ConstantName =
  let packageConstExists
    (pm : RT.PackageManager)
    (hackLocallyDefinedPackageConstants : Set<RT.FQConstantName.Package>)
    (constName : RT.FQConstantName.Package)
    : Ply<bool> =
    uply {
      match! pm.getConstant constName with
      | None -> return Set.contains constName hackLocallyDefinedPackageConstants
      | Some _ -> return true
    }


  let maybeResolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =
    resolveConstantName
      NRE.Constant
      FS2WT.Expr.parseFn // same format
      resolver.builtinConstants
      resolver.userConstants
      (packageConstExists
        resolver.packageManager
        resolver.hackLocallyDefinedPackageConstants)
      PT2RT.FQConstantName.Package.toRT
      true
      currentModule
      name

  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =
    resolveConstantName
      NRE.Constant
      FS2WT.Expr.parseFn // same format
      resolver.builtinConstants
      resolver.userConstants
      (packageConstExists
        resolver.packageManager
        resolver.hackLocallyDefinedPackageConstants)
      PT2RT.FQConstantName.Package.toRT
      resolver.allowError
      currentModule
      name


module FnName =
  let packageFnExists
    (pm : RT.PackageManager)
    (hackLocallyDefinedPackageFns : Set<RT.FQFnName.Package>)
    (fnName : RT.FQFnName.Package)
    : Ply<bool> =
    uply {
      match! pm.getFn fnName with
      | None -> return Set.contains fnName hackLocallyDefinedPackageFns
      | Some _ -> return true
    }

  let maybeResolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
    resolveFnName
      NRE.Function
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseFn
      resolver.builtinFns
      resolver.userFns
      (packageFnExists resolver.packageManager resolver.hackLocallyDefinedPackageFns)
      PT2RT.FQFnName.Package.toRT
      true
      currentModule
      name

  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
    resolveFnName
      NRE.Function
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseFn
      resolver.builtinFns
      resolver.userFns
      (packageFnExists resolver.packageManager resolver.hackLocallyDefinedPackageFns)
      PT2RT.FQFnName.Package.toRT
      resolver.allowError
      currentModule
      name
