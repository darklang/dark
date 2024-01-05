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
    builtinFns : Set<PT.FQFnName.BuiltIn>
    builtinConstants : Set<PT.FQConstantName.BuiltIn>

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

    packageManager : Option<RT.PackageManager>
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

    packageManager = None }


let create
  (builtinFns : List<PT.FQFnName.BuiltIn>)
  (builtinConstants : List<PT.FQConstantName.BuiltIn>)
  (userTypes : List<PT.FQTypeName.UserProgram>)
  (userFns : List<PT.FQFnName.UserProgram>)
  (userConstants : List<PT.FQConstantName.UserProgram>)
  (allowError : bool)
  (packageManager : Option<RT.PackageManager>)
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
  (packageManager : Option<RT.PackageManager>)
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


let fromBuiltins ((fns, constants) : LibExecution.Builtin.Contents) : NameResolver =
  { builtinFns =
      fns |> List.map (fun fn -> PT2RT.FQFnName.Builtin.fromRT fn.name) |> Set.ofList

    builtinConstants =
      constants
      |> List.map (fun fn -> PT2RT.FQConstantName.Builtin.fromRT fn.name)
      |> Set.ofList

    userTypes = Set.empty
    userFns = Set.empty
    userConstants = Set.empty

    allowError = true

    hackLocallyDefinedPackageTypes = Set.empty
    hackLocallyDefinedPackageFns = Set.empty
    hackLocallyDefinedPackageConstants = Set.empty

    packageManager = None }


let fromExecutionState (state : RT.ExecutionState) : NameResolver =
  { builtinFns =
      state.builtIns.fns
      |> Map.keys
      |> List.map PT2RT.FQFnName.Builtin.fromRT
      |> Set.ofList
    builtinConstants =
      state.builtIns.constants
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

    packageManager = Some state.packageManager }


// TODO: there's a lot going on here to resolve the outer portion of the name and to
// avoid repeat work for package/modules, but it's also adding a lot of code and
// complexity. Perhaps this changes when packages become identified by ID



// Desired resolution order:
// - 1. PACKAGE.* -> Exact package name
// - 2. Check exact name is a user name
// - 3. Check exact name is a builtin name
// - 4. Check exact name in
//   - a. current module
//   - b. parent module(s)
//   - c. darklang.stdlib package space // NOT IMPLEMENTED
let resolve
  (nameValidator : PT.FQName.NameValidator<'name>)
  (nameErrorType : NRE.NameType)
  (constructor : string -> 'name)
  (parser : string -> Result<string * int, string>)
  (builtinThings : Set<PT.FQName.BuiltIn<'name>>)
  (userThings : Set<PT.FQName.UserProgram<'name>>)
  (packageThingExists : RT.FQName.Package<'rtName> -> Ply<bool>)
  (packageNameMapper : PT.FQName.Package<'name> -> RT.FQName.Package<'rtName>)
  (allowError : bool)
  (currentModule : List<string>)
  (name : WT.Name)
  : Ply<PT.NameResolution<PT.FQName.FQName<'name>>> =

  uply {
    // These are named exactly during parsing
    match name with
    | WT.KnownBuiltin(modules, name, version) ->
      return Ok(PT.FQName.fqBuiltIn nameValidator modules (constructor name) version)

    | WT.Unresolved given ->
      let resolve
        (names : NEList<string>)
        : Ply<PT.NameResolution<PT.FQName.FQName<'name>>> =
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
            | "PACKAGE" :: owner :: modules ->
              let name =
                PT.FQName.package
                  nameValidator
                  owner
                  modules
                  (constructor name)
                  version

              let! packageThingExists = packageThingExists (packageNameMapper name)

              if packageThingExists then
                return Ok(PT.FQName.FQName.Package name)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
            | "Builtin" :: modules ->
              let (builtIn : PT.FQName.BuiltIn<'name>) =
                { modules = modules; name = constructor name; version = version }

              if Set.contains builtIn builtinThings then
                return Ok(PT.FQName.BuiltIn builtIn)
              else
                return
                  Error(
                    { nameType = nameErrorType
                      errorType = NRE.NotFound
                      names = NEList.toList names }
                  )
            | _ ->
              // 2. Name exactly matches something in the UserProgram space
              let (userProgram : PT.FQName.UserProgram<'name>) =
                { modules = modules; name = constructor name; version = version }

              if Set.contains userProgram userThings then
                return Ok(PT.FQName.UserProgram userProgram)
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

      let! (result : PT.NameResolution<PT.FQName.FQName<'name>>) =
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
    (pm : Option<RT.PackageManager>)
    (hackLocallyDefinedPackageTypes : Set<RT.FQTypeName.Package>)
    (typeName : RT.FQTypeName.Package)
    : Ply<bool> =
    uply {
      match pm with
      | None -> return false
      | Some pm ->
        match! pm.getType typeName with
        | None -> return Set.contains typeName hackLocallyDefinedPackageTypes
        | Some _ -> return true
    }

  let maybeResolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
    resolve
      PT.FQTypeName.assert'
      NRE.Type
      PT.FQTypeName.FQTypeName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseTypeName
      Set.empty
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
    resolve
      PT.FQTypeName.assert'
      NRE.Type
      PT.FQTypeName.FQTypeName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseTypeName
      Set.empty
      resolver.userTypes
      (packageTypeExists
        resolver.packageManager
        resolver.hackLocallyDefinedPackageTypes)
      PT2RT.TypeName.Package.toRT
      resolver.allowError
      currentModule
      name

module FnName =
  let packageFnExists
    (pm : Option<RT.PackageManager>)
    (hackLocallyDefinedPackageFns : Set<RT.FQFnName.Package>)
    (fnName : RT.FQFnName.Package)
    : Ply<bool> =
    uply {
      match pm with
      | None -> return false
      | Some pm ->
        match! pm.getFn fnName with
        | None -> return Set.contains fnName hackLocallyDefinedPackageFns
        | Some _ -> return true
    }

  let maybeResolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQFnName.FnName>> =
    resolve
      PT.FQFnName.assert'
      NRE.Function
      PT.FQFnName.FQFnName
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
    resolve
      PT.FQFnName.assert'
      NRE.Function
      PT.FQFnName.FQFnName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseFn
      resolver.builtinFns
      resolver.userFns
      (packageFnExists resolver.packageManager resolver.hackLocallyDefinedPackageFns)
      PT2RT.FQFnName.Package.toRT
      resolver.allowError
      currentModule
      name

module ConstantName =
  let packageConstExists
    (pm : Option<RT.PackageManager>)
    (hackLocallyDefinedPackageConstants : Set<RT.ConstantName.Package>)
    (constName : RT.ConstantName.Package)
    : Ply<bool> =
    uply {
      match pm with
      | None -> return false
      | Some pm ->
        match! pm.getConstant constName with
        | None -> return Set.contains constName hackLocallyDefinedPackageConstants
        | Some _ -> return true
    }


  let maybeResolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : Ply<PT.NameResolution<PT.FQConstantName.FQConstantName>> =
    resolve
      PT.FQConstantName.assert'
      NRE.Constant
      PT.FQConstantName.FQConstantName
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
    resolve
      PT.FQConstantName.assert'
      NRE.Constant
      PT.FQConstantName.FQConstantName
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
