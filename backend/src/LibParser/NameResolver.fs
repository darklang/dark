/// Conversion functions from WrittenTypes to ProgramTypes
module LibParser.NameResolver

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes
module NRE = LibExecution.NameResolutionError

type NameResolver =
  { builtinTypes : Set<PT.TypeName.BuiltIn>
    builtinFns : Set<PT.FnName.BuiltIn>
    builtinConstants : Set<PT.ConstantName.BuiltIn>

    userTypes : Set<PT.TypeName.UserProgram>
    userFns : Set<PT.FnName.UserProgram>
    userConstants : Set<PT.ConstantName.UserProgram> }

let empty : NameResolver =
  { builtinTypes = Set.empty
    builtinFns = Set.empty
    builtinConstants = Set.empty

    userTypes = Set.empty
    userFns = Set.empty
    userConstants = Set.empty }

let create
  (builtinTypes : List<PT.TypeName.BuiltIn>)
  (builtinFns : List<PT.FnName.BuiltIn>)
  (builtinConstants : List<PT.ConstantName.BuiltIn>)
  (userTypes : List<PT.TypeName.UserProgram>)
  (userFns : List<PT.FnName.UserProgram>)
  (userConstants : List<PT.ConstantName.UserProgram>)
  : NameResolver =
  { builtinTypes = Set.ofList builtinTypes
    builtinFns = Set.ofList builtinFns
    builtinConstants = Set.ofList builtinConstants

    userTypes = Set.ofList userTypes
    userFns = Set.ofList userFns
    userConstants = Set.ofList userConstants }

// TODO: this isn't a great way to deal with this but it'll do for now. When packages
// are included, this doesn't make much sense.
let merge (a : NameResolver) (b : NameResolver) : NameResolver =
  { builtinTypes = Set.union a.builtinTypes b.builtinTypes
    builtinFns = Set.union a.builtinFns b.builtinFns
    builtinConstants = Set.union a.builtinConstants b.builtinConstants

    userTypes = Set.union a.userTypes b.userTypes
    userFns = Set.union a.userFns b.userFns
    userConstants = Set.union a.userConstants b.userConstants

  }


let fromBuiltins
  ((fns, types, constants) : LibExecution.StdLib.Contents)
  : NameResolver =
  { builtinTypes =
      types
      |> List.map (fun typ -> PT2RT.TypeName.BuiltIn.fromRT typ.name)
      |> Set.ofList
    builtinFns =
      fns |> List.map (fun fn -> PT2RT.FnName.BuiltIn.fromRT fn.name) |> Set.ofList

    builtinConstants =
      constants
      |> List.map (fun fn -> PT2RT.ConstantName.BuiltIn.fromRT fn.name)
      |> Set.ofList

    userTypes = Set.empty
    userFns = Set.empty
    userConstants = Set.empty }


let fromExecutionState (state : RT.ExecutionState) : NameResolver =
  { builtinTypes =
      state.builtIns.types
      |> Map.keys
      |> List.map PT2RT.TypeName.BuiltIn.fromRT
      |> Set.ofList
    builtinFns =
      state.builtIns.fns
      |> Map.keys
      |> List.map PT2RT.FnName.BuiltIn.fromRT
      |> Set.ofList
    builtinConstants =
      state.builtIns.constants
      |> Map.keys
      |> List.map PT2RT.ConstantName.BuiltIn.fromRT
      |> Set.ofList

    userTypes =
      state.program.types
      |> Map.keys
      |> List.map PT2RT.TypeName.UserProgram.fromRT
      |> Set.ofList
    userFns =
      state.program.fns
      |> Map.keys
      |> List.map PT2RT.FnName.UserProgram.fromRT
      |> Set.ofList
    userConstants =
      state.program.constants
      |> Map.keys
      |> List.map PT2RT.ConstantName.UserProgram.fromRT
      |> Set.ofList }



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
  (currentModule : List<string>)
  (name : WT.Name)
  : PT.NameResolution<PT.FQName.T<'name>> =

  // These are named exactly during parsing
  match name with
  | WT.KnownBuiltin(modules, name, version) ->
    Ok(PT.FQName.fqBuiltIn nameValidator modules (constructor name) version)

  // Packages are unambiguous
  | WT.Unresolved({ head = "PACKAGE"; tail = owner :: rest } as names) ->
    match List.rev rest with
    | [] ->
      // This is a totally empty name, which _really_ shouldn't happen.
      Error(
        NRE.RTE.toRuntimeError
          { nameType = nameErrorType
            errorType = NRE.ErrorType.MissingModuleName
            names = NEList.toList names }

      )
    | name :: modules ->
      match parser name with
      | Ok(name, version) ->
        let modules =
          List.rev modules
          |> NEList.ofListUnsafe "Missing module name" [ "names", names ]
        Ok(
          PT.FQName.fqPackage nameValidator owner modules (constructor name) version
        )
      | Error _ ->
        Error(
          NRE.RTE.toRuntimeError
            { nameType = nameErrorType
              errorType = NRE.InvalidPackageName
              names = NEList.toList names }
        )

  | WT.Unresolved given ->
    let resolve (names : NEList<string>) : PT.NameResolution<PT.FQName.T<'name>> =
      let (modules, name) = NEList.splitLast names
      match parser name with
      | Error _msg ->
        Error(
          NRE.RTE.toRuntimeError
            { nameType = nameErrorType
              errorType = NRE.InvalidPackageName
              names = NEList.toList names }
        )
      | Ok(name, version) ->
        // 2. Name exactly matches something in the UserProgram space
        let (userProgram : PT.FQName.UserProgram<'name>) =
          { modules = modules; name = constructor name; version = version }

        if Set.contains userProgram userThings then
          Ok(PT.FQName.UserProgram userProgram)
        else
          // 3. Name exactly matches a BuiltIn thing
          let (builtIn : PT.FQName.BuiltIn<'name>) =
            { modules = modules; name = constructor name; version = version }

          if Set.contains builtIn builtinThings then
            Ok(PT.FQName.BuiltIn builtIn)
          else
            Error(
              NRE.RTE.toRuntimeError
                { nameType = nameErrorType
                  errorType = NRE.NotFound
                  names = NEList.toList names }
            )
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

    // List of modules lists, in order of priority
    let modulesToTry : List<NEList<string>> =
      let rec loop (modules : List<string>) : List<NEList<string>> =
        match modules with
        | [] -> [ given ]
        | _ ->
          let rest = List.initial modules |> Option.unwrap []
          (NEList.prependList modules given) :: loop rest
      loop currentModule

    let notFound =
      Error(
        NRE.RTE.toRuntimeError
          { nameType = nameErrorType
            errorType = NRE.NotFound
            names = NEList.toList given }
      )

    List.fold
      notFound
      (fun currentResult (modules : NEList<string>) ->
        match currentResult with
        | Ok _ -> currentResult
        | Error _ ->
          let newResult = resolve modules
          match newResult with
          | Ok _ -> newResult
          | Error _ -> currentResult // keep the first error message
      )
      modulesToTry


module TypeName =
  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : PT.NameResolution<PT.TypeName.T> =
    resolve
      PT.TypeName.assert'
      NRE.Type
      PT.TypeName.TypeName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseTypeName
      resolver.builtinTypes
      resolver.userTypes
      currentModule
      name

module FnName =
  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : PT.NameResolution<PT.FnName.T> =
    resolve
      PT.FnName.assert'
      NRE.Function
      PT.FnName.FnName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseFn
      resolver.builtinFns
      resolver.userFns
      currentModule
      name

module ConstantName =
  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : PT.NameResolution<PT.ConstantName.T> =
    resolve
      PT.ConstantName.assert'
      NRE.Constant
      PT.ConstantName.ConstantName
      FS2WT.Expr.parseFn // same format
      resolver.builtinConstants
      resolver.userConstants
      currentModule
      name
