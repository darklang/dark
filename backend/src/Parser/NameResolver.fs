/// Conversion functions from WrittenTypes to ProgramTypes
module Parser.NameResolver

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes

type NameResolver =
  { builtinTypes : Set<PT.TypeName.BuiltIn>
    builtinFns : Set<PT.FnName.BuiltIn>

    userTypes : Set<PT.TypeName.UserProgram>
    userFns : Set<PT.FnName.UserProgram> }

let empty : NameResolver =
  { builtinTypes = Set.empty
    builtinFns = Set.empty

    userTypes = Set.empty
    userFns = Set.empty }

let create
  (builtinTypes : List<PT.TypeName.BuiltIn>)
  (builtinFns : List<PT.FnName.BuiltIn>)
  (userTypes : List<PT.TypeName.UserProgram>)
  (userFns : List<PT.FnName.UserProgram>)
  : NameResolver =
  { builtinTypes = Set.ofList builtinTypes
    builtinFns = Set.ofList builtinFns

    userTypes = Set.ofList userTypes
    userFns = Set.ofList userFns }

// TODO: this isn't a great way to deal with this but it'll do for now. When packages
// are included, this doesn't make much sense.
let merge (a : NameResolver) (b : NameResolver) : NameResolver =
  { builtinTypes = Set.union a.builtinTypes b.builtinTypes
    builtinFns = Set.union a.builtinFns b.builtinFns

    userTypes = Set.union a.userTypes b.userTypes
    userFns = Set.union a.userFns b.userFns }


let fromBuiltins ((fns, types) : LibExecution.StdLib.Contents) : NameResolver =
  { builtinTypes =
      types
      |> List.map (fun typ -> PT2RT.TypeName.BuiltIn.fromRT typ.name)
      |> Set.ofList
    builtinFns =
      fns |> List.map (fun fn -> PT2RT.FnName.BuiltIn.fromRT fn.name) |> Set.ofList

    userTypes = Set.empty
    userFns = Set.empty }


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

    userTypes =
      state.program.types
      |> Map.keys
      |> List.map PT2RT.TypeName.UserProgram.fromRT
      |> Set.ofList
    userFns =
      state.program.fns
      |> Map.keys
      |> List.map PT2RT.FnName.UserProgram.fromRT
      |> Set.ofList }


// TODO: there's a lot going on her to resolve the outer portion of the name and to
// avoid repeat work for package/modules, but it's also adding a lot of code and
// complexity. Perhaps this changes when packages become identified by ID



// Desired resolution order:
// - 1. PACKAGE.* -> Exact package name
// - 2. Check exact name is a user name
// - 3. Check exact name is a builtin name
// - 4. Check exact name in
//   - a. current module // NOT IMPLEMENTED
//   - b. parent module(s) // NOT IMPLEMENTED
//   - c. darklang.stdlib package space // NOT IMPLEMENTED
let resolve
  (nameValidator : PT.FQName.NameValidator<'name>)
  (nameErrorType : LibExecution.Errors.NameResolution.NameType)
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
  | WT.Unresolved(("PACKAGE" :: owner :: rest) as names) ->
    match List.rev rest with
    | [] ->
      // This is a totally empty name, which _really_ shouldn't happen.
      Error(
        { nameType = nameErrorType
          errorType = LibExecution.Errors.NameResolution.MissingModuleName
          names = names }
      )
    | name :: modules ->
      match parser name with
      | Ok(name, version) ->
        let modules = List.rev modules |> NonEmptyList.ofList
        Ok(
          PT.FQName.fqPackage nameValidator owner modules (constructor name) version
        )
      | Error _ ->
        Error(
          { nameType = nameErrorType
            errorType = LibExecution.Errors.NameResolution.InvalidPackageName
            names = names }
        )


  | WT.Unresolved given ->
    let resolve (names : List<string>) : PT.NameResolution<PT.FQName.T<'name>> =
      // debuG "resolving" names
      match List.rev names with
      | [] ->
        // This is a totally empty name, which _really_ shouldn't happen.
        Error(
          { nameType = nameErrorType
            errorType = LibExecution.Errors.NameResolution.MissingModuleName
            names = names }
        )

      | name :: modules ->
        match parser name with
        | Error _msg ->
          Error(
            { nameType = nameErrorType
              errorType = LibExecution.Errors.NameResolution.InvalidPackageName
              names = names }
          )
        | Ok(name, version) ->
          let modules = List.reverse modules

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
                { nameType = nameErrorType
                  errorType = LibExecution.Errors.NameResolution.NotFound
                  names = names }
              )
    // 4. Check name in
    //   - a. exact name
    //   - b. current module
    //   - c. parent module(s) // NOT IMPLEMENTED
    //   - d. darklang.stdlib package space // NOT IMPLEMENTED
    // debuGSet "builtins" builtinThings
    // debuG "builtIn" builtIn
    // debuG "not found names" names
    //System.Environment.Exit(1)

    // Look in the current module and all parent modules
    // for X.Y, and current module A.B.C, try in the following order
    // X.Y
    // A.B.C.X.Y
    // A.B.X.Y
    // A.X.Y
    let modulesToTry =
      let rec loop (modules : List<string>) : List<List<string>> =
        match modules with
        | [] -> []
        | _ ->
          let rest = List.initial modules |> Option.unwrap []
          [ modules @ given ] @ loop rest
      loop currentModule

    List.fold
      (resolve given)
      (fun resolved modules ->
        match resolved with
        | Ok _ -> resolved
        | Error _ -> resolve modules)
      modulesToTry


module TypeName =
  let resolve
    (resolver : NameResolver)
    (currentModule : List<string>)
    (name : WT.Name)
    : PT.NameResolution<PT.TypeName.T> =
    resolve
      PT.TypeName.assert'
      LibExecution.Errors.NameResolution.Type
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
      LibExecution.Errors.NameResolution.Function
      PT.FnName.FnName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseFn
      resolver.builtinFns
      resolver.userFns
      currentModule
      name
