/// Conversion functions from WrittenTypes to ProgramTypes
module Parser.NameResolver

open Prelude
open Tablecloth

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module FS2WT = FSharpToWrittenTypes

type NameResolver =
  { userFns : Set<PT.FnName.UserProgram>
    userTypes : Set<PT.TypeName.UserProgram>
    builtinFns : Set<PT.FnName.BuiltIn>
    builtinTypes : Set<PT.TypeName.BuiltIn> }

let empty : NameResolver =
  { userFns = Set.empty
    userTypes = Set.empty
    builtinFns = Set.empty
    builtinTypes = Set.empty }

let create
  (userFns : List<PT.FnName.UserProgram>)
  (userTypes : List<PT.TypeName.UserProgram>)
  (builtinFns : List<PT.FnName.BuiltIn>)
  (builtinTypes : List<PT.TypeName.BuiltIn>)
  : NameResolver =
  { userFns = Set.ofList userFns
    userTypes = Set.ofList userTypes
    builtinFns = Set.ofList builtinFns
    builtinTypes = Set.ofList builtinTypes }


let fromContents ((fns, types) : LibExecution.StdLib.Contents) : NameResolver =
  { userFns = Set.empty
    userTypes = Set.empty
    builtinFns =
      fns
      |> List.map (fun fn -> fn.name)
      |> List.map (fun ({ name = RT.FnName.FnName name } as n) ->
        { PT.FQName.modules = n.modules
          PT.FQName.name = PT.FnName.FnName name
          PT.FQName.version = n.version })
      |> Set.ofList
    builtinTypes =
      types
      |> List.map (fun typ -> typ.name)
      |> List.map (fun ({ name = RT.TypeName.TypeName name } as n) ->
        { PT.FQName.modules = n.modules
          PT.FQName.name = PT.TypeName.TypeName name
          PT.FQName.version = n.version })
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
  (constructor : string -> 'name)
  (parser : string -> Result<string * int, string>)
  (userThings : Set<PT.FQName.UserProgram<'name>>)
  (builtinThings : Set<PT.FQName.BuiltIn<'name>>)
  (name : WT.Name)
  : PT.NameResolution<PT.FQName.T<'name>> =

  // These are named exactly during parsing
  match name with
  | WT.KnownBuiltin(modules, name, version) ->
    // TODO assert it's a valid builtin name
    Ok(PT.FQName.fqBuiltIn nameValidator modules (constructor name) version)

  // Packages are unambiguous
  | WT.Unresolved(("PACKAGE" :: owner :: rest) as names) ->
    match List.rev rest with
    | [] -> Error(LibExecution.Errors.MissingModuleName names)
    | name :: moduleLast :: moduleInit ->
      match parser name with
      | Ok(name, version) ->
        let modules = List.rev (moduleLast :: moduleInit) |> NonEmptyList.ofList
        Ok(
          PT.FQName.fqPackage nameValidator owner modules (constructor name) version
        )
      | Error _ -> Error(LibExecution.Errors.InvalidPackageName names)
    | _ -> Error(LibExecution.Errors.InvalidPackageName names)

  | WT.Unresolved names ->
    match List.rev names with
    | [] -> Error(LibExecution.Errors.MissingModuleName names)
    | name :: modules ->
      match parser name with
      | Error _msg -> Error(LibExecution.Errors.InvalidPackageName names)
      | Ok(name, version) ->
        // 2. Exact name is UserProgram
        let (userProgram : PT.FQName.UserProgram<'name>) =
          { modules = modules; name = constructor name; version = version }
        if Set.contains userProgram userThings then
          Ok(
            PT.FQName.fqUserProgram nameValidator modules (constructor name) version
          )
        else if List.isEmpty modules then
          Error(LibExecution.Errors.MissingModuleName names)
        else
          // 3. Exact name is BuiltIn
          let (builtIn : PT.FQName.BuiltIn<'name>) =
            { modules = modules; name = constructor name; version = version }
          if Set.contains builtIn builtinThings then
            Ok(PT.FQName.BuiltIn builtIn)
          else

            // 4. Check exact name in
            //   - a. current module // NOT IMPLEMENTED
            //   - b. parent module(s) // NOT IMPLEMENTED
            //   - c. darklang.stdlib package space
            Error(LibExecution.Errors.NotFound names)


module TypeName =
  let resolve
    (resolver : NameResolver)
    (name : WT.Name)
    : PT.NameResolution<PT.TypeName.T> =
    resolve
      PT.TypeName.assert'
      PT.TypeName.TypeName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseTypeName
      resolver.userTypes
      resolver.builtinTypes
      name

module FnName =
  let resolve
    (resolver : NameResolver)
    (name : WT.Name)
    : PT.NameResolution<PT.FnName.T> =
    resolve
      PT.FnName.assert'
      PT.FnName.FnName
      // TODO: move parsing fn into PT or WT
      FS2WT.Expr.parseFn
      resolver.userFns
      resolver.builtinFns
      name
