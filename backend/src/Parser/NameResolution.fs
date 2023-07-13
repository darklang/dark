module internal Parser.NameResolution

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module PT = LibExecution.ProgramTypes

module TypeName =
  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (name : PT.TypeName.T)
    : PT.TypeName.T =
    match name with
    | PT.FQName.Package _ -> name
    | PT.FQName.UserProgram n ->
      match n.modules with
      | "PACKAGE" :: owner :: package :: rest ->
        PT.FQName.Package
          { owner = owner
            modules = NonEmptyList.ofList (package :: rest)
            name = n.name
            version = n.version }
      | _ ->
        if Set.contains n userTypes then
          PT.FQName.UserProgram n
        else
          PT.FQName.BuiltIn
            { name = n.name; version = n.version; modules = n.modules }
    | PT.FQName.BuiltIn n ->
      let userName : PT.TypeName.UserProgram =
        { modules = n.modules; name = n.name; version = n.version }
      if Set.contains userName userTypes then
        PT.FQName.UserProgram(userName)
      else
        PT.FQName.BuiltIn(n)

module FnName =
  let resolveNames
    (userFns : Set<PT.FnName.UserProgram>)
    (name : PT.FnName.T)
    : PT.FnName.T =
    match name with
    | PT.FQName.Package _ -> name
    | PT.FQName.UserProgram n ->
      match n.modules with
      | "PACKAGE" :: owner :: package :: rest ->
        PT.FQName.Package
          { owner = owner
            modules = NonEmptyList.ofList (package :: rest)
            name = n.name
            version = n.version }
      | _ ->
        if Set.contains n userFns then
          PT.FQName.UserProgram n
        else
          PT.FQName.BuiltIn
            { name = n.name; version = n.version; modules = n.modules }
    | PT.FQName.BuiltIn n ->
      let userName : PT.FnName.UserProgram =
        { modules = n.modules; name = n.name; version = n.version }
      if Set.contains userName userFns then
        PT.FQName.UserProgram(userName)
      else
        PT.FQName.BuiltIn(n)


module TypeReference =

  let rec resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (typ : PT.TypeReference)
    : PT.TypeReference =
    let c = resolveNames userTypes
    match typ with
    | PT.TCustomType(tn, args) ->
      PT.TCustomType(TypeName.resolveNames userTypes tn, List.map c args)
    | PT.TFn(args, ret) -> PT.TFn(List.map c args, c ret)
    | PT.TTuple(first, second, theRest) ->
      PT.TTuple(c first, c second, List.map c theRest)
    | PT.TList arg -> PT.TList(c arg)
    | PT.TDict valArg -> PT.TDict(c valArg)
    | PT.TVariable _ -> typ
    | PT.TDB arg -> PT.TDB(c arg)
    | PT.TBool
    | PT.TBytes
    | PT.TInt
    | PT.TString
    | PT.TChar
    | PT.TFloat
    | PT.TDateTime
    | PT.TUuid
    | PT.TUnit
    | PT.TPassword -> typ


module Expr =
  // Second pass of parsing, fixing the thing it's impossible to get right on the
  // first pass, such as whether function names are user or stdlib names. Parse the
  // whole program once, and then run this on any expressions, passing in User types
  // and functions. It converts user types that are not in the list to Stdlib types.
  // TODO: we need some sort of unambiguous way to refer to user types
  let resolveNames
    (userFunctions : Set<PT.FnName.UserProgram>)
    (userTypes : Set<PT.TypeName.UserProgram>)
    (e : PT.Expr)
    : PT.Expr =
    let resolvePipeExprNames =
      (fun e ->
        match e with
        | PT.EPipeFnCall(id, name, typeArgs, args) ->
          PT.EPipeFnCall(id, name, typeArgs, args)
        | PT.EPipeEnum(id, typeName, caseName, fields) ->
          PT.EPipeEnum(
            id,
            TypeName.resolveNames userTypes typeName,
            caseName,
            fields
          )
        // pipes with variables might be fn calls
        | PT.EPipeVariable(id, name) ->
          match FS2WT.Expr.parseFn name with
          | Ok(name, version) ->
            if
              Set.contains (PT.FnName.userProgram [] name version) userFunctions
            then
              PT.EPipeFnCall(id, PT.FnName.fqUserProgram [] name version, [], [])
            else
              e
          | Error _ -> e
        | _ -> e)

    LibExecution.ProgramTypesAst.preTraversal
      identity
      resolvePipeExprNames
      identity
      (TypeName.resolveNames userTypes)
      (FnName.resolveNames userFunctions)
      identity
      identity
      e

module UserFunction =
  let resolveNames
    (userFunctions : Set<PT.FnName.UserProgram>)
    (userTypes : Set<PT.TypeName.UserProgram>)
    (f : PT.UserFunction.T)
    : PT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> List.map (fun p ->
          { p with typ = TypeReference.resolveNames userTypes p.typ })
      returnType = TypeReference.resolveNames userTypes f.returnType
      description = f.description
      deprecated = f.deprecated
      body = Expr.resolveNames userFunctions userTypes f.body }


module PackageFn =
  let resolveNames (f : PT.PackageFn.T) : PT.PackageFn.T =
    { tlid = f.tlid
      id = f.id
      name = f.name
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> List.map (fun p ->
          { p with typ = TypeReference.resolveNames Set.empty p.typ })
      returnType = TypeReference.resolveNames Set.empty f.returnType
      description = f.description
      deprecated = f.deprecated
      body = Expr.resolveNames Set.empty Set.empty f.body }


module TypeDeclaration =
  module EnumCase =
    let resolveNames
      (userTypes : Set<PT.TypeName.UserProgram>)
      (t : PT.TypeDeclaration.EnumCase)
      : PT.TypeDeclaration.EnumCase =
      { name = t.name
        fields =
          t.fields
          |> List.map (fun f ->
            { f with typ = TypeReference.resolveNames userTypes f.typ })
        description = t.description }


  module RecordField =
    let resolveNames
      (userTypes : Set<PT.TypeName.UserProgram>)
      (t : PT.TypeDeclaration.RecordField)
      : PT.TypeDeclaration.RecordField =
      { name = t.name
        typ = TypeReference.resolveNames userTypes t.typ
        description = t.description }

  module Definition =
    let resolveNames
      (userTypes : Set<PT.TypeName.UserProgram>)
      (t : PT.TypeDeclaration.Definition)
      : PT.TypeDeclaration.Definition =
      match t with
      | PT.TypeDeclaration.Enum(firstCase, additionalCases) ->
        PT.TypeDeclaration.Enum(
          EnumCase.resolveNames userTypes firstCase,
          additionalCases |> List.map (EnumCase.resolveNames userTypes)
        )
      | PT.TypeDeclaration.Record(firstField, additionalFields) ->
        PT.TypeDeclaration.Record(
          RecordField.resolveNames userTypes firstField,
          additionalFields |> List.map (RecordField.resolveNames userTypes)
        )
      | PT.TypeDeclaration.Alias typ ->
        PT.TypeDeclaration.Alias(TypeReference.resolveNames userTypes typ)

  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (t : PT.TypeDeclaration.T)
    : PT.TypeDeclaration.T =
    { definition = Definition.resolveNames userTypes t.definition
      typeParams = t.typeParams }


module UserType =
  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (t : PT.UserType.T)
    : PT.UserType.T =
    { tlid = t.tlid
      name = t.name
      declaration = TypeDeclaration.resolveNames userTypes t.declaration
      description = ""
      deprecated = PT.NotDeprecated }

module PackageType =
  let resolveNames (f : PT.PackageType.T) : PT.PackageType.T =
    { tlid = f.tlid
      id = f.id
      name = f.name
      description = f.description
      deprecated = f.deprecated
      declaration = TypeDeclaration.resolveNames Set.empty f.declaration }
