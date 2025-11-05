module internal LibParser.Package

open FSharp.Compiler.Syntax

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = NameResolver

open Utils

type WTPackageModule =
  { fns : List<WT.PackageFn.PackageFn>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue> }
let emptyWTModule = { fns = []; types = []; values = [] }


/// Update a Package by parsing a single F# let binding
let parseLetBinding
  (fileName : string)
  (modules : List<string>)
  (letBinding : SynBinding)
  : List<WT.PackageFn.PackageFn> * List<WT.PackageValue.PackageValue> =
  match modules with
  | owner :: modules ->
    if FS2WT.Function.hasArguments letBinding then
      [ FS2WT.PackageFn.fromSynBinding owner modules letBinding ], []
    else
      [], [ FS2WT.PackageValue.fromSynBinding owner modules letBinding ]
  | _ ->
    Exception.raiseInternal
      "Expected owner module containing things"
      [ "modules", modules; "binding", letBinding; "fileName", fileName ]

let parseTypeDef
  (fileName : string)
  (modules : List<string>)
  (defn : SynTypeDefn)
  : WT.PackageType.PackageType =
  match modules with
  | owner :: modules -> FS2WT.PackageType.fromSynTypeDefn owner modules defn
  | _ ->
    Exception.raiseInternal
      "Expected owner module"
      [ "modules", modules; "defn", defn; "fileName", fileName ]


let rec parseDecls
  (fileName : string)
  (moduleNames : List<string>)
  (decls : List<SynModuleDecl>)
  : WTPackageModule =
  List.fold
    (fun m decl ->
      match decl with
      | SynModuleDecl.Let(_, bindings, _) ->
        let (fns, values) =
          bindings |> List.map (parseLetBinding fileName moduleNames) |> List.unzip
        { m with
            fns = m.fns @ List.flatten fns
            values = m.values @ List.flatten values }

      | SynModuleDecl.Types(defns, _) ->
        let types = List.map (parseTypeDef fileName moduleNames) defns
        { m with types = m.types @ types }

      | SynModuleDecl.NestedModule(SynComponentInfo(_,
                                                    _,
                                                    _,
                                                    nestedModuleNames,
                                                    _,
                                                    _,
                                                    _,
                                                    _),
                                   _,
                                   nested,
                                   _,
                                   _,
                                   _) ->
        let moduleNames = moduleNames @ (nestedModuleNames |> List.map _.idText)
        let nestedDecls = parseDecls fileName moduleNames nested

        { fns = m.fns @ nestedDecls.fns
          types = m.types @ nestedDecls.types
          values = m.values @ nestedDecls.values }


      | _ ->
        Exception.raiseInternal
          $"Unsupported declaration"
          [ "decl", decl; "moduleNames", moduleNames ])
    emptyWTModule
    decls


let parse
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (filename : string)
  (contents : string)
  : Ply<List<PT.PackageOp>> =
  uply {
    match parseAsFSharpSourceFile filename contents with
    | ParsedImplFileInput(_,
                          _,
                          _,
                          _,
                          _,
                          [ SynModuleOrNamespace(longId,
                                                 _,
                                                 kind,
                                                 decls,
                                                 _,
                                                 _,
                                                 _,
                                                 _,
                                                 _) ],
                          _,
                          _,
                          _) ->
      let baseModule =
        match kind with
        | SynModuleOrNamespaceKind.NamedModule -> longIdentToList longId
        | _ -> []

      let modul : WTPackageModule = parseDecls filename baseModule decls


      let! fns =
        modul.fns
        |> Ply.List.mapSequentially (fun fn ->
          WT2PT.PackageFn.toPT
            builtins
            pm
            onMissing
            (WT2PT.PackageFn.Name.toModules fn.name)
            fn)

      let! types =
        modul.types
        |> Ply.List.mapSequentially (fun typ ->
          WT2PT.PackageType.toPT
            pm
            onMissing
            (WT2PT.PackageType.Name.toModules typ.name)
            typ)

      let! values =
        modul.values
        |> Ply.List.mapSequentially (fun value ->
          WT2PT.PackageValue.toPT
            builtins
            pm
            onMissing
            (WT2PT.PackageValue.Name.toModules value.name)
            value)

      // Generate PackageOps from parsed items
      let ops : List<PT.PackageOp> =
        [ // Add all types and their locations
          for (wtType, ptType) in List.zip modul.types types do
            yield PT.PackageOp.AddType ptType
            yield
              PT.PackageOp.SetTypeName(
                ptType.id,
                WT2PT.PackageType.Name.toLocation wtType.name
              )

          // Add all values and their locations
          for (wtValue, ptValue) in List.zip modul.values values do
            yield PT.PackageOp.AddValue ptValue
            yield
              PT.PackageOp.SetValueName(
                ptValue.id,
                WT2PT.PackageValue.Name.toLocation wtValue.name
              )

          // Add all functions and their locations
          for (wtFn, ptFn) in List.zip modul.fns fns do
            yield PT.PackageOp.AddFn ptFn
            yield
              PT.PackageOp.SetFnName(
                ptFn.id,
                WT2PT.PackageFn.Name.toLocation wtFn.name
              ) ]

      return ops

    // in the parsed package, types are being read as user, as opposed to the package that's right there
    | decl ->
      return
        Exception.raiseInternal "Unsupported Package declaration" [ "decl", decl ]
  }
