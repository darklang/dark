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
  { fns : List<WT.PackageFn.T>
    types : List<WT.PackageType.T>
    constants : List<WT.PackageConstant.T> }
let emptyWTModule = { fns = []; types = []; constants = [] }

type PTPackageModule =
  { fns : List<PT.PackageFn.T>
    types : List<PT.PackageType.T>
    constants : List<PT.PackageConstant.T> }
let emptyPTModule = { fns = []; types = []; constants = [] }


/// Update a Package by parsing a single F# let binding
let parseLetBinding
  (modules : List<string>)
  (letBinding : SynBinding)
  : List<WT.PackageFn.T> * List<WT.PackageConstant.T> =
  match modules with
  | owner :: modules ->
    if FS2WT.Function.hasArguments letBinding then
      [ FS2WT.PackageFn.fromSynBinding owner modules letBinding ], []
    else
      [], [ FS2WT.PackageConstant.fromSynBinding owner modules letBinding ]
  | _ ->
    Exception.raiseInternal
      "Expected owner, and at least 1 other modules"
      [ "modules", modules; "binding", letBinding ]

let parseTypeDef (modules : List<string>) (defn : SynTypeDefn) : WT.PackageType.T =
  match modules with
  | owner :: modules -> FS2WT.PackageType.fromSynTypeDefn owner modules defn
  | _ ->
    Exception.raiseInternal
      "Expected owner, and at least 1 other modules"
      [ "modules", modules; "defn", defn ]


let rec parseDecls
  (moduleNames : List<string>)
  (decls : List<SynModuleDecl>)
  : WTPackageModule =
  List.fold
    (fun m decl ->
      match decl with
      | SynModuleDecl.Let(_, bindings, _) ->
        let (fns, constants) =
          bindings |> List.map (parseLetBinding moduleNames) |> List.unzip
        { m with
            fns = m.fns @ List.flatten fns
            constants = m.constants @ List.flatten constants }

      | SynModuleDecl.Types(defns, _) ->
        let types = List.map (parseTypeDef moduleNames) defns
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
        let nestedDecls = parseDecls moduleNames nested

        { fns = m.fns @ nestedDecls.fns
          types = m.types @ nestedDecls.types
          constants = m.constants @ nestedDecls.constants }


      | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
    emptyWTModule
    decls


let parse
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (filename : string)
  (contents : string)
  : Ply<PTPackageModule> =
  uply {
    match parseAsFSharpSourceFile filename contents with
    | ParsedImplFileInput(_,
                          _,
                          _,
                          _,
                          _,
                          [ SynModuleOrNamespace(_, _, _, decls, _, _, _, _, _) ],
                          _,
                          _,
                          _) ->
      let baseModule = []

      let modul : WTPackageModule = parseDecls baseModule decls


      let typeNameToModules (p : PT.FQTypeName.Package) : List<string> =
        p.owner :: p.modules

      let fnNameToModules (p : PT.FQFnName.Package) : List<string> =
        p.owner :: p.modules

      let constantNameToModules (p : PT.FQConstantName.Package) : List<string> =
        p.owner :: p.modules

      let! fns =
        modul.fns
        |> Ply.List.mapSequentially (fun fn ->
          WT2PT.PackageFn.toPT
            builtins
            pm
            userStuff
            onMissing
            (fnNameToModules fn.name)
            fn)

      let! types =
        modul.types
        |> Ply.List.mapSequentially (fun typ ->
          WT2PT.PackageType.toPT
            pm
            userStuff
            onMissing
            (typeNameToModules typ.name)
            typ)

      let! constants =
        modul.constants
        |> Ply.List.mapSequentially (fun constant ->
          WT2PT.PackageConstant.toPT
            pm
            userStuff
            onMissing
            (constantNameToModules constant.name)
            constant)

      return { fns = fns; types = types; constants = constants }

    // in the parsed package, types are being read as user, as opposed to the package that's right there
    | decl ->
      return
        Exception.raiseInternal "Unsupported Package declaration" [ "decl", decl ]
  }
