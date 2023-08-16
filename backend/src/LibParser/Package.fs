module internal LibParser.Package

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes

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


/// Update a CanvasModule by parsing a single F# let binding
/// Depending on the attribute present, this may add a user function, a handler, or a DB
let parseLetBinding
  (modules : List<string>)
  (letBinding : SynBinding)
  : List<WT.PackageFn.T> * List<WT.PackageConstant.T> =
  match modules with
  | owner :: modules ->
    try
      [ FS2WT.PackageFn.fromSynBinding owner modules letBinding ], []
    with _ ->
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
    emptyWTModule
    (fun m decl ->
      // debuG "decl" decl
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

        let moduleNames =
          moduleNames @ (nestedModuleNames |> List.map (fun id -> id.idText))
        let nestedDecls = parseDecls moduleNames nested

        { fns = m.fns @ nestedDecls.fns
          types = m.types @ nestedDecls.types
          constants = m.constants @ nestedDecls.constants }


      | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
    decls

let parse
  (resolver : NameResolver.NameResolver)
  (filename : string)
  (contents : string)
  : PTPackageModule =
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

    let nameToModules (p : PT.FQName.Package<'a>) : List<string> =
      "PACKAGE" :: p.owner :: p.modules

    let fns =
      modul.fns
      |> List.map (fun fn ->
        WT2PT.PackageFn.toPT resolver (nameToModules fn.name) fn)
    let types =
      modul.types
      |> List.map (fun typ ->
        WT2PT.PackageType.toPT resolver (nameToModules typ.name) typ)
    let constants =
      modul.constants
      |> List.map (fun constant ->
        WT2PT.PackageConstant.toPT resolver (nameToModules constant.name) constant)

    { fns = fns; types = types; constants = constants }

  // in the parsed package, types are being read as user, as opposed to the package that's right there
  | decl ->
    Exception.raiseInternal "Unsupported Package declaration" [ "decl", decl ]
