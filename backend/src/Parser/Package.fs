module internal Parser.Package

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes

open Utils

type WTPackageModule = { fns : List<WT.PackageFn.T>; types : List<WT.PackageType.T> }
let emptyWTModule = { fns = []; types = [] }

type PTPackageModule = { fns : List<PT.PackageFn.T>; types : List<PT.PackageType.T> }

let emptyPTModule = { fns = []; types = [] }


/// Update a CanvasModule by parsing a single F# let binding
/// Depending on the attribute present, this may add a user function, a handler, or a DB
let parseLetBinding
  (modules : List<string>)
  (letBinding : SynBinding)
  : WT.PackageFn.T =
  match modules with
  | owner :: modules ->
    let modules = NonEmptyList.ofList modules
    FS2WT.PackageFn.fromSynBinding owner modules letBinding
  | _ ->
    Exception.raiseInternal
      "Expected owner, and at least 1 other modules"
      [ "modules", modules; "binding", letBinding ]

let parseTypeDef (modules : List<string>) (defn : SynTypeDefn) : WT.PackageType.T =
  match modules with
  | owner :: modules ->
    let modules = NonEmptyList.ofList modules
    FS2WT.PackageType.fromSynTypeDefn owner modules defn
  | _ ->
    Exception.raiseInternal
      "Expected owner, and at least 1 other modules"
      [ "modules", modules; "defn", defn ]


let rec parseDecls
  (modules : List<string>)
  (decls : List<SynModuleDecl>)
  : WTPackageModule =
  List.fold
    emptyWTModule
    (fun m decl ->
      match decl with
      | SynModuleDecl.Let(_, bindings, _) ->
        let fns = List.map (parseLetBinding modules) bindings
        { m with fns = m.fns @ fns }

      | SynModuleDecl.Types(defns, _) ->
        let types = List.map (parseTypeDef modules) defns
        { m with types = m.types @ types }

      | SynModuleDecl.NestedModule(SynComponentInfo(_,
                                                    _,
                                                    _,
                                                    nestedModules,
                                                    _,
                                                    _,
                                                    _,
                                                    _),
                                   _,
                                   nested,
                                   _,
                                   _,
                                   _) ->

        let modules = modules @ (nestedModules |> List.map (fun id -> id.idText))
        let nestedDecls = parseDecls modules nested
        { fns = m.fns @ nestedDecls.fns; types = m.types @ nestedDecls.types }


      | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
    decls


let parse (filename : string) (contents : string) : PTPackageModule =
  let resolver = WrittenTypesToProgramTypes.NameResolver.empty
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
    // At the toplevel, the module names will from the filenames
    let names = []
    let modul = parseDecls names decls
    let fns = modul.fns |> List.map (WT2PT.PackageFn.toPT resolver)
    let types = modul.types |> List.map (WT2PT.PackageType.toPT resolver)
    { fns = fns; types = types }
  // in the parsed package, types are being read as user, as opposed to the package that's right there
  | decl ->
    Exception.raiseInternal "Unsupported Package declaration" [ "decl", decl ]
