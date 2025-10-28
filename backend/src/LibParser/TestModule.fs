module LibParser.TestModule

open FSharp.Compiler.Syntax

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = NameResolver

open Utils

type WTTest =
  { name : string; lineNumber : int; actual : WT.Expr; expected : WT.Expr }

type WTModule =
  { name : List<string>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue>
    dbs : List<WT.DB.T>
    fns : List<WT.PackageFn.PackageFn>
    tests : List<WTTest> }

let emptyWTModule =
  { name = []; types = []; values = []; fns = []; dbs = []; tests = [] }

type PTTest =
  { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

type PTModule =
  { name : List<string>
    ops : List<PT.PackageOp>
    dbs : List<PT.DB.T>
    tests : List<PTTest> }

let emptyPTModule = { name = []; ops = []; dbs = []; tests = [] }


module UserDB =
  let fromSynTypeDefn (typeDef : SynTypeDefn) : WT.DB.T =
    match typeDef with
    | SynTypeDefn(SynComponentInfo(_, _params, _, [ id ], _, _, _, _),
                  SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(_, typ, _),
                                         _),
                  _members,
                  _,
                  _,
                  _) ->
      { name = id.idText; version = 0; typ = FS2WT.TypeReference.fromSynType typ }
    | _ ->
      Exception.raiseInternal $"Unsupported db definition" [ "typeDef", typeDef ]


/// Extracts a test from a SynExpr.
/// The test must be in the format `expected = actual`, otherwise an exception is raised
let parseTest (ast : SynExpr) : WTTest =
  let convert (x : SynExpr) : WT.Expr = FS2WT.Expr.fromSynExpr x

  match ast with
  | SynExpr.App(_,
                _,
                SynExpr.App(_,
                            _,
                            SynExpr.LongIdent(_, SynLongIdent([ ident ], _, _), _, _),
                            actual,
                            _),
                expected,
                range) when ident.idText = "op_Equality" ->
    { name = "test"
      lineNumber = range.Start.Line
      actual = convert actual
      expected = convert expected }
  | _ -> Exception.raiseInternal "Test case not in format `x = y`" [ "ast", ast ]


let parseFile
  (owner : string)
  (parsedAsFSharp : ParsedImplFileInput)
  : List<WTModule> =
  let parseTypeDecl
    (currentModule : List<string>)
    (typeDefn : SynTypeDefn)
    : List<WT.DB.T> * List<WT.PackageType.PackageType> =
    match typeDefn with
    | SynTypeDefn(SynComponentInfo(attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
      let attrs = attrs |> List.map _.Attributes |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        // TODO
        [ UserDB.fromSynTypeDefn typeDefn ], []
      else
        [], [ FS2WT.PackageType.fromSynTypeDefn owner currentModule typeDefn ]


  let parseSynBinding
    (currentModule : List<string>)
    (binding : SynBinding)
    : List<WT.PackageFn.PackageFn> * List<WT.PackageValue.PackageValue> =
    match binding with
    | SynBinding(_, _, _, _, _, _, _, signature, _, _, _, _, _) ->
      match signature with
      | SynPat.LongIdent(SynLongIdent _, _, _, _, _, _) ->
        [ FS2WT.PackageFn.fromSynBinding owner currentModule binding ], []
      | SynPat.Named _ ->
        [], [ FS2WT.PackageValue.fromSynBinding owner currentModule binding ]
      | _ -> Exception.raiseInternal $"Unsupported binding" [ "binding", binding ]

  let rec parseModule
    (currentModule : List<string>)
    (parentDBs : List<WT.DB.T>)
    (decls : List<SynModuleDecl>)
    : List<WTModule> =
    let (m, nested) =
      List.fold
        (fun ((m : WTModule), nested) decl ->
          match decl with
          | SynModuleDecl.Let(_, bindings, _) ->
            let (newUserFns, newPackageValues) =
              bindings |> List.map (parseSynBinding currentModule) |> List.unzip
            ({ m with
                fns = m.fns @ List.concat newUserFns
                values = m.values @ List.concat newPackageValues },
             nested)

          | SynModuleDecl.Types(defns, _) ->
            let (dbs, types) =
              List.map (parseTypeDecl currentModule) defns |> List.unzip
            ({ m with
                types = m.types @ List.concat types
                dbs = m.dbs @ List.concat dbs },
             nested)

          | SynModuleDecl.Expr(expr, _) ->
            ({ m with tests = m.tests @ [ parseTest expr ] }, nested)

          | SynModuleDecl.NestedModule(SynComponentInfo(_,
                                                        _,
                                                        _,
                                                        [ modName ],
                                                        _,
                                                        _,
                                                        _,
                                                        _),
                                       _,
                                       decls,
                                       _,
                                       _,
                                       _) ->
            (m,
             parseModule (currentModule @ [ modName.idText ]) m.dbs decls @ nested)
          | _ ->
            Exception.raiseInternal
              $"Unsupported declaration"
              [ "decl", decl; "currentModule", currentModule ])
        ({ emptyWTModule with name = currentModule; dbs = parentDBs }, [])
        decls
    m :: nested

  let decls =
    match parsedAsFSharp with
    | ParsedImplFileInput(_,
                          _,
                          _,
                          _,
                          _,
                          [ SynModuleOrNamespace(_, _, _, decls, _, _, _, _, _) ],
                          _,
                          _,
                          _) -> decls
    | _ ->
      Exception.raiseInternal
        $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
        [ "parsedAsFsharp", parsedAsFSharp ]
  parseModule [] [] decls


let toPT
  (owner : string)
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (m : WTModule)
  : Ply<PTModule> =
  uply {
    let currentModule = owner :: m.name

    let! typeOps =
      m.types
      |> Ply.List.mapSequentially (fun wtType ->
        uply {
          let! ptType = WT2PT.PackageType.toPT pm onMissing currentModule wtType
          let location : PT.PackageLocation =
            { owner = wtType.name.owner
              modules = wtType.name.modules
              name = wtType.name.name }
          return
            [ PT.PackageOp.AddType ptType
              PT.PackageOp.SetTypeName(ptType.id, location) ]
        })
      |> Ply.map List.flatten

    let! valueOps =
      m.values
      |> Ply.List.mapSequentially (fun wtValue ->
        uply {
          let! ptValue =
            WT2PT.PackageValue.toPT builtins pm onMissing currentModule wtValue
          let location : PT.PackageLocation =
            { owner = wtValue.name.owner
              modules = wtValue.name.modules
              name = wtValue.name.name }
          return
            [ PT.PackageOp.AddValue ptValue
              PT.PackageOp.SetValueName(ptValue.id, location) ]
        })
      |> Ply.map List.flatten

    let! fnOps =
      m.fns
      |> Ply.List.mapSequentially (fun wtFn ->
        uply {
          let! ptFn = WT2PT.PackageFn.toPT builtins pm onMissing currentModule wtFn
          let location : PT.PackageLocation =
            { owner = wtFn.name.owner
              modules = wtFn.name.modules
              name = wtFn.name.name }
          return
            [ PT.PackageOp.AddFn ptFn; PT.PackageOp.SetFnName(ptFn.id, location) ]
        })
      |> Ply.map List.flatten

    let! dbs =
      m.dbs |> Ply.List.mapSequentially (WT2PT.DB.toPT pm onMissing currentModule)

    let! (tests : List<PTTest>) =
      m.tests
      |> Ply.List.mapSequentially (fun test ->
        uply {
          let context =
            { WT2PT.Context.currentFnName = None
              WT2PT.Context.isInFunction = false
              WT2PT.Context.argMap = Map.empty }
          let exprToPT = WT2PT.Expr.toPT builtins pm onMissing currentModule context
          let! actual = exprToPT test.actual
          let! expected = exprToPT test.expected
          return
            { PTTest.actual = actual
              expected = expected
              lineNumber = test.lineNumber
              name = test.name }
        })

    let allOps = typeOps @ valueOps @ fnOps

    return { name = m.name; ops = allOps; dbs = dbs; tests = tests }
  }



// Helper functions for two-phase parsing (must be defined before parseTestFile)
let parseTestFile
  (owner : string)
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (filename : string)
  : Ply<List<PTModule>> =
  uply {
    let modulesWT =
      filename
      |> System.IO.File.ReadAllText
      |> parseAsFSharpSourceFile filename
      |> parseFile owner

    // First pass: parse with OnMissing.Allow to allow unresolved names
    let! firstPassModules =
      modulesWT
      |> Ply.List.mapSequentially (fun m ->
        toPT owner builtins PT.PackageManager.empty NR.OnMissing.Allow m)

    // Extract ops from first pass for second pass PackageManager
    let firstPassOps = firstPassModules |> List.collect _.ops

    // Second pass: re-parse with PackageManager containing first pass results
    let enhancedPM = LibPackageManager.PackageManager.withExtraOps pm firstPassOps
    let! reParsedModules =
      modulesWT
      |> Ply.List.mapSequentially (fun m ->
        toPT owner builtins enhancedPM onMissing m)

    // ID stabilization: adjust second pass IDs to match first pass IDs
    // Build location→ID maps from first pass
    let (firstPassTypeLocToId, firstPassValueLocToId, firstPassFnLocToId) =
      LibPackageManager.PackageManager.extractLocationMaps firstPassOps


    let adjustOp (allOps : List<PT.PackageOp>) (op : PT.PackageOp) : PT.PackageOp =
      match op with
      | PT.PackageOp.SetTypeName(_, loc) ->
        let stableId =
          firstPassTypeLocToId
          |> Map.tryFind loc
          |> Option.defaultWith (fun () -> System.Guid.NewGuid())
        PT.PackageOp.SetTypeName(stableId, loc)

      | PT.PackageOp.AddType typ ->
        let typLoc =
          allOps
          |> List.tryPick (function
            | PT.PackageOp.SetTypeName(id, loc) when id = typ.id -> Some loc
            | _ -> None)
        let stableId =
          typLoc
          |> Option.bind (fun loc -> Map.tryFind loc firstPassTypeLocToId)
          |> Option.defaultValue typ.id
        PT.PackageOp.AddType { typ with id = stableId }

      | PT.PackageOp.SetValueName(_, loc) ->
        let stableId =
          firstPassValueLocToId
          |> Map.tryFind loc
          |> Option.defaultWith (fun () -> System.Guid.NewGuid())
        PT.PackageOp.SetValueName(stableId, loc)

      | PT.PackageOp.AddValue value ->
        let valueLoc =
          allOps
          |> List.tryPick (function
            | PT.PackageOp.SetValueName(id, loc) when id = value.id -> Some loc
            | _ -> None)
        let stableId =
          valueLoc
          |> Option.bind (fun loc -> Map.tryFind loc firstPassValueLocToId)
          |> Option.defaultValue value.id
        PT.PackageOp.AddValue { value with id = stableId }

      | PT.PackageOp.SetFnName(_, loc) ->
        let stableId =
          firstPassFnLocToId
          |> Map.tryFind loc
          |> Option.defaultWith (fun () -> System.Guid.NewGuid())
        PT.PackageOp.SetFnName(stableId, loc)

      | PT.PackageOp.AddFn fn ->
        let fnLoc =
          allOps
          |> List.tryPick (function
            | PT.PackageOp.SetFnName(id, loc) when id = fn.id -> Some loc
            | _ -> None)
        let stableId =
          fnLoc
          |> Option.bind (fun loc -> Map.tryFind loc firstPassFnLocToId)
          |> Option.defaultValue fn.id
        PT.PackageOp.AddFn { fn with id = stableId }

    // Adjust IDs in each module's ops
    let adjustedModules =
      reParsedModules
      |> List.map (fun m ->
        let adjustedOps = m.ops |> List.map (adjustOp m.ops)
        { m with ops = adjustedOps })

    return adjustedModules
  }
