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

type WTExpected =
  | WTExpectedExpr of WT.Expr
  | WTExpectedError of string

type WTTest =
  { name : string; lineNumber : int; actual : WT.Expr; expected : WTExpected }

type WTModule =
  { name : List<string>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue>
    dbs : List<WT.DB.T>
    fns : List<WT.PackageFn.PackageFn>
    tests : List<WTTest> }

let emptyWTModule =
  { name = []; types = []; values = []; fns = []; dbs = []; tests = [] }

type PTExpected =
  | PTExpectedExpr of PT.Expr
  | PTExpectedError of string

type PTTest =
  { name : string; lineNumber : int; actual : PT.Expr; expected : PTExpected }

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
/// The test must be in the format `actual = expected` or `actual = error="msg"`,
/// otherwise an exception is raised.
let parseTest (ast : SynExpr) : WTTest =
  let convert (x : SynExpr) : WT.Expr = FS2WT.Expr.fromSynExpr x
  let isBuiltinTestDerrorSqlMessage (expr : SynExpr) : bool =
    match expr with
    | SynExpr.LongIdent(_, SynLongIdent(idents, _, _), _, _) ->
      idents |> List.map _.idText = [ "Builtin"; "testDerrorSqlMessage" ]
    | _ -> false

  match ast with
  | SynExpr.App(_,
                _,
                SynExpr.App(_,
                            _,
                            SynExpr.LongIdent(_, SynLongIdent([ ident ], _, _), _, _),
                            actual,
                            _),
                expectedExpr,
                range) when ident.idText = "op_Equality" ->
    let actual, expected =
      match actual, expectedExpr with
      // `x = error="msg"` is parsed as `(x = error) = "msg"`
      | SynExpr.App(_, _, actualExpr, SynExpr.Ident marker, _),
        SynExpr.Const(SynConst.String(errorMessage, _, _), _) when
        marker.idText = "error"
        ->
        convert actualExpr, WTExpectedError(String.normalize errorMessage)
      // Also support direct shape where RHS parses as `error "msg"`
      | _,
        SynExpr.App(_,
                    _,
                    SynExpr.Ident marker,
                    SynExpr.Const(SynConst.String(errorMessage, _, _), _),
                    _) when marker.idText = "error" ->
        convert actual, WTExpectedError(String.normalize errorMessage)
      // Back-compat for existing SQL compiler tests that used Builtin.testDerrorSqlMessage
      | _,
        SynExpr.App(_,
                    _,
                    fnExpr,
                    SynExpr.Const(SynConst.String(errorMessage, _, _), _),
                    _) when isBuiltinTestDerrorSqlMessage fnExpr ->
        let msg = LibExecution.RTQueryCompiler.errorTemplate + errorMessage
        convert actual, WTExpectedError(String.normalize msg)
      | _ -> convert actual, WTExpectedExpr(convert expectedExpr)

    { name = "test"
      lineNumber = range.Start.Line
      actual = actual
      expected = expected }
  | _ ->
    Exception.raiseInternal
      "Test case not in format `x = y` or `x = error=\"msg\"`"
      [ "ast", ast ]


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
          return
            [ PT.PackageOp.AddType ptType
              PT.PackageOp.SetTypeName(
                ptType.id,
                WT2PT.PackageType.Name.toLocation wtType.name
              ) ]
        })
      |> Ply.map List.flatten

    let! valueOps =
      m.values
      |> Ply.List.mapSequentially (fun wtValue ->
        uply {
          let! ptValue =
            WT2PT.PackageValue.toPT builtins pm onMissing currentModule wtValue
          return
            [ PT.PackageOp.AddValue ptValue
              PT.PackageOp.SetValueName(
                ptValue.id,
                WT2PT.PackageValue.Name.toLocation wtValue.name
              ) ]
        })
      |> Ply.map List.flatten

    let! fnOps =
      m.fns
      |> Ply.List.mapSequentially (fun wtFn ->
        uply {
          let! ptFn = WT2PT.PackageFn.toPT builtins pm onMissing currentModule wtFn
          return
            [ PT.PackageOp.AddFn ptFn
              PT.PackageOp.SetFnName(
                ptFn.id,
                WT2PT.PackageFn.Name.toLocation wtFn.name
              ) ]
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
              WT2PT.Context.argMap = Map.empty
              WT2PT.Context.localBindings = Set.empty }
          let exprToPT = WT2PT.Expr.toPT builtins pm onMissing currentModule context
          let! actual = exprToPT test.actual
          let! expected =
            uply {
              match test.expected with
              | WTExpectedExpr expected ->
                let! expected = exprToPT expected
                match expected with
                // Back-compat for SQL compiler tests:
                // `x = Builtin.testDerrorSqlMessage "..."` should assert on error message text.
                | PT.EApply(_,
                            PT.EFnName(_,
                                       Ok(PT.FQFnName.Builtin { name = "testDerrorSqlMessage"
                                                                version = 0 })),
                            [],
                            { head = PT.EString(_, [ PT.StringText errorMessage ])
                              tail = [] }) ->
                  let msg =
                    LibExecution.RTQueryCompiler.errorTemplate + errorMessage
                  return PTExpected.PTExpectedError(String.normalize msg)
                | _ -> return PTExpected.PTExpectedExpr expected
              | WTExpectedError msg -> return PTExpected.PTExpectedError msg
            }
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
  (filename : string)
  : Ply<List<PTModule>> =
  uply {
    // test modules should always allow NREs
    let onMissing = NR.OnMissing.Allow

    let modulesWT =
      filename
      |> System.IO.File.ReadAllText
      |> parseAsFSharpSourceFile filename
      |> parseFile owner

    // First pass: parse with OnMissing.Allow to allow unresolved names
    let! firstPassModules =
      modulesWT
      |> Ply.List.mapSequentially (
        toPT owner builtins PT.PackageManager.empty onMissing
      )

    // Extract ops from first pass for second pass PackageManager
    let firstPassOps = firstPassModules |> List.collect _.ops

    // Second pass: re-parse with PackageManager containing first pass results
    let enhancedPM = LibPackageManager.PackageManager.withExtraOps pm firstPassOps
    let! reParsedModules =
      modulesWT
      |> Ply.List.mapSequentially (toPT owner builtins enhancedPM onMissing)

    // ID stabilization: adjust second pass IDs to match first pass IDs
    let firstPassPM = LibPackageManager.PackageManager.createInMemory firstPassOps

    // Adjust IDs in each module's ops
    let! adjustedModules =
      reParsedModules
      |> Ply.List.mapSequentially (fun m ->
        uply {
          let! adjustedOps =
            LibPackageManager.PackageManager.stabilizeOpsAgainstPM
              PT.mainBranchId
              firstPassPM
              m.ops
          return { m with ops = adjustedOps }
        })

    return adjustedModules
  }
