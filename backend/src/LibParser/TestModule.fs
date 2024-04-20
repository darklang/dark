module LibParser.TestModule

open FSharp.Compiler.Syntax

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes
module NR = NameResolver

open Utils

type WTTest =
  { name : string; lineNumber : int; actual : WT.Expr; expected : WT.Expr }

type WTModule =
  { name : List<string>
    types : List<WT.UserType.T>
    fns : List<WT.UserFunction.T>
    constants : List<WT.UserConstant.T>
    dbs : List<WT.DB.T>
    tests : List<WTTest> }

let emptyWTModule =
  { name = []; types = []; constants = []; fns = []; dbs = []; tests = [] }

type PTTest =
  { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

type PTModule =
  { name : List<string>
    types : List<PT.UserType.T>
    fns : List<PT.UserFunction.T>
    constants : List<PT.UserConstant.T>
    dbs : List<PT.DB.T>
    tests : List<PTTest> }

let emptyPTModule =
  { name = []; types = []; fns = []; constants = []; dbs = []; tests = [] }

type RTTest =
  { name : string; lineNumber : int; actual : RT.Expr; expected : RT.Expr }

type RTModule =
  { name : List<string>
    types : List<PT.UserType.T>
    fns : List<PT.UserFunction.T>
    constants : List<PT.UserConstant.T>
    dbs : List<PT.DB.T>
    tests : List<RTTest> }


let emptyRTModule =
  { name = []; types = []; fns = []; constants = []; dbs = []; tests = [] }


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


let parseFile (parsedAsFSharp : ParsedImplFileInput) : List<WTModule> =
  let parseTypeDecl
    (moduleName : List<string>)
    (typeDefn : SynTypeDefn)
    : List<WT.DB.T> * List<WT.UserType.T> =
    match typeDefn with
    | SynTypeDefn(SynComponentInfo(attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
      let attrs = attrs |> List.map _.Attributes |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        [ UserDB.fromSynTypeDefn typeDefn ], []
      else
        [], [ FS2WT.UserType.fromSynTypeDefn moduleName typeDefn ]

  let parseSynBinding
    (moduleName : List<string>)
    (binding : SynBinding)
    : List<WT.UserFunction.T> * List<WT.UserConstant.T> =
    match binding with
    | SynBinding(_, _, _, _, _, _, _, signature, _, _, _, _, _) ->
      match signature with
      | SynPat.LongIdent(SynLongIdent _, _, _, _, _, _) ->
        [ FS2WT.UserFunction.fromSynBinding moduleName binding ], []
      | SynPat.Named _ ->
        [], [ FS2WT.UserConstant.fromSynBinding moduleName binding ]
      | _ -> Exception.raiseInternal $"Unsupported binding" [ "binding", binding ]

  let rec parseModule
    (moduleName : List<string>)
    (parentDBs : List<WT.DB.T>)
    (decls : List<SynModuleDecl>)
    : List<WTModule> =
    let (m, nested) =
      List.fold
        (fun ((m : WTModule), nested) decl ->
          match decl with
          | SynModuleDecl.Let(_, bindings, _) ->
            let (newUserFns, newUserConstants) =
              bindings |> List.map (parseSynBinding moduleName) |> List.unzip
            ({ m with
                fns = m.fns @ List.concat newUserFns
                constants = m.constants @ List.concat newUserConstants },
             nested)

          | SynModuleDecl.Types(defns, _) ->
            let (dbs, types) =
              List.map (parseTypeDecl moduleName) defns |> List.unzip
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
            (m, parseModule (moduleName @ [ modName.idText ]) m.dbs decls @ nested)
          | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
        ({ emptyWTModule with name = moduleName; dbs = parentDBs }, [])
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
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (hackPackageStuff : NR.HackPackageStuff)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (m : WTModule)
  : Ply<PTModule> =
  uply {
    let! fns =
      m.fns
      |> Ply.List.mapSequentially (
        WT2PT.UserFunction.toPT
          builtins
          pm
          hackPackageStuff
          userStuff
          onMissing
          m.name
      )
    let! types =
      m.types
      |> Ply.List.mapSequentially (
        WT2PT.UserType.toPT pm hackPackageStuff userStuff onMissing m.name
      )
    let! constants =
      m.constants
      |> Ply.List.mapSequentially (
        WT2PT.UserConstant.toPT pm hackPackageStuff userStuff onMissing m.name
      )
    let! dbs =
      m.dbs
      |> Ply.List.mapSequentially (
        WT2PT.DB.toPT pm hackPackageStuff userStuff onMissing m.name
      )
    let! (tests : List<PTTest>) =
      m.tests
      |> Ply.List.mapSequentially (fun test ->
        uply {
          let! actual =
            WT2PT.Expr.toPT
              builtins
              pm
              hackPackageStuff
              userStuff
              onMissing
              m.name
              test.actual
          let! expected =
            WT2PT.Expr.toPT
              builtins
              pm
              hackPackageStuff
              userStuff
              onMissing
              m.name
              test.expected
          return
            { PTTest.actual = actual
              expected = expected
              lineNumber = test.lineNumber
              name = test.name }
        })

    return
      { name = m.name
        fns = fns
        types = types
        constants = constants
        dbs = dbs
        tests = tests }
  }



// Below are the fns that we intend to expose to the rest of the codebase

/// Returns a flattened list of modules in the file.
let parseTestFile
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (hackPackageStuff : NR.HackPackageStuff)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (filename : string)
  : Ply<List<PTModule>> =
  uply {
    let modules =
      filename
      |> System.IO.File.ReadAllText
      |> parseAsFSharpSourceFile filename
      |> parseFile

    let fns =
      modules |> List.map _.fns |> List.concat |> List.map _.name |> Set.ofList
    let types =
      modules |> List.map _.types |> List.concat |> List.map _.name |> Set.ofList
    let constants =
      modules |> List.map _.constants |> List.concat |> List.map _.name |> Set.ofList

    let updatedUserStuff : NR.UserStuff =
      { types = Set.union userStuff.types types
        constants = Set.union userStuff.constants constants
        fns = Set.union userStuff.fns fns }

    let! result =
      modules
      |> Ply.List.mapSequentially (
        toPT builtins pm hackPackageStuff updatedUserStuff onMissing
      )

    return result
  }

let parseSingleTestFromFile
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (hackPackageStuff : NR.HackPackageStuff)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (filename : string)
  (testSource : string)
  : Ply<RTTest> =
  uply {
    let wtTest =
      testSource
      |> parseAsFSharpSourceFile filename
      |> singleExprFromImplFile
      |> parseTest

    let mapExpr = WT2PT.Expr.toPT builtins pm hackPackageStuff userStuff onMissing []

    let! actual = wtTest.actual |> mapExpr |> Ply.map PT2RT.Expr.toRT
    let! expected = wtTest.expected |> mapExpr |> Ply.map PT2RT.Expr.toRT
    return
      { actual = actual
        expected = expected
        lineNumber = wtTest.lineNumber
        name = wtTest.name }
  }
