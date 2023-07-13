module Parser.TestModule

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes

open Utils
type WTTest =
  { name : string; lineNumber : int; actual : WT.Expr; expected : WT.Expr }

type WTModule =
  { types : List<WT.UserType.T>
    dbs : List<WT.DB.T>
    fns : List<WT.UserFunction.T>
    modules : List<string * WTModule>
    tests : List<WTTest> }

let emptyWTModule = { types = []; dbs = []; fns = []; modules = []; tests = [] }

type PTTest =
  { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

type PTModule =
  { types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    modules : List<string * PTModule>
    tests : List<PTTest> }

let emptyPTModule = { types = []; dbs = []; fns = []; modules = []; tests = [] }

type RTTest =
  { name : string; lineNumber : int; actual : RT.Expr; expected : RT.Expr }

type RTModule =
  { types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    modules : List<string * RTModule>
    tests : List<RTTest> }


let emptyRTModule = { types = []; dbs = []; fns = []; modules = []; tests = [] }


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

  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (db : PT.DB.T)
    : PT.DB.T =
    { db with typ = NameResolution.TypeReference.resolveNames userTypes db.typ }


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


let parseFile (parsedAsFSharp : ParsedImplFileInput) : WTModule =
  let parseTypeDecl (typeDefn : SynTypeDefn) : List<WT.DB.T> * List<WT.UserType.T> =
    match typeDefn with
    | SynTypeDefn(SynComponentInfo(attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
      let attrs = attrs |> List.map (fun attr -> attr.Attributes) |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        [ UserDB.fromSynTypeDefn typeDefn ], []
      else
        [], [ FS2WT.UserType.fromSynTypeDefn typeDefn ]

  let rec parseModule (parent : WTModule) (decls : List<SynModuleDecl>) : WTModule =
    List.fold
      { types = parent.types
        fns = parent.fns
        dbs = parent.dbs
        modules = []
        tests = [] }
      (fun m decl ->
        match decl with
        | SynModuleDecl.Let(_, bindings, _) ->
          let newUserFns = List.map FS2WT.UserFunction.fromSynBinding bindings
          { m with fns = m.fns @ newUserFns }

        | SynModuleDecl.Types(defns, _) ->
          let (dbs, types) = List.map parseTypeDecl defns |> List.unzip
          { m with
              types = m.types @ List.concat types
              dbs = m.dbs @ List.concat dbs }

        | SynModuleDecl.Expr(expr, _) ->
          { m with tests = m.tests @ [ parseTest expr ] }

        | SynModuleDecl.NestedModule(SynComponentInfo(attrs,
                                                      _,
                                                      _,
                                                      [ name ],
                                                      _,
                                                      _,
                                                      _,
                                                      _),
                                     _,
                                     decls,
                                     _,
                                     _,
                                     _) ->
          let nested = parseModule m decls
          { m with modules = m.modules @ [ (name.idText, nested) ] }
        | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
      decls

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
  parseModule emptyWTModule decls


let rec toPT (resolver : WT2PT.NameResolver) (m : WTModule) : PTModule =
  { fns = m.fns |> List.map (WT2PT.UserFunction.toPT resolver)
    types = m.types |> List.map (WT2PT.UserType.toPT resolver)
    dbs = m.dbs |> List.map (WT2PT.DB.toPT resolver)
    modules = m.modules |> List.map (fun (name, m) -> (name, toPT resolver m))
    tests =
      m.tests
      |> List.map (fun test ->
        { actual = WT2PT.Expr.toPT resolver test.actual
          expected = WT2PT.Expr.toPT resolver test.expected
          lineNumber = test.lineNumber
          name = test.name }) }



let rec resolveNames (m : PTModule) : PTModule =
  let fnNames = m.fns |> List.map (fun fn -> fn.name) |> Set
  let typeNames = m.types |> List.map (fun t -> t.name) |> Set
  let fixExpr = NameResolution.Expr.resolveNames fnNames typeNames
  { fns =
      m.fns |> List.map (NameResolution.UserFunction.resolveNames fnNames typeNames)
    types = m.types |> List.map (NameResolution.UserType.resolveNames typeNames)
    dbs = m.dbs |> List.map (UserDB.resolveNames typeNames)
    modules = m.modules |> List.map (fun (name, m) -> (name, resolveNames m))
    tests =
      m.tests
      |> List.map (fun test ->
        { test with actual = fixExpr test.actual; expected = fixExpr test.expected }) }





// Below are the fns that we intend to expose to the rest of the codebase
let parseTestFile (filename : string) : PTModule =
  let resolver = WrittenTypesToProgramTypes.NameResolver.empty
  filename
  |> System.IO.File.ReadAllText
  |> parseAsFSharpSourceFile filename
  |> parseFile
  |> toPT resolver
  |> resolveNames

let parseSingleTestFromFile (filename : string) (testSource : string) : RTTest =
  let resolver = WrittenTypesToProgramTypes.NameResolver.empty
  let wtTest =
    testSource
    |> parseAsFSharpSourceFile filename
    |> singleExprFromImplFile
    |> parseTest
  { actual =
      wtTest.actual
      |> WT2PT.Expr.toPT resolver
      |> NameResolution.Expr.resolveNames Set.empty Set.empty
      |> PT2RT.Expr.toRT
    expected =
      wtTest.expected
      |> WT2PT.Expr.toPT resolver
      |> NameResolution.Expr.resolveNames Set.empty Set.empty
      |> PT2RT.Expr.toRT
    lineNumber = wtTest.lineNumber
    name = wtTest.name }
