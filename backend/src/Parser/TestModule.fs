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
  { name : List<string>
    types : List<WT.UserType.T>
    dbs : List<WT.DB.T>
    fns : List<WT.UserFunction.T>
    tests : List<WTTest> }

let emptyWTModule = { name = []; types = []; dbs = []; fns = []; tests = [] }

type PTTest =
  { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

type PTModule =
  { name : List<string>
    types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    tests : List<PTTest> }

let emptyPTModule = { name = []; types = []; dbs = []; fns = []; tests = [] }

type RTTest =
  { name : string; lineNumber : int; actual : RT.Expr; expected : RT.Expr }

type RTModule =
  { name : List<string>
    types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    tests : List<RTTest> }


let emptyRTModule = { name = []; types = []; dbs = []; fns = []; tests = [] }


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
      let attrs = attrs |> List.map (fun attr -> attr.Attributes) |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        [ UserDB.fromSynTypeDefn typeDefn ], []
      else
        [], [ FS2WT.UserType.fromSynTypeDefn moduleName typeDefn ]

  let rec parseModule
    (moduleName : List<string>)
    (decls : List<SynModuleDecl>)
    : List<WTModule> =
    let (m, nested) =
      List.fold
        ({ emptyWTModule with name = moduleName }, [])
        (fun (m, nested) decl ->
          match decl with
          | SynModuleDecl.Let(_, bindings, _) ->
            let newUserFns =
              List.map (FS2WT.UserFunction.fromSynBinding moduleName) bindings
            ({ m with fns = m.fns @ newUserFns }, nested)

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
            (m, parseModule (moduleName @ [ modName.idText ]) decls @ nested)
          | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
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
  parseModule [] decls


let rec toPT (resolver : NameResolver.NameResolver) (m : WTModule) : PTModule =
  { name = m.name
    fns = m.fns |> List.map (WT2PT.UserFunction.toPT resolver m.name)
    types = m.types |> List.map (WT2PT.UserType.toPT resolver m.name)
    dbs = m.dbs |> List.map (WT2PT.DB.toPT resolver m.name)
    tests =
      m.tests
      |> List.map (fun test ->
        { actual = WT2PT.Expr.toPT resolver m.name test.actual
          expected = WT2PT.Expr.toPT resolver m.name test.expected
          lineNumber = test.lineNumber
          name = test.name }) }




// Below are the fns that we intend to expose to the rest of the codebase

/// Returns a flattened list of modules in the file.
let parseTestFile
  (baseResolver : NameResolver.NameResolver)
  (filename : string)
  : List<PTModule> =
  let modules =
    filename
    |> System.IO.File.ReadAllText
    |> parseAsFSharpSourceFile filename
    |> parseFile


  let fns = modules |> List.map (fun m -> m.fns) |> List.concat
  let fnNames = fns |> List.map (fun fn -> fn.name) |> Set.ofList

  let types = modules |> List.map (fun m -> m.types) |> List.concat
  let typeNames = types |> List.map (fun typ -> typ.name) |> Set.ofList

  let programResolver =
    { NameResolver.empty with userFns = fnNames; userTypes = typeNames }
  let resolver = NameResolver.merge baseResolver programResolver

  modules |> List.map (toPT resolver)

let parseSingleTestFromFile
  (resolver : NameResolver.NameResolver)
  (filename : string)
  (testSource : string)
  : RTTest =
  let wtTest =
    testSource
    |> parseAsFSharpSourceFile filename
    |> singleExprFromImplFile
    |> parseTest
  { actual = wtTest.actual |> WT2PT.Expr.toPT resolver [] |> PT2RT.Expr.toRT
    expected = wtTest.expected |> WT2PT.Expr.toPT resolver [] |> PT2RT.Expr.toRT
    lineNumber = wtTest.lineNumber
    name = wtTest.name }
