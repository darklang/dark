module Parser.TestModule

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module PTP = ProgramTypes
open Utils

type Test = { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

type T =
  { types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    constants : List<PT.UserConstant.T>
    modules : List<string * T>
    tests : List<Test> }

let empty =
  { types = []; dbs = []; fns = []; constants = []; modules = []; tests = [] }


module UserDB =
  let fromSynTypeDefn (typeDef : SynTypeDefn) : PT.DB.T =
    match typeDef with
    | SynTypeDefn(SynComponentInfo(_, _params, _, [ id ], _, _, _, _),
                  SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(_, typ, _),
                                         _),
                  _members,
                  _,
                  _,
                  _) ->
      { tlid = gid ()
        name = id.idText
        version = 0
        typ = PTP.TypeReference.fromSynType typ }
    | _ ->
      Exception.raiseInternal $"Unsupported db definition" [ "typeDef", typeDef ]


/// Extracts a test from a SynExpr.
/// The test must be in the format `expected = actual`, otherwise an exception is raised
let parseTest (ast : SynExpr) : Test =
  let convert (x : SynExpr) : PT.Expr = PTP.Expr.fromSynExpr x

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


let parseFile (parsedAsFSharp : ParsedImplFileInput) : T =
  let parseTypeDecl (typeDefn : SynTypeDefn) : List<PT.DB.T> * List<PT.UserType.T> =
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
        [], [ PTP.UserType.fromSynTypeDefn typeDefn ]

  let rec parseModule (parent : T) (decls : List<SynModuleDecl>) : T =
    let m =
      List.fold
        { types = parent.types
          fns = parent.fns
          dbs = parent.dbs
          constants = parent.constants
          modules = []
          tests = [] }
        (fun m decl ->
          match decl with
          | SynModuleDecl.Let(_, bindings, _) ->
            let newUserFns = List.map PTP.UserFunction.fromSynBinding bindings
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
    let fnNames = m.fns |> List.map (fun fn -> fn.name) |> Set
    let typeNames = m.types |> List.map (fun t -> t.name) |> Set
    let constantNames = m.constants |> List.map (fun c -> c.name) |> Set
    let fixup = ProgramTypes.Expr.resolveNames fnNames typeNames constantNames
    { fns = m.fns |> List.map (fun fn -> { fn with body = fixup fn.body })
      types = m.types
      dbs = m.dbs
      modules = m.modules
      constants = m.constants
      tests =
        m.tests
        |> List.map (fun test ->
          { test with actual = fixup test.actual; expected = fixup test.expected }) }




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
  parseModule empty decls


// Below are the fns that we intend to expose to the rest of the codebase
let parseTestFile (filename : string) : T =
  filename
  |> System.IO.File.ReadAllText
  |> parseAsFSharpSourceFile filename
  |> parseFile

let parseSingleTestFromFile (filename : string) (testSource : string) : Test =
  testSource
  |> parseAsFSharpSourceFile filename
  |> singleExprFromImplFile
  |> parseTest
