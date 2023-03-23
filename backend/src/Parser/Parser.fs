/// Converts strings of F# into Dark. Used for testing.
module Parser

// refer to https://fsharp.github.io/fsharp-compiler-docs

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module FS2PT = FSharpToProgramTypes

let parse (input : string) : SynExpr =
  let file = "test.fs"
  let checker = FSharpChecker.Create()

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions = { FSharpParsingOptions.Default with SourceFiles = [| file |] }

  let results =
    checker.ParseFile(file, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously

  match results.ParseTree with
  | (ParsedInput.ImplFile (ParsedImplFileInput (_,
                                                _,
                                                _,
                                                _,
                                                _,
                                                [ SynModuleOrNamespace (_,
                                                                        _,
                                                                        _,
                                                                        [ SynModuleDecl.Expr (expr,
                                                                                              _) ],
                                                                        _,
                                                                        _,
                                                                        _,
                                                                        _,
                                                                        _) ],
                                                _,
                                                _,
                                                _))) -> expr
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parseTree", results.ParseTree; "input", input ]


let longIdentToList (li : LongIdent) : List<string> =
  li |> List.map (fun id -> id.idText)


type Test = { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

let convertToTest availableTypes (ast : SynExpr) : Test =
  let convert (x : SynExpr) : PT.Expr = FS2PT.Expr.fromSynExpr availableTypes x

  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_,
                              _,
                              SynExpr.LongIdent (_,
                                                 SynLongIdent ([ ident ], _, _),
                                                 _,
                                                 _),
                              actual,
                              _),
                 expected,
                 range) when ident.idText = "op_Equality" ->
    // Exception.raiseInternal $"whole thing: {actual}"
    { name = "test"
      lineNumber = range.Start.Line
      actual = convert actual
      expected = convert expected }
  | _ -> Exception.raiseInternal "Test case not in format `x = y`" [ "ast", ast ]

type Module =
  { types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    packageFns : List<PT.Package.Fn>
    modules : List<string * Module>
    tests : List<Test> }

let emptyModule =
  { types = []; dbs = []; fns = []; modules = []; tests = []; packageFns = [] }

let parseTestFile
  (stdlibTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (filename : string)
  : Module =
  let checker = FSharpChecker.Create()
  let input = System.IO.File.ReadAllText filename

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions =
    { FSharpParsingOptions.Default with SourceFiles = [| filename |] }

  let fsharpFilename = System.IO.Path.GetFileNameWithoutExtension filename + ".fs"

  let results =
    checker.ParseFile(fsharpFilename, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously


  let parseTypeDecl
    (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
    (typeDef : SynTypeDefn)
    : List<PT.DB.T> * List<PT.UserType.T> =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
      let attrs = attrs |> List.map (fun attr -> attr.Attributes) |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        [ FS2PT.UserDB.fromSynTypeDefn availableTypes typeDef ], []
      else
        [], [ FS2PT.UserType.fromSynTypeDefn availableTypes typeDef ]

  let getPackage (attrs : SynAttributes) : Option<string * string * string> =
    attrs
    |> List.map (fun attr -> attr.Attributes)
    |> List.concat
    |> List.filterMap (fun (attr : SynAttribute) ->
      if longIdentToList attr.TypeName.LongIdent = [ "Package" ] then
        match attr.ArgExpr with
        | SynExpr.Paren (SynExpr.Tuple (_,
                                        [ SynExpr.Const (SynConst.String (p1, _, _),
                                                         _)
                                          SynExpr.Const (SynConst.String (p2, _, _),
                                                         _)
                                          SynExpr.Const (SynConst.String (p3, _, _),
                                                         _) ],
                                        _,
                                        _),
                         _,
                         _,
                         _) -> Some(p1, p2, p3)
        | _ -> None
      else
        None)
    |> List.tryHead

  let rec parseModule
    (parent : Module)
    (attrs : SynAttributes)
    (decls : List<SynModuleDecl>)
    : Module =
    let package = getPackage attrs

    List.fold
      { types = parent.types
        fns = parent.fns
        packageFns = parent.packageFns
        dbs = parent.dbs
        modules = []
        tests = [] }
      (fun m decl ->
        let availableTypes =
          (m.types @ parent.types)
          |> List.map (fun t -> PT.FQTypeName.User t.name, t.definition)
          |> (@) stdlibTypes

        match decl with
        | SynModuleDecl.Let (_, bindings, _) ->
          match package with
          | Some package ->
            let newPackageFns =
              List.map (FS2PT.PackageFn.fromSynBinding package) bindings
            { m with packageFns = m.packageFns @ newPackageFns }

          | None ->
            let newUserFns =
              List.map (FS2PT.UserFunction.fromSynBinding availableTypes) bindings
            { m with fns = m.fns @ newUserFns }

        | SynModuleDecl.Types (defns, _) ->
          let (dbs, types) =
            List.map (parseTypeDecl availableTypes) defns |> List.unzip
          { m with
              types = m.types @ List.concat types
              dbs = m.dbs @ List.concat dbs }

        | SynModuleDecl.Expr (SynExpr.Do (expr, _), _) ->
          { m with tests = m.tests @ [ convertToTest availableTypes expr ] }

        | SynModuleDecl.Expr (expr, _) ->
          { m with tests = m.tests @ [ convertToTest availableTypes expr ] }

        | SynModuleDecl.NestedModule (SynComponentInfo (attrs,
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
          let nested = parseModule m attrs decls
          { m with modules = m.modules @ [ (name.idText, nested) ] }
        | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
      decls

  match results.ParseTree with
  | (ParsedInput.ImplFile (ParsedImplFileInput (_,
                                                _,
                                                _,
                                                _,
                                                _,
                                                [ SynModuleOrNamespace ([ _id ],
                                                                        _,
                                                                        _,
                                                                        decls,
                                                                        _,
                                                                        attrs,
                                                                        _,
                                                                        _,
                                                                        _) ],
                                                _,
                                                _,
                                                _))) ->
    parseModule emptyModule attrs decls
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parseTree", results.ParseTree; "input", input; "filename", filename ]


let parsePTExprWithTypes
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (code : string)
  : PT.Expr =
  code |> parse |> FS2PT.Expr.fromSynExpr availableTypes

let parseRTExprWithTypes
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  parsePTExprWithTypes availableTypes code
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parsePTExpr = parsePTExprWithTypes []

let parseRTExpr = parseRTExprWithTypes []
