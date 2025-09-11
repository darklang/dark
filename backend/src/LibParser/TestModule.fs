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
    types : List<PT.PackageType.PackageType>
    fns : List<PT.PackageFn.PackageFn>
    values : List<PT.PackageValue.PackageValue>
    dbs : List<PT.DB.T>
    tests : List<PTTest> }

let emptyPTModule =
  { name = []; types = []; fns = []; values = []; dbs = []; tests = [] }


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

    let! types =
      m.types
      |> Ply.List.mapSequentially (WT2PT.PackageType.toPT pm onMissing currentModule)

    let! values =
      m.values
      |> Ply.List.mapSequentially (
        WT2PT.PackageValue.toPT builtins pm onMissing currentModule
      )

    let! dbs =
      m.dbs |> Ply.List.mapSequentially (WT2PT.DB.toPT pm onMissing currentModule)

    let! fns =
      m.fns
      |> Ply.List.mapSequentially (
        WT2PT.PackageFn.toPT builtins pm onMissing currentModule
      )

    let! (tests : List<PTTest>) =
      m.tests
      |> Ply.List.mapSequentially (fun test ->
        uply {
          let exprToPT = WT2PT.Expr.toPT builtins pm onMissing currentModule
          let! actual = exprToPT test.actual
          let! expected = exprToPT test.expected
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
        values = values
        dbs = dbs
        tests = tests }
  }



// Below are the fns that we intend to expose to the rest of the codebase

/// Returns a flattened list of modules in the file.
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

    // Initial pass, so we can re-parse with all names in context
    let! (afterFirstPass : List<PTModule>) =
      modulesWT |> Ply.List.mapSequentially (toPT owner builtins pm onMissing)

    // Now, parse again, but with the names in context (so fewer are marked as unresolved)
    let pm =
      pm
      |> PT.PackageManager.withExtras
        (afterFirstPass |> List.collect _.types)
        (afterFirstPass |> List.collect _.values)
        (afterFirstPass |> List.collect _.fns)

    let! (afterSecondPass : List<PTModule>) =
      modulesWT |> Ply.List.mapSequentially (toPT owner builtins pm onMissing)

    // The IDs that weren't locked-in have changed - let's fix that now.
    let adjusted : List<PTModule> =
      afterSecondPass
      |> List.map (fun m ->
        let originalModule =
          afterFirstPass
          |> List.find (fun original -> original.name = m.name)
          |> Option.unwrap m

        { m with
            types =
              m.types
              |> List.map (fun typ ->
                { typ with
                    hash =
                      originalModule.types
                      |> List.find (fun original -> original.name = typ.name)
                      |> Option.map _.hash
                      |> Option.defaultValue typ.hash })
            values =
              m.values
              |> List.map (fun c ->
                { c with
                    hash =
                      originalModule.values
                      |> List.find (fun original -> original.name = c.name)
                      |> Option.map _.hash
                      |> Option.defaultValue c.hash })
            fns =
              m.fns
              |> List.map (fun fn ->
                { fn with
                    hash =
                      originalModule.fns
                      |> List.find (fun original -> original.name = fn.name)
                      |> Option.map _.hash
                      |> Option.defaultValue fn.hash }) })

    return adjusted
  }
