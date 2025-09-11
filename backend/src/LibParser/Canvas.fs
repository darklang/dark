module LibParser.Canvas

open FSharp.Compiler.Syntax

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module WT = WrittenTypes
module FS2WT = FSharpToWrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module NR = NameResolver

open Utils
open ParserException

type WTCanvasModule =
  { owner : string
    name : List<string>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue>
    dbs : List<WT.DB.T>
    fns : List<WT.PackageFn.PackageFn>
    // TODO: consider breaking this down into httpHandlers, crons, workers, and repls
    handlers : List<WT.Handler.Spec * WT.Expr>
    exprs : List<WT.Expr> }

let emptyRootWTModule owner canvasName =
  { owner = owner
    name = [ "Canvas"; canvasName ]
    types = []
    values = []
    dbs = []
    fns = []
    handlers = []
    exprs = [] }

type PTCanvasModule =
  { types : List<PT.PackageType.PackageType>
    values : List<PT.PackageValue.PackageValue>
    fns : List<PT.PackageFn.PackageFn>

    dbs : List<PT.DB.T>
    // TODO: consider breaking this down into httpHandlers, crons, workers, and repls
    handlers : List<PT.Handler.Spec * PT.Expr>
    exprs : List<PT.Expr> }



/// Extracts the parts we care about from an F# attribute
///
/// Given `[<HttpHandler("method", "path")>]`, returns `("HttpHandler", ["method"; "path"])`
/// wch is easier to work with than the AST presented normally
let (|SimpleAttribute|_|) (attr : SynAttribute) =
  let attrName =
    match longIdentToList attr.TypeName.LongIdent with
    | [ attrName ] -> attrName
    | _ -> raiseParserError "Unsupported attribute name" [] (Some attr.Range)

  let rec parseAttrArgs (attr : SynExpr) : List<string> =
    match attr with
    | SynExpr.Paren(expr, _, _, _) -> parseAttrArgs expr

    | SynExpr.Const(SynConst.String(s, _, _), _) -> [ s ]

    | SynExpr.Tuple(_, args, _, _) ->
      args
      |> List.map (fun arg ->
        match arg with
        | SynExpr.Const(SynConst.String(s, _, _), _) -> s
        | _ ->
          raiseParserError
            $"Couldn't parse attribute argument"
            [ "arg", arg ]
            (Some attr.Range))

    | _ ->
      raiseParserError
        "Couldn't parse attribute argument"
        [ "attr", attr ]
        (Some attr.Range)

  Some(attrName, parseAttrArgs attr.ArgExpr)


/// Update a CanvasModule by parsing a single F# let binding
/// Depending on the attribute present, this may add a user function, a handler, or a DB
let parseLetBinding (m : WTCanvasModule) (letBinding : SynBinding) : WTCanvasModule =
  match letBinding with
  | SynBinding(_, _, _, _, attrs, _, _, pat, returnInfo, expr, _, _, _) ->
    let expr = FS2WT.Expr.fromSynExpr expr

    let attrs = attrs |> List.collect _.Attributes

    match attrs with
    | [] ->
      // functions require a return type to be defined,
      // and let exprs don't allow return types to be defined,
      // so we can use this to distinguish between the two
      // when mapping from F#
      match returnInfo with
      | None ->
        let newExpr =
          WT.ELet(gid (), FS2WT.LetPattern.fromSynPat pat, expr, WT.EUnit(gid ()))
        { m with exprs = m.exprs @ [ newExpr ] }
      | Some _ ->
        let newFn = FS2WT.PackageFn.fromSynBinding m.owner m.name letBinding
        { m with fns = newFn :: m.fns }

    | [ attr ] ->
      match attr with
      | SimpleAttribute("HttpHandler", [ method; route ]) ->
        let newHttpHanlder = WT.Handler.Spec.HTTP(route, method)
        { m with handlers = (newHttpHanlder, expr) :: m.handlers }

      | SimpleAttribute("REPL", [ _name ]) ->
        //let newHandler = PT.Handler.Spec.REPL(name, randomIds ())
        //{ m with handlers = (newHandler, expr) :: m.handlers }
        raiseParserError
          "Not currently supporting REPLs, as we can't test them well yet"
          [ "attr", attr ]
          (Some attr.Range)

      | SimpleAttribute("Worker", [ _name ]) ->
        //let newWorker = PT.Handler.Spec.Worker(name, randomIds ())
        //{ m with handlers = (newWorker, expr) :: m.handlers }
        raiseParserError
          "Not currently supporting Workers, as we can't test them well yet"
          [ "attr", attr ]
          (Some attr.Range)

      | SimpleAttribute("Cron", [ _name; _interval ]) ->
        //let newCron = PT.Handler.Spec.Cron(name, interval, randomIds ())
        //{ m with handlers = (newCron, expr) :: m.handlers }
        raiseParserError
          "Not currently supporting Crons, as we can't test them well yet"
          [ "attr", attr ]
          (Some attr.Range)

      | _ ->
        raiseParserError
          $"Not sure how to handle this attribute"
          [ "attr", attr ]
          (Some attr.Range)

    | _ ->
      raiseParserError
        "Can only currently support 1 attribute [<...>] on a let binding"
        [ "attrs", attrs ]
        (Some letBinding.RangeOfBindingWithoutRhs)



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
      raiseParserError
        "Unsupported db definition"
        [ "typeDef", typeDef ]
        (Some typeDef.Range)

let parseTypeDefn (m : WTCanvasModule) (typeDefn : SynTypeDefn) : WTCanvasModule =
  match typeDefn with
  | SynTypeDefn(SynComponentInfo(attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
    let isDB =
      attrs
      |> List.map _.Attributes
      |> List.concat
      |> List.exists (fun attr -> longIdentToList attr.TypeName.LongIdent = [ "DB" ])

    let (newDBs, newTypes) =
      if isDB then
        [ UserDB.fromSynTypeDefn typeDefn ], []
      else
        [], [ FS2WT.PackageType.fromSynTypeDefn m.owner m.name typeDefn ]

    { m with types = m.types @ newTypes; dbs = m.dbs @ newDBs }


/// An F# module has a list of declarations, which can be:
/// - a group of let bindings
/// - a group of type definitions
/// - a single expression
/// - a nested module (not currently supported)
/// - etc
///
/// This builds up a CanvasModule by parsing each declaration:
/// - type definitions are parsed as user types
/// - let bindings with no attributes are parsed as user functions
/// - let bindings with attributes are parsed
///   as handlers (i.e. with `[<HttpHandler("method", "path")>]`)
///   or as DBs (i.e. with `[<DB>] DBName = Type`)
/// - ? expressions are parsed as init commands (not currently supported)
/// - anything else fails
let parseDecls
  (owner : string)
  (canvasName : string)
  (decls : List<SynModuleDecl>)
  : WTCanvasModule =
  List.fold
    (fun m decl ->
      match decl with
      | SynModuleDecl.Let(_, bindings, _) ->
        List.fold (fun m b -> parseLetBinding m b) m bindings

      | SynModuleDecl.Types(defns, _) ->
        List.fold (fun m d -> parseTypeDefn m d) m defns

      | SynModuleDecl.Expr(expr, _) ->
        { m with exprs = m.exprs @ [ FS2WT.Expr.fromSynExpr expr ] }

      | _ ->
        raiseParserError
          "Unsupported declaration"
          [ "decl", decl; "owner", owner; "canvasName", canvasName ]
          (Some decl.Range))
    (emptyRootWTModule owner canvasName)
    decls


let toPT
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (m : WTCanvasModule)
  : Ply<PTCanvasModule> =
  uply {
    let! types =
      m.types
      |> Ply.List.mapSequentially (
        WT2PT.PackageType.toPT pm onMissing (m.owner :: m.name)
      )

    let! values =
      m.values
      |> Ply.List.mapSequentially (
        WT2PT.PackageValue.toPT builtins pm onMissing (m.owner :: m.name)
      )

    let! dbs =
      m.dbs
      |> Ply.List.mapSequentially (WT2PT.DB.toPT pm onMissing (m.owner :: m.name))

    let! fns =
      m.fns
      |> Ply.List.mapSequentially (
        WT2PT.PackageFn.toPT builtins pm onMissing (m.owner :: m.name)
      )

    let! handlers =
      m.handlers
      |> Ply.List.mapSequentially (fun (spec, expr) ->
        uply {
          let spec = WT2PT.Handler.Spec.toPT spec
          let! expr = WT2PT.Expr.toPT builtins pm onMissing (m.owner :: m.name) expr
          return (spec, expr)
        })

    let! exprs =
      m.exprs
      |> Ply.List.mapSequentially (
        WT2PT.Expr.toPT builtins pm onMissing (m.owner :: m.name)
      )

    return
      { types = types
        values = values
        dbs = dbs
        fns = fns
        handlers = handlers
        exprs = exprs }
  }


let parse
  (owner : string)
  (canvasName : string)
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (filename : string)
  (source : string)
  : Ply<PTCanvasModule> =

  uply {
    let parsedAsFSharp = parseAsFSharpSourceFile filename source

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
        raiseParserError
          "Wrong shape tree - parseable canvas definitions must contain a single module with declarations inside"
          [ "parsedAsFsharp", parsedAsFSharp ]
          None // whole file

    let moduleWT = parseDecls owner canvasName decls

    // Initial pass, so we can re-parse with all names in context
    let! initialResult = toPT builtins pm onMissing moduleWT

    let pm =
      pm
      |> PT.PackageManager.withExtras
        initialResult.types
        initialResult.values
        initialResult.fns

    // Now, parse again, but with the names in context (so fewer are marked as unresolved)
    let! result = toPT builtins pm onMissing moduleWT

    let adjusted =
      { types = result.types |> List.map (fun typ -> { typ with hash = typ.hash })
        // id =
        //   initialResult.types
        //   |> List.find (fun original -> original.name = typ.name)
        //   |> Option.map _.id
        //   |> Option.defaultValue typ.id })
        values = result.values |> List.map (fun c -> { c with hash = c.hash })
        // id =
        //   initialResult.values
        //   |> List.find (fun original -> original.name = c.name)
        //   |> Option.map _.id
        //   |> Option.defaultValue c.id })
        fns = result.fns |> List.map (fun fn -> { fn with hash = fn.hash })
        // id =
        //   initialResult.fns
        //   |> List.find (fun original -> original.name = fn.name)
        //   |> Option.map _.id
        //   |> Option.defaultValue fn.id })

        dbs = result.dbs
        handlers = result.handlers
        exprs = result.exprs }

    return adjusted
  }
