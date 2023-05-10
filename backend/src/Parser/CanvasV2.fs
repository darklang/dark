module Parser.CanvasV2

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

open Utils

type CanvasModule =
  { types : List<PT.UserType.T>
    fns : List<PT.UserFunction.T>
    dbs : List<PT.DB.T>
    // TODO: consider breaking this down into httpHandlers, crons, workers, and repls
    handlers : List<PT.Handler.Spec * PT.Expr>
    exprs : List<PT.Expr> }

let emptyModule = { types = []; fns = []; dbs = []; handlers = []; exprs = [] }


/// Extracts the parts we care about from an F# attribute
///
/// Given `[<HttpHandler("method", "path")>]`, returns `("HttpHandler", ["method"; "path"])`
/// wch is easier to work with than the AST presented normally
let (|SimpleAttribute|_|) (attr : SynAttribute) =
  let attrName =
    match longIdentToList attr.TypeName.LongIdent with
    | [ attrName ] -> attrName
    | _ -> Exception.raiseInternal $"Unsupported attribute name" []

  let rec parseAttrArgs (attr : SynExpr) : List<string> =
    match attr with
    | SynExpr.Paren (expr, _, _, _) -> parseAttrArgs expr

    | SynExpr.Const (SynConst.String (s, _, _), _) -> [ s ]

    | SynExpr.Tuple (_, args, _, _) ->
      args
      |> List.map (fun arg ->
        match arg with
        | SynExpr.Const (SynConst.String (s, _, _), _) -> s
        | _ ->
          Exception.raiseInternal $"Couldn't parse attribute argument" [ "arg", arg ])

    | _ ->
      Exception.raiseInternal $"Couldn't parse attribute argument" [ "attr", attr ]

  Some(attrName, parseAttrArgs attr.ArgExpr)

/// Update a CanvasModule by parsing a single F# let binding
/// Depending on the attribute present, this may add a user function, a handler, or a DB
let parseLetBinding (m : CanvasModule) (letBinding : SynBinding) : CanvasModule =
  match letBinding with
  | SynBinding (_, _, _, _, attrs, _, _, pat, _returnInfo, expr, _, _, _) ->
    let expr = ProgramTypes.Expr.fromSynExpr expr

    let attrs = attrs |> List.collect (fun l -> l.Attributes)

    match attrs with
    | [] ->
      // TODO: if the fn has no params, it's just a let expr
      // (probably need a refactor here)
      let newFn = ProgramTypes.UserFunction.fromSynBinding letBinding
      { m with fns = newFn :: m.fns }

    | [ attr ] ->
      match attr with
      | SimpleAttribute ("HttpHandler", [ method; route ]) ->
        let newHttpHanlder = PT.Handler.Spec.HTTP(route, method)
        { m with handlers = (newHttpHanlder, expr) :: m.handlers }

      | SimpleAttribute ("REPL", [ name ]) ->
        //let newHandler = PT.Handler.Spec.REPL(name, randomIds ())
        //{ m with handlers = (newHandler, expr) :: m.handlers }
        Exception.raiseInternal
          $"Not currently supporting REPLs, as we can't test them well yet"
          [ "attr", attr ]

      | SimpleAttribute ("Worker", [ name ]) ->
        //let newWorker = PT.Handler.Spec.Worker(name, randomIds ())
        //{ m with handlers = (newWorker, expr) :: m.handlers }
        Exception.raiseInternal
          $"Not currently supporting Workers, as we can't test them well yet"
          [ "attr", attr ]

      | SimpleAttribute ("Cron", [ name; interval ]) ->
        //let newCron = PT.Handler.Spec.Cron(name, interval, randomIds ())
        //{ m with handlers = (newCron, expr) :: m.handlers }
        Exception.raiseInternal
          $"Not currently supporting Crons, as we can't test them well yet"
          [ "attr", attr ]

      | _ ->
        Exception.raiseInternal
          $"Not sure how to handle this attribute"
          [ "attr", attr ]

    | _ ->
      Exception.raiseInternal
        $"Can only currently support 1 attribute [<...>] on a let binding"
        [ "attrs", attrs ]



module UserDB =
  let fromSynTypeDefn (typeDef : SynTypeDefn) : PT.DB.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev (_,
                                                                             typ,
                                                                             _),
                                           _),
                   _members,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = id.idText
        version = 0
        typ = Parser.ProgramTypes.TypeReference.fromSynType typ }
    | _ ->
      Exception.raiseInternal $"Unsupported db definition" [ "typeDef", typeDef ]

let parseTypeDefn (m : CanvasModule) (typeDefn : SynTypeDefn) : CanvasModule =
  match typeDefn with
  | SynTypeDefn (SynComponentInfo (attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
    let isDB =
      attrs
      |> List.map (fun attr -> attr.Attributes)
      |> List.concat
      |> List.exists (fun attr -> longIdentToList attr.TypeName.LongIdent = [ "DB" ])

    let (newDBs, newTypes) =
      if isDB then
        [ UserDB.fromSynTypeDefn typeDefn ], []
      else
        [], [ Parser.ProgramTypes.UserType.fromSynTypeDefn typeDefn ]

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
let parseDecls (decls : List<SynModuleDecl>) : CanvasModule =
  List.fold
    emptyModule
    (fun m decl ->
      match decl with
      | SynModuleDecl.Let (_, bindings, _) ->
        List.fold m (fun m b -> parseLetBinding m b) bindings

      | SynModuleDecl.Types (defns, _) ->
        List.fold m (fun m d -> parseTypeDefn m d) defns

      | SynModuleDecl.Expr (expr, _) ->
        { m with exprs = m.exprs @ [ ProgramTypes.Expr.fromSynExpr expr ] }

      | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
    decls

let postProcessModule (m : CanvasModule) : CanvasModule =
  let userFnNames = m.fns |> List.map (fun f -> f.name) |> Set
  let userTypeNames = m.types |> List.map (fun t -> t.name) |> Set
  let fixExpr = ProgramTypes.Expr.completeParse userFnNames userTypeNames
  { handlers = m.handlers |> List.map (fun (spec, expr) -> (spec, fixExpr expr))
    exprs = m.exprs |> List.map fixExpr
    fns =
      m.fns
      |> List.map (ProgramTypes.UserFunction.completeParse userFnNames userTypeNames)
    types = m.types |> List.map (ProgramTypes.UserType.completeParse userTypeNames)
    dbs =
      m.dbs
      |> List.map (fun db ->
        { db with typ = ProgramTypes.TypeReference.completeParse userTypeNames db.typ }) }

let parse (filename : string) (source : string) : CanvasModule =
  let parsedAsFSharp = parseAsFSharpSourceFile filename source

  let decls =
    match parsedAsFSharp with
    | ParsedImplFileInput (_,
                           _,
                           _,
                           _,
                           _,
                           [ SynModuleOrNamespace (_, _, _, decls, _, _, _, _, _) ],
                           _,
                           _,
                           _) -> decls
    | _ ->
      Exception.raiseInternal
        $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
        [ "parsedAsFsharp", parsedAsFSharp ]

  decls |> parseDecls |> postProcessModule


let parseFromFile (filename : string) : CanvasModule =
  filename |> System.IO.File.ReadAllText |> parse filename
