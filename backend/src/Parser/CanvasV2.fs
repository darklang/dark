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
let parseLetBinding
  (availableTypes : AvailableTypes)
  (m : CanvasModule)
  (letBinding : SynBinding)
  : CanvasModule =
  match letBinding with
  | SynBinding (_, _, _, _, attrs, _, _, pat, _returnInfo, expr, _, _, _) ->
    let expr = ProgramTypes.Expr.fromSynExpr availableTypes expr

    let attrs = attrs |> List.collect (fun l -> l.Attributes)

    let randomIds () : PT.Handler.ids =
      { moduleID = gid (); nameID = gid (); modifierID = gid () }

    match attrs with
    | [] ->
      let newFn = ProgramTypes.UserFunction.fromSynBinding availableTypes letBinding
      { m with fns = newFn :: m.fns }

    | [ attr ] ->
      match attr with
      | SimpleAttribute ("DB", [ name ]) ->
        // let newDB = PT.DB(name, randomIds ())
        // { m with dbs = newDB :: m.dbs }
        Exception.raiseInternal
          $"Not currently supporting DBs - probably makes sense to wait until they are based on a type rather than 'columns'"
          [ "attr", attr ]

      | SimpleAttribute ("HttpHandler", [ method; route ]) ->
        let newHttpHanlder = PT.Handler.Spec.HTTP(route, method, randomIds ())
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
///   or as DBs (i.e. with `[<DB("name")>]`)
/// - ? expressions are parsed as init commands (not currently supported)
/// - anything else fails
let parseDecls
  (availableTypes : AvailableTypes)
  (decls : List<SynModuleDecl>)
  : CanvasModule =
  List.fold
    emptyModule
    (fun m decl ->
      let availableTypes =
        (m.types)
        |> List.map (fun t -> PT.FQTypeName.User t.name, t.definition)
        |> (@) availableTypes

      match decl with
      | SynModuleDecl.Let (_, bindings, _) ->
        List.fold m (fun m b -> parseLetBinding availableTypes m b) bindings

      | SynModuleDecl.Types (defns, _) ->
        let newTypes =
          List.map (ProgramTypes.UserType.fromSynTypeDefn availableTypes) defns
        { m with types = m.types @ newTypes }

      | SynModuleDecl.Expr (expr, _) ->
        { m with
            exprs = m.exprs @ [ ProgramTypes.Expr.fromSynExpr availableTypes expr ] }

      | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
    decls

let postProcessModule (m : CanvasModule) : CanvasModule =
  let fnNames = m.fns |> List.map (fun f -> f.name) |> Set
  let fixup = ProgramTypes.Expr.fixupPass fnNames
  { m with
      handlers = m.handlers |> List.map (fun (spec, expr) -> (spec, fixup expr))
      exprs = m.exprs |> List.map fixup
      fns = m.fns |> List.map (fun f -> { f with body = fixup f.body }) }


let parseFromFile
  (availableTypes : AvailableTypes)
  (filename : string)
  : CanvasModule =
  let parsedAsFSharp =
    filename |> System.IO.File.ReadAllText |> parseAsFSharpSourceFile

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

  decls |> parseDecls availableTypes |> postProcessModule
