module Parser.CanvasV1

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

module PTP = ProgramTypes
open Utils

type CanvasModule =
  { types : List<PT.UserType.T>
    fns : List<PT.UserFunction.T>
    httpHandlers : List<PT.Expr> }

let emptyModule = { types = []; fns = []; httpHandlers = [] }

let parseDecls availableTypes (decls : List<SynModuleDecl>) : CanvasModule =
  List.fold
    emptyModule
    (fun m decl ->
      let availableTypes =
        (m.types)
        |> List.map (fun t -> PT.FQTypeName.User t.name, t.definition)
        |> (@) availableTypes

      match decl with
      | SynModuleDecl.Let (_, bindings, _) ->
        let newFns =
          List.map (PTP.UserFunction.fromSynBinding availableTypes) bindings
        { m with fns = m.fns @ newFns }

      | SynModuleDecl.Types (defns, _) ->
        let newTypes = List.map (PTP.UserType.fromSynTypeDefn availableTypes) defns
        { m with types = m.types @ newTypes }

      | SynModuleDecl.Expr (expr, _) ->
        { m with
            httpHandlers =
              m.httpHandlers @ [ PTP.Expr.fromSynExpr availableTypes expr ] }

      | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
    decls


let parseHandlerFromFile
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (filename : string)
  : CanvasModule =
  let parsedAsFSharp =
    filename |> System.IO.File.ReadAllText |> parseAsFSharpSourceFile

  match parsedAsFSharp with
  | ParsedImplFileInput (_,
                         _,
                         _,
                         _,
                         _,
                         [ SynModuleOrNamespace (_, _, _, decls, _, _, _, _, _) ],
                         _,
                         _,
                         _) -> parseDecls availableTypes decls
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parsedAsFsharp", parsedAsFSharp ]
