module LibExperimentalStdLib.LibTreeSitter

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open LibExecution.RuntimeTypes

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs



// Ideally, all of this would be in Dark code
module SyntaxTree =
  // This is what's sent to us from the frontend
  type TreeSitterNode =
    { typ : string
      text : string
      children : List<TreeSitterNode> }

  type TreeSitterTreeTpl = Node of (string * string * List<TreeSitterTreeTpl>)

  let rec toTuple (node : TreeSitterNode) : TreeSitterTreeTpl =
    Node (node.typ, node.text, (node.children |> List.map toTuple))

  // Ideally this would return the _Dark_ PT.Expr, not the F# one
  let toExpr (node : TreeSitterNode) : PT.Expr =
    let t = toTuple node

    match t with
    | Node ("string", s, []) -> PT.Expr.EString (gid(), [PT.StringText s])
    | Node ("int", s, []) -> PT.Expr.EInt (gid(), int s)
    //| Node ("float", s, []) -> PT.Expr.EFloat (gid(), float s)
    //| Node ("bool", s, []) -> PT.Expr.EBool (gid(), bool s)
    | _ -> Exception.raiseInternal $"Couldn't parse expression" [ "node", node ]


  //type toFunction (node: TreeSitterNode) : PT.UserFunction.T



let fns : List<BuiltInFn> =
  [ { name = fn "TreeSitter" "parseAsExpr" 0
      typeParams = []
      parameters = [ Param.make "code" TString ""]
      returnType = TResult(TString, TString)
      description =
        "Parses and executes arbitrary Dark code in the context of the current canvas."
      fn =
        function
        | state, _, [ DString code; DDict userInputs ] ->
          uply {
            // TODO: return an appropriate error if this fails
            let expr = Parser.RuntimeTypes.parseExprWithTypes Map.empty code

            let symtable = LibExecution.Interpreter.withGlobals state userInputs

            // TODO: return an appropriate error if this fails
            let! evalResult = LibExecution.Interpreter.eval state symtable expr

            return
              LibExecution.DvalReprDeveloper.toRepr evalResult
              |> DString
              |> Ok
              |> DResult
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Experiments" "parseAndSerializeExpr" 0
      typeParams = []
      parameters = [ Param.make "code" TString "" ]
      returnType = TResult(TString, TString)
      description = "Parses Dark code and serializes the result to JSON."
      fn =
        function
        | _, _, [ DString code ] ->
          uply {
            let expr = Parser.ProgramTypes.parseExprWithTypes Map.empty code
            let serializedExpr = Json.Vanilla.serialize expr
            return serializedExpr |> DString |> Ok |> DResult
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Experiments" "readFromStaticDir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TResult(TBytes, TString)
      description =
        "Reads a file at backend/static/<param path>, and returns its contents as Bytes wrapped in a Result"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let contents =
                RestrictedFileIO.readfileBytes
                  RestrictedFileIO.Config.BackendStatic
                  path
              return DResult(Ok(DBytes contents))
            with
            | e -> return DResult(Error(DString($"Error reading file: {e.Message}")))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]



