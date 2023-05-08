module Parser.Utils

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

/// Takes a long identifier and returns a list of its components, stringified
///
/// for example, F# code with System.IO.File would return ["System"; "IO"; "File"]
let longIdentToList (li : LongIdent) : List<string> =
  li |> List.map (fun id -> id.idText)

let parseAsFSharpSourceFile
  (filename : string)
  (input : string)
  : ParsedImplFileInput =
  let checker = FSharpChecker.Create()

  let filename = if filename.EndsWith ".fs" then filename else filename + ".fs"

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions =
    { FSharpParsingOptions.Default with SourceFiles = [| filename |] }

  let results =
    checker.ParseFile(filename, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously

  match results.ParseTree with
  | ParsedInput.ImplFile fsharpImplFile -> fsharpImplFile
  | _ ->
    Exception.raiseInternal
      $"Coudln't parse input as F# source file"
      [ "parseTree", results.ParseTree; "input", input ]


let singleExprFromImplFile (parsedAsFSharp : ParsedImplFileInput) : SynExpr =
  match parsedAsFSharp with
  | ParsedImplFileInput (_,
                         _,
                         _,
                         _,
                         _,
                         [ SynModuleOrNamespace (_,
                                                 _,
                                                 _,
                                                 [ SynModuleDecl.Expr (expr, _) ],
                                                 _,
                                                 _,
                                                 _,
                                                 _,
                                                 _) ],
                         _,
                         _,
                         _) -> expr
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parseTree", parsedAsFSharp ]

type AvailableTypes =
  Map<string, (LibExecution.ProgramTypes.FQTypeName.T * LibExecution.ProgramTypes.CustomType.T)>
