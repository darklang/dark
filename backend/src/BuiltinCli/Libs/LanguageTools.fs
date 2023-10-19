/// Builtin functions for building the Language Server Protocol,
/// needed for our VS Code extension
///
/// todo maybe this should be in a separate Builtins project
module BuiltinCli.Libs.LanguageTools

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes

let typ (addlModules : List<string>) (name : string) (version : int) =
  TypeName.fqPackage
    "Darklang"
    ("LanguageTools" :: "LanguageServerProtocol" :: addlModules)
    name
    version

type Pos = { line : int; character : int }
type Range = { start : Pos; end_ : Pos }

module Range =
  let fromParserErrorRange (range : LibParser.ParserException.Range) : Range =
    let posFromParserError (pos : LibParser.ParserException.Pos) : Pos =
      { line = pos.line; character = pos.column }
    { start = posFromParserError range.start; end_ = posFromParserError range.end_ }

  let posToDT (pos : Pos) : Dval =
    let fields = [ "line", DInt pos.line; "character", DInt pos.character ]
    let typeName = typ [] "Position" 0
    DRecord(typeName, typeName, [], Map fields)

  let typeName = typ [] "Range" 0

  let toDT (range : Range) : Dval =
    let fields = [ "start", posToDT range.start; "end_", posToDT range.end_ ]
    DRecord(typeName, typeName, [], Map fields)


type ScriptParseError =
  | FailedToParse of msg : string * range : Option<Range>
  | Other of msg : string

module ScriptParseError =
  let typeName = typ [] "ScriptParseError" 0

  let toDT (err : ScriptParseError) : Dval =
    let caseName, fields =
      match err with
      | FailedToParse(msg, range) ->
        let range =
          range
          |> Option.map Range.toDT
          |> Dval.option (KTCustomType(Range.typeName, []))
        "FailedToParse", [ DString msg; range ]

      | Other(msg) -> "Other", [ DString msg ]

    DEnum(typeName, typeName, [], caseName, fields)


let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "LanguageTools" ] "parseCliScript" 0
      typeParams = []
      parameters = [ Param.make "sourceCode" TString "" ]
      returnType =
        TypeReference.result TString (TCustomType(Ok ScriptParseError.typeName, []))
      description = "Tries to parse Darklang code as a script"
      fn =
        let errType = KTCustomType(ScriptParseError.typeName, [])
        let resultOk = Dval.resultOk KTUnit errType
        let resultError = Dval.resultError KTUnit errType
        (function
        | state, [], [ DString sourceCode ] ->
          uply {
            let nameResolver = LibParser.NameResolver.fromExecutionState state

            try
              let! _parsed =
                LibParser.Canvas.parse nameResolver "code.dark" sourceCode

              return resultOk DUnit

            with
            | :? LibParser.ParserException.ParserException as e ->
              return
                FailedToParse(
                  e.Message,
                  e.range |> Option.map Range.fromParserErrorRange
                )
                |> ScriptParseError.toDT
                |> resultError
            | e -> return Other(e.Message) |> ScriptParseError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []

let contents = (fns, types, constants)
