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

module ScriptParseError =
  type ScriptParseError =
    | FailedToParse of startPos : (int * int) * endPos : (int * int) * msg : string
    | Other of msg : string * metadata : List<string * string>

  let toDT (err : ScriptParseError) : Dval =
    let caseName, fields =
      match err with
      | FailedToParse((startLine, startChar), (endLine, endChar), msg) ->
        "FailedToParse",
        [ DTuple(DInt startLine, DInt startChar, [])
          DTuple(DInt endLine, DInt endChar, [])
          DString msg ]

      | Other(msg, metadata) ->
        "Other",
        [ DString msg
          metadata |> List.map (fun (k, v) -> k, DString v) |> Dval.dict KTString ]

    let typeName =
      TypeName.fqPackage "Darklang" [ "LanguageServerProtocol" ] "ScriptParseError" 0

    DEnum(typeName, typeName, [], caseName, fields)


let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "LanguageTools" ] "parseCliScript" 0
      typeParams = []
      parameters = [ Param.make "sourceCode" TString "" ]
      returnType = TypeReference.result TString TString // TODO: custom types
      description = "Tries to parse Darklang code as a script"
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | state, [], [ DString sourceCode ] ->
          uply {
            let nameResolver = LibParser.NameResolver.fromExecutionState state

            try
              let! _parsed =
                LibParser.Canvas.parse nameResolver "code.dark" sourceCode

              return resultOk DUnit
            with e ->
              let msg = Exception.getMessages e |> String.concat "\n"
              // let _metadata =
              //   Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)

              // TODO: lines of where it failed?
              (*
                type ScriptParseError =
                  | Other of msg : string * metadata : List<string * string>

              *)
              return resultError (DString msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents = (fns, types, constants)
