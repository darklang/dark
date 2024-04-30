module BuiltinExecution.Libs.Parser

open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

open LibTreeSitter

module VT = ValueType
module Dval = LibExecution.Dval


let packageFnType
  (addlModules : List<string>)
  (name : string)
  (version : int)
  : FQTypeName.FQTypeName =
  FQTypeName.fqPackage
    "Darklang"
    ([ "LanguageTools"; "Parser" ] @ addlModules)
    name
    version


let pointTypeName = packageFnType [] "Point" 0
let rangeTypeName = packageFnType [] "Range" 0
let parsedNodeTypeName = packageFnType [] "ParsedNode" 0

let fns : List<BuiltInFn> =
  [ { name = fn "parserParseToSimplifiedTree" 0
      typeParams = []
      parameters = [ Param.make "sourceCode" TString "" ]
      returnType = TCustomType(Ok parsedNodeTypeName, [])
      description = "Parses some Darklang code"
      fn =
        (function
        | _, _, [ DString sourceCode ] ->
          let rec mapNodeAtCursor (cursor : TreeCursor) : Dval =
            let mutable children = []

            if cursor.GotoFirstChild() then
              children <- children @ [ mapNodeAtCursor cursor ]

              while cursor.GotoNextSibling() do
                children <- children @ [ mapNodeAtCursor cursor ]

              cursor.GotoParent() |> ignore<bool>

            let fields =
              let mapPoint (point : Point) =
                let fields =
                  [ "row", DInt64 point.row; "column", DInt64 point.column ]
                DRecord(pointTypeName, pointTypeName, [], Map fields)

              let startPos = cursor.Current.StartPosition
              let endPos = cursor.Current.EndPosition

              let sourceRange =
                let fields = [ "start", mapPoint startPos; "end_", mapPoint endPos ]
                DRecord(rangeTypeName, rangeTypeName, [], Map fields)

              let sourceText =
                let lines = String.splitOnNewline sourceCode
                if lines.Length = 0 then
                  ""
                else
                  match startPos.row with
                  | row when row = endPos.row ->
                    lines[row][startPos.column .. (endPos.column - 1)]
                  | _ ->
                    let firstLine = lines[startPos.row][startPos.column ..]
                    let middleLines =
                      if startPos.row + 1 <= endPos.row - 1 then
                        lines[startPos.row + 1 .. endPos.row - 1]
                      else
                        []
                    let lastLine = lines[endPos.row][.. endPos.column - 1]

                    String.concat "\n" (firstLine :: middleLines @ [ lastLine ])

              let fieldName =
                if cursor.FieldName = null then
                  Dval.optionNone KTString
                else
                  Dval.optionSome KTString (DString cursor.FieldName)

              [ ("fieldName", fieldName)
                ("typ", DString cursor.Current.Kind)
                ("text", DString sourceText)
                ("range", sourceRange)
                ("children", DList(VT.customType parsedNodeTypeName [], children)) ]

            DRecord(parsedNodeTypeName, parsedNodeTypeName, [], Map fields)


          let parser = new Parser(Language = DarklangLanguage.create ())

          let tree =
            parser.Parse(Encoding.UTF8.GetBytes sourceCode, InputEncoding.Utf8, None)

          tree.Root.Walk() |> mapNodeAtCursor |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
