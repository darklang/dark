module BuiltinExecution.Libs.Parser

open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text
open System.Globalization
open System

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

open LibTreeSitter

module VT = ValueType
module Dval = LibExecution.Dval
module IDs = LibExecution.PackageIDs.Type.LanguageTools.Parser



let pointTypeName = FQTypeName.fqPackage IDs.point
let rangeTypeName = FQTypeName.fqPackage IDs.range
let parsedNodeTypeName = FQTypeName.fqPackage IDs.parsedNode

let fns : List<BuiltInFn> =
  [ { name = fn "parserParseToSimplifiedTree" 0
      typeParams = []
      parameters = [ Param.make "sourceCode" TString "" ]
      returnType = TCustomType(Ok parsedNodeTypeName, [])
      description = "Parses some Darklang code"
      fn =
        (function
        | _, _, [ DString sourceCode ] ->
          // This was added to handle EGCs correctly
          let byteIndexToCharIndex (byteIndex : int) (text : string) : int =
            let bytes = Encoding.UTF8.GetBytes(text)
            let subText = Encoding.UTF8.GetString(bytes, 0, byteIndex)
            StringInfo.ParseCombiningCharacters(subText).Length

          let getUnicodeAwareSubstring
            (line : string)
            (startIndex : int)
            (endIndex : int)
            =
            let textElements = StringInfo.GetTextElementEnumerator(line)
            let mutable result = ""
            let mutable currentIndex = 0
            while textElements.MoveNext() do
              if currentIndex >= startIndex && currentIndex < endIndex then
                result <- result + (textElements.GetTextElement())
              currentIndex <- currentIndex + 1
            result

          let rec mapNodeAtCursor (cursor : TreeCursor) : Dval =
            let mutable children = []

            if cursor.GotoFirstChild() then
              children <- children @ [ mapNodeAtCursor cursor ]

              while cursor.GotoNextSibling() do
                children <- children @ [ mapNodeAtCursor cursor ]

              cursor.GotoParent() |> ignore<bool>

            let fields =
              let mapPoint (point : Point) =
                let pointRow = point.row + 1
                let fields =
                  [ "row", DInt64 pointRow; "column", DInt64 point.column ]
                DRecord(pointTypeName, pointTypeName, [], Map fields)

              let startPos = cursor.Current.StartPosition
              let endPos = cursor.Current.EndPosition

              let range =
                let fields = [ "start", mapPoint startPos; "end_", mapPoint endPos ]
                DRecord(rangeTypeName, rangeTypeName, [], Map fields)

              let sourceText =
                let lines = String.splitOnNewline sourceCode
                if lines.Length = 0 then
                  ""
                else
                  let startLine = lines[startPos.row]
                  let endLine = lines[endPos.row]
                  let startCharIndex = byteIndexToCharIndex startPos.column startLine
                  let endCharIndex = byteIndexToCharIndex endPos.column endLine

                  match startPos.row with
                  | row when row = endPos.row ->
                    getUnicodeAwareSubstring startLine startCharIndex endCharIndex
                  | _ ->
                    let firstLine =
                      getUnicodeAwareSubstring
                        startLine
                        startCharIndex
                        startLine.Length
                    let middleLines =
                      if startPos.row + 1 <= endPos.row - 1 then
                        lines[startPos.row + 1 .. endPos.row - 1]
                        |> List.map (fun line ->
                          getUnicodeAwareSubstring line 0 line.Length)
                      else
                        []
                    let lastLine = getUnicodeAwareSubstring endLine 0 endCharIndex
                    String.concat "\n" (firstLine :: middleLines @ [ lastLine ])

              let fieldName =
                if cursor.FieldName = null then
                  Dval.optionNone KTString
                else
                  Dval.optionSome KTString (DString cursor.FieldName)

              [ ("fieldName", fieldName)
                ("typ", DString cursor.Current.Kind)
                ("text", DString sourceText)
                ("range", range)
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
