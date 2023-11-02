module BuiltinExecution.Libs.Parser

open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibTreeSitter
open LibTreeSitter.Darklang

module VT = ValueType
module Dval = LibExecution.Dval


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []


let packageFnType
  (addlModules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage
    "Darklang"
    ([ "LanguageTools"; "Parser" ] @ addlModules)
    name
    version


let pointTypeName = packageFnType [] "Point" 0
let rangeTypeName = packageFnType [] "Range" 0
let parsedNodeTypeName = packageFnType [] "ParsedNode" 0

let fns : List<BuiltInFn> =
  [ { name = fn [ "Parser" ] "parseToSimplifiedTree" 0
      typeParams = []
      parameters = [ Param.make "sourceCode" TString "" ]
      returnType = TCustomType(Ok(packageFnType [] "ParsedNode" 0), [])
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
                let fields = [ "row", DInt point.Row; "column", DInt point.Column ]
                DRecord(pointTypeName, pointTypeName, [], Map fields)

              let startPos = cursor.Current.StartPosition
              let endPos = cursor.Current.EndPosition

              let sourceRange =
                let fields = [ "start", mapPoint startPos; "end_", mapPoint endPos ]
                DRecord(rangeTypeName, rangeTypeName, [], Map fields)

              let sourceText =
                let lines = String.splitOnNewline sourceCode

                match startPos.Row with
                | row when row = endPos.Row ->
                  lines[row][startPos.Column .. (endPos.Column - 1)]
                | _ ->
                  let firstLine = lines[startPos.Row][startPos.Column ..]
                  let middleLines =
                    if startPos.Row + 1 <= endPos.Row - 1 then
                      lines[startPos.Row + 1 .. endPos.Row - 1]
                    else
                      []
                  let lastLine = lines[endPos.Row][.. endPos.Column - 1]

                  String.concat "\n" (firstLine :: middleLines @ [ lastLine ])

              [ ("fieldName",
                 // TODO: Option<String>
                 DString(if cursor.FieldName = null then "" else cursor.FieldName))
                ("typ", DString cursor.Current.Kind)
                ("text", DString sourceText)
                ("sourceRange", sourceRange)
                ("children", DList(VT.customType parsedNodeTypeName [], children)) ]

            DRecord(parsedNodeTypeName, parsedNodeTypeName, [], Map fields)


          let parser = new Parser(Language = DarklangLanguage.Create())

          let tree =
            parser.Parse(Encoding.UTF8.GetBytes sourceCode, InputEncoding.Utf8)

          tree.Root.Walk() |> mapNodeAtCursor |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
