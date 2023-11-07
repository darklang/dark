/// Low-level tests around our .NET bindings for our tree-sitter parser
module Tests.TreeSitter

open Expecto

open Prelude
open TestUtils.TestUtils
open System.Text
open LibTreeSitter
open LibTreeSitter.Darklang

let testMultibyteCharacters =
  testCase "Basic function declaration parse test"
  <| fun _ ->
    let parser = new Parser(Language = DarklangLanguage.Create())
    let source =
      "let add (a: Int) (b: Int): Int =\n  let sum = a + b\n  sum"
      |> Encoding.UTF8.GetBytes

    let tree = parser.Parse(source, InputEncoding.Utf8)

    let rootNode = tree.Root
    let fnDefNode = rootNode.Child 0

    Expect.equal fnDefNode.Kind "fn_def" "Expected 'fn_def'"

    Expect.equal
      (fnDefNode.ChildByFieldName("return_type")).Kind
      "type"
      "Expected 'type'"

let toStringTest =
  testCase "Basic function declaration parse test"
  <| fun _ ->
    let parser = new Parser(Language = DarklangLanguage.Create())
    let source = "let increment (i: Int): Int =\n  i + 1" |> Encoding.UTF8.GetBytes

    let tree = parser.Parse(source, InputEncoding.Utf8)

    Expect.equal
      (tree.Root.ToString())
      "(source_file (fn_def name: (identifier) params: (fn_params_def first: (fn_param_def identifier: (identifier) typ: (type))) return_type: (type) body: (expression (identifier))) (ERROR (infix_operator) (UNEXPECTED '1')))"
      ""

let tests = testList "TreeSitter" [ testMultibyteCharacters; toStringTest ]
