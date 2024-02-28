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

    Expect.equal fnDefNode.Kind "fn_decl" "Expected 'fn_decl'"

    Expect.equal
      (fnDefNode.ChildByFieldName("return_type")).Kind
      "type_reference"
      "Expected 'type_reference'"

let toStringTest =
  testCase "Basic function declaration parse test"
  <| fun _ ->
    let parser = new Parser(Language = DarklangLanguage.Create())
    let source = "let increment (i: Int64): Int64 = i + 1L" |> Encoding.UTF8.GetBytes

    let tree = parser.Parse(source, InputEncoding.Utf8)

    Expect.equal
      (tree.Root.ToString())
      "(source_file (fn_decl keyword_let: (keyword) name: (fn_identifier) params: (fn_decl_params (fn_decl_param symbol_left_paren: (symbol) identifier: (variable_identifier) symbol_colon: (symbol) typ: (type_reference (builtin_type)) symbol_right_paren: (symbol))) symbol_colon: (symbol) return_type: (type_reference (builtin_type)) symbol_equals: (symbol) body: (expression (infix_operation left: (expression (variable_identifier)) operator: (operator) right: (expression (int64_literal digits: (digits (positive_digits)) suffix: (symbol)))))))"
      ""

let tests = testList "TreeSitter" [ testMultibyteCharacters; toStringTest ]
