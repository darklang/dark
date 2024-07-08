/// Tests the new, tree-sitter-based parser.
///
/// Currently just focused on round-tripping:
///   source (input)
///   -> parsed TreeSitter tree
///   -> parsed CLI Script
///   -> PT.SourceFile
///   -> pretty-print back to text (expected)
///
module Tests.NewParser

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PackageIDs = LibExecution.PackageIDs


let pmPT = LibCloud.PackageManager.pt

let t
  (name : string)
  (input : string)
  (expected : string)
  (extraTypes : List<PT.PackageType.PackageType>)
  (extraConstants : List<PT.PackageConstant.PackageConstant>)
  (extraFns : List<PT.PackageFn.PackageFn>)
  (allowUnresolved : bool)
  =
  let fnname =
    RT.FQFnName.FQFnName.Package
      PackageIDs.Fn.LanguageTools.Parser.parseAndPrettyPrint

  testTask name {
    let pm =
      if allowUnresolved then
        pmPT
      else
        PT.PackageManager.withExtras pmPT extraTypes extraConstants extraFns
    let canvasID = System.Guid.NewGuid()
    let! state = executionStateFor pm canvasID false false Map.empty

    let args = NEList.singleton (RT.DString input)
    let! exeResult = LibExecution.Execution.executeFunction state fnname [] args
    let! actual = state |> unwrapExecutionResult exeResult |> Ply.toTask

    return
      Expect.equalDval actual (RT.DString expected) "Didn't round-trip as expected"
  }


let tID = System.Guid.NewGuid()
let person3 : PT.PackageType.PackageType =
  { id = tID
    name = { owner = "Tests"; modules = []; name = "Person3" }
    description = ""
    deprecated = PT.NotDeprecated
    declaration =
      { typeParams = []
        definition =
          PT.TypeDeclaration.Record(
            { head =
                { name = "name"; typ = PT.TypeReference.TString; description = "" }
              tail =
                [ { name = "age"; typ = PT.TypeReference.TInt64; description = "" }
                  { name = "hasPet"; typ = PT.TypeReference.TBool; description = "" } ] }
          ) } }


let typeReferences =
  [
    // all built-ins
    t "unit alias" "type MyUnit = Unit" "type MyUnit =\n  Unit" [] [] [] false

    t "bool alias" "type MyBool = Bool" "type MyBool =\n  Bool" [] [] [] false
    t "int8 alias" "type MyInt8 = Int8" "type MyInt8 =\n  Int8" [] [] [] false
    t "uint8 alias" "type MyUInt8 = UInt8" "type MyUInt8 =\n  UInt8" [] [] [] false
    t "int16 alias" "type MyInt16 = Int16" "type MyInt16 =\n  Int16" [] [] [] false
    t
      "uint16 alias"
      "type MyUInt16 = UInt16"
      "type MyUInt16 =\n  UInt16"
      []
      []
      []
      false
    t "int32 alias" "type MyInt32 = Int32" "type MyInt32 =\n  Int32" [] [] [] false
    t
      "uint32 alias"
      "type MyUInt32 = UInt32"
      "type MyUInt32 =\n  UInt32"
      []
      []
      []
      false
    t "int64 alias" "type MyInt64 = Int64" "type MyInt64 =\n  Int64" [] [] [] false
    t
      "uint64 alias"
      "type MyUInt64 = UInt64"
      "type MyUInt64 =\n  UInt64"
      []
      []
      []
      false
    t
      "int128 alias"
      "type MyInt128 = Int128"
      "type MyInt128 =\n  Int128"
      []
      []
      []
      false
    t
      "uint128 alias"
      "type MyUInt128 = UInt128"
      "type MyUInt128 =\n  UInt128"
      []
      []
      []
      false

    t "float alias" "type MyFloat = Float" "type MyFloat =\n  Float" [] [] [] false

    t "char alias" "type MyChar = Char" "type MyChar =\n  Char" [] [] [] false
    t
      "string alias"
      "type MyString = String"
      "type MyString =\n  String"
      []
      []
      []
      false

    t
      "datetime alias"
      "type MyDateTime = DateTime"
      "type MyDateTime =\n  DateTime"
      []
      []
      []
      false
    t "uuid alias" "type MyUuid = Uuid" "type MyUuid =\n  Uuid" [] [] [] false

    t
      "string list alias"
      "type MyList = List<String>"
      "type MyList =\n  List<String>"
      []
      []
      []
      false
    t
      "string list list alias"
      "type MyList = List<List<String>>"
      "type MyList =\n  List<List<String>>"
      []
      []
      []
      false
    t
      "custom type list alias"
      "type MyList = List<MyString>"
      "type MyList =\n  List<MyString>"
      []
      []
      []
      false
    t
      "generic list alias"
      "type MyList = List<'a>"
      "type MyList =\n  List<'a>"
      []
      []
      []
      false
    t
      "int64 dict alias"
      "type MyDict = Dict<Int64>"
      "type MyDict =\n  Dict<Int64>"
      []
      []
      []
      false
    t
      "custom type dict alias"
      "type MyDict = Dict<MyList>"
      "type MyDict =\n  Dict<MyList>"
      []
      []
      []
      false
    t
      "tuple2 alias"
      "type MyTuple2 = (String * Int64)"
      "type MyTuple2 =\n  (String * Int64)"
      []
      []
      []
      false
    t
      "tuple3 alias"
      "type MyTuple3 = (String * Int64 * Bool)"
      "type MyTuple3 =\n  (String * Int64 * Bool)"
      []
      []
      []
      false
    t
      "tuple4 alias"
      "type MyTuple = (String * Int64 * Bool * Unit)"
      "type MyTuple =\n  (String * Int64 * Bool * Unit)"
      []
      []
      []
      false

    t
      "fn with one arg"
      "type MyFn = 'a -> String"
      "type MyFn =\n  'a -> String"
      []
      []
      []
      false
    t
      "fn with two args"
      "type MyFn = 'a -> 'b -> 'c"
      "type MyFn =\n  'a -> 'b -> 'c"
      []
      []
      []
      false
    t
      "fn with three args"
      "type MyFn = 'a -> 'b -> 'c -> 'd"
      "type MyFn =\n  'a -> 'b -> 'c -> 'd"
      []
      []
      []
      false
    t
      "fn with tuple arg"
      "type MyFn = (String * Int64 * Bool) -> Dict<Int64> -> List<List<String>>"
      "type MyFn =\n  (String * Int64 * Bool) -> Dict<Int64> -> List<List<String>>"
      []
      []
      []
      false
    t
      "fn with generics"
      "type MyFn = PACKAGE.Darklang.LanguageTools.ID -> 'a -> 'b"
      "type MyFn =\n  PACKAGE.Darklang.LanguageTools.ID -> 'a -> 'b"
      []
      []
      []
      false

    t "db with generic" "type MyDB = DB<'a>" "type MyDB =\n  DB<'a>" [] [] [] false
    t
      "db with custom type"
      "type MyDB = DB<Person>"
      "type MyDB =\n  DB<Person>"
      []
      []
      []
      false
    t
      "db with generic 2"
      "type MyDB<'a> = DB<'a>"
      "type MyDB<'a> =\n  DB<'a>"
      []
      []
      []
      false
    t
      "db with generic applied"
      "type GenericDB = DB<Generic<String>>"
      "type GenericDB =\n  DB<Generic<String>>"
      []
      []
      []
      false

    t "variable" "type MyVar = 'a" "type MyVar =\n  'a" [] [] [] false


    // single-part qualified name
    t
      "unknown single-part qualified name"
      "type ID = Test"
      "type ID =\n  Test"
      []
      []
      []
      false


    // fully-qualified package name (multi-part)
    t
      "option alias, shortcut name"
      "type MyOption = PACKAGE.Darklang.Stdlib.Option.Option"
      "type MyOption =\n  PACKAGE.Darklang.Stdlib.Option.Option"
      []
      []
      []
      false
    t
      "option alias, unapplied"
      "type MyOption = Stdlib.Option.Option"
      "type MyOption =\n  PACKAGE.Darklang.Stdlib.Option.Option"
      []
      []
      []
      false
    t
      "option alias, applied"
      "type MyOption = Stdlib.Option.Option<Int64>"
      "type MyOption =\n  PACKAGE.Darklang.Stdlib.Option.Option<Int64>"
      []
      []
      []
      false ]
  |> testList "type references"


let typeDeclarations =
  [ t "unit" "type SimpleAlias = Unit" "type SimpleAlias =\n  Unit" [] [] [] false

    // Alias with type params
    t
      "type param list"
      "type MyType<'a> = List<'a>"
      "type MyType<'a> =\n  List<'a>"
      []
      []
      []
      false
    t
      "type param tuple"
      "type MyType<'a, 'b> = (List<'a> * List<'b>)"
      "type MyType<'a, 'b> =\n  (List<'a> * List<'b>)"
      []
      []
      []
      false
    t
      "type param record"
      "type Generic<'a> = { x: 'a }"
      "type Generic<'a> =\n  { x: 'a }"
      []
      []
      []
      false

    // // Enum type TODO
    // ("type Thing = | A | B of Int64 | C of String * Bool"  "type Thing =\n  | A\n  | B of Int64\n  | C of String * Bool"

    // Record type decls
    t
      "record, 1 field"
      "type Person = {name: String}"
      "type Person =\n  { name: String }"
      []
      []
      []
      false

    t
      "record, 2 fields"
      "type Person = {name: String; age: Int64}"
      "type Person =\n  { name: String\n    age: Int64 }"
      []
      []
      []
      false
    t
      "record, 3 fields"
      "type Person = {name: String; age: Int64; hasPet: Bool}"
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool }"
      []
      []
      []
      false
    t
      "record, 4 fields"
      "type Person = {name: String; age: Int64; hasPet: Bool; pet: Pet}"
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool\n    pet: Pet }"
      []
      []
      []
      false
    t
      "record, newlines as separators"
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool\n    pet: Pet }"
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool\n    pet: Pet }"
      []
      []
      []
      false
    t
      "record, newlines as separators 2"
      """type Person =
  { name: String
    age: Int64
    hasPet: Bool
    pet: Pet }"""
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool\n    pet: Pet }"
      []
      []
      []
      false

    // Enum type decls
    t
      "enum, no fields"
      "type Color = | Red | Green | Blue"
      "type Color =\n  | Red\n  | Green\n  | Blue"
      []
      []
      []
      false
    t
      "enum, one field"
      "type MyEnum = | A of Int64"
      "type MyEnum =\n  | A of Int64"
      []
      []
      []
      false
    t
      "enum, 2 tuple field"
      "type MyEnum = | A of Int64 * Int64"
      "type MyEnum =\n  | A of Int64 * Int64"
      []
      []
      []
      false
    t
      "enum, 3 tuple field"
      "type MyEnum = | A of Int64 * Bool * String | B of Int64"
      "type MyEnum =\n  | A of Int64 * Bool * String\n  | B of Int64"
      []
      []
      []
      false
    t
      "enum, named tuple fields"
      "type MyEnum = | A of x:Int64 * y:Int64"
      "type MyEnum =\n  | A of x: Int64 * y: Int64"
      []
      []
      []
      false
    t
      "enum, mult lines"
      "type Color =\n  | Red\n  | Green\n  | Blue"
      "type Color =\n  | Red\n  | Green\n  | Blue"
      []
      []
      []
      false
    t
      "enum, mult lines with field"
      "type MyEnum =\n  | A of Int64\n  | B of String"
      "type MyEnum =\n  | A of Int64\n  | B of String"
      []
      []
      []
      false
    t
      "enum, mult lines with named fields"
      "type MyEnum =\n  | A of x: Int64\n  | B of y: String"
      "type MyEnum =\n  | A of x: Int64\n  | B of y: String"
      []
      []
      []
      false
    t
      "enum, mult lines with named tuple field"
      "type MyEnum =\n  | A of x: Int64 * y: Int64\n  | B of z: String"
      "type MyEnum =\n  | A of x: Int64 * y: Int64\n  | B of z: String"
      []
      []
      []
      false ]
  |> testList "type declarations"

let exprs =
  [
    // units
    t "unit literal" "()" "()" [] [] [] false

    // bools
    t "true literal" "true" "true" [] [] [] false
    t "false literal" "false" "false" [] [] [] false

    // parens (disappear)
    t "parens, basic" "(true)" "true" [] [] [] false

    // int literals
    t "int literal 1y" "1y" "1y" [] [] [] false
    t "int literal -1y" "-1y" "-1y" [] [] [] false
    t "int literal 1uy" "1uy" "1uy" [] [] [] false
    t "int literal 1s" "1s" "1s" [] [] [] false
    t "int literal 1us" "1us" "1us" [] [] [] false
    t "int literal 1l" "1l" "1l" [] [] [] false
    t "int literal -1l" "-1l" "-1l" [] [] [] false
    t "int literal 1ul" "1ul" "1ul" [] [] [] false
    t "int literal 0L" "0L" "0L" [] [] [] false
    t "int literal 1900L" "1900L" "1900L" [] [] [] false
    t "int literal -1900L" "-1900L" "-1900L" [] [] [] false
    t "int literal 1UL" "1UL" "1UL" [] [] [] false
    t "int literal 1Q" "1Q" "1Q" [] [] [] false
    t "int literal -1Q" "-1Q" "-1Q" [] [] [] false
    t "int literal 1Q" "1Q" "1Q" [] [] [] false

    // float literals
    t "float literal -1.0" "-1.0" "-1.0" [] [] [] false
    t "float literal -1.5" "-1.5" "-1.5" [] [] [] false
    t "float literal 1.5" "1.5" "1.5" [] [] [] false
    t "float literal 0.0" "0.0" "0.0" [] [] [] false
    t "float literal 0.775" "0.775" "0.775" [] [] [] false

    // string literals
    t "empty string" "\"\"" "\"\"" [] [] [] false
    t "hello" "\"hello\"" "\"hello\"" [] [] [] false
    t "hello tab world" "\"hello\\tworld\"" "\"hello\\tworld\"" [] [] [] false

    // char literals
    t "the letter a" "'a'" "'a'" [] [] [] false
    t "a newline char" "'\\n'" "'\\n'" [] [] [] false
    t "a tab char" "'\t'" "'\t'" [] [] [] false

    // list literal
    t "empty list" "[]" "[]" [] [] [] false
    t "string list" "[\"hello\"]" "[\"hello\"]" [] [] [] false
    t "int list 2" "[1L; 2L]" "[1L; 2L]" [] [] [] false
    t "int list 3" "[1L; 2L; 3L;]" "[1L; 2L; 3L]" [] [] [] false
    t
      "bool list"
      "[true; false; true; false]"
      "[true; false; true; false]"
      []
      []
      []
      false
    t "int list list" "[[1L; 2L]; [3L; 4L]]" "[[1L; 2L]; [3L; 4L]]" [] [] [] false
    t
      "list with newline as a separator"
      "[ true\n false\n true ]"
      "[true; false; true]"
      []
      []
      []
      false

    // dict literal
    t "empty dict" "Dict { }" "Dict {  }" [] [] [] false
    t "simple int dict" "Dict { a = 1L }" "Dict { a = 1L }" [] [] [] false
    t
      "string dict"
      "Dict { a = \"hello\"; b = \"test\" }"
      "Dict { a = \"hello\"; b = \"test\" }"
      []
      []
      []
      false
    t
      "longer int dict"
      "Dict { a = 1L; b = 2L; c = 3L }"
      "Dict { a = 1L; b = 2L; c = 3L }"
      []
      []
      []
      false

    // tuple literals
    t "tuple 2" "(1L, \"hello\")" "(1L, \"hello\")" [] [] [] false
    t "tuple 3" "(1L, \"hello\", 2L)" "(1L, \"hello\", 2L)" [] [] [] false
    t
      "tuple 4"
      "(1L, \"hello\", 2L, true)"
      "(1L, \"hello\", 2L, true)"
      []
      []
      []
      false
    t "tuple with expr" "(1L, 2L + 3L, 4L)" "(1L, (2L) + (3L), 4L)" [] [] [] false // CLEANUP

    // record literals
    t
      "record, 1 field"
      "Person1 {name =\"John\"} "
      "Person1 { name = \"John\" }"
      []
      []
      []
      true
    t
      "record, 2 fields"
      "Person2 {name =\"John\"; age = 30L} "
      "Person2 { name = \"John\"; age = 30L }"
      []
      []
      []
      true
    t
      "record, 3 fields"
      "Person3 {name =\"John\"; age = 30L; hasPet = true} "
      "Person3 { name = \"John\"; age = 30L; hasPet = true }"
      [ person3 ]
      []
      []
      false

    t
      "record with newline as separator"
      "Person3\n {name =\"John\"\n age = 30L\n hasPet = true} "
      "Person3 { name = \"John\"; age = 30L; hasPet = true }"
      [ person3 ]
      []
      []
      false

    // record update
    t
      "record update 1"
      "{ RecordForUpdate { x = 4L; y = 1L } with y = 2L }"
      "{ RecordForUpdate { x = 4L; y = 1L } with y = 2L }"
      []
      []
      []
      true
    t "record update 2" "{ myRec with y = 2L }" "{ myRec with y = 2L }" [] [] [] true
    t
      "record update 3"
      "{ myRec with y = 2L; z = 42L }"
      "{ myRec with y = 2L; z = 42L }"
      []
      []
      []
      false

    // enum literal
    t "simple enum literal" "Color.Red" "Color.Red" [] [] [] false
    t "option none, short" "Stdlib.Option.None" "Stdlib.Option.None" [] [] [] false
    t
      "option none, long"
      "PACKAGE.Darklang.Stdlib.Option.Option.None"
      "PACKAGE.Darklang.Stdlib.Option.Option.None"
      []
      []
      []
      false
    t
      "option some"
      "PACKAGE.Darklang.Stdlib.Option.Option.Some 1L"
      "PACKAGE.Darklang.Stdlib.Option.Option.Some(1L)"
      []
      []
      []
      false
    t
      "custom enum tupled params"
      "MyEnum.A(1L, 2L)"
      "MyEnum.A((1L, 2L))"
      []
      []
      []
      false
    t "custom enum fn params" "MyEnum.A 1L 2L" "MyEnum.A(1L, 2L)" [] [] [] false

    // qualified constant
    t
      "qualified constant"
      "Stdlib.List.empty"
      "PACKAGE.Darklang.Stdlib.List.empty"
      []
      []
      []
      false

    // variables and let bindings
    t
      "assumed var name"
      "assumedlyAVariableName"
      "assumedlyAVariableName"
      []
      []
      []
      false
    // TODO: this is ugly
    t "simple let expr" "let x = 1L\n  x" "let x =\n  1L\nx" [] [] [] false

    // field access
    t "field access 1" "person.name" "person.name" [] [] [] false
    t
      "field access 2"
      "(Person { name =\"Janice\" }).name"
      "(Person { name = \"Janice\" }).name"
      []
      []
      []
      false
    t
      "nested field access"
      "record.someField.anotherFieldInsideThat"
      "record.someField.anotherFieldInsideThat"
      []
      []
      []
      false
    t
      "field access in context"
      "person.age + 1L"
      "(person.age) + (1L)"
      []
      []
      []
      false

    // lambda
    t "simple lambda" "fun x -> x + 1L" "(fun x ->\n  (x) + (1L))" [] [] [] false
    t
      "lambda wrapped with parens"
      "(fun x -> x + 1L)"
      "(fun x ->\n  (x) + (1L))"
      []
      []
      []
      false
    t "lambda, 2 args" "fun x y -> x * y" "(fun x y ->\n  (x) * (y))" [] [] [] false
    t "lambda, unit arg" "fun () -> 1L" "(fun () ->\n  1L)" [] [] [] false
    t
      "lambda with notable body"
      "fun var -> (Stdlib.String.toUppercase (Stdlib.String.fromChar var))"
      "(fun var ->\n  PACKAGE.Darklang.Stdlib.String.toUppercase (PACKAGE.Darklang.Stdlib.String.fromChar var))"
      []
      []
      []
      false
    t
      "lambda with notable body 2"
      "fun (str1, str2) -> str1 ++ str2"
      "(fun (str1, str2) ->\n  (str1) ++ (str2))"
      []
      []
      []
      false


    // if expressions
    t "if, 1" "if true then 1L" "if true then\n  1L" [] [] [] false
    t
      "if, 2"
      "if true then 1L else 2L"
      "if true then\n  1L\nelse\n  2L"
      []
      []
      []
      false
    t
      "if, 3"
      "if a < b then 1L else if c > d then 2L"
      "if (a) < (b) then\n  1L\nelse if (c) > (d) then\n  2L"
      []
      []
      []
      false
    t
      "if, 4"
      "if a < b then 1L else if c > d then 2L else 3L"
      "if (a) < (b) then\n  1L\nelse if (c) > (d) then\n  2L\nelse\n  3L"
      []
      []
      []
      false

    t "if, 5" "if true then\n 1L" "if true then\n  1L" [] [] [] false
    t
      "if, 6"
      "if true then\n 1L\nelse\n 2L"
      "if true then\n  1L\nelse\n  2L"
      []
      []
      []
      false
    t
      "if, 7"
      "if true then\n a\nelse if false then\n c"
      "if true then\n  a\nelse if false then\n  c"
      []
      []
      []
      false

    t
      "if, 8"
      "if a > b then\n a\nelse if c > d then\n c\nelse d"
      "if (a) > (b) then\n  a\nelse if (c) > (d) then\n  c\nelse\n  d"
      []
      []
      []
      false

    t
      "if, 9"
      "if true then\n\ta\nelse\n\tb"
      "if true then\n  a\nelse\n  b"
      []
      []
      []
      false

    t
      "if, many branches"
      """if true then
  a
else if false then
  c
else if true then
  d"""
      """if true then
  a
else if false then
  c
else if true then
  d"""
      []
      []
      []
      false

    t
      "else for inner if"
      """
if a > b then
  if c > d then
    c
  else
    b"""
      """if (a) > (b) then
  if (c) > (d) then
    c
  else
    b"""
      []
      []
      []
      false

    t
      "else for outer if"
      """
if a > b then
  if c > d then
    c
else
  b"""
      """if (a) > (b) then
  if (c) > (d) then
    c
else
  b"""
      []
      []
      []
      false

    t
      "nested if"
      """if a > b then
  a
else
  if c > d then
    c
  else
    if e > f then
      e
    else
      if g > h then
        g
      else
        h"""
      """if (a) > (b) then
  a
else if (c) > (d) then
  c
else if (e) > (f) then
  e
else if (g) > (h) then
  g
else
  h"""
      []
      []
      []
      false


    // match expressions
    t
      "match, unit"
      "match () with\n| () -> true"
      "match () with\n| () ->\n  true"
      []
      []
      []
      false
    t
      "match, bool"
      "match true with\n| true -> true"
      "match true with\n| true ->\n  true"
      []
      []
      []
      false

    t
      "match, int 1y"
      "match 1y with\n| 1y -> true"
      "match 1y with\n| 1y ->\n  true"
      []
      []
      []
      false
    t
      "match, int -1y"
      "match -1y with\n| -1y -> true"
      "match -1y with\n| -1y ->\n  true"
      []
      []
      []
      false
    t
      "match, int 0uy"
      "match 0uy with\n| 0uy -> true"
      "match 0uy with\n| 0uy ->\n  true"
      []
      []
      []
      false
    t
      "match, int 1s"
      "match 1s with\n| 1s -> true"
      "match 1s with\n| 1s ->\n  true"
      []
      []
      []
      false
    t
      "match, int 2us"
      "match 2us with\n| 2us -> true"
      "match 2us with\n| 2us ->\n  true"
      []
      []
      []
      false
    t
      "match, int 3l"
      "match 3l with\n| 3l -> true"
      "match 3l with\n| 3l ->\n  true"
      []
      []
      []
      false
    t
      "match, int 5ul"
      "match 5ul with\n| 5ul -> true"
      "match 5ul with\n| 5ul ->\n  true"
      []
      []
      []
      false
    t
      "match, int 7L"
      "match 7L with\n| 7L -> true"
      "match 7L with\n| 7L ->\n  true"
      []
      []
      []
      false
    t
      "match, int 8UL"
      "match 8UL with\n| 8UL -> true"
      "match 8UL with\n| 8UL ->\n  true"
      []
      []
      []
      false
    t
      "match, int 9Q"
      "match 9Q with\n| 9Q -> true"
      "match 9Q with\n| 9Q ->\n  true"
      []
      []
      []
      false
    t
      "match, int 10Z"
      "match 10Z with\n| 10Z -> true"
      "match 10Z with\n| 10Z ->\n  true"
      []
      []
      []
      false
    t
      "match, float 0.9"
      "match 0.9 with\n| 0.9 -> true"
      "match 0.9 with\n| 0.9 ->\n  true"
      []
      []
      []
      false
    t
      "match, string"
      "match \"str\" with\n| \"str\" -> true"
      "match \"str\" with\n| \"str\" ->\n  true"
      []
      []
      []
      false
    t
      "match, char"
      "match 'c' with\n| 'c' -> true"
      "match 'c' with\n| 'c' ->\n  true"
      []
      []
      []
      false
    t
      "match, var"
      "match var with\n| var -> true"
      "match var with\n| var ->\n  true"
      []
      []
      []
      false
    t
      "match, str 2"
      "match \"str\" with\n| \"str\" -> true\n| \"other\" -> false"
      "match \"str\" with\n| \"str\" ->\n  true\n| \"other\" ->\n  false"
      []
      []
      []
      false
    t
      "match, int list 1"
      "match [1L; 2L] with\n| [1L; 2L] -> true"
      "match [1L; 2L] with\n| [1L; 2L] ->\n  true"
      []
      []
      []
      false
    t
      "match, int list 2"
      "match [1L; 2L; 3L] with\n| head :: tail ->\n \"pass\""
      "match [1L; 2L; 3L] with\n| head :: tail ->\n  \"pass\""
      []
      []
      []
      false
    t
      "match, int tuple"
      "match (1L, 2L) with\n| (1L, 2L) -> true"
      "match (1L, 2L) with\n| (1L, 2L) ->\n  true"
      []
      []
      []
      false
    t
      "match, enum"
      "match Stdlib.Result.Result.Ok 5L with\n| Ok 5L -> true\n| Error e -> false"
      "match PACKAGE.Darklang.Stdlib.Result.Result.Ok(5L) with\n| Ok 5L ->\n  true\n| Error e ->\n  false"
      []
      []
      []
      false
    t
      "match, string 3"
      "match \"str\" with\n| \"str\" when true -> true"
      "match \"str\" with\n| \"str\" when true ->\n  true"
      []
      []
      []
      false
    t
      "match, when simple"
      "match x with\n| y when y > 1L -> true\n| z when z < 1L -> false\n| w -> w"
      "match x with\n| y when (y) > (1L) ->\n  true\n| z when (z) < (1L) ->\n  false\n| w ->\n  w"
      []
      []
      []
      false
    t
      "match, ignored 1"
      "match true with\n| _ -> true"
      "match true with\n| _ ->\n  true"
      []
      []
      []
      false
    t
      "match, ignored 2"
      "match true with\n| _var -> true"
      "match true with\n| _var ->\n  true"
      []
      []
      []
      false


    // pipe expression
    t "pipe, infix" "1L |> (+) 2L" "1L\n|> (+) 2L" [] [] [] false
    t "pipe, into var" "1L |> x" "1L\n|> x" [] [] [] false
    t
      "pipe, into lambda"
      "1L |> (fun x -> x + 1L)"
      "1L\n|> fun x -> (x) + (1L)"
      []
      []
      []
      false
    t
      "pipe, into lambda, 2"
      "1L |> fun x -> x + 1L"
      "1L\n|> fun x -> (x) + (1L)"
      []
      []
      []
      false
    t
      "pipe, into enum"
      "3L |> Stdlib.Result.Result.Ok"
      "3L\n|> PACKAGE.Darklang.Stdlib.Result.Result.Ok"
      []
      []
      []
      false
    t
      "pipe, into enum 2"
      "33L |> MyEnum.A 21L"
      "33L\n|> MyEnum.A (21L)"
      []
      []
      []
      false
    t
      "pipe, into fn call"
      "1L |> Stdlib.Int64.add 2L"
      "1L\n|> PACKAGE.Darklang.Stdlib.Int64.add 2L"
      []
      []
      []
      false
    t
      "pipe, into fn call 2"
      "1L |> Stdlib.Int64.toString"
      "1L\n|> PACKAGE.Darklang.Stdlib.Int64.toString"
      []
      []
      []
      false
    t
      "pipe, into fn call 3"
      "\"true\" |> Builtin.jsonParse<Bool>"
      "\"true\"\n|> Builtin.jsonParse<Bool>"
      []
      []
      []
      false
    t
      "pipe, into fn call 4"
      "Stdlib.Int64.add 1L 2L |> Stdlib.Int64.add 1L"
      "PACKAGE.Darklang.Stdlib.Int64.add 1L 2L\n|> PACKAGE.Darklang.Stdlib.Int64.add 1L"
      []
      []
      []
      false
    t
      "pipe, into fn call 5"
      "[1L; 2L] |> Stdlib.List.last |> Builtin.unwrap"
      "[1L; 2L]\n|> PACKAGE.Darklang.Stdlib.List.last\n|> Builtin.unwrap"
      []
      []
      []
      false

    // fn calls
    // CLEANUP these are ugly
    t "fn call, add once" "1L + 2L" "(1L) + (2L)" [] [] [] false
    t "fn call, add twice" "1L + b + 3L" "((1L) + (b)) + (3L)" [] [] [] false
    t
      "fn call, add thrice"
      "1L + 2L * 3L - 4L"
      "((1L) + ((2L) * (3L))) - (4L)"
      []
      []
      []
      false
    t "fn call, >" "1L > 2L" "(1L) > (2L)" [] [] [] false
    t "fn call, >=" "1L >= 2L" "(1L) >= (2L)" [] [] [] false
    t "fn call, <" "1L < 2L" "(1L) < (2L)" [] [] [] false
    t "fn call, <=" "1L <= 2L" "(1L) <= (2L)" [] [] [] false
    t "fn call, ==" "1L == 2L" "(1L) == (2L)" [] [] [] false
    t "fn call, !=" "1L != 2L" "(1L) != (2L)" [] [] [] false
    t "fn call, ^" "1L ^ 2L" "(1L) ^ (2L)" [] [] [] false
    t "fn call, ++" "strVar ++ \"str\"" "(strVar) ++ (\"str\")" [] [] [] false
    t "fn call, &&" "true && false" "(true) && (false)" [] [] [] false
    t "fn call, ||" "true || false" "(true) || (false)" [] [] [] false
    t "fn call, and short" "and true false" "and true false" [] [] [] true
    t "fn call, and longer" "Bool.and true false" "Bool.and true false" [] [] [] true
    t
      "fn call, and longest"
      "Darklang.Stdlib.Bool.and true false"
      "PACKAGE.Darklang.Stdlib.Bool.and true false"
      []
      []
      []
      false
    t
      "fn call, and stdlib shortcut"
      "Stdlib.Bool.and true false"
      "PACKAGE.Darklang.Stdlib.Bool.and true false"
      []
      []
      []
      false
    t
      "fn call, builtin simple"
      "Builtin.int64Add 1L 2L"
      "Builtin.int64Add 1L 2L"
      []
      []
      []
      false
    t
      "fn call, builtin with type arg"
      "Builtin.jsonParse<Bool> \"true\""
      "Builtin.jsonParse<Bool> \"true\""
      []
      []
      []
      false ]
  |> testList "exprs"


let constantDeclarations =
  [ t "unit" "const unitConst = ()" "const unitConst = ()" [] [] [] false

    // ints
    t "int8, max" "const maxInt8 = 127y" "const maxInt8 = 127y" [] [] [] false
    t "uint8, max" "const maxUInt8 = 255uy" "const maxUInt8 = 255uy" [] [] [] false
    t "int16, max" "const maxInt16 = 32767s" "const maxInt16 = 32767s" [] [] [] false
    t
      "uint16, max"
      "const maxUInt16 = 65535us"
      "const maxUInt16 = 65535us"
      []
      []
      []
      false
    t
      "int32, max"
      "const maxInt32 = 2147483647l"
      "const maxInt32 = 2147483647l"
      []
      []
      []
      false
    t
      "uint32, max"
      "const maxUInt32 = 4294967295ul"
      "const maxUInt32 = 4294967295ul"
      []
      []
      []
      false
    t
      "int64, max"
      "const maxInt64 = 9223372036854775807L"
      "const maxInt64 = 9223372036854775807L"
      []
      []
      []
      false
    t
      "uint64, max"
      "const maxUInt64 = 18446744073709551615UL"
      "const maxUInt64 = 18446744073709551615UL"
      []
      []
      []
      false
    t
      "int128, max"
      "const maxInt128 = 170141183460469231731687303715884105727Q"
      "const maxInt128 = 170141183460469231731687303715884105727Q"
      []
      []
      []
      false
    t
      "uint128, max"
      "const maxUInt128 = 340282366920938463463374607431768211455Z"
      "const maxUInt128 = 340282366920938463463374607431768211455Z"
      []
      []
      []
      false

    // bools
    t "true alias" "const trueConst = true" "const trueConst = true" [] [] [] false
    t
      "false alias"
      "const falseConst = false"
      "const falseConst = false"
      []
      []
      []
      false

    // strings
    t
      "hello"
      "const greeting = \"hello\""
      "const greeting = \"hello\""
      []
      []
      []
      false
    t "newline" "const newline = '\\n'" "const newline = '\\n'" [] [] [] false

    // floats
    t "pi" "const pi = 3.14159" "const pi = 3.14159" [] [] [] false

    // dicts
    t
      "dict, empty"
      "const emptyDict = Dict {}"
      "const emptyDict = Dict {  }"
      []
      []
      []
      false
    t
      "dict, one entry"
      "const dict = Dict { a = 1L }"
      "const dict = Dict { a = 1L }"
      []
      []
      []
      false
    t
      "dict, two entries"
      "const dict = Dict { a = \"hello\"; b = \"test\" }"
      "const dict = Dict { a = \"hello\"; b = \"test\" }"
      []
      []
      []
      false

    // tuples
    t
      "tuple, 2"
      "const tuple2Const = (1L, 2L)"
      "const tuple2Const = (1L, 2L)"
      []
      []
      []
      false
    t
      "tuple, 3"
      "const tuple3Const = (1L, 2L, 3L)"
      "const tuple3Const = (1L, 2L, 3L)"
      []
      []
      []
      false

    // lists
    t "list, empty" "const emptyList = []" "const emptyList = []" [] [] [] false
    t
      "list, int"
      "const listOfInts = [1L; 2L; 3L]"
      "const listOfInts = [1L; 2L; 3L]"
      []
      []
      []
      false
    t
      "list, list, int"
      "const listOfLists = [[1L; 2L]; [3L; 4L]]"
      "const listOfLists = [[1L; 2L]; [3L; 4L]]"
      []
      []
      []
      false

    // enums
    t
      "option, none"
      "const none = Stdlib.Option.Option.None"
      "const none = PACKAGE.Darklang.Stdlib.Option.Option.None"
      []
      []
      []
      false
    t
      "option, some 1"
      "const some = Stdlib.Option.Option.Some 1L"
      "const some = PACKAGE.Darklang.Stdlib.Option.Option.Some(1L)"
      []
      []
      []
      false
    t
      "enum, tupled args"
      "const a = MyEnum.A(1L, 2L)"
      "const a = MyEnum.A((1L, 2L))"
      []
      []
      []
      false
    t
      "enum, fn args"
      "const a = MyEnum.A 1L 2L"
      "const a = MyEnum.A(1L, 2L)"
      []
      []
      []
      false ]
  |> testList "constant declarations"

let functionDeclarations =
  [ t
      "single builtin param"
      "let helloWorld (i: Int64): String = \"Hello world\""
      "let helloWorld (i: Int64): String =\n  \"Hello world\""
      []
      []
      []
      false

    t
      "single package param"
      "let double2 (i: PACKAGE.Darklang.LanguageTools.ID) : Int64 = i + i"
      "let double2 (i: PACKAGE.Darklang.LanguageTools.ID): Int64 =\n  (i) + (i)"
      []
      []
      []
      false

    t
      "single unit param"
      "let emptyString () : String = \"\""
      "let emptyString (_: Unit): String =\n  \"\""
      []
      []
      []
      false

    t
      "multiple param"
      "let isHigher (a: Int64) (b: Int64) : Bool = Stdlib.Int64.greaterThan a b"
      "let isHigher (a: Int64) (b: Int64): Bool =\n  PACKAGE.Darklang.Stdlib.Int64.greaterThan a b"
      []
      []
      []
      false

    t
      "single type param"
      "let myFn<'a> (param: 'a): Unit  = ()"
      "let myFn<'a> (param: 'a): Unit =\n  ()"
      []
      []
      []
      false

    t
      "two type params"
      "let myFn<'a, 'b> (paramOne: 'a) (paramTwo: 'b): Unit  = ()"
      "let myFn<'a, 'b> (paramOne: 'a) (paramTwo: 'b): Unit =\n  ()"
      []
      []
      []
      false

    t
      "package fn call"
      "let sum (a : Int64) (b : Int64) : Int64 = PACKAGE.Darklang.Stdlib.Int64.add a b"
      "let sum (a: Int64) (b: Int64): Int64 =\n  PACKAGE.Darklang.Stdlib.Int64.add a b"
      []
      []
      []
      false ]
  |> testList "function declarations"

let moduleDeclarations =
  [ t
      "simple module"
      "module MyModule =\n  type ID = Int64"
      "module MyModule =\n  type ID =\n    Int64"
      []
      []
      []
      false

    t
      "module with types, fns, and consts"
      """module MyModule =
  type ID = Int64
  type MyString = String
  let myFn (i: Int64): Int64 = 1L
  const x = 100L"""
      "module MyModule =\n  type ID =\n    Int64\n\n  type MyString =\n    String\n\n  let myFn (i: Int64): Int64 =\n    1L\n\n  const x = 100L"
      []
      []
      []
      false

    t
      "module with types, fns, conts, and newlines"
      """module MyModule =
  type ID = Int64

  type MyString = String

  let myFn (i: Int64): Int64 = 1L

  const x = 100L"""
      "module MyModule =\n  type ID =\n    Int64\n\n  type MyString =\n    String\n\n  let myFn (i: Int64): Int64 =\n    1L\n\n  const x = 100L"
      []
      []
      []
      false

    t
      "nested module declaration"
      """module MyModule1 =
  type ID = Int64
  module MyModule2 =
    type ID = Int64
    module MyModule3 =
      type ID = Int64
      const x = 100L
      1L"""
      "module MyModule1 =\n  type ID =\n    Int64\n\n  module MyModule2 =\n    type ID =\n      Int64\n\n    module MyModule3 =\n      type ID =\n        Int64\n\n      const x = 100L\n\n      1L"
      []
      []
      []
      false ]
  |> testList "module declarations"


let sourceFiles =
  [
    // CLEANUP the output here is a bit broken
    t
      "simple script"
      "
type BookID = Int64

let getTitle (bookId: BookID): String =
  let book = Library.getBook bookId
  getNameFromBook book

let curiousGeorgeBookId = 101L
Builtin.printLine (getTitle curiousGeorgeBookId)

0L
  "
      "type BookID =\n  Int64

let getTitle (bookId: BookID): String =
  let book =\n    Library.getBook bookId
  getNameFromBook book

let curiousGeorgeBookId =\n  101L
Builtin.printLine (getTitle curiousGeorgeBookId)

0L"
      []
      []
      []
      false ]
  |> testList "cli scripts"

let tests =
  testList
    "NewParser"
    [ typeReferences
      typeDeclarations
      constantDeclarations
      exprs
      functionDeclarations
      moduleDeclarations
      sourceFiles ]
