/// Tests the new tree-sitter-based parser.
///
/// Currently just focused on round-tripping:
///   source (input)
///   -> parsed TreeSitter tree
///   -> parsed CLI Script
///   -> PT.SourceFile
///   -> pretty-print back to text (expected)
module Tests.NewParser

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs

// Test package type definitions as tuples with locations


let t
  (name : string)
  (input : string)
  (expected : string)
  (extraTypes : List<PT.PackageType.PackageType * PT.PackageLocation>)
  (extraValues : List<PT.PackageValue.PackageValue * PT.PackageLocation>)
  (extraFns : List<PT.PackageFn.PackageFn * PT.PackageLocation>)
  (allowUnresolved : bool)
  =
  let parseFnName =
    RT.FQFnName.FQFnName.Package
      PackageIDs.Fn.LanguageTools.Parser.parsePTSourceFileWithOps

  let prettyPrintFnName =
    RT.FQFnName.FQFnName.Package PackageIDs.Fn.PrettyPrinter.ProgramTypes.sourceFile

  testTask name {
    // First phase: parse with base PM to get PackageOps
    let basePM =
      if allowUnresolved then
        pmPT
      else
        pmPT |> PT.PackageManager.withExtras extraTypes extraValues extraFns
    let canvasID = System.Guid.NewGuid()
    let! parseExeState = executionStateFor basePM canvasID false false Map.empty

    let args = NEList.singleton (RT.DString input)
    let! parseResult =
      LibExecution.Execution.executeFunction parseExeState parseFnName [] args
    let! parseDval = unwrapExecutionResult parseExeState parseResult |> Ply.toTask

    match parseDval with
    | RT.DEnum(tn, _, _, "Ok", [ RT.DTuple(sourceFile, opsList, []) ]) when
      tn = Dval.resultType
      ->
      // Extract PackageOps from the Dval list
      let packageOps =
        match opsList with
        | RT.DList(_vt, ops) ->
          ops |> List.choose LibExecution.ProgramTypesToDarkTypes.PackageOp.fromDT
        | _ -> []

      // Second phase: enhance PM with PackageOps and pretty print
      let enhancedPM =
        LibPackageManager.PackageManager.withExtraOps basePM packageOps
      let! ppExeState = executionStateFor enhancedPM canvasID false false Map.empty

      let ppArgs = NEList.singleton sourceFile
      let! ppResult =
        LibExecution.Execution.executeFunction ppExeState prettyPrintFnName [] ppArgs
      let! resultDval = unwrapExecutionResult ppExeState ppResult |> Ply.toTask

      match resultDval with
      | RT.DString result ->
        return
          Expect.RT.equalDval
            (RT.DString result)
            (RT.DString expected)
            "Didn't round-trip as expected"
      | _ -> return failtest $"Unexpected pretty print result: {resultDval}"

    | RT.DEnum(tn, _, _, "Error", [ RT.DString errMsg ]) when tn = Dval.resultType ->
      return failtest $"Parse error: {errMsg}"
    | _ -> return failtest $"Unexpected parse result format: {parseDval}"
  }


let person : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { id = System.Guid.NewGuid()
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
                    { name = "hasPet"
                      typ = PT.TypeReference.TBool
                      description = "" } ] }
            ) } }
  let location : PT.PackageLocation =
    { owner = "Tests"; modules = []; name = "Person" }
  (packageType, location)

let myString : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { id = System.Guid.NewGuid()
      description = ""
      deprecated = PT.NotDeprecated
      declaration =
        { typeParams = []
          definition = PT.TypeDeclaration.Alias PT.TypeReference.TString } }
  let location : PT.PackageLocation =
    { owner = "Tests"; modules = []; name = "MyString" }
  (packageType, location)

let pet : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { id = System.Guid.NewGuid()
      description = ""
      deprecated = PT.NotDeprecated
      declaration =
        { typeParams = []
          definition = PT.TypeDeclaration.Alias PT.TypeReference.TString } }
  let location : PT.PackageLocation = { owner = "Tests"; modules = []; name = "Pet" }
  (packageType, location)

let myEnum : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { id = System.Guid.NewGuid()
      description = ""
      deprecated = PT.NotDeprecated
      declaration =
        { typeParams = []
          definition =
            PT.TypeDeclaration.Enum(
              NEList.ofList
                ({ name = "A"; fields = []; description = "" })
                [ ({ name = "B"
                     fields =
                       [ { typ = PT.TypeReference.TInt64
                           label = None
                           description = "" } ]
                     description = "" })
                  ({ name = "C"
                     fields =
                       [ { typ =
                             PT.TypeReference.TTuple(
                               PT.TypeReference.TInt64,
                               PT.TypeReference.TInt64,
                               []
                             )
                           label = None
                           description = "" } ]
                     description = "" })
                  ({ name = "D"
                     fields =
                       [ { typ = PT.TypeReference.TInt64
                           label = None
                           description = "" }

                         { typ = PT.TypeReference.TInt64
                           label = None
                           description = "" } ]
                     description = "" }) ]
            ) } }
  let location : PT.PackageLocation =
    { owner = "Tests"; modules = []; name = "MyEnum" }
  (packageType, location)

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
      [ myString ]
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
      "type MyDict = Dict<MyString>"
      "type MyDict =\n  Dict<MyString>"
      [ myString ]
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
      "type MyFn = LanguageTools.ID -> 'a -> 'b"
      "type MyFn =\n  LanguageTools.ID -> 'a -> 'b"
      []
      []
      []
      false

    t "db with generic" "type MyDB = DB<'a>" "type MyDB =\n  DB<'a>" [] [] [] false
    t
      "db with custom type"
      "type MyDB = DB<Person>"
      "type MyDB =\n  DB<Person>"
      [ person ]
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
      "type ID = Person"
      "type ID =\n  Person"
      [ person ]
      []
      []
      false


    // fully-qualified package name (multi-part)
    t
      "option alias, shortcut name"
      "type MyOption = Stdlib.Option.Option"
      "type MyOption =\n  Stdlib.Option.Option"
      []
      []
      []
      false
    t
      "option alias, unapplied"
      "type MyOption = Stdlib.Option.Option"
      "type MyOption =\n  Stdlib.Option.Option"
      []
      []
      []
      false
    t
      "option alias, applied"
      "type MyOption = Stdlib.Option.Option<Int64>"
      "type MyOption =\n  Stdlib.Option.Option<Int64>"
      []
      []
      []
      false

    t
      "json alias"
      "type Json =\n  Stdlib.AltJson.Json"
      "type Json =\n  Stdlib.AltJson.Json"
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

    // Record type decls
    t
      "type param record"
      "type Generic<'a> = { x: 'a }"
      "type Generic<'a> =\n  { x: 'a }"
      []
      []
      []
      false

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
      [ pet ]
      []
      []
      false
    t
      "record, newlines as separators"
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool\n    pet: Pet }"
      "type Person =\n  { name: String\n    age: Int64\n    hasPet: Bool\n    pet: Pet }"
      [ pet ]
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
      [ pet ]
      []
      []
      false

    // // Enum type TODO
    // ("type Thing = | A | B of Int64 | C of String * Bool"  "type Thing =\n  | A\n  | B of Int64\n  | C of String * Bool"
    // Enum type decls
    t
      "type param, enum"
      "type MyEnum<'a> = | A | B of 'a"
      "type MyEnum<'a> =\n  | A\n  | B of 'a"
      []
      []
      []
      false

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
    t "egc" "\"👩‍👩‍👧‍👦\"" "\"👩‍👩‍👧‍👦\"" [] [] [] false
    t "unicode" "\"żółw\"" "\"żółw\"" [] [] [] false
    t "string interpolation" "$\"hello {name}\"" "$\"hello {name}\"" [] [] [] false
    t
      "string interpolation - multiple expr to eval"
      "$\"Name: {name}, Age: {age}\""
      "$\"Name: {name}, Age: {age}\""
      []
      []
      []
      false
    t
      "multiline string "
      "\"\"\"int64Multiply's 2nd argument (`b`) should be an Int64. However, a Float (1.0) was passed instead.



    Expected: (b: Int64)

    Actual: a Float: 1.0\"\"\""

      "\"int64Multiply's 2nd argument (`b`) should be an Int64. However, a Float (1.0) was passed instead.\\n\\n\\n\\n    Expected: (b: Int64)\\n\\n    Actual: a Float: 1.0\""
      []
      []
      []
      false

    t
      "multiline string - interpolated"
      "$\"\"\"test {\"1\"}\"\"\" == \"test 1\""
      "($\"test {\"1\"}\") == (\"test 1\")"
      []
      []
      []
      false

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

    t
      "list of function calls"
      """[
  Stdlib.Tuple2.second (4L, 5L)
  Stdlib.Int64.add 1L 2L
  Stdlib.List.head [1L; 2L]
]"""
      "[Stdlib.Tuple2.second (4L, 5L); Stdlib.Int64.add 1L 2L; Stdlib.List.head [1L; 2L]]"
      []
      []
      []
      false

    t
      "list of function calls -indented"
      """[
  Stdlib.Tuple2.second (4L, 5L)
  (Stdlib.Int64.add
    1L
    2L)
  Stdlib.List.head [1L; 2L]
]"""
      "[Stdlib.Tuple2.second (4L, 5L); Stdlib.Int64.add 1L 2L; Stdlib.List.head [1L; 2L]]"
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
    t
      "dict with double_backtick_identifier"
      "Dict { ``Content-Length`` = 1L }"
      "Dict { Content-Length = 1L }"
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
      "Tests.Person {name =\"John\"; age = 30L; hasPet = true} "
      "Person { name = \"John\"; age = 30L; hasPet = true }"
      [ person ]
      []
      []
      false

    t
      "record with newline as separator"
      "Tests.Person\n {name =\"John\"\n age = 30L\n hasPet = true} "
      "Person { name = \"John\"; age = 30L; hasPet = true }"
      [ person ]
      []
      []
      false

    // record with type args
    t
      "record with type args"
      "Generic<Tests.Person> { x = Person { name = \"John\"; age = 30L; hasPet = true } }"
      "Generic<Person> { x = Person { name = \"John\"; age = 30L; hasPet = true } }"
      [ person ]
      []
      []
      false

    // record update
    t
      "record update 1"
      "{ Tests.Person { name = \"John\"; age = 30L; hasPet = true } with age = 31L }"
      "{ Person { name = \"John\"; age = 30L; hasPet = true } with age = 31L }"
      [ person ]
      []
      []
      false
    t
      "record update 2"
      "{ person with age = 31L }"
      "{ person with age = 31L }"
      [ person ]
      []
      []
      false
    t
      "record update 3"
      "{ person with age = 31L; hasPet = false }"
      "{ person with age = 31L; hasPet = false }"
      [ person ]
      []
      []
      false
    t
      "record update 4"
      """(let myRec = Tests.Person { name = "John"; age = 30L; hasPet = true }
  { myRec with
      name = "Jane"
      age = 31L
      hasPet = false })"""
      """let myRec =
  Person { name = "John"; age = 30L; hasPet = true }
{ myRec with name = "Jane"; age = 31L; hasPet = false }"""
      [ person ]
      []
      []
      false

    // enum literal
    t "simple enum literal" "Tests.MyEnum.A" "MyEnum.A" [ myEnum ] [] [] false
    t
      "enum with type args"
      "Generic<Int64>.A 1L"
      "Generic<Int64>.A(1L)"
      []
      []
      []
      false
    t
      "option none, short"
      "Stdlib.Option.Option.None"
      "Stdlib.Option.Option.None"
      []
      []
      []
      false
    t
      "option none, long"
      "Stdlib.Option.Option.None"
      "Stdlib.Option.Option.None"
      []
      []
      []
      false
    t
      "option some"
      "Stdlib.Option.Option.Some 1L"
      "Stdlib.Option.Option.Some(1L)"
      []
      []
      []
      false
    t
      "custom enum tupled params"
      "Tests.MyEnum.C((1L, 2L))"
      "MyEnum.C((1L, 2L))"
      [ myEnum ]
      []
      []
      false
    t
      "custom enum fn params"
      "Tests.MyEnum.D(1L, 2L)"
      "MyEnum.D(1L, 2L)"
      [ myEnum ]
      []
      []
      false
    t "custom enum fn params" "MyEnum.D(1L, 2L)" "MyEnum.D(1L, 2L)" [] [] [] false
    t
      "custom enum indexed params"
      """Tests.MyEnum.D(
  1L,
  2L
)"""
      """MyEnum.D(1L, 2L)"""
      [ myEnum ]
      []
      []
      false

    t
      "enum with indented field"
      """Stdlib.Result.Result.Error
  Stdlib.List.ChunkBySizeError.SizeMustBeGreaterThanZero"""
      """Stdlib.Result.Result.Error(Stdlib.List.ChunkBySizeError.SizeMustBeGreaterThanZero)"""
      []
      []
      []
      false


    // qualified value
    t "qualified value" "Stdlib.List.empty" "Stdlib.List.empty" [] [] [] false

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
    t "let expr with indent" "let x =\n  1L\nx" "let x =\n  1L\nx" [] [] [] false

    t
      "tuple destructuring"
      "let (var1, var2) =\n  var3\n(var1, var2)"
      "let (var1, var2) =\n  var3\n(var1, var2)"
      []
      []
      []
      false

    t
      "tuple destructuring 2"
      "let (var1, var2) =\n  (var3, var4)\n(var1, var2)"
      "let (var1, var2) =\n  (var3, var4)\n(var1, var2)"
      []
      []
      []
      false

    // field access
    t "field access 1" "person.name" "person.name" [] [] [] false
    t
      "field access 2"
      "(Tests.Person { name =\"Janice\" }).name"
      "(Person { name = \"Janice\" }).name"
      [ person ]
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
      "(fun var ->\n  Stdlib.String.toUppercase (Stdlib.String.fromChar var))"
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
      "match Stdlib.Result.Result.Ok 5L with\n| Ok(5L) -> true\n| Error(e) -> false"
      "match Stdlib.Result.Result.Ok(5L) with\n| Ok(5L) ->\n  true\n| Error(e) ->\n  false"
      []
      []
      []
      false
    t
      "match, enum no parens with one arg"
      "match Stdlib.Result.Result.Ok 5L with\n| Ok 5L -> true\n| Error e  -> false"
      "match Stdlib.Result.Result.Ok(5L) with\n| Ok(5L) ->\n  true\n| Error(e) ->\n  false"
      []
      []
      []
      false
    t
      "match, enum with 2 args"
      "match MyEnum.D(5L, 3L) with\n| D(5L, 3L) -> true\n| _  -> false"
      "match MyEnum.D(5L, 3L) with\n| D(5L, 3L) ->\n  true\n| _ ->\n  false"
      []
      []
      []
      false
    t
      "match, enum no parens with 1 tuple arg"
      "match Stdlib.Result.Result.Ok((5L, 3L)) with\n| Ok((5L, 3L)) -> true\n| Error e  -> false"
      "match Stdlib.Result.Result.Ok((5L, 3L)) with\n| Ok((5L, 3L)) ->\n  true\n| Error(e) ->\n  false"
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

    t
      "match, multiple patterns"
      "match (1L, 2L) with\n| (1L, 2L) | (2L, 1L) -> true\n| _ -> false"
      "match (1L, 2L) with\n| (1L, 2L) | (2L, 1L) ->\n  true\n| _ ->\n  false"
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
      "3L\n|> Stdlib.Result.Result.Ok"
      []
      []
      []
      false
    t
      "pipe, into enum 2"
      "33L |> Tests.MyEnum.D(21L)"
      "33L\n|> MyEnum.D(21L)"
      [ myEnum ]
      []
      []
      false
    t
      "pipe, into fn call"
      "1L |> Stdlib.Int64.add 2L"
      "1L\n|> Stdlib.Int64.add 2L"
      []
      []
      []
      false
    t
      "pipe, into fn call 2"
      "1L |> Stdlib.Int64.toString"
      "1L\n|> Stdlib.Int64.toString"
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
      "Stdlib.Int64.add 1L 2L\n|> Stdlib.Int64.add 1L"
      []
      []
      []
      false
    t
      "pipe, into fn call 5"
      "[1L; 2L] |> Stdlib.List.last |> Builtin.unwrap"
      "[1L; 2L]\n|> Stdlib.List.last\n|> Builtin.unwrap"
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
    t
      "fn call, and longer"
      "Stdlib.Bool.and true false"
      "Stdlib.Bool.and true false"
      []
      []
      []
      true
    t
      "fn call, and longest"
      "Darklang.Stdlib.Bool.and true false"
      "Stdlib.Bool.and true false"
      []
      []
      []
      false
    t
      "fn call, and stdlib shortcut"
      "Stdlib.Bool.and true false"
      "Stdlib.Bool.and true false"
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
      false
    t
      "fn call with indentation"
      """Stdlib.Tuple3.mapAllThree
  (fun x -> Stdlib.String.toUppercase x)
  (fun x -> x - 2L)
  (fun x -> Stdlib.String.toUppercase x)
  ("one", 2L, "pi")
"""
      """Stdlib.Tuple3.mapAllThree (fun x ->
  Stdlib.String.toUppercase x) (fun x ->
  (x) - (2L)) (fun x ->
  Stdlib.String.toUppercase x) ("one", 2L, "pi")"""
      []
      []
      []
      false
    t
      "fn call with db reference"
      """Stdlib.DB.set Tests.Person { name = "John"; age = 30L; hasPet = true } "key" TestDB"""
      """Stdlib.DB.set Person { name = "John"; age = 30L; hasPet = true } "key" TestDB"""
      [ person ]
      []
      []
      false ]
  |> testList "exprs"


let valueDeclarations =
  [ t "unit" "val unitVal = ()" "val unitVal = ()" [] [] [] false

    // ints
    t "int8, max" "val maxInt8 = 127y" "val maxInt8 = 127y" [] [] [] false
    t "uint8, max" "val maxUInt8 = 255uy" "val maxUInt8 = 255uy" [] [] [] false
    t "int16, max" "val maxInt16 = 32767s" "val maxInt16 = 32767s" [] [] [] false
    t
      "uint16, max"
      "val maxUInt16 = 65535us"
      "val maxUInt16 = 65535us"
      []
      []
      []
      false
    t
      "int32, max"
      "val maxInt32 = 2147483647l"
      "val maxInt32 = 2147483647l"
      []
      []
      []
      false
    t
      "uint32, max"
      "val maxUInt32 = 4294967295ul"
      "val maxUInt32 = 4294967295ul"
      []
      []
      []
      false
    t
      "int64, max"
      "val maxInt64 = 9223372036854775807L"
      "val maxInt64 = 9223372036854775807L"
      []
      []
      []
      false
    t
      "uint64, max"
      "val maxUInt64 = 18446744073709551615UL"
      "val maxUInt64 = 18446744073709551615UL"
      []
      []
      []
      false
    t
      "int128, max"
      "val maxInt128 = 170141183460469231731687303715884105727Q"
      "val maxInt128 = 170141183460469231731687303715884105727Q"
      []
      []
      []
      false
    t
      "uint128, max"
      "val maxUInt128 = 340282366920938463463374607431768211455Z"
      "val maxUInt128 = 340282366920938463463374607431768211455Z"
      []
      []
      []
      false

    // bools
    t "true alias" "val trueVal = true" "val trueVal = true" [] [] [] false
    t "false alias" "val falseVal = false" "val falseVal = false" [] [] [] false

    // strings
    t "hello" "val greeting = \"hello\"" "val greeting = \"hello\"" [] [] [] false
    t "newline" "val newline = '\\n'" "val newline = '\\n'" [] [] [] false

    // floats
    t "pi" "val pi = 3.14159" "val pi = 3.14159" [] [] [] false

    // dicts
    t
      "dict, empty"
      "val emptyDict = Dict {}"
      "val emptyDict = Dict {  }"
      []
      []
      []
      false
    t
      "dict, one entry"
      "val dict = Dict { a = 1L }"
      "val dict = Dict { a = 1L }"
      []
      []
      []
      false
    t
      "dict, two entries"
      "val dict = Dict { a = \"hello\"; b = \"test\" }"
      "val dict = Dict { a = \"hello\"; b = \"test\" }"
      []
      []
      []
      false

    // tuples
    t "tuple, 2" "val tuple2Val = (1L, 2L)" "val tuple2Val = (1L, 2L)" [] [] [] false
    t
      "tuple, 3"
      "val tuple3Val = (1L, 2L, 3L)"
      "val tuple3Val = (1L, 2L, 3L)"
      []
      []
      []
      false

    // lists
    t "list, empty" "val emptyList = []" "val emptyList = []" [] [] [] false
    t
      "list, int"
      "val listOfInts = [1L; 2L; 3L]"
      "val listOfInts = [1L; 2L; 3L]"
      []
      []
      []
      false
    t
      "list, list, int"
      "val listOfLists = [[1L; 2L]; [3L; 4L]]"
      "val listOfLists = [[1L; 2L]; [3L; 4L]]"
      []
      []
      []
      false

    // records
    t
      "record with fields"
      "val myPerson = Tests.Person { name = \"Alice\"; age = 30L; hasPet = true }"
      "val myPerson = Person { name = \"Alice\"; age = 30L; hasPet = true }"
      [ person ]
      []
      []
      false

    // enums
    t
      "option, none"
      "val none = Stdlib.Option.Option.None"
      "val none = Stdlib.Option.Option.None"
      []
      []
      []
      false
    t
      "option, some 1"
      "val some = Stdlib.Option.Option.Some(1L)"
      "val some = Stdlib.Option.Option.Some(1L)"
      []
      []
      []
      false
    t
      "enum, tupled args"
      "val a = MyEnum.C((1L, 2L))"
      "val a = MyEnum.C((1L, 2L))"
      [ myEnum ]
      []
      []
      false
    t
      "enum, fn args"
      "val a = MyEnum.D(1L, 2L)"
      "val a = MyEnum.D(1L, 2L)"
      [ myEnum ]
      []
      []
      false ]
  |> testList "value declarations"

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
      "let double2 (i: LanguageTools.ID) : Int64 = (i + i)"
      "let double2 (i: LanguageTools.ID): Int64 =\n  (i) + (i)"
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
      "let isHigher (a: Int64) (b: Int64) : Bool =\n  Stdlib.Int64.greaterThan a b"
      "let isHigher (a: Int64) (b: Int64): Bool =\n  Stdlib.Int64.greaterThan a b"
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
      "let myFn<'a, 'b> (paramOne: 'a) (paramTwo: 'b): Unit = ()"
      "let myFn<'a, 'b> (paramOne: 'a) (paramTwo: 'b): Unit =\n  ()"
      []
      []
      []
      false

    t
      "package fn call"
      "let sum (a : Int64) (b : Int64) : Int64 =\n  Stdlib.Int64.add a b"
      "let sum (a: Int64) (b: Int64): Int64 =\n  Stdlib.Int64.add a b"
      []
      []
      []
      false
    t
      "fn declaration with newline"
      """let myFn
  (a: String)
  (b: Int64)
  (c: Bool)
  : Bool =
  true"""
      "let myFn (a: String) (b: Int64) (c: Bool): Bool =\n  true"
      []
      []
      []
      false
    t
      "fn declaration with indented body"
      "let helloPerson (name: String): String =\n  let greeting = \"Hello \"\n  greeting ++ name"
      "let helloPerson (name: String): String =\n  let greeting =\n    \"Hello \"\n  (greeting) ++ (name)"
      []
      []
      []
      false

    t
      "self reference recursive call"
      """let factorial (n: Int64): Int64 =
  if n <= 1L then
    1L
  else
    n * (factorial (n - 1L))"""
      "let factorial (n: Int64): Int64 =\n  if (n) <= (1L) then\n    1L\n  else\n    (n) * (factorial (n) - (1L))"
      []
      []
      []
      false
    t
      "self reference, shadowed name"
      """let incr (y: Int64) (z: Int64): Int64 =
  if Stdlib.Int64.lessThanOrEqualTo z 0L then
    y
  else
    let result = incr y (Stdlib.Int64.subtract z 1L)
    let incr = (fun x -> Stdlib.Int64.add x 2L)
    let lambdaResult = incr z
    Stdlib.Int64.add result lambdaResult"""
      """let incr (y: Int64) (z: Int64): Int64 =
  if Stdlib.Int64.lessThanOrEqualTo z 0L then
    y
  else
    let result =
      incr y (Stdlib.Int64.subtract z 1L)
    let incr =
      (fun x ->
        Stdlib.Int64.add x 2L)
    let lambdaResult =
      incr z
    Stdlib.Int64.add result lambdaResult"""
      []
      []
      []
      false ]
  |> testList "function declarations"

let moduleDeclarations =
  [ t
      "simple module"
      "module MyModule =\n  type ID = Int64"
      "module Tests =\n  module MyModule =\n    type ID =\n      Int64"
      []
      []
      []
      false

    t
      "module with types, fns, and vals"
      """module MyModule =
  type ID = Int64
  type MyString = String
  let myFn (i: Int64): Int64 = 1L
  val x = 100L"""
      "module Tests =\n  module MyModule =\n    type ID =\n      Int64\n\n    type MyString =\n      String\n\n    let myFn (i: Int64): Int64 =\n      1L\n\n    val x = 100L"
      []
      []
      []
      false

    t
      "module with types, fns, vals, and newlines"
      """module MyModule =
  type ID = Int64

  type MyString = String

  let myFn (i: Int64): Int64 = 1L

  val x = 100L"""
      "module Tests =\n  module MyModule =\n    type ID =\n      Int64\n\n    type MyString =\n      String\n\n    let myFn (i: Int64): Int64 =\n      1L\n\n    val x = 100L"
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
      val x = 100L
      1L"""
      "module Tests =\n  module MyModule1 =\n    type ID =\n      Int64\n\n    module MyModule2 =\n      type ID =\n        Int64\n\n      module MyModule3 =\n        type ID =\n          Int64\n\n        val x = 100L\n\n        1L"
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
    let book = (Library.getBook bookId)
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
      valueDeclarations
      exprs
      functionDeclarations
      moduleDeclarations
      sourceFiles ]
