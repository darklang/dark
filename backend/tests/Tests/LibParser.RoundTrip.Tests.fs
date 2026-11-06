/// Parse-then-print round trips that have to run here rather than as package tests.
///
/// The round-trip cases live in `packages/darklang/tests/languageTools/roundTrip/`. These stay
/// because they need a package manager only F# can build: declarations injected with
/// `PT.PackageManager.withExtras`, under placeholder hashes the store never sees.
///
///  - **The expression cases refer to `Tests.Person` and `Tests.MyEnum`.** `Tests` is the owner the
///    parser stamps on a source file, and the printer drops it from a stored name as scaffolding, so
///    a package test, which has to store the type to print it, prints `Person`. With the type
///    injected here it prints as written.
///  - **The module cases declare `type ID = Int64`.** The store already holds that content under a
///    shorter name, and names are looked up by content hash, so a package test prints the module's
///    type under that other module. Here every declaration keeps a hash of its own.
module Tests.LibParserRoundTrip

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs


/// The parsed shape of a source file, for comparing two parses structurally.
module SourceFileAst =
  module PT2DT = LibExecution.ProgramTypesToDarkTypes
  module D = LibExecution.DvalDecoder

  type Definitions =
    { types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn>
      exprs : List<PT.Expr * List<string>> }

  type Declaration =
    | Type of PT.PackageType.PackageType
    | Value of PT.PackageValue.PackageValue
    | Function of PT.PackageFn.PackageFn
    | Test of PT.PackageTest.PackageTest
    | Module of Definitions

  type SourceFile = { declarations : List<Declaration>; exprsToEval : List<PT.Expr> }

  let rec definitionsOfDval (d : RT.Dval) : Definitions =
    match d with
    | RT.DRecord(_, _, _, fields) ->
      { types = fields |> D.field "types" |> D.list PT2DT.PackageType.fromDT
        values = fields |> D.field "values" |> D.list PT2DT.PackageValue.fromDT
        fns = fields |> D.field "fns" |> D.list PT2DT.PackageFn.fromDT
        exprs =
          fields
          |> D.field "exprs"
          |> D.list (fun d ->
            match d with
            | RT.DTuple(e, path, []) -> (PT2DT.Expr.fromDT e, D.list D.string path)
            | _ -> Exception.raiseInternal "Invalid Definitions.exprs entry" []) }
    | _ -> Exception.raiseInternal "Invalid Definitions" []

  and declarationOfDval (d : RT.Dval) : Declaration =
    match d with
    | RT.DEnum(_, _, _, "Type", [ t ]) -> Type(PT2DT.PackageType.fromDT t)
    | RT.DEnum(_, _, _, "Value", [ v ]) -> Value(PT2DT.PackageValue.fromDT v)
    | RT.DEnum(_, _, _, "Function", [ f ]) -> Function(PT2DT.PackageFn.fromDT f)
    | RT.DEnum(_, _, _, "Test", [ test ]) -> Test(PT2DT.PackageTest.fromDT test)
    | RT.DEnum(_, _, _, "Module", [ m ]) -> Module(definitionsOfDval m)
    | _ -> Exception.raiseInternal "Invalid Declaration" []

  let ofDval (d : RT.Dval) : SourceFile =
    match d with
    | RT.DRecord(_, _, _, fields) ->
      { declarations = fields |> D.field "declarations" |> D.list declarationOfDval
        exprsToEval = fields |> D.field "exprsToEval" |> D.list PT2DT.Expr.fromDT }
    | _ -> Exception.raiseInternal "Invalid SourceFile" []


module RoundTripExpect =
  module Canonical = LibSerialization.Hashing.Canonical

  /// The bytes content-addressing hashes: ids, `originalName`, descriptions and locations are
  /// skipped; names compare by what they resolved to. "Same bytes" is exactly "same program".
  let private canon (write : System.IO.BinaryWriter -> unit) : string =
    use ms = new System.IO.MemoryStream()
    use w = new System.IO.BinaryWriter(ms)
    write w
    w.Flush()
    System.Convert.ToHexString(ms.ToArray())

  let rec private definitionsCanon
    (d : SourceFileAst.Definitions)
    : List<string * string> =
    [ yield!
        d.types
        |> List.mapi (fun i t ->
          $"type {i}", canon (fun w -> Canonical.writeType Canonical.Normal w t))
      yield!
        d.values
        |> List.mapi (fun i v ->
          $"value {i}", canon (fun w -> Canonical.writeValue Canonical.Normal w v))
      yield!
        d.fns
        |> List.mapi (fun i f ->
          $"fn {i}", canon (fun w -> Canonical.writeFn Canonical.Normal w f))
      yield!
        d.exprs
        |> List.mapi (fun i (e, path) ->
          let at = String.concat "." path
          $"module expr {i} at {at}",
          canon (fun w -> Canonical.writeExpr Canonical.Normal w e)) ]

  let private sourceFileCanon
    (sf : SourceFileAst.SourceFile)
    : List<string * string> =
    [ yield!
        sf.declarations
        |> List.mapi (fun i decl ->
          match decl with
          | SourceFileAst.Type t ->
            [ $"decl {i} type",
              canon (fun w -> Canonical.writeType Canonical.Normal w t) ]
          | SourceFileAst.Value v ->
            [ $"decl {i} value",
              canon (fun w -> Canonical.writeValue Canonical.Normal w v) ]
          | SourceFileAst.Function f ->
            [ $"decl {i} fn",
              canon (fun w -> Canonical.writeFn Canonical.Normal w f) ]
          | SourceFileAst.Test test ->
            [ $"decl {i} test",
              canon (fun w -> Canonical.writeTest Canonical.Normal w test) ]
          | SourceFileAst.Module m ->
            definitionsCanon m
            |> List.map (fun (k, v) -> $"decl {i} module / {k}", v))
        |> List.concat
      yield!
        sf.exprsToEval
        |> List.mapi (fun i e ->
          $"expr {i}", canon (fun w -> Canonical.writeExpr Canonical.Normal w e)) ]

  /// Canonical contents and their name bindings, in source order.
  let nameBindings (ops : List<PT.PackageOp>) : List<string> =
    ops
    |> List.choose (fun op ->
      match op with
      | PT.PackageOp.AddType t ->
        Some $"content {canon (fun w -> Canonical.writeType Canonical.Normal w t)}"
      | PT.PackageOp.AddValue v ->
        Some $"content {canon (fun w -> Canonical.writeValue Canonical.Normal w v)}"
      | PT.PackageOp.AddFn f ->
        Some $"content {canon (fun w -> Canonical.writeFn Canonical.Normal w f)}"
      | PT.PackageOp.AddTrait t ->
        Some $"content {canon (fun w -> Canonical.writeTrait Canonical.Normal w t)}"
      | PT.PackageOp.AddTraitImpl i ->
        Some $"content {canon (fun w -> Canonical.writeImpl Canonical.Normal w i)}"
      | PT.PackageOp.AddTest test ->
        Some
          $"content {canon (fun w -> Canonical.writeTest Canonical.Normal w test)}"
      // `previous` (the hash this binding replaced) is what makes a rebind distinguishable from a
      // fresh one; the round trip does not care which it was.
      | PT.PackageOp.SetName(loc, target, _previous) ->
        let kind =
          match target with
          | PT.Reference.PackageType _ -> "type"
          | PT.Reference.PackageValue _ -> "value"
          | PT.Reference.PackageFn _ -> "fn"
          | PT.Reference.PackageTrait _ -> "trait"
          | PT.Reference.PackageTraitImpl _ -> "impl"
          | PT.Reference.PackageTest _ -> "test"
        let path = String.concat "." (loc.owner :: loc.modules)
        Some $"bind {kind} {path}.{loc.name}"
      | _ -> None)

  /// Unresolved names in traversal order, which the canonical serializer omits.
  let rec collectUnresolved (d : RT.Dval) : List<string> =
    let fromFields (fields : RT.RecordFields) : List<string> =
      let own =
        match
          RT.RecordFields.tryFind "originalName" fields,
          RT.RecordFields.tryFind "resolved" fields
        with
        | Some(RT.DList(_, segs)), Some(RT.DEnum(_, _, _, "Error", _)) ->
          let name =
            segs
            |> List.choose (fun s ->
              match s with
              | RT.DString s -> Some s
              | _ -> None)
            |> String.concat "."
          [ name ]
        | _ -> []
      own
      @ (fields
         |> RT.RecordFields.toList
         |> List.collect (fun (_, v) -> collectUnresolved v))

    match d with
    | RT.DList(_, items) -> items |> List.collect collectUnresolved
    | RT.DTuple(a, b, rest) -> (a :: b :: rest) |> List.collect collectUnresolved
    | RT.DRecord(_, _, _, fields) -> fromFields fields
    | RT.DEnum(_, _, _, _, fields) -> fields |> List.collect collectUnresolved
    | RT.DDict(_, _, entries) ->
      entries
      |> RT.DictMap.toList
      |> List.collect (fun (k, v) -> collectUnresolved k @ collectUnresolved v)
    | RT.DApplicable(RT.AppLambda lambda) ->
      let closed =
        RT.Captures.toList lambda.closedRegisters
        |> List.collect (fun (_, value) -> collectUnresolved value)
      closed
    | RT.DApplicable(RT.AppNamedFn namedFn) ->
      namedFn.argsSoFar |> List.collect collectUnresolved

    | RT.DUnit
    | RT.DBool _
    | RT.DInt8 _
    | RT.DUInt8 _
    | RT.DInt16 _
    | RT.DUInt16 _
    | RT.DInt32 _
    | RT.DUInt32 _
    | RT.DInt64 _
    | RT.DUInt64 _
    | RT.DInt128 _
    | RT.DUInt128 _
    | RT.DBigInt _
    | RT.DFloat _
    | RT.DFloat32 _
    | RT.DDecimal _
    | RT.DChar _
    | RT.DString _
    | RT.DDateTime _
    | RT.DUuid _
    | RT.DBytes _
    | RT.DStream _
    | RT.DPromise _ -> []

  /// Two parses of "the same" source describe the same program.
  let sourceFileEqual
    (actual : SourceFileAst.SourceFile)
    (expected : SourceFileAst.SourceFile)
    (printed : string)
    : unit =
    let a = sourceFileCanon actual
    let e = sourceFileCanon expected

    Expect.equal
      (List.map fst a)
      (List.map fst e)
      "Re-parsing the printed source gave a different set of items"

    List.iter2
      (fun (label, ab) (_, eb) ->
        if ab <> eb then
          failtest (
            $"Re-parsing the printed source changed the meaning of {label}.\n"
            + $"The printer emitted text that parses to a different program:\n{printed}"
          ))
      a
      e


/// The widths a test renders at, on top of the 80 that every test checks against its
/// expected output.
///
/// The property here belongs to the printer, not to any one test: whatever layout it
/// picks has to parse back to the same program. Asserting that at all three widths in
/// every test is a few hundred witnesses to one fact, and it was most of this file's
/// runtime -- each extra width costs a print and a parse, both of which run the Dark
/// parser through the interpreter. One width per test, chosen by a hash of its name,
/// still exercises each width across dozens of tests while a single test pays for one.
///
/// `DARK_TEST_ALL_WIDTHS=1` puts the full sweep back, for when a layout bug is what
/// you are actually chasing.
let private allWidths = [ 20; 48; 200 ]

let private sweepWidths (name : string) : List<int> =
  if System.Environment.GetEnvironmentVariable "DARK_TEST_ALL_WIDTHS" = "1" then
    allWidths
  else
    [ allWidths[int (stableHash name % uint32 allWidths.Length)] ]


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
    RT.FQFnName.fqPackage (
      PackageRefs.Fn.LanguageTools.Parser.parsePTSourceFileWithOps ()
    )

  let prettyPrintFnName =
    RT.FQFnName.fqPackage (
      PackageRefs.Fn.PrettyPrinter.ProgramTypes.sourceFileAtWidth ()
    )

  testTask name {
    let basePM =
      if allowUnresolved then
        pmPT
      else
        pmPT |> PT.PackageManager.withExtras extraTypes extraValues extraFns [] []

    // Parsing and printing are separate steps, not one `roundOnceAt width src`, because
    // the width sweep below prints one tree several times and has no reason to re-parse
    // the source for each width.
    let parseSrc (src : string) : Task<RT.Dval * List<PT.PackageOp>> =
      task {
        let! parseExeState = executionStateFor basePM false
        // The branch to resolve names against: main, named rather than taken from the process.
        let args = NEList.ofList (RT.DUuid PT.BranchId.Main.Guid) [ RT.DString src ]
        let! parseResult =
          LibExecution.Execution.executeFunction parseExeState parseFnName [] args
        let! parseDval =
          unwrapExecutionResult parseExeState parseResult |> Ply.toTask

        match parseDval with
        | RT.DEnum(tn, _, _, "Ok", [ RT.DTuple(sourceFile, opsList, []) ]) when
          tn = Dval.resultType ()
          ->
          let packageOps =
            match opsList with
            | RT.DList(_vt, ops) ->
              ops
              |> List.choose LibExecution.ProgramTypesToDarkTypes.PackageOp.fromDT
            | _ -> []
          return (sourceFile, packageOps)

        | RT.DEnum(tn, _, _, "Error", [ RT.DString errMsg ]) when
          tn = Dval.resultType ()
          ->
          return failtest $"Parse error: {errMsg}"
        | _ -> return failtest $"Unexpected parse result format: {parseDval}"
      }

    /// Prints a parsed tree with its own name bindings available.
    let printAt
      (width : int)
      ((sourceFile, packageOps) : RT.Dval * List<PT.PackageOp>)
      : Task<string> =
      task {
        let enhancedPM = LibDB.PackageManager.withExtraOps basePM packageOps
        let! ppExeState = executionStateFor enhancedPM false

        let ppArgs =
          NEList.ofList
            (RT.DUuid PT.BranchId.Main.Guid)
            [ RT.DInt64(int64 width); sourceFile ]
        let! ppResult =
          LibExecution.Execution.executeFunction
            ppExeState
            prettyPrintFnName
            []
            ppArgs
        let! resultDval = unwrapExecutionResult ppExeState ppResult |> Ply.toTask

        match resultDval with
        | RT.DString result -> return result
        | _ -> return failtest $"Unexpected pretty print result: {resultDval}"
      }

    let! first = parseSrc input
    let (firstTree, firstOps) = first
    let! firstPrint = printAt 80 first
    Expect.RT.equalDval
      (RT.DString firstPrint)
      (RT.DString expected)
      "Didn't round-trip as expected"

    // Printing the printer's output must be a fixed point.
    let! second = parseSrc firstPrint
    let (secondTree, secondOps) = second
    let! secondPrint = printAt 80 second
    Expect.RT.equalDval
      (RT.DString secondPrint)
      (RT.DString firstPrint)
      "Printing is not idempotent: printing the printer's own output changed it"

    // Text idempotency does not detect stable output with different semantics.
    RoundTripExpect.sourceFileEqual
      (SourceFileAst.ofDval secondTree)
      (SourceFileAst.ofDval firstTree)
      firstPrint

    // Canonical content omits bindings and unresolved-name spellings.
    Expect.equal
      (RoundTripExpect.nameBindings secondOps)
      (RoundTripExpect.nameBindings firstOps)
      "Re-parsing the printed source bound different names"

    Expect.equal
      (RoundTripExpect.collectUnresolved secondTree)
      (RoundTripExpect.collectUnresolved firstTree)
      "Re-parsing the printed source changed an unresolved name"

    // Every chosen layout must parse to the same program.
    for width in sweepWidths name do
      let! sweepPrint = printAt width first
      let! (sweepTree, sweepOps) = parseSrc sweepPrint
      Expect.equal
        (RoundTripExpect.nameBindings sweepOps)
        (RoundTripExpect.nameBindings firstOps)
        $"Width-{width} rendering bound different names"

      Expect.equal
        (RoundTripExpect.collectUnresolved sweepTree)
        (RoundTripExpect.collectUnresolved firstTree)
        $"Width-{width} rendering changed an unresolved name"
      RoundTripExpect.sourceFileEqual
        (SourceFileAst.ofDval sweepTree)
        (SourceFileAst.ofDval firstTree)
        $"(printed at width {width})\n{sweepPrint}"
  }


let person : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { hash = PT.Hash ""
      description = ""
      declaration =
        { typeParams = []
          bounds = []
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

let myEnum : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { hash = PT.Hash ""
      description = ""
      declaration =
        { typeParams = []
          bounds = []
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
                     description = "" })
                  ({ name = "E"
                     fields =
                       [ { typ = PT.TypeReference.TInt64
                           label = None
                           description = "" }

                         { typ = PT.TypeReference.TInt64
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

let exprs =
  [ t
      "record, 3 fields"
      "Tests.Person {name =\"John\", age = 30, hasPet = true} "
      "Tests.Person { name = \"John\", age = 30, hasPet = true }"
      [ person ]
      []
      []
      false

    t
      "record with newline as separator"
      "Tests.Person\n {name =\"John\"\n age = 30\n hasPet = true} "
      "Tests.Person { name = \"John\", age = 30, hasPet = true }"
      [ person ]
      []
      []
      false

    t
      "record with type args"
      "Generic<Tests.Person> { x = Person { name = \"John\", age = 30, hasPet = true } }"
      "Generic<Tests.Person> { x = Person { name = \"John\", age = 30, hasPet = true } }"
      [ person ]
      []
      []
      false

    t
      "record update 1"
      "{ Tests.Person { name = \"John\", age = 30, hasPet = true } with age = 31 }"
      "{ Tests.Person { name = \"John\", age = 30, hasPet = true } with age = 31 }"
      [ person ]
      []
      []
      false

    t
      "record update 4"
      "(let myRec = Tests.Person { name = \"John\", age = 30, hasPet = true }\n  { myRec with\n      name = \"Jane\"\n      age = 31\n      hasPet = false })"
      "let myRec = Tests.Person { name = \"John\", age = 30, hasPet = true }\n{ myRec with name = \"Jane\", age = 31, hasPet = false }"
      [ person ]
      []
      []
      false

    t "simple enum literal" "Tests.MyEnum.A" "Tests.MyEnum.A" [ myEnum ] [] [] false

    t
      "custom enum tupled params"
      "Tests.MyEnum.C((1, 2))"
      "Tests.MyEnum.C((1, 2))"
      [ myEnum ]
      []
      []
      false

    t
      "custom enum fn params"
      "Tests.MyEnum.D(1, 2)"
      "Tests.MyEnum.D(1, 2)"
      [ myEnum ]
      []
      []
      false

    t
      "custom enum indexed params"
      "Tests.MyEnum.D(\n  1,\n  2\n)"
      "Tests.MyEnum.D(1, 2)"
      [ myEnum ]
      []
      []
      false

    t
      "field access 2"
      "(Tests.Person { name =\"Janice\" }).name"
      "(Tests.Person { name = \"Janice\" }).name"
      [ person ]
      []
      []
      false

    t
      "pipe, into enum 2"
      "33 |> Tests.MyEnum.D(21)"
      "33 |> Tests.MyEnum.D(21)"
      [ myEnum ]
      []
      []
      false

    t
      "pipe, into enum, several fields"
      "33 |> Tests.MyEnum.E(21, 42)"
      "33 |> Tests.MyEnum.E(21, 42)"
      [ myEnum ]
      []
      []
      false

    t
      "fn call with an uppercase reference"
      """Stdlib.Box.set(Tests.Person { name = "John", age = 30, hasPet = true }, "key", TestBox)"""
      """Stdlib.Box.set(
  Tests.Person { name = "John", age = 30, hasPet = true },
  "key",
  TestBox
)"""
      [ person ]
      []
      []
      false ]
  |> testList "exprs"

let valueDeclarations =
  [ t
      "record with fields"
      "val myPerson = Tests.Person { name = \"Alice\", age = 30, hasPet = true }"
      "val myPerson = Tests.Person { name = \"Alice\", age = 30, hasPet = true }"
      [ person ]
      []
      []
      false ]
  |> testList "value declarations"

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
      "module with types, fns, and vals"
      "module MyModule =\n  type ID = Int64\n  type MyString = String\n  let myFn(i: Int64): Int64 = 1\n  val x = 100"
      "module MyModule =\n  type ID =\n    Int64\n\n  type MyString =\n    String\n\n  let myFn(i: Int64): Int64 =\n    1\n\n  val x = 100"
      []
      []
      []
      false

    t
      "module with types, fns, vals, and newlines"
      "module MyModule =\n  type ID = Int64\n\n  type MyString = String\n\n  let myFn(i: Int64): Int64 = 1\n\n  val x = 100"
      "module MyModule =\n  type ID =\n    Int64\n\n  type MyString =\n    String\n\n  let myFn(i: Int64): Int64 =\n    1\n\n  val x = 100"
      []
      []
      []
      false

    t
      "nested module declaration"
      "module MyModule1 =\n  type ID = Int64\n  module MyModule2 =\n    type ID = Int64\n    module MyModule3 =\n      type ID = Int64\n      val x = 100\n      1"
      "module MyModule1 =\n  type ID =\n    Int64\n\n  module MyModule2 =\n    type ID =\n      Int64\n\n    module MyModule3 =\n      type ID =\n        Int64\n\n      val x = 100\n\n      1"
      []
      []
      []
      false ]
  |> testList "module declarations"

let tests =
  testList "LibParserRoundTrip" [ exprs; valueDeclarations; moduleDeclarations ]
