/// Tests the hand-written parser (LibParser).
///
/// Most cases parse source into ProgramTypes, then pretty-print it back to the
/// expected text. A few cases also evaluate parsed expressions or assert parse
/// errors for rejected syntax.
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

/// Cases where printing the printer's own output changes it, with the reason.
///
/// These are pre-existing bugs the idempotency check found, not licence to add more. Two causes:
///
///  - **Module declarations re-nest.** The printer emits the owner as a `module` wrapper, and these
///    files are parsed with owner "Tests", so printing `module Tests = …` and feeding it back yields
///    `module Tests = module Tests = …`, one level deeper each pass. That is a real hazard for anything
///    that prefills a buffer from the printer and saves it back through the parser.
///
///  - **Parenthesisation is unstable.** `EApply` parenthesises its arguments and `EInfix` wraps both
///    sides, so `factorial (n) - (1L)` re-prints as `(factorial n) - (1L)`. Same tree, different text;
///    the printer just doesn't agree with itself about which parens are needed.
///
/// Both are fixed by the source-printer work, at which point this list should be empty.
let knownNonIdempotent : Set<string> = Set.empty


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
      | PT.PackageOp.SetName(loc, target) ->
        let kind =
          match target with
          | PT.Reference.PackageType _ -> "type"
          | PT.Reference.PackageValue _ -> "value"
          | PT.Reference.PackageFn _ -> "fn"
        let path = String.concat "." (loc.owner :: loc.modules)
        Some $"bind {kind} {path}.{loc.name}"
      | _ -> None)

  /// Unresolved names in traversal order, which the canonical serializer omits.
  let rec collectUnresolved (d : RT.Dval) : List<string> =
    let fromFields (fields : RT.DvalMap) : List<string> =
      let own =
        match Map.tryFind "originalName" fields, Map.tryFind "resolved" fields with
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
      @ (fields |> Map.toList |> List.collect (fun (_, v) -> collectUnresolved v))

    match d with
    | RT.DList(_, items) -> items |> List.collect collectUnresolved
    | RT.DTuple(a, b, rest) -> (a :: b :: rest) |> List.collect collectUnresolved
    | RT.DRecord(_, _, _, fields) -> fromFields fields
    | RT.DEnum(_, _, _, _, fields) -> fields |> List.collect collectUnresolved
    | RT.DDict(_, entries) ->
      entries |> Map.toList |> List.collect (fun (_, v) -> collectUnresolved v)
    | RT.DApplicable(RT.AppLambda lambda) ->
      let closed =
        lambda.closedRegisters
        |> List.collect (fun (_, value) -> collectUnresolved value)
      closed @ (lambda.argsSoFar |> List.collect collectUnresolved)
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
    | RT.DInt _
    | RT.DFloat _
    | RT.DChar _
    | RT.DString _
    | RT.DDateTime _
    | RT.DUuid _
    | RT.DDB _
    | RT.DBlob _
    | RT.DStream _ -> []

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
        pmPT |> PT.PackageManager.withExtras extraTypes extraValues extraFns

    // Parse, then print with the resulting name bindings available.
    let roundOnceAt
      (width : int)
      (src : string)
      : Task<RT.Dval * List<PT.PackageOp> * string> =
      task {
        let! parseExeState = executionStateFor basePM false Map.empty
        let args = NEList.singleton (RT.DString src)
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

          let enhancedPM = LibDB.PackageManager.withExtraOps basePM packageOps
          let! ppExeState = executionStateFor enhancedPM false Map.empty

          let ppArgs =
            NEList.ofList
              (RT.DUuid PT.mainBranchId)
              [ Dval.int (bigint width); sourceFile ]
          let! ppResult =
            LibExecution.Execution.executeFunction
              ppExeState
              prettyPrintFnName
              []
              ppArgs
          let! resultDval = unwrapExecutionResult ppExeState ppResult |> Ply.toTask

          match resultDval with
          | RT.DString result -> return (sourceFile, packageOps, result)
          | _ -> return failtest $"Unexpected pretty print result: {resultDval}"

        | RT.DEnum(tn, _, _, "Error", [ RT.DString errMsg ]) when
          tn = Dval.resultType ()
          ->
          return failtest $"Parse error: {errMsg}"
        | _ -> return failtest $"Unexpected parse result format: {parseDval}"
      }

    let roundOnce (src : string) : Task<RT.Dval * List<PT.PackageOp> * string> =
      roundOnceAt 80 src

    let! (firstTree, firstOps, firstPrint) = roundOnce input
    Expect.RT.equalDval
      (RT.DString firstPrint)
      (RT.DString expected)
      "Didn't round-trip as expected"

    // Printing the printer's output must be a fixed point.
    if Set.contains name knownNonIdempotent then
      return ()
    else
      let! (secondTree, secondOps, secondPrint) = roundOnce firstPrint
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
      for width in [ 20; 48; 200 ] do
        let! (_, _, sweepPrint) = roundOnceAt width input
        let! (sweepTree, sweepOps, _) = roundOnce sweepPrint
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


/// Parses `input` as a Darklang expression, evaluates it, and asserts the
/// resulting runtime Dval equals `expected`. Distinct from `t` (which
/// compares pretty-printed source) because round-trip tests can hide
/// symmetric bugs between decode and re-encode; this catches a decode bug
/// directly.
let tEval (name : string) (input : string) (expected : RT.Dval) =
  let parseFnName =
    RT.FQFnName.fqPackage (PackageRefs.Fn.LanguageTools.Parser.parsePTExpr ())

  testTask name {
    let! parseExeState = executionStateFor pmPT false Map.empty

    let args = NEList.singleton (RT.DString input)
    let! parseResult =
      LibExecution.Execution.executeFunction parseExeState parseFnName [] args
    let! parseDval = unwrapExecutionResult parseExeState parseResult |> Ply.toTask

    match parseDval with
    | RT.DEnum(tn, _, _, "Ok", [ exprDT ]) when tn = Dval.resultType () ->
      let ptExpr = LibExecution.ProgramTypesToDarkTypes.Expr.fromDT exprDT
      let instructions =
        ptExpr |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT Map.empty 0 None
      let! exeState = executionStateFor pmPT false Map.empty
      let! actual = LibExecution.Execution.executeExpr exeState instructions
      match actual with
      | Ok result ->
        return Expect.RT.equalDval result expected "Decoded runtime value mismatch"
      | Error(rte, _) -> return failtest $"Evaluation failed: {rte}"

    | RT.DEnum(tn, _, _, "Error", [ RT.DString msg ]) when tn = Dval.resultType () ->
      return failtest $"Parse error: {msg}"
    | _ -> return failtest $"Unexpected parse result format: {parseDval}"
  }


/// Parses `input` as a Darklang source file declaring one fn (the same path the
/// CLI and LSP use to create package fns), registers it, then executes it with
/// `arg` and asserts the result. Execution catches resolution bugs inside the fn
/// body that a pretty-print roundtrip can't.
let tEvalSourceFileFn
  (name : string)
  (input : string)
  (arg : RT.Dval)
  (expected : RT.Dval)
  =
  let parseFnName =
    RT.FQFnName.fqPackage (
      PackageRefs.Fn.LanguageTools.Parser.parsePTSourceFileWithOps ()
    )

  testTask name {
    let! parseExeState = executionStateFor pmPT false Map.empty

    let args = NEList.singleton (RT.DString input)
    let! parseResult =
      LibExecution.Execution.executeFunction parseExeState parseFnName [] args
    let! parseDval = unwrapExecutionResult parseExeState parseResult |> Ply.toTask

    match parseDval with
    | RT.DEnum(tn, _, _, "Ok", [ RT.DTuple(_, opsList, []) ]) when
      tn = Dval.resultType ()
      ->
      // register the declared fn, as the CLI does
      let packageOps =
        match opsList with
        | RT.DList(_vt, ops) ->
          ops |> List.choose LibExecution.ProgramTypesToDarkTypes.PackageOp.fromDT
        | _ -> []
      let enhancedPM = LibDB.PackageManager.withExtraOps pmPT packageOps

      let fnHashes =
        packageOps
        |> List.choose (fun op ->
          match op with
          | PT.AddFn fn ->
            let (PT.Hash hashStr) = fn.hash
            Some hashStr
          | _ -> None)

      match fnHashes with
      | [ hashStr ] ->
        let! exeState = executionStateFor enhancedPM false Map.empty
        let! actual =
          LibExecution.Execution.executeFunction
            exeState
            (RT.FQFnName.fqPackage hashStr)
            []
            (NEList.singleton arg)
        match actual with
        | Ok result ->
          return Expect.RT.equalDval result expected "Unexpected execution result"
        | Error(rte, _) -> return failtest $"Evaluation failed: {rte}"
      | hashes ->
        return failtest $"Expected exactly one declared fn; got {List.length hashes}"

    | RT.DEnum(tn, _, _, "Error", [ RT.DString errMsg ]) when tn = Dval.resultType () ->
      return failtest $"Parse error: {errMsg}"
    | _ -> return failtest $"Unexpected parse result format: {parseDval}"
  }


/// Uses `parseForCli` (the same parse path the CLI uses) because it surfaces
/// `unparseableStuff` as `Result.Error`; `parsePTSourceFileWithOps` swallows
/// per-declaration parse failures into a side list and returns Ok overall.
let parseForCliDval (input : string) =
  task {
    let parseFnName =
      RT.FQFnName.fqPackage (
        PackageRefs.Fn.LanguageTools.Parser.CliScript.parseForCli ()
      )
    let! parseExeState = executionStateFor pmPT false Map.empty
    let args =
      NEList.ofList
        (RT.DUuid PT.mainBranchId)
        [ RT.DString "Tests"
          RT.DString "test"
          RT.DString "test"
          RT.DString input ]
    let! parseResult =
      LibExecution.Execution.executeFunction parseExeState parseFnName [] args
    let! parseDval = unwrapExecutionResult parseExeState parseResult |> Ply.toTask
    return parseDval
  }


/// Asserts `input` is rejected with `Result.Error (ParseError ...)`
/// instead of succeeding. `ParseError` has a single case today, so this
/// checks that parsing failed, not which variant.
let tParseRejected (name : string) (input : string) =
  testTask name {
    let! parseDval = parseForCliDval input

    match parseDval with
    | RT.DEnum(tn, _, _, "Error", [ RT.DEnum(_, _, _, _, _) ]) when
      tn = Dval.resultType ()
      ->
      return ()
    | _ ->
      return
        failtest
          $"Expected Result.Error containing a ParseError variant; got {parseDval}"
  }


/// Like `tParseRejected`, but also asserts the rendered `Message` names the
/// given note. The message carries a "Parse error at line/col: ..." prefix, so we
/// assert containment rather than exact equality.
let tParseErrorNote (name : string) (input : string) (expectedNote : string) =
  testTask name {
    let! parseDval = parseForCliDval input

    match parseDval with
    | RT.DEnum(tn, _, _, "Error", [ errVariant ]) when tn = Dval.resultType () ->
      match errVariant with
      | RT.DEnum(_, _, _, "Message", [ RT.DString message ]) ->
        return
          Expect.stringContains
            message
            expectedNote
            $"wrong ParseError note for {input}"
      | other -> return failtest $"Expected a Message variant; got {other}"
    | _ ->
      return
        failtest
          $"Expected Result.Error containing a ParseError variant; got {parseDval}"
  }


let person : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { hash = PT.Hash ""
      description = ""
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
    { hash = PT.Hash ""
      description = ""
      declaration =
        { typeParams = []
          definition = PT.TypeDeclaration.Alias PT.TypeReference.TString } }
  let location : PT.PackageLocation =
    { owner = "Tests"; modules = []; name = "MyString" }
  (packageType, location)

let pet : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { hash = PT.Hash ""
      description = ""
      declaration =
        { typeParams = []
          definition = PT.TypeDeclaration.Alias PT.TypeReference.TString } }
  let location : PT.PackageLocation = { owner = "Tests"; modules = []; name = "Pet" }
  (packageType, location)

let myEnum : (PT.PackageType.PackageType * PT.PackageLocation) =
  let packageType : PT.PackageType.PackageType =
    { hash = PT.Hash ""
      description = ""
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
    t "blob alias" "type MyBlob = Blob" "type MyBlob =\n  Blob" [] [] [] false
    t
      "stream of primitive alias"
      "type MyStream = Stream<UInt8>"
      "type MyStream =\n  Stream<UInt8>"
      []
      []
      []
      false
    t
      "stream of blob alias"
      "type MyStream = Stream<Blob>"
      "type MyStream =\n  Stream<Blob>"
      []
      []
      []
      false
    t
      "stream of generic alias"
      "type MyStream = Stream<'a>"
      "type MyStream =\n  Stream<'a>"
      []
      []
      []
      false

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

    t
      "fn with fn arg"
      "type HigherOrder = (Int64 -> String) -> Bool"
      "type HigherOrder =\n  (Int64 -> String) -> Bool"
      []
      []
      []
      false
    t
      "fn with fn return"
      "type Curried = Int64 -> (String -> Bool)"
      "type Curried =\n  Int64 -> (String -> Bool)"
      []
      []
      []
      false
    t
      "tuple containing fn"
      "type FunctionAndFlag = ((Int64 -> String) * Bool)"
      "type FunctionAndFlag =\n  ((Int64 -> String) * Bool)"
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
      "type MyOption =\n  Option"
      []
      []
      []
      false
    t
      "option alias, unapplied"
      "type MyOption = Stdlib.Option.Option"
      "type MyOption =\n  Option"
      []
      []
      []
      false
    t
      "option alias, applied"
      "type MyOption = Stdlib.Option.Option<Int64>"
      "type MyOption =\n  Option<Int64>"
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
      false

    // Unqualified Result/Option type names resolve via the stdlib fallback from any
    // module, mirroring how their constructors (Ok/Error, Some/None) already
    // resolve unqualified everywhere. The printer knows about that fallback too, so
    // these print back unqualified: input and output are the same text, which is
    // what you want from a round-trip and wasn't true while the printer re-qualified
    // them to `Stdlib.X.Y`.
    t
      "unqualified Result resolves through stdlib fallback"
      "type MyResult = Result<Int64, String>"
      "type MyResult =\n  Result<Int64, String>"
      []
      []
      []
      false
    t
      "unqualified Option resolves through stdlib fallback"
      "type MyOpt = Option<Int64>"
      "type MyOpt =\n  Option<Int64>"
      []
      []
      []
      false
    t
      "unqualified Result unapplied resolves through stdlib fallback"
      "type MyResult = Result"
      "type MyResult =\n  Result"
      []
      []
      []
      false ]
  |> testList "type references"


let typeDeclarations =
  [ t "unit" "type SimpleAlias = Unit" "type SimpleAlias =\n  Unit" [] [] [] false

    t
      "enum with fn field in product"
      "type WithCallback = | WithCallback of (Int64 -> String) * Bool"
      "type WithCallback =\n  | WithCallback of (Int64 -> String) * Bool"
      []
      []
      []
      false

    t
      "type doc comment"
      "/// User-facing id\ntype UserID = Int64"
      "/// User-facing id\ntype UserID =\n  Int64"
      []
      []
      []
      false

    t
      "four slash comment is not a doc comment"
      "//// plain comment\ntype Plain = Int64"
      "type Plain =\n  Int64"
      []
      []
      []
      false

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
      "record type field name with hyphen"
      "type Headers = { ``Content-Length``: String }"
      "type Headers =\n  { ``Content-Length``: String }"
      []
      []
      []
      false

    t
      "record type field name that is a keyword"
      "type Metadata = { ``type``: String }"
      "type Metadata =\n  { ``type``: String }"
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

    // Enum type declarations.
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
      "enum field label with hyphen"
      "type MyEnum = | A of ``Content-Length``: String"
      "type MyEnum =\n  | A of ``Content-Length``: String"
      []
      []
      []
      false
    t
      "enum field label that is a keyword"
      "type MyEnum = | A of ``type``: String"
      "type MyEnum =\n  | A of ``type``: String"
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
    t "int literal 1Z" "1Z" "1Z" [] [] [] false

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
    // string-escape round-trips: parser decodes, pretty-printer re-encodes.
    t "string newline escape" "\"a\\nb\"" "\"a\\nb\"" [] [] [] false
    t "string quote escape" "\"a\\\"b\"" "\"a\\\"b\"" [] [] [] false
    t "string backslash escape" "\"a\\\\b\"" "\"a\\\\b\"" [] [] [] false
    t "string bell escape" "\"\\a\"" "\"\\a\"" [] [] [] false
    t "string backspace escape" "\"\\b\"" "\"\\b\"" [] [] [] false
    t "string form-feed escape" "\"\\f\"" "\"\\f\"" [] [] [] false
    t "string vertical-tab escape" "\"\\v\"" "\"\\v\"" [] [] [] false
    t "string carriage-return escape" "\"\\r\"" "\"\\r\"" [] [] [] false
    t "string hex escape \\xHH" "\"\\x41\"" "\"A\"" [] [] [] false
    t "string hex escape \\XHHHH" "\"\\X0041\"" "\"A\"" [] [] [] false
    t "string unicode escape \\uHHHH" "\"\\u00E9\"" "\"é\"" [] [] [] false
    t "string unicode escape \\UHHHHHHHH" "\"\\U00000041\"" "\"A\"" [] [] [] false
    t "string forward-slash escape" "\"\\/\"" "\"/\"" [] [] [] false
    t "string single-quote escape in double quotes" "\"\\'\"" "\"'\"" [] [] [] false
    // Decoded-value assertions: prove the byte/char actually decoded, not
    // just that it survived a symmetric round-trip through the pretty-printer.
    tEval "decoded \\r is CR" "\"\\r\"" (RT.DString "\r")
    tEval "decoded \\x41 is A" "\"\\x41\"" (RT.DString "A")
    tEval "decoded \\X0041 is A" "\"\\X0041\"" (RT.DString "A")
    tEval "decoded \\u00E9 is é" "\"\\u00E9\"" (RT.DString "é")
    tEval "decoded \\U00000041 is A" "\"\\U00000041\"" (RT.DString "A")
    tEval "decoded \\/ is /" "\"\\/\"" (RT.DString "/")
    tEval "decoded \\' in string is '" "\"\\'\"" (RT.DString "'")
    tEval "decoded \\n is LF" "\"a\\nb\"" (RT.DString "a\nb")
    tEval "decoded \\t is HT" "\"a\\tb\"" (RT.DString "a\tb")
    tEval "decoded \\\\ is backslash" "\"a\\\\b\"" (RT.DString "a\\b")
    tEval
      "decoded \\u00E9 above-BMP \\U0001F600"
      "\"\\U0001F600\""
      (RT.DString "\U0001F600")
    // Interpolation text segments decode escapes too.
    tEval
      "decoded interpolation with escape"
      "$\"a\\nb {\"x\"}\""
      (RT.DString "a\nb x")
    // Char literals: decode covers all the same shapes the string side does.
    tEval "decoded '\\n' is LF" "'\\n'" (RT.DChar "\n")
    tEval "decoded '\\r' is CR" "'\\r'" (RT.DChar "\r")
    tEval "decoded '\\\\' is backslash" "'\\\\'" (RT.DChar "\\")
    tEval "decoded '\\'' is single-quote" "'\\''" (RT.DChar "'")
    tEval "decoded '\\x41' is A" "'\\x41'" (RT.DChar "A")
    tEval "decoded '\\X0041' is A" "'\\X0041'" (RT.DChar "A")
    tEval "decoded '\\u00E9' is é" "'\\u00E9'" (RT.DChar "é")
    tEval "decoded '\\U00000041' is A" "'\\U00000041'" (RT.DChar "A")
    tEval "decoded '\\/' is /" "'\\/'" (RT.DChar "/")
    t
      "string match pattern with escape"
      "match s with\n| \"a\\nb\" -> 1L\n| _ -> 0L"
      "match s with\n| \"a\\nb\" -> 1L\n| _ -> 0L"
      []
      []
      []
      true
    t "egc" "\"👩‍👩‍👧‍👦\"" "\"👩‍👩‍👧‍👦\"" [] [] [] false
    t "unicode" "\"żółw\"" "\"żółw\"" [] [] [] false
    t "string interpolation" "$\"hello {name}\"" "$\"hello {name}\"" [] [] [] false
    t
      "interpolation with escape"
      "$\"a\\nb {name}\""
      "$\"a\\nb {name}\""
      []
      []
      []
      false
    // Invalid escape sequences are rejected instead of dropped.
    tParseRejected "invalid escape in string literal" "\"\\d+\""
    tParseRejected "invalid escape in interpolation" "$\"a\\d+b\""
    tParseRejected "invalid escape in char literal" "'\\d'"
    tParseRejected
      "invalid escape in string match pattern"
      "match s with\n| \"\\d+\" -> 1L\n| _ -> 0L"
    tParseRejected
      "invalid escape in char match pattern"
      "match c with\n| '\\d' -> 1L\n| _ -> 0L"
    // A qualified enum-case pattern is unsupported (use the unqualified case name).
    // Reject it instead of building a truncated pattern from the first path
    // segment. The rejection holds across path lengths and field bindings.
    tParseRejected
      "qualified enum-case match pattern (fully qualified)"
      "match x with\n| Stdlib.Result.Result.Ok n -> 1L\n| _ -> 0L"
    tParseRejected
      "qualified enum-case match pattern (two segments)"
      "match x with\n| Result.Ok n -> 1L\n| _ -> 0L"
    tParseRejected
      "qualified enum-case match pattern (no field binding)"
      "match x with\n| Stdlib.Result.Ok -> 1L\n| _ -> 0L"
    // The diagnostic names the qualified path problem and points to the
    // unqualified case-name form.
    tParseErrorNote
      "qualified enum-case match pattern suggests unqualified case name"
      "match x with\n| Stdlib.Result.Result.Ok n -> 1L\n| _ -> 0L"
      "Invalid match pattern. Enum patterns use the unqualified case name (e.g. `| Ok n`), not a qualified path like `| Stdlib.Result.Result.Ok n`."
    // Well-formed escapes that are not valid Unicode scalars are rejected:
    //   - surrogate range (D800..DFFF)
    //   - above the Unicode max (>0x10FFFF)
    tParseRejected "surrogate codepoint \\uD800" "\"\\uD800\""
    tParseRejected "surrogate codepoint \\U0000D800" "\"\\U0000D800\""
    tParseRejected "codepoint above max \\U00110000" "\"\\U00110000\""
    tParseRejected "surrogate codepoint in char" "'\\uD800'"
    tParseRejected "codepoint above max in char" "'\\U00110000'"
    // Pure syntax-rejection cases (no escape involvement).
    tParseRejected "bang produces a parse error" "!true"
    tParseRejected "garbage tokens produce a parse error" "@@@"
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
      "$\"test {\"1\"}\" == \"test 1\""
      []
      []
      []
      false

    // char literals
    t "the letter a" "'a'" "'a'" [] [] [] false
    t "a newline char" "'\\n'" "'\\n'" [] [] [] false
    t "a tab char" "'\t'" "'\\t'" [] [] [] false

    // list literal
    t "empty list" "[]" "[]" [] [] [] false
    t "string list" "[\"hello\"]" "[\"hello\"]" [] [] [] false
    t "int list 2" "[1L, 2L]" "[1L, 2L]" [] [] [] false
    t "int list 3" "[1L, 2L, 3L,]" "[1L, 2L, 3L]" [] [] [] false
    t
      "bool list"
      "[true, false, true, false]"
      "[true, false, true, false]"
      []
      []
      []
      false
    t "int list list" "[[1L, 2L], [3L, 4L]]" "[[1L, 2L], [3L, 4L]]" [] [] [] false
    t
      "list with newline as a separator"
      "[ true\n false\n true ]"
      "[true, false, true]"
      []
      []
      []
      false

    t
      "list of function calls"
      """[
  Stdlib.Tuple2.second (4L, 5L)
  Stdlib.Int64.add 1L 2L
  Stdlib.List.head [1L, 2L]
]"""
      "[\n  Stdlib.Tuple2.second (4L, 5L),\n  Stdlib.Int64.add 1L 2L,\n  Stdlib.List.head [1L, 2L]\n]"
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
  Stdlib.List.head [1L, 2L]
]"""
      "[\n  Stdlib.Tuple2.second (4L, 5L),\n  Stdlib.Int64.add 1L 2L,\n  Stdlib.List.head [1L, 2L]\n]"
      []
      []
      []
      false

    // dict literal
    t "empty dict" "Dict { }" "Dict {}" [] [] [] false
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
      "Dict { ``Content-Length`` = 1L }"
      []
      []
      []
      false

    // Keep the printer's field-name rule in sync with the lexer: quote names
    // that cannot be bare, and leave legal bare names alone.
    t
      "dict field name that is a keyword"
      "Dict { ``type`` = 1L }"
      "Dict { ``type`` = 1L }"
      []
      []
      []
      false
    t
      "dict field name that is a keyword (match)"
      "Dict { ``match`` = 1L }"
      "Dict { ``match`` = 1L }"
      []
      []
      []
      false
    t
      "dict field name with a leading digit"
      "Dict { ``2fa`` = 1L }"
      "Dict { ``2fa`` = 1L }"
      []
      []
      []
      false
    t
      "dict field name with an apostrophe stays bare"
      "Dict { foo' = 1L }"
      "Dict { foo' = 1L }"
      []
      []
      []
      false
    t
      "dict bare field name is not over-quoted"
      "Dict { name = 1L }"
      "Dict { name = 1L }"
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
    t "tuple with expr" "(1L, 2L + 3L, 4L)" "(1L, 2L + 3L, 4L)" [] [] [] false

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
      "record field name with hyphen"
      "Headers { ``Content-Length`` = \"text/html\" }"
      "Headers { ``Content-Length`` = \"text/html\" }"
      []
      []
      []
      true

    t
      "record field name that is a keyword"
      "Metadata { ``type`` = \"json\" }"
      "Metadata { ``type`` = \"json\" }"
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
      "Tests.Person { name = \"John\"; age = 30L; hasPet = true }"
      [ person ]
      []
      []
      false

    t
      "record with newline as separator"
      "Tests.Person\n {name =\"John\"\n age = 30L\n hasPet = true} "
      "Tests.Person { name = \"John\"; age = 30L; hasPet = true }"
      [ person ]
      []
      []
      false

    // record with type args
    t
      "record with type args"
      "Generic<Tests.Person> { x = Person { name = \"John\"; age = 30L; hasPet = true } }"
      "Generic<Tests.Person> { x = Person { name = \"John\"; age = 30L; hasPet = true } }"
      [ person ]
      []
      []
      false

    // record update
    t
      "record update 1"
      "{ Tests.Person { name = \"John\"; age = 30L; hasPet = true } with age = 31L }"
      "{ Tests.Person { name = \"John\"; age = 30L; hasPet = true } with age = 31L }"
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
      "record update field name that is a keyword"
      "{ metadata with ``type`` = \"json\" }"
      "{ metadata with ``type`` = \"json\" }"
      []
      []
      []
      true
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
      """let myRec = Tests.Person { name = "John"; age = 30L; hasPet = true }
{ myRec with name = "Jane"; age = 31L; hasPet = false }"""
      [ person ]
      []
      []
      false

    // enum literal
    t "simple enum literal" "Tests.MyEnum.A" "Tests.MyEnum.A" [ myEnum ] [] [] false
    t
      "enum with type args"
      "Generic<Int64>.A 1L"
      "Generic<Int64>.A(1L)"
      []
      []
      []
      false
    // Literal braces in an interpolated string are written doubled; printed back, they must be
    // doubled again, or `{{x}}` (the text `{x}`) comes back as an interpolation of `x`.
    t
      "interpolated string, literal braces"
      "$\"{{x}} = {x}\""
      "$\"{{x}} = {x}\""
      []
      []
      []
      false
    t "option none, short" "Stdlib.Option.Option.None" "Option.None" [] [] [] false
    t "option none, long" "Stdlib.Option.Option.None" "Option.None" [] [] [] false
    t "option some" "Stdlib.Option.Option.Some 1L" "Option.Some(1L)" [] [] [] false
    t
      "custom enum tupled params"
      "Tests.MyEnum.C((1L, 2L))"
      "Tests.MyEnum.C((1L, 2L))"
      [ myEnum ]
      []
      []
      false
    t
      "custom enum fn params"
      "Tests.MyEnum.D(1L, 2L)"
      "Tests.MyEnum.D(1L, 2L)"
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
      """Tests.MyEnum.D(1L, 2L)"""
      [ myEnum ]
      []
      []
      false

    t
      "enum with indented field"
      """Stdlib.Result.Result.Error
  Stdlib.List.ChunkBySizeError.SizeMustBeGreaterThanZero"""
      """Result.Error(Stdlib.List.ChunkBySizeError.SizeMustBeGreaterThanZero)"""
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
    // Pretty-printing normalizes the let body indentation.
    t "simple let expr" "let x = 1L\n  x" "let x = 1L\nx" [] [] [] false
    // A nested function definition desugars to a lambda bound by a let, so the
    // roundtrip is intentionally lossy: the param/return types are dropped and the
    // `let f (x) = ...` sugar prints as `let f = (fun x -> ...)`. (Top-level
    // `let f (x): R = ...` is a fn_decl and keeps its types - this only applies to
    // a `let` nested inside an expression.)
    t
      "nested function definition (desugars to lambda)"
      "let result =\n  let double (x: Int64): Int64 = x * 2L\n  double 5L\nresult"
      "let result =\n  let double = (fun x -> x * 2L)\n  double 5L\nresult"
      []
      []
      []
      false
    t "let expr with indent" "let x =\n  1L\nx" "let x = 1L\nx" [] [] [] false

    t
      "tuple destructuring"
      "let (var1, var2) = var3\n(var1, var2)"
      "let (var1, var2) = var3\n(var1, var2)"
      []
      []
      []
      false

    t
      "tuple destructuring 2"
      "let (var1, var2) = (var3, var4)\n(var1, var2)"
      "let (var1, var2) = (var3, var4)\n(var1, var2)"
      []
      []
      []
      false

    // field access
    t "field access 1" "person.name" "person.name" [] [] [] false
    t
      "field access 2"
      "(Tests.Person { name =\"Janice\" }).name"
      "(Tests.Person { name = \"Janice\" }).name"
      [ person ]
      []
      []
      false
    t
      "field access with hyphenated field name"
      "headers.``Content-Length``"
      "headers.``Content-Length``"
      []
      []
      []
      false
    t
      "field access with keyword field name"
      "metadata.``type``"
      "metadata.``type``"
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
    t "field access in context" "person.age + 1L" "person.age + 1L" [] [] [] false

    // lambda
    t "simple lambda" "fun x -> x + 1L" "(fun x -> x + 1L)" [] [] [] false
    t
      "lambda wrapped with parens"
      "(fun x -> x + 1L)"
      "(fun x -> x + 1L)"
      []
      []
      []
      false
    t "lambda, 2 args" "fun x y -> x * y" "(fun x y -> x * y)" [] [] [] false
    t "lambda, unit arg" "fun () -> 1L" "(fun () -> 1L)" [] [] [] false
    t
      "lambda with notable body"
      "fun var -> (Stdlib.String.toUppercase (Stdlib.String.fromChar var))"
      "(fun var -> Stdlib.String.toUppercase (Stdlib.String.fromChar var))"
      []
      []
      []
      false
    t
      "lambda with notable body 2"
      "fun (str1, str2) -> str1 ++ str2"
      "(fun (str1, str2) -> str1 ++ str2)"
      []
      []
      []
      false


    // if expressions
    t "if, 1" "if true then 1L" "if true then 1L" [] [] [] false
    t "if, 2" "if true then 1L else 2L" "if true then 1L else 2L" [] [] [] false
    t
      "if, 3"
      "if a < b then 1L else if c > d then 2L"
      "if a < b then 1L else if c > d then 2L"
      []
      []
      []
      false
    t
      "if, 4"
      "if a < b then 1L else if c > d then 2L else 3L"
      "if a < b then 1L else if c > d then 2L else 3L"
      []
      []
      []
      false

    t "if, 5" "if true then\n 1L" "if true then 1L" [] [] [] false
    t "if, 6" "if true then\n 1L\nelse\n 2L" "if true then 1L else 2L" [] [] [] false
    t
      "if, 7"
      "if true then\n a\nelse if false then\n c"
      "if true then a else if false then c"
      []
      []
      []
      false

    t
      "if, 8"
      "if a > b then\n a\nelse if c > d then\n c\nelse d"
      "if a > b then a else if c > d then c else d"
      []
      []
      []
      false

    t "if, 9" "if true then\n\ta\nelse\n\tb" "if true then a else b" [] [] [] false

    t
      "if, many branches"
      """if true then
  a
else if false then
  c
else if true then
  d"""
      "if true then a else if false then c else if true then d"
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
      // A nested `if` in a then-branch always gets its own line; see "else for outer if".
      """if a > b then
  if c > d then c else b"""
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
      // A nested `if` in a then-branch always gets its own line: on one row the parser would give
      // this `else` to the inner `if`, making it "else for inner if" above. Indentation decides.
      """if a > b then
  if c > d then c
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
      """if a > b then
  a
else if c > d then c else if e > f then e else if g > h then g else h"""
      []
      []
      []
      false


    // match expressions
    t
      "match, unit"
      "match () with\n| () -> true"
      "match () with\n| () -> true"
      []
      []
      []
      false
    t
      "match, bool"
      "match true with\n| true -> true"
      "match true with\n| true -> true"
      []
      []
      []
      false

    t
      "match, int 1y"
      "match 1y with\n| 1y -> true"
      "match 1y with\n| 1y -> true"
      []
      []
      []
      false
    t
      "match, int -1y"
      "match -1y with\n| -1y -> true"
      "match -1y with\n| -1y -> true"
      []
      []
      []
      false
    t
      "match, int 0uy"
      "match 0uy with\n| 0uy -> true"
      "match 0uy with\n| 0uy -> true"
      []
      []
      []
      false
    t
      "match, int 1s"
      "match 1s with\n| 1s -> true"
      "match 1s with\n| 1s -> true"
      []
      []
      []
      false
    t
      "match, int 2us"
      "match 2us with\n| 2us -> true"
      "match 2us with\n| 2us -> true"
      []
      []
      []
      false
    t
      "match, int 3l"
      "match 3l with\n| 3l -> true"
      "match 3l with\n| 3l -> true"
      []
      []
      []
      false
    t
      "match, int 5ul"
      "match 5ul with\n| 5ul -> true"
      "match 5ul with\n| 5ul -> true"
      []
      []
      []
      false
    t
      "match, int 7L"
      "match 7L with\n| 7L -> true"
      "match 7L with\n| 7L -> true"
      []
      []
      []
      false
    t
      "match, int 8UL"
      "match 8UL with\n| 8UL -> true"
      "match 8UL with\n| 8UL -> true"
      []
      []
      []
      false
    t
      "match, int 9Q"
      "match 9Q with\n| 9Q -> true"
      "match 9Q with\n| 9Q -> true"
      []
      []
      []
      false
    t
      "match, int 10Z"
      "match 10Z with\n| 10Z -> true"
      "match 10Z with\n| 10Z -> true"
      []
      []
      []
      false
    t
      "match, float 0.9"
      "match 0.9 with\n| 0.9 -> true"
      "match 0.9 with\n| 0.9 -> true"
      []
      []
      []
      false
    t
      "match, string"
      "match \"str\" with\n| \"str\" -> true"
      "match \"str\" with\n| \"str\" -> true"
      []
      []
      []
      false
    t
      "match, char"
      "match 'c' with\n| 'c' -> true"
      "match 'c' with\n| 'c' -> true"
      []
      []
      []
      false
    t
      "match, var"
      "match var with\n| var -> true"
      "match var with\n| var -> true"
      []
      []
      []
      false
    t
      "match, str 2"
      "match \"str\" with\n| \"str\" -> true\n| \"other\" -> false"
      "match \"str\" with\n| \"str\" -> true\n| \"other\" -> false"
      []
      []
      []
      false
    t
      "match, int list 1"
      "match [1L, 2L] with\n| [1L, 2L] -> true"
      "match [1L, 2L] with\n| [1L, 2L] -> true"
      []
      []
      []
      false
    t
      "match, int list 2"
      "match [1L, 2L, 3L] with\n| head :: tail ->\n \"pass\""
      "match [1L, 2L, 3L] with\n| head :: tail -> \"pass\""
      []
      []
      []
      false
    t
      "match, int tuple"
      "match (1L, 2L) with\n| (1L, 2L) -> true"
      "match (1L, 2L) with\n| (1L, 2L) -> true"
      []
      []
      []
      false
    t
      "match, enum"
      "match Stdlib.Result.Result.Ok 5L with\n| Ok(5L) -> true\n| Error(e) -> false"
      "match Result.Ok(5L) with\n| Ok(5L) -> true\n| Error(e) -> false"
      []
      []
      []
      false
    t
      "match, enum no parens with one arg"
      "match Stdlib.Result.Result.Ok 5L with\n| Ok 5L -> true\n| Error e  -> false"
      "match Result.Ok(5L) with\n| Ok(5L) -> true\n| Error(e) -> false"
      []
      []
      []
      false
    t
      "match, enum with 2 args"
      "match MyEnum.D(5L, 3L) with\n| D(5L, 3L) -> true\n| _  -> false"
      "match MyEnum.D(5L, 3L) with\n| D(5L, 3L) -> true\n| _ -> false"
      []
      []
      []
      false
    t
      "match, enum no parens with 1 tuple arg"
      "match Stdlib.Result.Result.Ok((5L, 3L)) with\n| Ok((5L, 3L)) -> true\n| Error e  -> false"
      "match Result.Ok((5L, 3L)) with\n| Ok((5L, 3L)) -> true\n| Error(e) -> false"
      []
      []
      []
      false
    t
      "match, string 3"
      "match \"str\" with\n| \"str\" when true -> true"
      "match \"str\" with\n| \"str\" when true -> true"
      []
      []
      []
      false
    t
      "match, when simple"
      "match x with\n| y when y > 1L -> true\n| z when z < 1L -> false\n| w -> w"
      "match x with\n| y when y > 1L -> true\n| z when z < 1L -> false\n| w -> w"
      []
      []
      []
      false
    t
      "match, ignored 1"
      "match true with\n| _ -> true"
      "match true with\n| _ -> true"
      []
      []
      []
      false
    t
      "match, ignored 2"
      "match true with\n| _var -> true"
      "match true with\n| _var -> true"
      []
      []
      []
      false

    t
      "match, multiple patterns"
      "match (1L, 2L) with\n| (1L, 2L) | (2L, 1L) -> true\n| _ -> false"
      "match (1L, 2L) with\n| (1L, 2L) | (2L, 1L) -> true\n| _ -> false"
      []
      []
      []
      false


    // pipe expression
    t "pipe, infix" "1L |> (+) 2L" "1L |> (+) 2L" [] [] [] false
    t
      "pipe, computed arg"
      "[1L, 2L] |> Stdlib.List.take (2L - 1L)"
      "[1L, 2L] |> Stdlib.List.take (2L - 1L)"
      []
      []
      []
      false
    t
      "pipe, if head"
      "(if true then 1L else 2L) |> Stdlib.Int64.add 1L"
      "(if true then 1L else 2L) |> Stdlib.Int64.add 1L"
      []
      []
      []
      false
    t "pipe, into var" "1L |> x" "1L |> x" [] [] [] false
    t
      "pipe, into lambda"
      "1L |> (fun x -> x + 1L)"
      "1L |> (fun x -> x + 1L)"
      []
      []
      []
      false
    t
      "pipe, into lambda, 2"
      "1L |> fun x -> x + 1L"
      "1L |> (fun x -> x + 1L)"
      []
      []
      []
      false
    t
      "pipe, into enum"
      "3L |> Stdlib.Result.Result.Ok"
      "3L |> Result.Ok"
      []
      []
      []
      false
    t
      "pipe, into enum 2"
      "33L |> Tests.MyEnum.D(21L)"
      "33L |> Tests.MyEnum.D(21L)"
      [ myEnum ]
      []
      []
      false
    t
      "pipe, lambda then another stage"
      "1L |> (fun x -> x + 1L) |> Stdlib.Int64.add 2L"
      "1L |> (fun x -> x + 1L) |> Stdlib.Int64.add 2L"
      []
      []
      []
      false
    t
      "pipe, into enum, several fields"
      "33L |> Tests.MyEnum.E(21L, 42L)"
      "33L |> Tests.MyEnum.E(21L, 42L)"
      [ myEnum ]
      []
      []
      false
    t
      "pipe, into fn call"
      "1L |> Stdlib.Int64.add 2L"
      "1L |> Stdlib.Int64.add 2L"
      []
      []
      []
      false
    t
      "pipe, into fn call 2"
      "1L |> Stdlib.Int64.toString"
      "1L |> Stdlib.Int64.toString"
      []
      []
      []
      false
    t
      "pipe, into fn call 3"
      "\"true\" |> Builtin.jsonParse<Bool>"
      "\"true\" |> Builtin.jsonParse<Bool>"
      []
      []
      []
      false
    t
      "pipe, into fn call 4"
      "Stdlib.Int64.add 1L 2L |> Stdlib.Int64.add 1L"
      "Stdlib.Int64.add 1L 2L |> Stdlib.Int64.add 1L"
      []
      []
      []
      false
    t
      "pipe, into fn call 5"
      "[1L, 2L] |> Stdlib.List.last |> Builtin.unwrap"
      "[1L, 2L] |> Stdlib.List.last |> Builtin.unwrap"
      []
      []
      []
      false

    // Infix calls pretty-print with explicit operand parentheses.
    t "fn call, add once" "1L + 2L" "1L + 2L" [] [] [] false
    t "fn call, add twice" "1L + b + 3L" "1L + b + 3L" [] [] [] false
    t "fn call, add thrice" "1L + 2L * 3L - 4L" "1L + 2L * 3L - 4L" [] [] [] false
    t "fn call, >" "1L > 2L" "1L > 2L" [] [] [] false
    t "fn call, >=" "1L >= 2L" "1L >= 2L" [] [] [] false
    t "fn call, <" "1L < 2L" "1L < 2L" [] [] [] false
    t "fn call, <=" "1L <= 2L" "1L <= 2L" [] [] [] false
    t "fn call, ==" "1L == 2L" "1L == 2L" [] [] [] false
    t "fn call, !=" "1L != 2L" "1L != 2L" [] [] [] false
    t "fn call, ^" "1L ^ 2L" "1L ^ 2L" [] [] [] false
    t "fn call, ++" "strVar ++ \"str\"" "strVar ++ \"str\"" [] [] [] false
    t "fn call, &&" "true && false" "true && false" [] [] [] false
    t "fn call, ||" "true || false" "true || false" [] [] [] false
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
      """Stdlib.Tuple3.mapAllThree
  (fun x -> Stdlib.String.toUppercase x)
  (fun x -> x - 2L)
  (fun x -> Stdlib.String.toUppercase x)
  ("one", 2L, "pi")"""
      []
      []
      []
      false
    t
      "fn call with db reference"
      """Stdlib.DB.set Tests.Person { name = "John"; age = 30L; hasPet = true } "key" TestDB"""
      """Stdlib.DB.set
  Tests.Person { name = "John"; age = 30L; hasPet = true }
  "key"
  TestDB"""
      [ person ]
      []
      []
      false ]
  |> testList "exprs"


let valueDeclarations =
  [ t "unit" "val unitVal = ()" "val unitVal = ()" [] [] [] false

    t
      "value doc comment"
      "/// Default retry count\nval retries = 3L"
      "/// Default retry count\nval retries = 3L"
      []
      []
      []
      false

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
      "val emptyDict = Dict {}"
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
      "val listOfInts = [1L, 2L, 3L]"
      "val listOfInts = [1L, 2L, 3L]"
      []
      []
      []
      false
    t
      "list, list, int"
      "val listOfLists = [[1L, 2L], [3L, 4L]]"
      "val listOfLists = [[1L, 2L], [3L, 4L]]"
      []
      []
      []
      false

    // records
    t
      "record with fields"
      "val myPerson = Tests.Person { name = \"Alice\"; age = 30L; hasPet = true }"
      "val myPerson = Tests.Person { name = \"Alice\"; age = 30L; hasPet = true }"
      [ person ]
      []
      []
      false

    // enums
    t
      "option, none"
      "val none = Stdlib.Option.Option.None"
      "val none = Option.None"
      []
      []
      []
      false
    t
      "option, some 1"
      "val some = Stdlib.Option.Option.Some(1L)"
      "val some = Option.Some(1L)"
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
      "function doc comment"
      "/// Greets a user\nlet greet (name: String): String = \"Hello \" ++ name"
      "/// Greets a user\nlet greet (name: String): String =\n  \"Hello \" ++ name"
      []
      []
      []
      false

    t
      "nested fn type in parameter"
      "let apply (f: (Int64 -> String) -> Bool): Bool = true"
      "let apply (f: (Int64 -> String) -> Bool): Bool =\n  true"
      []
      []
      []
      false

    t
      "nested fn type in return"
      "let curry (): Int64 -> (String -> Bool) = fun x y -> true"
      "let curry (): Int64 -> (String -> Bool) =\n  (fun x y -> true)"
      []
      []
      []
      false

    t
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
      "let double2 (i: LanguageTools.ID): Int64 =\n  i + i"
      []
      []
      []
      false

    // `()` is stored as one parameter named `_` of type Unit, and prints back as `()`.
    t
      "single unit param"
      "let emptyString () : String = \"\""
      "let emptyString (): String =\n  \"\""
      []
      []
      []
      false

    // The long form parses to the same thing and normalizes to `()`, so either spelling round-trips.
    t
      "single unit param, written long"
      "let emptyString (_: Unit) : String = \"\""
      "let emptyString (): String =\n  \"\""
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
      "let helloPerson (name: String): String =\n  let greeting = \"Hello \"\n  greeting ++ name"
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
      "let factorial (n: Int64): Int64 =\n  if n <= 1L then 1L else n * factorial (n - 1L)"
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
    let result = incr y (Stdlib.Int64.subtract z 1L)
    let incr = (fun x -> Stdlib.Int64.add x 2L)
    let lambdaResult = incr z
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
      "module MyModule =\n  type ID =\n    Int64"
      []
      []
      []
      false

    // Unqualified Result resolves from inside a non-Stdlib module (here
    // Tests.MyModule), the scenario that motivated the stdlib fallback.
    // Previously this required the fully-qualified `Stdlib.Result.Result`.
    t
      "unqualified Result resolves inside a non-Stdlib module"
      "module MyModule =\n  type MyResult = Result<Int64, String>"
      "module MyModule =\n  type MyResult =\n    Result<Int64, String>"
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
      "module MyModule =\n  type ID =\n    Int64\n\n  type MyString =\n    String\n\n  let myFn (i: Int64): Int64 =\n    1L\n\n  val x = 100L"
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
      "module MyModule =\n  type ID =\n    Int64\n\n  type MyString =\n    String\n\n  let myFn (i: Int64): Int64 =\n    1L\n\n  val x = 100L"
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
      "module MyModule1 =\n  type ID =\n    Int64\n\n  module MyModule2 =\n    type ID =\n      Int64\n\n    module MyModule3 =\n      type ID =\n        Int64\n\n      val x = 100L\n\n      1L"
      []
      []
      []
      false ]
  |> testList "module declarations"


let sourceFiles =
  [
    // Source files pretty-print declarations and trailing expressions together.
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
  let book = Library.getBook bookId
  getNameFromBook book

let curiousGeorgeBookId = 101L
Builtin.printLine (getTitle curiousGeorgeBookId)
0L"
      []
      []
      []
      false

    t
      "mixed module, declaration, and trailing expression"
      """module Helpers =
  /// One from Helpers
  val one = 1L

/// Top-level value
val two = 2L

two"""
      "module Helpers =\n  /// One from Helpers\n  val one = 1L\n\n/// Top-level value\nval two = 2L\n\ntwo"
      []
      []
      []
      false

    // In the CLI/LSP path, a nested recursive fn should resolve its own name as
    // a local variable while WT2PT converts the body, not as an unresolved fn
    // name. This test executes the fn so we catch that case directly.
    tEvalSourceFileFn
      "fn decl containing a nested recursive fn (CLI/LSP path)"
      "let myRev (list: List<Int64>): List<Int64> =
  let helper (l: List<Int64>) (acc: List<Int64>): List<Int64> =
    match l with
    | [] -> acc
    | head :: tail -> helper tail (Stdlib.List.push acc head)
  helper list []"
      (RT.DList(
        LibExecution.ValueType.int64,
        [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ]
      ))
      (RT.DList(
        LibExecution.ValueType.int64,
        [ RT.DInt64 3L; RT.DInt64 2L; RT.DInt64 1L ]
      )) ]
  |> testList "cli scripts"

let tests =
  testList
    "LibParserRoundTrip"
    [ typeReferences
      typeDeclarations
      valueDeclarations
      exprs
      functionDeclarations
      moduleDeclarations
      sourceFiles ]
