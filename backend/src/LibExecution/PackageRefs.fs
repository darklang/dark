/// All Darklang code exists in package space, referenced by a hash of the content.
/// In many places throughout our F# codebase, we reference these package items.
///
/// e.g. in order to return an `Option` from a Builtin, we need to know the hash of
/// the `Option` package type when constructing the `DEnum` value.
///
/// Hashes are loaded at runtime from `package-ref-hashes.txt`, which lives in the
/// source tree alongside this file and is auto-updated by `reload-packages`.
/// Hash changes show up as diffs in that file rather than in this F# source.
module LibExecution.PackageRefs

open Prelude

/// Hashes loaded from `package-ref-hashes.txt`.
/// Local dev: reads the source-tree file directly (picks up reload-packages changes
/// without a rebuild). Release: falls back to the embedded resource in the DLL.
/// Keys are "type/{modules}.{name}" or "fn/{modules}.{name}".
let private hashes : Map<string, string> =
  let parseLines (lines : string seq) =
    lines
    |> Seq.choose (fun line ->
      let line = line.Trim()
      if line = "" then
        None
      else
        match line.Split('|') with
        | [| fqn; hash |] -> Some(fqn, hash)
        | _ -> None)
    |> Map.ofSeq

  let sourceTreePath =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "package-ref-hashes.txt")
    |> System.IO.Path.GetFullPath

  try
    if System.IO.File.Exists(sourceTreePath) then
      System.IO.File.ReadAllLines(sourceTreePath) |> parseLines
    else
      // Release binary: source tree not available, use embedded resource
      use stream =
        System.Reflection.Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("LibExecution.package-ref-hashes.txt")
      if stream <> null then
        use reader = new System.IO.StreamReader(stream)
        reader.ReadToEnd().Split('\n') |> parseLines
      else
        Exception.raiseInternal
          "PackageRefs: package-ref-hashes.txt not found"
          [ "triedPath", sourceTreePath ]
  with
  | :? Exception.InternalException -> reraise ()
  | ex ->
    Exception.raiseInternal
      "PackageRefs: failed to load package-ref-hashes.txt"
      [ "path", sourceTreePath; "message", ex.Message ]


module Type =
  /// All type refs registered by `p`. Used by PackageRefsGenerator.
  let mutable _lookup : Map<string list * string, string> = Map []

  let private p modules name : string =
    let fqn = $"""type/{String.concat "." modules}.{name}"""
    let hash =
      hashes
      |> Map.tryFind fqn
      |> Option.defaultWith (fun () ->
        Exception.raiseInternal "PackageRefs: type hash not found" [ "fqn", fqn ])
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result = p [ "Result" ] "Result"
    let option = p [ "Option" ] "Option"

    let int8ParseError = p [ "Int8" ] "ParseError"
    let uint8ParseError = p [ "UInt8" ] "ParseError"
    let int16ParseError = p [ "Int16" ] "ParseError"
    let uint16ParseError = p [ "UInt16" ] "ParseError"
    let int32ParseError = p [ "Int32" ] "ParseError"
    let uint32ParseError = p [ "UInt32" ] "ParseError"
    let int64ParseError = p [ "Int64" ] "ParseError"
    let uint64ParseError = p [ "UInt64" ] "ParseError"
    let int128ParseError = p [ "Int128" ] "ParseError"
    let uint128ParseError = p [ "UInt128" ] "ParseError"
    let floatParseError = p [ "Float" ] "ParseError"
    let uuidParseError = p [ "Uuid" ] "ParseError"

    module Http =
      let request = p [ "Http" ] "Request"
      let response = p [ "Http" ] "Response"

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader = p [] "BadHeader"
      let badUrlDetails = p [] "BadUrlDetails"
      let requestError = p [] "RequestError"
      let response = p [] "Response"

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part = p [ "JsonPath"; "Part" ] "Part"
        let parseError = p [] "ParseError"

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError = p [ "ParseError" ] "ParseError"
      let json = p [] "Json"

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome = p [] "ExecutionOutcome"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os = p [] "OS"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers = p [ "Modifiers" ] "Modifiers"
        let key = p [ "Key" ] "Key"
        let keyRead = p [ "KeyRead" ] "KeyRead"

  module Builtins =
    let private p addl = p ("Builtins" :: addl)
    let purity = p [] "Purity"
    let paramInfo = p [] "ParamInfo"
    let functionInfo = p [] "FunctionInfo"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign = p [] "Sign"
    let builtinValue = p [] "BuiltinValue"
    let builtinFnParam = p [] "BuiltinFunctionParameter"
    let builtinFn = p [] "BuiltinFunction"

    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point = p [] "Point"
      let range = p [] "Range"
      let parsedNode = p [] "ParsedNode"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule = p [] "PTCliScriptModule"

    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let hash = p [] "Hash"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package = p [] "Package"
        let fqTypeName = p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin = p [] "Builtin"
        let fqValueName = p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin = p [] "Builtin"
        let fqFnName = p [] "FQFnName"

      let nameResolutionError = p [] "NameResolutionError"
      let nameResolution = p [] "NameResolution"
      let typeReference = p [] "TypeReference"
      let letPattern = p [] "LetPattern"
      let matchPattern = p [] "MatchPattern"
      let stringSegment = p [] "StringSegment"
      let dval = p [] "Dval"
      let knownType = p [] "KnownType"
      let valueType = p [] "ValueType"
      let applicableNamedFn = p [] "ApplicableNamedFn"
      let applicableLambda = p [] "ApplicableLambda"
      let applicable = p [] "Applicable"

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module Bools =
          let error = p [ "Bools" ] "Error"
        module Ints =
          let error = p [ "Ints" ] "Error"
        module Strings =
          let error = p [ "Strings" ] "Error"
        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error = p [] "Error"
        module Dicts =
          let error = p [ "Dicts" ] "Error"
        module Lets =
          let error = p [ "Lets" ] "Error"
        module Matches =
          let error = p [ "Matches" ] "Error"
        module Enums =
          let error = p [ "Enums" ] "Error"
        module Records =
          let error = p [ "Records" ] "Error"
        module Applications =
          let error = p [ "Applications" ] "Error"
        module Statements =
          let error = p [ "Statements" ] "Error"
        module Unwraps =
          let error = p [ "Unwraps" ] "Error"
        module Jsons =
          let error = p [ "Jsons" ] "Error"
        module CLIs =
          let error = p [ "CLIs" ] "Error"

        let error = p [] "Error"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let hash = p [] "Hash"
      let nameResolutionError = p [] "NameResolutionError"
      let nameResolution = p [] "NameResolution"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package = p [] "Package"
        let fqTypeName = p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin = p [] "Builtin"
        let fqValueName = p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin = p [] "Builtin"
        let fqFnName = p [] "FQFnName"

      let typeReference = p [] "TypeReference"
      let letPattern = p [] "LetPattern"
      let matchPattern = p [] "MatchPattern"
      let matchCase = p [] "MatchCase"
      let stringSegment = p [] "StringSegment"
      let binaryOperation = p [] "BinaryOperation"
      let infixFnName = p [] "InfixFnName"
      let infix = p [] "Infix"
      let pipeExpr = p [] "PipeExpr"
      let expr = p [] "Expr"
      let deprecation = p [] "Deprecation"
      let packageLocation = p [] "PackageLocation"
      let locatedItem = p [] "LocatedItem"

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField = p [] "RecordField"
        let enumField = p [] "EnumField"
        let enumCase = p [] "EnumCase"
        let definition = p [] "Definition"
        let typeDeclaration = p [] "TypeDeclaration"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let packageType = p [] "PackageType"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let packageValue = p [] "PackageValue"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let parameter = p [] "Parameter"
        let packageFn = p [] "PackageFn"

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType = p [] "EntityType"
        let searchDepth = p [] "SearchDepth"
        let searchQuery = p [] "SearchQuery"
        let searchResults = p [] "SearchResults"

      let packageOp = p [] "PackageOp"
      let itemKind = p [] "ItemKind"
      let propagateRepoint = p [] "PropagateRepoint"
      let secret = p [] "Secret"
      let db = p [] "DB"

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval = p [] "CronInterval"
        let spec = p [] "Spec"
        let handler = p [] "Handler"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let errorMessage = p [] "ErrorMessage"

  module Cli =
    let executionError = p [ "Cli"; "ExecutionError" ] "ExecutionError"
    let script = p [ "Cli"; "Scripts" ] "Script"

  module DarkPackages =
    let stats = p [ "DarkPackages" ] "Stats"

  module SCM =
    let private p addl = p ("SCM" :: addl)

    module Branch =
      let private p addl = p ("Branch" :: addl)
      let branch = p [] "Branch"

    module Merge =
      let private p addl = p ("Merge" :: addl)
      let mergeError = p [] "MergeError"

    module PackageOps =
      let private p addl = p ("PackageOps" :: addl)
      let commit = p [] "Commit"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program = p [] "Program"

    module Infra =
      let tableSize = p [ "Infra" ] "TableSize"


module Fn =
  /// All fn refs registered by `p`. Used by PackageRefsGenerator.
  let mutable _lookup : Map<string list * string, string> = Map []

  let private p modules name : string =
    let fqn = $"""fn/{String.concat "." modules}.{name}"""
    let hash =
      hashes
      |> Map.tryFind fqn
      |> Option.defaultWith (fun () ->
        Exception.raiseInternal "PackageRefs: fn hash not found" [ "fqn", fqn ])
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module HttpClient =
      let request = p [ "HttpClient" ] "request"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr = p [ "TestParsing" ] "parsePTExpr"
      let parsePTSourceFileWithOps = p [ "TestParsing" ] "parsePTSourceFileWithOps"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseForCli = p [] "parseForCli"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let dval = p [] "dval"
      let fnName = p [] "fnName"
      let typeReference = p [] "typeReference"

      module Dval =
        let private p addl = p ("Dval" :: addl)
        let valueTypeName = p [] "valueTypeName"

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString = p [] "toString"
        let toErrorMessage = p [] "toErrorMessage"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let sourceFile = p [] "sourceFile"

  module Cli =
    let executeCliCommand = p [ "Cli" ] "executeCliCommand"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile = p [] "parseSingleTestFromFile"
