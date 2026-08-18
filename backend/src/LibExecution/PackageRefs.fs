/// All Darklang code exists in package space, referenced by a hash of the content.
/// In many places throughout our F# codebase, we reference these package items.
///
/// e.g. in order to return an `Option` from a Builtin, we need to know the hash of
/// the `Option` package type when constructing the `DEnum` value.
///
/// Hashes are loaded lazily from `package-ref-hashes.txt` on first access. The file lives in
/// the source tree alongside this file, and in a release binary as an embedded resource.
///
/// NOT tracked in git: an MSBuild target creates an empty one if missing (fresh clone,
/// or CI before reload-packages), and `reload-packages` fills it. Two branches that both
/// touch packages produce conflicting hashes, and a rebase on GitHub cannot regenerate
/// the file, so tracking it means a manual pull-reload-commit on every such branch.
///
/// The consequence to know: `kernelHash` fingerprints what THIS build reloaded. Two
/// machines on the same commit agree, because the hashes derive from the package sources
/// in that commit. A store that has drifted does not, and nothing here detects that.
///
/// All bindings are `unit -> string` functions (via partial application of `p`) so
/// that module initialization does not trigger file loading. This allows code that
/// depends on this module (e.g. migrations) to load without the hash file being
/// populated yet. Call sites use `PackageRefs.Type.Stdlib.option ()`.
module LibExecution.PackageRefs

open Prelude

let private parseLines (lines : string seq) =
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

/// A read failure is REPORTED, not swallowed into an empty map.
///
/// An empty map is not a benign default: every kernel package lookup then fails much
/// later with a bare `Function <64 hex> couldn't be found`, which reads like a build
/// problem and sends you looking in the wrong place. Say what went wrong, where it
/// went wrong.
let private warnOnce =
  System.Collections.Concurrent.ConcurrentDictionary<string, bool>()

let private warn (msg : string) : unit =
  if warnOnce.TryAdd(msg, true) then System.Console.Error.WriteLine msg

let private loadHashes () : Map<string, string> =
  let sourceTreePath =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "package-ref-hashes.txt")
    |> System.IO.Path.GetFullPath
  try
    if System.IO.File.Exists(sourceTreePath) then
      let content = System.IO.File.ReadAllLines(sourceTreePath)
      if content.Length = 0 then
        warn
          $"package-ref-hashes.txt is empty ({sourceTreePath}). \
            Kernel package lookups will fail; run `scripts/build/reload-packages` to regenerate it."
        Map.empty
      else
        content |> parseLines
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
        warn
          "package-ref-hashes.txt not found in the source tree or as an embedded resource. \
           Kernel package lookups will fail."
        Map.empty
  with
  | :? Exception.InternalException -> reraise ()
  | e ->
    warn
      $"package-ref-hashes.txt could not be read ({e.Message}). Kernel package lookups will fail."
    Map.empty

/// The pins that are NOT allowed to move quietly.
///
/// Ordinary pins move all the time: something below them changes, `reload-packages` rewrites the line, the
/// diff is the review. These are different in kind -- they are depended on by essentially every package, so
/// their identity moving means something changed underneath the whole tree, or hashing itself is wrong.
/// The regenerator refuses to rewrite one without `DARK_REPIN_HARDENED=1`.
///
/// Keep this list SHORT: a pin is hardened because moving it is alarming, not because it is important.
/// Harden too many and the override flag becomes routine.
let hardened : Set<string> =
  Set.ofList
    [ "type/Stdlib.Option.Option"
      "type/Stdlib.Result.Result"
      "type/LanguageTools.ProgramTypes.PackageOp"
      "type/LanguageTools.ProgramTypes.PackageLocation" ]

// Deliberately NOT hardened: `fn/Cli.executeCliCommand`. It moves whenever the CLI changes, and hardening
// a pin that moves routinely just teaches everyone the override flag.

/// Mutable hash cache. Loaded on first access, can be reloaded after
/// the hash file is regenerated (e.g. during reload-packages in CI).
let mutable private hashCache : Map<string, string> option = None

let private getHashes () : Map<string, string> =
  match hashCache with
  | Some h -> h
  | None ->
    let h = loadHashes ()
    hashCache <- Some h
    h

/// Force-reload hashes from disk. Call after PackageRefsGenerator writes
/// the hash file so that subsequent PackageRefs calls get real values.
let reloadHashes () : unit = hashCache <- Some(loadHashes ())

/// Set hashes directly from a map (for installed CLIs where the
/// source tree isn't available to write/read the hashes file).
let setHashes (hashes : Map<string, string>) : unit = hashCache <- Some hashes


module Type =
  /// All type refs registered by `p`. Used by PackageRefsGenerator.
  let mutable _lookup : Map<string list * string, string> = Map []

  /// Registers the FQN in `_lookup` at module init (for PackageRefsGenerator),
  /// but defers the actual hash lookup until the returned function is called.
  /// Returns "" if the hash file is empty (CI before reload-packages).
  let private p modules name : (unit -> string) =
    _lookup <- _lookup |> Map.add (modules, name) ""
    fun () ->
      let fqn = $"""type/{String.concat "." modules}.{name}"""
      let h = getHashes ()
      match Map.tryFind fqn h with
      | Some hash ->
        _lookup <- _lookup |> Map.add (modules, name) hash
        hash
      | None ->
        if Map.isEmpty h then
          "" // Hash file not yet populated (CI before reload-packages)
        else
          // Non-empty hash file that doesn't know this ref means the file is stale, which happens
          // whenever a ref is added (or an older binary regenerates the file in-place -- `growIfNeeded`
          // rewrites it, so running a previous release inside the source tree is enough). The fix is
          // always the same, so say it rather than making the next person find it in AGENTS.md.
          Exception.raiseInternal
            "PackageRefs: type hash not found. The hash file is stale; regenerate it with `> backend/src/LibExecution/package-ref-hashes.txt && ./scripts/build/reload-packages`"
            [ "fqn", fqn ]

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result = p [ "Result" ] "Result"
    let option = p [ "Option" ] "Option"

    let sqliteValue = p [ "Sqlite" ] "Value"

    let intParseError = p [ "Int" ] "ParseError"
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
      let streamResponse = p [] "StreamResponse"

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

      module FileSystem =
        let private p addl = p ("FileSystem" :: addl)
        let fileError = p [] "FileError"

      module Posix =
        let private p addl = p ("Posix" :: addl)
        let error = p [] "Error"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os = p [] "OS"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers = p [ "Modifiers" ] "Modifiers"
        let key = p [ "Key" ] "Key"
        let keyRead = p [ "KeyRead" ] "KeyRead"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign = p [] "Sign"
    let builtinValue = p [] "BuiltinValue"
    let builtinFnParam = p [] "BuiltinFunctionParameter"
    let builtinFn = p [] "BuiltinFunction"
    let builtinFnPurity = p [] "BuiltinFunctionPurity"

    /// The structured capability model -- the F#/Dark wire form (see
    /// `CapabilitiesToDarkTypes`).
    module Capabilities =
      let private p addl = p ("Capabilities" :: addl)
      let scope = p [] "Scope"
      let hostMatch = p [] "HostMatch"
      let urlScope = p [] "UrlScope"
      let httpRule = p [] "HttpRule"
      let execRule = p [] "ExecRule"
      let rw = p [] "RW"
      let capabilities = p [] "Capabilities"

    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point = p [] "Point"
      let range = p [] "Range"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule = p [] "PTCliScriptModule"
        let parseError = p [] "ParseError"

    /// Package-type refs for the Dark `LanguageTools.WrittenTypes` — the
    /// range-complete syntax tree consumed by the semantic highlighter / LSP.
    /// The parser's F# `LibParser.WrittenTypes` is converted 1:1 into these Dark
    /// values (Dvals) by `WrittenTypesToDarkTypes` in `Builtins.Language/Libs/Parser.fs`.
    module WrittenTypes =
      let private p addl = p ("WrittenTypes" :: addl)
      let parsedFile = p [] "ParsedFile"
      let sourceFile = p [ "SourceFile" ] "SourceFile"
      let sourceFileDeclaration = p [ "SourceFile" ] "SourceFileDeclaration"
      let typeReference = p [ "TypeReference" ] "TypeReference"
      let expr = p [] "Expr"
      let matchPattern = p [] "MatchPattern"
      let matchCase = p [] "MatchCase"
      let pipeExpr = p [] "PipeExpr"
      let letPattern = p [] "LetPattern"
      let stringSegment = p [] "StringSegment"
      let infix = p [] "Infix"
      let infixFnName = p [] "InfixFnName"
      let binaryOperation = p [] "BinaryOperation"
      let moduleIdentifier = p [] "ModuleIdentifier"
      let variableIdentifier = p [] "VariableIdentifier"
      let fnIdentifier = p [] "FnIdentifier"
      let qualifiedFnIdentifier = p [] "QualifiedFnIdentifier"
      let typeIdentifier = p [] "TypeIdentifier"
      let qualifiedTypeIdentifier = p [] "QualifiedTypeIdentifier"
      let valueIdentifier = p [] "ValueIdentifier"
      let typeReferenceBuiltin = p [ "TypeReference" ] "Builtin"
      let fnDeclaration = p [ "FnDeclaration" ] "FnDeclaration"
      let fnParameter = p [ "FnDeclaration" ] "Parameter"
      let fnNormalParameter = p [ "FnDeclaration" ] "NormalParameter"
      let fnUnitParameter = p [ "FnDeclaration" ] "UnitParameter"
      let valueDeclaration = p [ "ValueDeclaration" ] "ValueDeclaration"
      let moduleDeclaration = p [ "ModuleDeclaration" ] "ModuleDeclaration"
      let moduleDeclarationDeclaration = p [ "ModuleDeclaration" ] "Declaration"
      let typeDeclaration = p [ "TypeDeclaration" ] "TypeDeclaration"
      let typeDeclDefinition = p [ "TypeDeclaration" ] "Definition"
      let typeDeclRecordField = p [ "TypeDeclaration" ] "RecordField"
      let typeDeclEnumField = p [ "TypeDeclaration" ] "EnumField"
      let typeDeclEnumCase = p [ "TypeDeclaration" ] "EnumCase"

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
      let dval = p [] "Dval"
      let knownType = p [] "KnownType"
      let valueType = p [] "ValueType"
      let stringSegment = p [] "StringSegment"
      let instruction = p [] "Instruction"
      let instructions = p [] "Instructions"
      let lambdaImpl = p [] "LambdaImpl"
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
      let resolvedName = p [] "ResolvedName"
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
      let reference = p [] "Reference"
      let deprecationKind = p [] "DeprecationKind"
      let branchEventKind = p [] "BranchEventKind"
      let propagateRepoint = p [] "PropagateRepoint"
      let db = p [] "DB"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let errorMessage = p [] "ErrorMessage"

  module Tracing =
    let traceSummary = p [ "Tracing" ] "TraceSummary"
    let inputVar = p [ "Tracing" ] "InputVar"
    let fnCall = p [ "Tracing" ] "FnCall"
    let traceData = p [ "Tracing" ] "TraceData"

  module Cli =
    let executionError = p [ "Cli"; "ExecutionError" ] "ExecutionError"
    let unhandled = p [ "Cli"; "ExecutionError" ] "Unhandled"
    let script = p [ "Cli"; "Scripts" ] "Script"

  module DarkPackages =
    let stats = p [ "DarkPackages" ] "Stats"


module Fn =
  /// All fn refs registered by `p`. Used by PackageRefsGenerator.
  let mutable _lookup : Map<string list * string, string> = Map []

  /// Registers the FQN in `_lookup` at module init (for PackageRefsGenerator),
  /// but defers the actual hash lookup until the returned function is called.
  /// Returns "" if the hash file is empty (CI before reload-packages).
  let private p modules name : (unit -> string) =
    _lookup <- _lookup |> Map.add (modules, name) ""
    fun () ->
      let fqn = $"""fn/{String.concat "." modules}.{name}"""
      let h = getHashes ()
      match Map.tryFind fqn h with
      | Some hash ->
        _lookup <- _lookup |> Map.add (modules, name) hash
        hash
      | None ->
        if Map.isEmpty h then
          "" // Hash file not yet populated (CI before reload-packages)
        else
          Exception.raiseInternal
            "PackageRefs: fn hash not found. The hash file is stale; regenerate it with `> backend/src/LibExecution/package-ref-hashes.txt && ./scripts/build/reload-packages`"
            [ "fqn", fqn ]

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module HttpClient =
      let request = p [ "HttpClient" ] "request"
      let stream = p [ "HttpClient" ] "stream"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr = p [ "Parse" ] "parsePTExpr"
      let parsePTExprInContext = p [ "Parse" ] "parsePTExprInContext"
      let parsePTSourceFileWithOps = p [ "Parse" ] "parsePTSourceFileWithOps"

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


/// A single fingerprint for the pinned ABI: the sorted (name, hash) pairs, hashed.
///
/// `darkBuild` (a git sha) says which COMMIT wrote something. It doesn't say what
/// the PT shapes and the hashing actually were, which is the thing that decides
/// whether another build can read it -- two commits can share an ABI, and a one-line
/// change to a kernel type can break it. Someone holding nothing but a bundle and
/// sqlite should be able to tell.
///
/// Sorted before hashing so it's stable regardless of file order, and short enough
/// to eyeball.
let kernelHash () : string =
  let material =
    getHashes ()
    |> Map.toList
    |> List.sortBy fst
    |> List.map (fun (fqn, hash) -> $"{fqn}|{hash}")
    |> String.concat "\n"

  use sha = System.Security.Cryptography.SHA256.Create()
  material
  |> System.Text.Encoding.UTF8.GetBytes
  |> sha.ComputeHash
  |> System.Convert.ToHexString
  |> fun s -> s.ToLowerInvariant().Substring(0, 16)
