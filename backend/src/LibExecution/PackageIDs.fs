/// All Darklang code exists in package space, referenced by ID. In many places
/// throughout our F# codebase, we reference these IDs. (i.e. in order to return an
/// `Option` from a function, we need to know the ID of the `Option` package type).
///
/// So, we define their IDs here, and reference via those IDs. When parsing, we have
/// a lookup of name -> ID handy; if a parsed definition matches one of those names,
/// we ensure that we use the corresponding ID when saving it to the DB.
///
/// Some tests exist to ensure each of these IDs is unique.
///
/// If you need to generate some UUIDs, https://www.uuidgenerator.net is useful.
///
/// Note: all of these types are assumed to be owned by the Darklang user
module LibExecution.PackageIDs

open Prelude

// The way this is set up, we provide the name of the package item, with the ID.
// Doing this at once helps things to be legible, and makes sure you don't forget
// to add it to a separate lookup table. Maybe it's not ideal to use mutation so
// much, but it seems kinda reasonable.


module Type =
  let mutable private _lookup = Map []
  let private p modules name (id : string) : System.Guid =
    let id = System.Guid.Parse id
    _lookup <- _lookup |> Map.add (modules, name) id
    id

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result = p [ "Result" ] "Result" "c1cb018c-8264-4080-be7f-b06b8a0e1729"
    let option = p [ "Option" ] "Option" "9ce3b596-968f-44c9-bcd4-511007cd9225"

    let int8ParseError =
      p [ "Int8" ] "ParseError" "0a8a9ce5-3a3a-4146-8297-c7076dd37c53"
    let uint8ParseError =
      p [ "UInt8" ] "ParseError" "f836299f-d97c-4b27-ae8f-924a442bd3ce"
    let int16ParseError =
      p [ "Int16" ] "ParseError" "1c4e3ff7-bc2b-48c7-adf3-5131aefa2fad"
    let uint16ParseError =
      p [ "UInt16" ] "ParseError" "c894f8be-09ed-4268-b2f9-5837c45c5dca"
    let int32ParseError =
      p [ "Int32" ] "ParseError" "9650cfbc-9433-45c2-9c3d-1b8daf6724e2"
    let uint32ParseError =
      p [ "UInt32" ] "ParseError" "29cd7b48-9627-4f0f-a7dc-58d6c441b498"
    let int64ParseError =
      p [ "Int64" ] "ParseError" "ce40066f-b791-4912-96fb-d52d733c3d9b"
    let uint64ParseError =
      p [ "UInt64" ] "ParseError" "ef7e9d82-e575-48e4-b8e1-b8f3be6f4760"
    let int128ParseError =
      p [ "Int128" ] "ParseError" "42e1e0ba-67e2-46b0-8baf-06f86266d3ad"
    let uint128ParseError =
      p [ "UInt128" ] "ParseError" "688d57a6-0182-4c32-a3dc-24b5b5140055"
    let floatParseError =
      p [ "Float" ] "ParseError" "a537b642-d02b-48b8-8ce3-46a99fbfe6fd"
    let uuidParseError =
      p [ "Uuid" ] "ParseError" "3d3dd7bd-0a4e-4816-95e6-70bfafa3fb75"

    module Http =
      let request = p [ "Http" ] "Request" "926dcb16-4708-451a-9fff-d0f19166c3c8"
      let response = p [ "Http" ] "Response" "875ece77-ff5d-46c4-b68e-7d88685ead6f"

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader = p [] "BadHeader" "8d92aa0f-cce8-4556-846f-3183abfaa631"
      let badUrlDetails = p [] "BadUrlDetails" "31c0dd4a-6461-425b-84db-21064eb62235"
      let requestError = p [] "RequestError" "1f765ab5-7b59-45e6-a970-ee9f9d28c379"
      let response = p [] "Response" "973d78f8-6591-4cc3-8c81-19d58dc2fca7"

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part =
            p [ "JsonPath"; "Part" ] "Part" "705ae7c9-e726-4072-b154-7c1a33dcb386"
        let jsonPath = p [] "JsonPath" "e6d38cec-6d64-4fd9-8d20-b793a35831fb"
        let parseError = p [] "ParseError" "7b0a5338-a64a-446c-913c-6aaf15617053"

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError =
        p [ "ParseError" ] "ParseError" "c990825c-ef78-49a6-b5dd-f69a430722e1"
      let json = p [] "Json" "d80a6028-92e8-4da2-89b3-1cfc5d275024"

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome =
        p [] "ExecutionOutcome" "754e11ce-8842-4961-8b40-cdce2cd0d7c8"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os = p [] "OS" "88f1ded5-e860-4f8e-8848-da7abac5824c"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers =
          p [ "Modifiers" ] "Modifiers" "75e354e3-28a8-4086-bad8-8cea633d3358"
        let key = p [ "Key" ] "Key" "c871205d-d08d-4bfe-afa5-4b207f07182b"
        let keyRead =
          p [ "KeyRead" ] "KeyRead" "14ae96ae-a1cf-4275-aa7e-01437bf5e822"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign = p [] "Sign" "7f0c6dba-6439-4a6c-b689-838cbbd66692"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue = p [] "BuiltinValue" "b68d3b0a-d139-4933-9127-1d06712e1514"
    let builtinFnParam =
      p [] "BuiltinFunctionParameter" "8dec56ad-3b32-4f53-83d6-c86200a1d39f"
    let builtinFn = p [] "BuiltinFunction" "fd899262-dc09-46cf-a025-95f31841718c"


    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point = p [] "Point" "f4d649a4-740b-4a0a-863f-4c74c4503fad"
      let range = p [] "Range" "e194e06f-a765-483d-b6a5-16856ed547f0"
      let parsedNode = p [] "ParsedNode" "3612c001-663c-4695-9b1c-d3a1582a8057"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule =
          p [] "PTCliScriptModule" "4b7dcac5-03ee-4a15-8e97-7ce0ccc110fe"


    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      let nameResolverOnMissing =
        p [] "OnMissing" "3c6057e7-143b-4af8-96c4-c193c1ccfeb3"


    module WrittenTypes =
      let private p addl = p ("WrittenTypes" :: addl)
      let name = p [] "Name" "8381f3b9-2311-4eb3-b040-b4c144afa6f1"
      let range = p [] "Range" "1f00b6b2-e29a-4eca-a14f-cc7a56d6ec89"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package = p [] "Package" "ac7f7a1e-f81b-4940-9a21-323078751059"
        let builtin = p [] "Builtin" "67c7c385-91e9-4f37-91c8-117a0d6a1ace"
        let fqTypeName = p [] "FQTypeName" "690be187-31be-4cef-914a-397d6bd27ac3"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let package = p [] "Package" "594f972b-1461-459a-9b27-04fbd8332ce8"
        let builtin = p [] "Builtin" "e32ad5e3-156d-4c36-ab58-04528cbd9885"
        let fqValueName = p [] "FQValueName" "9998a7ea-c1c5-4246-89a4-9167bfbbd7f6"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let package = p [] "Package" "bf2a5c81-0d4d-4ee8-b50f-5491bc5724c4"
        let builtin = p [] "Builtin" "ad2bbc2a-aee5-42bf-86ef-2c9b626bb699"
        let fqFnName = p [] "FQFnName" "7defa320-7dd9-4eaa-8e55-152cb6ef9f5d"

      let nameResolutionError =
        p [] "NameResolutionError" "ada30799-1227-4902-b580-76bca80c9e92"
      let nameResolution =
        p [] "NameResolution" "aafe54e1-d970-4ce0-81a1-1569af86671f"

      let typeReference = p [] "TypeReference" "691c34cb-16c0-4013-9aed-f431ec34d36e"
      let param = p [] "Param" "dcfb5f50-2de5-4b13-aa7a-579c8338c360"
      let letPattern = p [] "LetPattern" "5ca5d251-0703-49ce-a40d-28c2e4575431"
      let matchPattern = p [] "MatchPattern" "003c6684-4f9d-4085-bdba-a7f3bea7f587"
      let matchCase = p [] "MatchCase" "5fb0f282-5f7c-4fb8-b107-b63429080e69"
      let stringSegment = p [] "StringSegment" "ccadbf5b-1802-4db7-a30b-7b9073db78cd"

      let dval = p [] "Dval" "528b682c-a249-4a50-bd93-85e1e8cb529e"
      let knownType = p [] "KnownType" "50940368-5c6b-4f0b-9966-48b9e9443f5d"
      let valueType = p [] "ValueType" "eeb27326-120b-4a71-bd13-a6dc545e5ade"

      let applicableNamedFn =
        p [] "ApplicableNamedFn" "5cbd8499-14f8-4eab-8e40-f88776e64cd3"
      let applicableLambda =
        p [] "ApplicableLambda" "ee577e2c-3e66-42f4-8600-fbe1075adba7"
      let applicable = p [] "Applicable" "b152337b-e794-4a02-9c53-e0c4b19cb479"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module TypeCheckers =
          let private p addl = p ("TypeCheckers" :: addl)
          let pathPart = p [] "PathPart" "96cebb20-4dcf-4848-80c7-55b31d91d572"
          let path = p [] "Path" "d8dd84df-c0cb-4128-a858-d2e342ca89f6"
          let error = p [] "Error" "c5dbd3b4-38d4-40e7-8147-48e3ec64b028"

        module Bools =
          let error = p [ "Bools" ] "Error" "1afb20b6-4d3f-4566-b5aa-6b2898eaa2ae"

        module Ints =
          let error = p [ "Ints" ] "Error" "8f753bfe-9e35-4a9e-a47e-c1dbb5f83037"

        module Strings =
          let error = p [ "Strings" ] "Error" "3883b503-4f5b-437b-96b3-72029ed7dc81"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error = p [] "Error" "f327ad98-ec15-4cfe-bcfe-6f0f5a444349"

        module Dicts =
          let error = p [ "Dicts" ] "Error" "0c4bfb7f-14d3-4d25-a9c0-f8638fd52acf"

        module Lets =
          let error = p [ "Lets" ] "Error" "a0fb3f52-2acd-4f07-9f10-2dfad642387d"

        module Matches =
          let error = p [ "Matches" ] "Error" "59f0d63a-c9a1-424e-9da2-a16a88a1f518"

        module Enums =
          let error = p [ "Enums" ] "Error" "9f6119bb-4423-4ba7-98e6-87fafa37a584"

        module Records =
          let error = p [ "Records" ] "Error" "5b53bb20-7a3e-4f96-bd1c-5a090b491d8d"

        module Applications =
          let error =
            p [ "Applications" ] "Error" "8eda2125-db3e-4ef8-b185-80c202f3e1e3"

        module Statements =
          let error =
            p [ "Statements" ] "Error" "b945ee85-01ca-4d82-b4a8-041cc74c389b"

        module Unwraps =
          let error = p [ "Unwraps" ] "Error" "a69d2c00-2839-449e-9d19-c4d3b5b27550"

        module Jsons =
          let error = p [ "Jsons" ] "Error" "595907db-ab8d-4fe5-b9cf-d1bd8041e9bb"

        module CLIs =
          let error = p [ "CLIs" ] "Error" "6756f735-2a6a-41ac-a6a8-6e0b7354ca1b"

        let error = p [] "Error" "722cd3b3-d6af-4d28-96f2-87afd44c3898"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let nameResolutionError =
        p [] "NameResolutionError" "de779c1d-bebc-43d9-bb3d-4c160cca62eb"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package = p [] "Package" "ad2b1288-5005-4943-a03b-caa8056a2aee"
        let fqTypeName = p [] "FQTypeName" "fa606ff4-1daf-4194-b684-62a0b1215953"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin = p [] "Builtin" "2d751fa5-042d-4c63-8c81-8c97ab0f630c"
        let package = p [] "Package" "26810c53-0af7-400d-8755-f6c1c9a61ca7"
        let fqValueName = p [] "FQValueName" "e72942a3-48dd-4f00-b451-aff4903114ba"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin = p [] "Builtin" "b6f429d0-da28-4612-b4b1-06ff1cfc19d8"
        let package = p [] "Package" "e86c8979-6412-4051-9eff-1e868c170f33"
        let fqFnName = p [] "FQFnName" "13bb2d41-3a99-4a5e-8f25-1e3bcb010263"

      let typeReference = p [] "TypeReference" "eae9e2f3-6b17-40b1-8f9d-ef6a9aa70473"

      let letPattern = p [] "LetPattern" "ae02dd2f-c10b-4b26-9c83-de1715254eef"
      let matchPattern = p [] "MatchPattern" "a8acadfe-8a57-4206-adc6-47e04590e6f5"
      let matchCase = p [] "MatchCase" "1383f092-844d-433d-aa9d-cb89e5dae0ab"
      let stringSegment = p [] "StringSegment" "0fb605e0-a278-4711-82f7-9bd8eb42d605"
      let binaryOperation =
        p [] "BinaryOperation" "dde84462-2cc6-48e1-8a08-2db584167219"
      let infixFnName = p [] "InfixFnName" "0e3b1195-74ed-43f5-9103-e10e79001c34"
      let infix = p [] "Infix" "a6045143-ecfc-4f9b-95c7-62565b86a1c5"
      let pipeExpr = p [] "PipeExpr" "580bc173-e5ef-4f3e-8d12-e88e99fc980a"
      let expr = p [] "Expr" "138c055e-34c3-47f2-a067-7d78d84fe638"

      let deprecation = p [] "Deprecation" "e6ac931d-eac0-42df-a197-a9bcf1094b09"

      let packageLocation = p [] "PackageLocation" "a8e4f2b1-3c5d-4e6f-8a9b-1c2d3e4f5a6b"
      let branchID = p [] "BranchID" "c1d2e3f4-5a6b-7c8d-9e0f-1a2b3c4d5e6f"
      let locatedItem = p [] "LocatedItem" "4869d6bc-f934-4341-8cab-4c42968c7790"

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField = p [] "RecordField" "148003cf-828c-4645-8a7a-f0fbc3fc5c81"
        let enumField = p [] "EnumField" "7f309b94-16e0-4f88-947c-a3c3254c639e"
        let enumCase = p [] "EnumCase" "1e3331c7-9d24-412d-a8cc-cd07bcd1381a"
        let definition = p [] "Definition" "7cdaa047-6395-41a3-b97a-01776c3fb476"
        let typeDeclaration =
          p [] "TypeDeclaration" "13e258cf-12ac-4a2d-ab7b-16897941812c"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let name = p [] "Name" "5207810f-879c-4e2a-84b5-73b0401213cb"
        let packageType = p [] "PackageType" "27e3390c-137a-4df2-97db-7403442835d1"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let name = p [] "Name" "f48ac737-360c-4c85-a5fb-8916c0ba4b3f"
        let packageValue = p [] "PackageValue" "e5190f19-b9de-406c-ba3e-b2f9d83998dd"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let name = p [] "Name" "2503f569-76cc-4e47-afb3-c47058654b48"
        let parameter = p [] "Parameter" "208d5c13-071e-4e83-994e-d71c5f40aa2e"
        let packageFn = p [] "PackageFn" "2b363281-c5e8-4223-abf3-5e5d6db27f8c"

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType = p [] "EntityType" "6c481427-3909-4417-b3f2-4df40b64bfcc"
        let searchDepth = p [] "SearchDepth" "138313e7-a9de-481d-a717-f8e11d730694"
        let searchQuery = p [] "SearchQuery" "3937be09-aa05-40d6-b42d-3146e9774c82"
        let searchResults =
          p [] "SearchResults" "0660f9dc-a816-4185-9e5c-f936325f83d5"

      let secret = p [] "Secret" "37427120-d71d-41f2-b094-68757570bc41"
      let db = p [] "DB" "7f219668-f8ac-4b17-a404-1171985dadf9"

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval = p [] "CronInterval" "54c7e81e-d0c0-46c3-b74c-f0205dc3febd"
        let spec = p [] "Spec" "21f6bc66-5118-433d-a654-ce81f130a45f"
        let handler = p [] "Handler" "7fb42e37-cd74-439f-8870-d6d21133f69e"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let errorMessage = p [] "ErrorMessage" "9f7f9a1f-8240-4562-9023-8699f5f3aa2c"

  module Cli =
    let executionError =
      p
        [ "Cli"; "ExecutionError" ]
        "ExecutionError"
        "1ca71c83-7a12-43ae-a973-b4fb09c3e4bc"

    let script =
      p [ "Cli"; "Scripts" ] "Script" "de1629b6-cdfa-46b1-84c0-e0c60048d93b"


  module DarkPackages =
    let stats = p [ "DarkPackages" ] "Stats" "0fcd5847-701c-4da7-9f24-adbe8b5eb397"



  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program = p [] "Program" "9494c8ca-457c-4cb3-8b10-0f29009f16c7"
      let secret = p [] "Secret" "704688f7-092d-4786-a054-233dfa630f1b"

    module Infra =
      let tableSize =
        p [ "Infra" ] "TableSize" "041736ae-9aa1-40b8-b9ea-7d593c5d0368"

    module Worker =
      let scheduleRule =
        p [ "Worker" ] "ScheduleRule" "0669d155-6352-4851-8968-ef676b18d8ad"

    module Test =
      let private p addl = p ("Test" :: addl)
      let ptTest = p [] "PTTest" "e6fc7686-68f9-4fbe-a6cb-b615dd41ee7e"

  // what we expose to the outside world
  let idForName
    (owner : string)
    (modules : List<string>)
    (name : string)
    : System.Guid =
    match owner with
    | "Darklang" ->
      match Map.get (modules, name) _lookup with
      | Some id -> id
      | None -> System.Guid.NewGuid()
    | _ -> System.Guid.NewGuid()


module Value =
  // There are no referenced Values at this point,
  // but we may be thankful later for hooking this up in the meantime.
  let idForName
    (_owner : string)
    (_modules : List<string>)
    (_name : string)
    : System.Guid =
    System.Guid.NewGuid()



module Fn =
  let mutable private _lookup = Map []

  let private p modules name (id : string) : System.Guid =
    let id = System.Guid.Parse id
    _lookup <- _lookup |> Map.add (modules, name) id
    id

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module List =
      let map = p [ "List" ] "map" "3da8ef47-9817-4d1e-be1b-b9921ccf797f"

    module HttpClient =
      let request =
        p [ "HttpClient" ] "request" "fa4a4756-cb6b-4575-b192-ec8f02f13f40"


  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      module FnName =
        let private p addl = p ("FnName" :: addl)
        let resolve = p [] "resolve" "7532eda4-f3cf-44e5-a4d6-52fed5aa63f0"



    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr =
        p [ "TestParsing" ] "parsePTExpr" "d96d3e6b-6c0d-4559-ae36-353eaf738fa9"

      let parseAndPrettyPrint =
        p
          [ "TestParsing" ]
          "parseAndPrettyPrint"
          "361fb7f2-523b-4b50-8f29-cc99d5f03e3a"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseCliScript = p [] "parse" "e7574db7-f7e6-4263-adc2-a05b14309cbc"

    module PackageManager =
      let private p addl = p ("PackageManager" :: addl)
      let pm = p [] "pm" "591fa719-7986-40b8-a57f-6b78a1d0cd66"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let expr = p [] "expr" "be111b49-95f6-4022-ad68-a64de172f3ee"

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString = p [] "toString" "75debdd5-1e4f-4437-8124-38d7afaea931"
        let toErrorMessage =
          p [] "toErrorMessage" "d861d9c4-45da-4789-8f41-b0e481422190"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let fullForReference =
          p [] "fullForReference" "ed6bacaa-4fa6-444b-9f6b-0e323e233a5d"

  module Cli =
    let executeCliCommand =
      p [ "Cli" ] "executeCliCommand" "9b4aa7ca-82f4-4fc5-be9c-bdfb97ad4ac2"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        p [] "parseSingleTestFromFile" "53f3fbc6-25fd-427a-ab0d-ba0559543c99"

  // what we expose to the outside world
  let idForName
    (owner : string)
    (modules : List<string>)
    (name : string)
    : System.Guid =
    match owner with
    | "Darklang" ->
      match Map.get (modules, name) _lookup with
      | Some id -> id
      | None -> System.Guid.NewGuid()
    | _ -> System.Guid.NewGuid()
