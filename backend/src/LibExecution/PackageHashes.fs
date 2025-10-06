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
/// These are SHA-256 hashes of the package definitions, computed and stored in the database.
///
/// Note: all of these types are assumed to be owned by the Darklang user
module LibExecution.PackageHashes

open Prelude

// The way this is set up, we provide the name of the package item, with the hash.
// Doing this at once helps things to be legible, and makes sure you don't forget
// to add it to a separate lookup table. Maybe it's not ideal to use mutation so
// much, but it seems kinda reasonable.


module Type =
  let mutable private _lookup = Map []
  let private p modules name (hash : string) : Hash =
    let h = Hash hash
    _lookup <- _lookup |> Map.add (modules, name) h
    h

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result =
      p
        [ "Result" ]
        "Result"
        "f2ca0aff55b71c247fd286f61786e51d0ebc89aaac025704404920fc85f52745"
    let option =
      p
        [ "Option" ]
        "Option"
        "98cf8ebdbf66c26968134a3ddf1aca90dd0684cd94050b82666b533d22cc4618"

    let int8ParseError =
      p
        [ "Int8" ]
        "ParseError"
        "723d3b12d346bbc6318b22a167d6a25bc1a5224fd876a42ac47418f21e6d0d04"
    let uint8ParseError =
      p
        [ "UInt8" ]
        "ParseError"
        "7865f4fadff990e9e5944abae8a609aeefdd6c56ef152c323b86a78d2815ecfb"
    let int16ParseError =
      p
        [ "Int16" ]
        "ParseError"
        "add778da274300c79e68c0cc5ddaa95df679c46f0735aeac0bd51510f30b645c"
    let uint16ParseError =
      p
        [ "UInt16" ]
        "ParseError"
        "69cbec68701c6f7ebeadbf7cfe4d7a96693166def207f1f46ac0ba2b5d5fc23b"
    let int32ParseError =
      p
        [ "Int32" ]
        "ParseError"
        "3be67cdb4177a571ab07179d5e542ec36465a7c1b05d2ff111ac58eb06c13831"
    let uint32ParseError =
      p
        [ "UInt32" ]
        "ParseError"
        "bdc05b4062460e953cb3e00a7ae7917e69ee068b2835f1291be6f8cb6a9f0c91"
    let int64ParseError =
      p
        [ "Int64" ]
        "ParseError"
        "ccadf6866458212e7af3852b0406652bc9a8311b76dd963275e579021e5d8ad1"
    let uint64ParseError =
      p
        [ "UInt64" ]
        "ParseError"
        "6f25d1208b6a9e8089958d21dbc5c0848ac985609786631b877f0c99940d1edb"
    let int128ParseError =
      p
        [ "Int128" ]
        "ParseError"
        "d22af0a98784721ee661da7eaaa7e078fd74ebfc3f199f2b68006bf0b067a5dc"
    let uint128ParseError =
      p
        [ "UInt128" ]
        "ParseError"
        "f8588dfcad3f7bdcc80c91b3f0c5469ca585213338ddcc1896081624510fac0c"
    let floatParseError =
      p
        [ "Float" ]
        "ParseError"
        "18a0d38830f3c20ba2b0c0bd1d51b7c2f4cb18c5fc8a1956e90c97156164b08b"
    let uuidParseError =
      p
        [ "Uuid" ]
        "ParseError"
        "b5c1dc828bd2df5052cc3bfac031342402ff6ca41837ac757bd76ffd695c2e80"

    module Http =
      let request =
        p
          [ "Http" ]
          "Request"
          "24517efd8e7b0748026bc7037cc32c609684a7d8519d045ef25d8ca3339ebdb2"
      let response =
        p
          [ "Http" ]
          "Response"
          "c681aec35d1284bee3399bf585dc5cf665dd782318e5ce14f9d44444d813e942"

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader =
        p
          []
          "BadHeader"
          "34556d311f03747434e7d4ec9abf7da79f41918abf2fbea710760240946512e8"
      let badUrlDetails =
        p
          []
          "BadUrlDetails"
          "d4a499f2cdf102fb77358ad1514b01c4fbbf5ca506ee56e9f0216c3ec1c7fb17"
      let requestError =
        p
          []
          "RequestError"
          "976ec33550bfefc0d1d7df3f1e4838ed686a4bf5c94ce7fa9b54a12917eb9ca5"
      let response =
        p
          []
          "Response"
          "25125d07e30e145a0bdc9dfd101721ec8c97ac7d9273fce148567264f434c059"

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part =
            p
              [ "JsonPath"; "Part" ]
              "Part"
              "54cd794088c75d4935814dcb1c8bc25de829cc543681df8b141725fdb4c37295"
        let jsonPath =
          p
            []
            "JsonPath"
            "1c0aa81ac92ff89628b40a83d6fa03f705815f2a40153469cb98a8f806271fae"
        let parseError =
          p
            []
            "ParseError"
            "9e270f0b38a396ab3db27e1b68475bff9f7fe95bda020b59312a39fb7ded46b9"

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError =
        p
          [ "ParseError" ]
          "ParseError"
          "f2ec4315189875aa1ad2f2fdc840723c06cf340585bb303853613ada1f56d73f"
      let json =
        p
          []
          "Json"
          "398e73cc62c5e5e83bd88bbc425ac0315674e7c4c3b997d7bfc7d3a99bc8057d"

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome =
        p
          []
          "ExecutionOutcome"
          "d0d7902c552c70983f9421a08d755c4718a15963fe626ef361e90a34146326b9"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os =
          p
            []
            "OS"
            "554cdb23faa85cc315312eaee8e5fa9cf809f7e19162b30c38618381869654b7"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers =
          p
            [ "Modifiers" ]
            "Modifiers"
            "2d96a56417ea47c08474b4c86a8d7e487ab334feb0239847d84f7c9f392c0e1e"
        let key =
          p
            [ "Key" ]
            "Key"
            "1f8b254aba63d966e97335de86ea9bc170536f4ad97168d0eb934e1cf02492b3"
        let keyRead =
          p
            [ "KeyRead" ]
            "KeyRead"
            "3063347055790766cd54c03d137fddfa3c1daf9f33fc74bc2bab4a51928e169d"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      p [] "Sign" "de4e0fc727cbd3971209447b661626f3721814267628a539b7203ca30380c4ef"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      p
        []
        "BuiltinValue"
        "9a1aa08b60f37e143e3d058422c8d0eb301bdd78fc51902c4f351ee51e80383f"
    let builtinFnParam =
      p
        []
        "BuiltinFunctionParameter"
        "2b014f76be8d5f0ccd0fcb213232f0bd73d12af93e5a70e203225b09061e29a7"
    let builtinFn =
      p
        []
        "BuiltinFunction"
        "60d8e5fae09c863b35e59fff1d6d0c208101807862e0a03ebc52977b4ce89b49"


    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point =
        p
          []
          "Point"
          "5e3e43eac96bdc06b27d1f195af8f2f96b48a9e61d121614c702abdeab24fbb4"
      let range =
        p
          []
          "Range"
          "b6a2c2b1f3c9c55988f6e401f66a02de7784a88befe8d44c89fc7612865dfd85"
      let parsedNode =
        p
          []
          "ParsedNode"
          "6d3d752eb9ab3c8f0d07bbf5d0dee946874145c1356723ca392a48da19296c12"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule =
          p
            []
            "PTCliScriptModule"
            "490c41248baab9a970212eab9a06f8b7b132bbd9bcb225ecbe98d695948066e0"


    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      let nameResolverOnMissing =
        p
          []
          "OnMissing"
          "0aa2ddc822db46014d15ccadbb5a4b490e8d023354790197faf868a134649c55"


    module WrittenTypes =
      let private p addl = p ("WrittenTypes" :: addl)
      let name =
        p
          []
          "Name"
          "eaa469b32f1f3986cb155bcaaab8d683f72c539fb2e9065e352c5ecf81c38536"
      let range =
        p
          []
          "Range"
          "bc9bbcdec8a500b0ce39d9cfdf702e64f8568f33a52adc29d97ce9b254aac3fe"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          p
            []
            "Package"
            "974888e01b444e390d03e65d839bf7e6f574327643dcbc2c9802ef38509750f0"
        let builtin =
          p
            []
            "Builtin"
            "0317989ec75f36cc9730a2d639661f55a0eff917bc90846782d98fed64abdc91" //TODO this isn't available in the db
        let fqTypeName =
          p
            []
            "FQTypeName"
            "bf6d74749e2fc5b75e60903a8a6af0a466bcdcad8a2fb038978b8b026afb674e"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let package =
          p
            []
            "Package"
            "d20b366b25d9ebe5675ca5af97e165f0b1058ed5e3f0d714378ca451d18a8256"
        let builtin =
          p
            []
            "Builtin"
            "532823b61dd2cb8971ff916758a278168fe15081c2193becb37f48f634d4ac91"
        let fqValueName =
          p
            []
            "FQValueName"
            "6234c6ef5c70f735e174d3155d30779bd086f3b8f96320da3776122ccfee0a12"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let package =
          p
            []
            "Package"
            "c75180954ca5d21b51f0caf5e3a4800caa0a2209e1c1281db7e528c2f23bb88b"
        let builtin =
          p
            []
            "Builtin"
            "564e119a6f502fe3cec6c506eafa2f74e64423610c6914d40ac09dd866443ba8"
        let fqFnName =
          p
            []
            "FQFnName"
            "d3afa67b930b5c6b797f3c175729431642d83afb6e2efaf41af4f22b11acfbde"

      let nameResolutionError =
        p
          []
          "NameResolutionError"
          "ae5996765368608f6ec590c18bbe6cb417caeed9d8fb7ddce3bf17de505bfde3"
      let nameResolution =
        p
          []
          "NameResolution"
          "e0c95e51c2360f5e23ea5dc0f86057728535ac8d6c3c979ebdf1ff73d539a835"

      let typeReference =
        p
          []
          "TypeReference"
          "9305fb8bfec38293e610e4d04fefdf4531267847a7c8022121e498e0f18fc90c"
      let param =
        p
          []
          "Param"
          "33afa9180045d083fd9c20768952e0116099f76ef4a8ce545c3167bc2358e4e9" // todo not found in db
      let letPattern =
        p
          []
          "LetPattern"
          "a44b34d89291bcaf6459a5b2925f3a0afcd9f9587e04a53f3b8ad5ec502eadd4"
      let matchPattern =
        p
          []
          "MatchPattern"
          "83a8142d4c26a567f6e09a4de0caf87c5f1ffdafd8de226eba7ad18af24b08f7"
      let matchCase = p [] "MatchCase" "5fb0f282-5f7c-4fb8-b107-b63429080e69" // todo not found in db
      let stringSegment =
        p
          []
          "StringSegment"
          "7ae4abd635868a80d0093957599ed4ee8b603f6df85f88663f3d6559e9e81cb8"

      let dval =
        p
          []
          "Dval"
          "4122288ce71c06d3c0d34026ddb50887ba2dc183718c082989f9571f67146227"
      let knownType =
        p
          []
          "KnownType"
          "69d35e01d4c8c7ad1d642a260c4ca19c45eadc6f1d3e862bd997b9f544415f1d"
      let valueType =
        p
          []
          "ValueType"
          "ad6deb4b5afbe711fb063d0830d06c66f5004cd79766eaeab248c060d80e664b"

      let applicableNamedFn =
        p
          []
          "ApplicableNamedFn"
          "e9bfbd4652642a474edd3ee4e516aadf585b0655f9aa4b343a100b881a84bf3e"
      let applicableLambda =
        p
          []
          "ApplicableLambda"
          "bf833aeafdbaf804945f025b7b74b3b4c68884f1e8482a3a7293b634c3b78f14"
      let applicable =
        p
          []
          "Applicable"
          "d559753f6e852a353298c05efb648ed4439ff1de01938a58e13d7d352a691544"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module TypeChecking =
          let private p addl = p ("TypeChecking" :: addl)
          let reverseTypeCheckPath =
            p
              []
              "ReverseTypeCheckPath"
              "f26bd8b03d911e1744e7466a540ae1eb0cda16911ce7a52f69ab5361a562c593"
          let typeCheckPathPart =
            p
              []
              "TypeCheckPathPart"
              "4ed370c84d4b4ecd22ab9e4fa0499fd13bd8a0ced834078e8e2c71e61327a2f1"

        module Bools =
          let error =
            p
              [ "Bools" ]
              "Error"
              "f162134852628decb707533d43976b7d32c9e2db26f6977cd5c45ed492f5145c"

        module Ints =
          let error =
            p
              [ "Ints" ]
              "Error"
              "f3bcbfbb7b681fc585749d24db8be4ed261fd227aa2a7d244f9ef1a7a4f9f295"

        module Strings =
          let error =
            p
              [ "Strings" ]
              "Error"
              "29f6c0b72b8eb1ab4fb3ead7a85f5f520991ab7fba0b87f4b13b92fb0dbb1414"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            p
              []
              "Error"
              "60a0c7bd1c83fb5f090ca12da7fc741b7a8145c1332d8ff38cd98393dfd7062f"

        module Dicts =
          let error =
            p
              [ "Dicts" ]
              "Error"
              "e2d84a46882d5268a2024235d11f58845be4f2f76db45d406653bef745aed3c0"

        module Lets =
          let error =
            p
              [ "Lets" ]
              "Error"
              "675e811a9f0cdc7c2db5fee58ff9f1957a8657bac0ff1c15a1412920166f0ae0"

        module Matches =
          let error =
            p
              [ "Matches" ]
              "Error"
              "9a4ee8c55c050238a73e339a6e77f24c1f6ebd64ade899363088450284f9da01"

        module Enums =
          let error =
            p
              [ "Enums" ]
              "Error"
              "a196688066cc9b580b1310497359871f298540afc0bd9e0d5da402cdd1768f66"

        module Records =
          let error =
            p
              [ "Records" ]
              "Error"
              "88646fa860a3a1b8c5500eb1c918fcfa5a7ab1b77068cbd54c48821a14fbc0d9"

        module Applications =
          let error =
            p
              [ "Applications" ]
              "Error"
              "45066a4b6ed2b05ed788cf4223d0c307be6f83e7143a9a81f29843619823aaf3"

        module Statements =
          let error =
            p
              [ "Statements" ]
              "Error"
              "ab2f1004694571e99a35ec0fea76b28f8806c73da70336a8f1fafcfa7aa5d4dd"

        module Unwraps =
          let error =
            p
              [ "Unwraps" ]
              "Error"
              "9e7efe45181c1de0e340968f11bfef542dd02f4c5e73d764b81d3555a4627da5"

        module Jsons =
          let error =
            p
              [ "Jsons" ]
              "Error"
              "425311d8cf1c5a303a5822ecc94029021906de4ac3ad5c5a7f2f9aeb8b569896"

        module CLIs =
          let error =
            p
              [ "CLIs" ]
              "Error"
              "74174b00a0157a7705d7f17203f6b8fbf76c5abeae1a0079b6c9b030ad30ac8d"

        let error =
          p
            []
            "Error"
            "0721149a6b713fb9eb081ed344e49ae1fc250fe7fc18afac0e1efda809d52689"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let nameResolutionError =
        p
          []
          "NameResolutionError"
          "2139de7ac2863b884b270703c86fb8282b2cea5a2c5c6718b8cf83042a50bb54"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          p
            []
            "Package"
            "1cdc669e41a999fd20658135ac133273244f6076db9d90412edfe3bde1978e30"
        let fqTypeName =
          p
            []
            "FQTypeName"
            "47c06c5916a3e71ea26fa171d49b820445079de62f5c74b4a3d92fdc1f2961fa"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          p
            []
            "Builtin"
            "7ca58cd5b97d7eb00d027794b1d469e8f29e45d05ff0e6888b2c6d8f308468de"
        let package =
          p
            []
            "Package"
            "427891785530de89a1306ea54d2a8a0b2def57c00beb92e277c86164cbfa923f"
        let fqValueName =
          p
            []
            "FQValueName"
            "25eb8dff9ae11131cca61ec0f19aa445d1cafa1f2975856f55bb43846b9cadf4"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          p
            []
            "Builtin"
            "caa5404431402161a6b63b1de55ecc9b6fa42be5eeb7ea004ca0fbb13473ff01"
        let package =
          p
            []
            "Package"
            "cf94968c6feee8634da577deed36df388d9d4f10f3ddcf7495638548822ed857"
        let fqFnName =
          p
            []
            "FQFnName"
            "5e2d23d461a644fbfc0f9110a98f46c52a108b9c17c479ac4fa00f4d87310c7c"

      let typeReference =
        p
          []
          "TypeReference"
          "6e7788e682f3f545802f4e712c5e9b154a324a472b4b92dc7589892e7c2808ee"

      let letPattern =
        p
          []
          "LetPattern"
          "c1b3a896a8a342329857f79ecbf904b59738b7be822e5664d20a97b5c799b052"
      let matchPattern =
        p
          []
          "MatchPattern"
          "7f9fd1b3c3014fad212989e50242d7a05a88943f6597285bf3263a2f44810c43"
      let matchCase =
        p
          []
          "MatchCase"
          "2d1dee96ec534c466e849f03f6a76870a3eeb7ee560891a90e6620f4953eabeb"
      let stringSegment =
        p
          []
          "StringSegment"
          "db694cec98b7550089f83f5a3d5f8b8289d74bbdeef6f100c5cbcc49188842e1"
      let binaryOperation =
        p
          []
          "BinaryOperation"
          "088ebf13936a02b162c9a293e0fbacd3157f9fad6527ebec538d51d87f7a5600"
      let infixFnName =
        p
          []
          "InfixFnName"
          "2c0c00e62370eddd68feb1b3f3fc301588cf85ef5c6fcb66d1934e3fbb927e3a"
      let infix =
        p
          []
          "Infix"
          "ef63da034364982ca83f8f536c239c954666a8d6b6ac68031051c292854284c2"
      let pipeExpr =
        p
          []
          "PipeExpr"
          "741f0ebd3ae98e71ffb459f8916c93a92bf12e98e12f8c970a259e46bc239875"
      let expr =
        p
          []
          "Expr"
          "d2baf32f80b609ffa52913703feacff82b88b867dd6505ec776a7b73961a7368"

      let deprecation =
        p
          []
          "Deprecation"
          "37c9103fa3c2c36f168721651edf92b313357e484fe91b26713cea274e651f0f"

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField =
          p
            []
            "RecordField"
            "a06c4d2abd4dac06c528d4afa7cc7abbe3fa808b064f4c852a2144e57eb7086d"
        let enumField =
          p
            []
            "EnumField"
            "d0df16f02ee120936df402645549b7c3db7a0ec93c897c1242d23c022bfc5725"
        let enumCase =
          p
            []
            "EnumCase"
            "7a6bec7d073e3a0749d2d01a6cbe7ef460688784b052fbd030e1a4ae2cc97a1d"
        let definition =
          p
            []
            "Definition"
            "89e592313c3af61a687b2f798176ccca52251b49a375ea405dfef993b9d7097b"
        let typeDeclaration =
          p
            []
            "TypeDeclaration"
            "cde67a91c47fd49bcb7c9d658e24f40dc0d893b35e2ecc9670f8450337c836e1"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let name =
          p
            []
            "Name"
            "fde1b6b064a3fd9e7c0429f669e505108ebfdf90d4037b61fd7362c17641cc96"
        let packageType =
          p
            []
            "PackageType"
            "a3a2829233b934ca70859289dfa96709ab5591df379f688f328162f7f239b960"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let name =
          p
            []
            "Name"
            "5ce2157edeabcf462a14e6deb622479b27c6c88eb720d35c14980fdd836facc2"
        let packageValue =
          p
            []
            "PackageValue"
            "043a2c2aa0fc267726aed6c3a7e162a2ade75d12a78ab4c5e6684ad7a6f75ea1"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let name =
          p
            []
            "Name"
            "804e27c34e6951e3a102cfe8d1184a0f431a98974569820e6e80f9a72269d62f"
        let parameter =
          p
            []
            "Parameter"
            "5832ff971549a97f918a412c13d27f05295bc71b7449939448f2110b89945c2e"
        let packageFn =
          p
            []
            "PackageFn"
            "fc28143135cbddb46bc0e69d20bfe523af7661664ae3b0c743e729739bbd23f9"

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType =
          p
            []
            "EntityType"
            "4c786d25c9f8c987e50b9f64958c529d18d433e6edd9564d3304922a37cd929c"
        let searchDepth =
          p
            []
            "SearchDepth"
            "a775bd255cb2aa8395ce92914b4703423cd5513d71182c3a0cdfb6bc90f83089"
        let searchQuery =
          p
            []
            "SearchQuery"
            "8a21e04bf4b6493298e9a07649fbead39ca40be3a3c30721e508ea12d32916e4"
        let searchResults =
          p
            []
            "SearchResults"
            "a23ef08ad5af1e7ce0523541505232b1c57076982443096e4c84d6e47dc6f01e"

      let secret =
        p
          []
          "Secret"
          "07d8c33e2444f1a209f6119e4eb2a3bebb6658f31e7dfd7f2ee7e078c4ed15a4"
      let db =
        p [] "DB" "ab6864b7ddbbc7d330b0958544817e0926a551937454afcce1b6f00acf50de8c"

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval =
          p
            []
            "CronInterval"
            "66edaed8c0e10841b71a29d352430899e3066168ae62185df918222ae3f355bc"
        let spec =
          p
            []
            "Spec"
            "3549e40c965145ca1b6bf87438f8210347a5537372b337ba8bdd724893da6701"
        let handler =
          p
            []
            "Handler"
            "99a37f8db14750e4ea910a442e63be70f097520148f4c9ef121d24813a515e4c"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let errorMessage =
          p
            []
            "ErrorMessage"
            "edace5ed6b5f982f033949cd4a7e37f15079840d223e9b0cb73d89e33e7e5d82"

  module Cli =
    let executionError =
      p
        [ "Cli"; "ExecutionError" ]
        "ExecutionError"
        "09e0e4d05d0c69471e4dfa129d7fdbcea9299dad12c88d5969903ba072ac81e2"

    let script =
      p
        [ "Cli"; "Scripts" ]
        "Script"
        "b85b96f94e6d5583a3414510f8d20a23ce84deb77c5d45faaf758bf5593b888a"


  module DarkPackages =
    let stats =
      p
        [ "DarkPackages" ]
        "Stats"
        "a4d6aa38f26c5714367c611ee52ecdf75699e9454d96da7d60a4a3744e435d43"



  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program =
        p
          []
          "Program"
          "4ec3372b5dfd32c9b376503a68ac50cbc91d7651bab92622a008fd712f4a4e2f"
      let secret =
        p
          []
          "Secret"
          "31a275f1f46e0cd804030e8c1a1aeffb7d5601b610765271762205715343eb10"

    module Infra =
      let tableSize =
        p
          [ "Infra" ]
          "TableSize"
          "780fc788a8c33f5edf9674756d6be6f406606491e4f1bb0e81a292b7cb36e65c"

    module Worker =
      let scheduleRule =
        p
          [ "Worker" ]
          "SchedulingRule"
          "a117f1d9577e6508b25118bf2818045901aa383ddb7ed7cd3ad2cbe301272fc5"

    module Test =
      let private p addl = p ("Test" :: addl)
      let ptTest =
        p
          []
          "PTTest"
          "b80899f88804b950574bc71e1205fd0dd04551372db992c87378ad271755ffc6"

  // what we expose to the outside world
  let idForName (owner : string) (modules : List<string>) (name : string) : Hash =
    match owner with
    | "Darklang" ->
      match Map.get (modules, name) _lookup with
      | Some hash -> hash
      | None ->
        // Generate placeholder hash for unknown types
        let placeholderString =
          $"""{owner}.{String.concat "." modules}.{name}""".ToLowerInvariant()
        let hashStr =
          System.Convert
            .ToHexString(System.Text.Encoding.UTF8.GetBytes(placeholderString))
            .ToLowerInvariant()
        Hash hashStr
    | _ ->
      // Generate placeholder hash for non-Darklang types
      let placeholderString =
        $"""{owner}.{String.concat "." modules}.{name}""".ToLowerInvariant()
      let hashStr =
        System.Convert
          .ToHexString(System.Text.Encoding.UTF8.GetBytes(placeholderString))
          .ToLowerInvariant()
      Hash hashStr


module Value =
  // There are no referenced Values at this point,
  // but we may be thankful later for hooking this up in the meantime.
  let idForName (owner : string) (modules : List<string>) (name : string) : Hash =
    // Generate placeholder hash
    let placeholderString =
      $"""{owner}.{String.concat "." modules}.{name}""".ToLowerInvariant()
    let hashStr =
      System.Convert
        .ToHexString(System.Text.Encoding.UTF8.GetBytes(placeholderString))
        .ToLowerInvariant()
    Hash hashStr



module Fn =
  let mutable private _lookup = Map []

  let private p modules name (hash : string) : Hash =
    let h = Hash hash
    _lookup <- _lookup |> Map.add (modules, name) h
    h

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module List =
      let map =
        p
          [ "List" ]
          "map"
          "539d6ca2c6ea456db05e1549ada0d60ed7234b14e1e5eff9dccb93ce092eda7a"

    module HttpClient =
      let request =
        p
          [ "HttpClient" ]
          "request"
          "9a432806825b15e7f7455c8db5efdd48154171f525ada6879fb8991886576246"


  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      module FnName =
        let private p addl = p ("FnName" :: addl)
        let resolve =
          p
            []
            "resolve"
            "cdfc3fa85082d2edf067cae2dd6a66fedc263ad3332d936610b47b18772b62d6"



    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr =
        p
          [ "TestParsing" ]
          "parsePTExpr"
          "e06604ffff616ac182a35af11060daec1a2f3a5ec209c7def5291ac1cdd49400"

      let parseAndPrettyPrint =
        p
          [ "TestParsing" ]
          "parseAndPrettyPrint"
          "69b1f0067d874180e6a9171ec78aa4dda043af6fb22752850d8366c220910c68"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseCliScript =
          p
            []
            "parse"
            "747201c477f57c719891592c9e8cd3bf537d054f7fdd56acc1a27a528d845c61"

    module PackageManager =
      let private p addl = p ("PackageManager" :: addl)
      let pm =
        p [] "pm" "654f8aa0036dfd610413a489278da6471676be35ea2dd0896d5f11d63215c8e0"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let expr =
        p
          []
          "expr"
          "d301153776b66cfb840a6d1bf1f42e5c30a35f8fdc61b3baf58c9c46bdafa0b1" // not found in db

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString =
          p
            []
            "toString"
            "73d855f72275e9c04bf7790359530555f09a9b4f25dfda52e64f628e1feacdda"
        let toErrorMessage =
          p
            []
            "toErrorMessage"
            "440939962bf0590aa0b3e78f4e3d4518ba7891eba55894eecbed30dfc1834a66"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let fullForReference =
          p
            []
            "fullForReference"
            "df1e90d6c4ad49ec172faee9bd956cd3461e753d44e37a2cfe35904d505fb354"

  module Cli =
    let executeCliCommand =
      p
        [ "Cli" ]
        "executeCliCommand"
        "99a9d6d9d9aae074746940098b2070a4b5421d389b183150d0a10f86ba9093d3"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        p
          []
          "parseSingleTestFromFile"
          "16a3337f39ad70a59fc62b577fe4c1ccb81e40c6d6115d8bab2c27c87acaaad5"

  // what we expose to the outside world
  let idForName (owner : string) (modules : List<string>) (name : string) : Hash =
    match owner with
    | "Darklang" ->
      match Map.get (modules, name) _lookup with
      | Some hash -> hash
      | None ->
        // Generate placeholder hash for unknown functions
        let placeholderString =
          $"""{owner}.{String.concat "." modules}.{name}""".ToLowerInvariant()
        let hashStr =
          System.Convert
            .ToHexString(System.Text.Encoding.UTF8.GetBytes(placeholderString))
            .ToLowerInvariant()
        Hash hashStr
    | _ ->
      // Generate placeholder hash for non-Darklang functions
      let placeholderString =
        $"""{owner}.{String.concat "." modules}.{name}""".ToLowerInvariant()
      let hashStr =
        System.Convert
          .ToHexString(System.Text.Encoding.UTF8.GetBytes(placeholderString))
          .ToLowerInvariant()
      Hash hashStr
