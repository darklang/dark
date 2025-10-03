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
module LibExecution.PackageIDs

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
        "f55eca9a45989118be50a8511cdc9af5266934fb22359e53577e82316b6e3aca"
    let option =
      p
        [ "Option" ]
        "Option"
        "b60a9ff7cfa63bb6eb31c9038a1e87a0efd6dc9b98a925bcc8eb58ea9dfbf5b7"

    let int8ParseError =
      p
        [ "Int8" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let uint8ParseError =
      p
        [ "UInt8" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let int16ParseError =
      p
        [ "Int16" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let uint16ParseError =
      p
        [ "UInt16" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let int32ParseError =
      p
        [ "Int32" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let uint32ParseError =
      p
        [ "UInt32" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let int64ParseError =
      p
        [ "Int64" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let uint64ParseError =
      p
        [ "UInt64" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let int128ParseError =
      p
        [ "Int128" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let uint128ParseError =
      p
        [ "UInt128" ]
        "ParseError"
        "81dc45d6e3e2de81f7d18d78afc8fa12ab0c1e20c4ea1f60f3b23e0633dc0ce0"
    let floatParseError =
      p
        [ "Float" ]
        "ParseError"
        "b81811e4f70c1abf79dafa496d5d6b2e8f48664ff17f6c318e5463dc809c1dc8"
    let uuidParseError =
      p
        [ "Uuid" ]
        "ParseError"
        "b81811e4f70c1abf79dafa496d5d6b2e8f48664ff17f6c318e5463dc809c1dc8"

    module Http =
      let request =
        p
          [ "Http" ]
          "Request"
          "bb4c497134c251dd99dd0c3a8377902a46ec003da995e022dfcf265a2a1c9ea1"
      let response =
        p
          [ "Http" ]
          "Response"
          "99f8eb662f0e21aaccc1b94af28a39135b45cbc7a7d4b5a1b0b67ce841dbfb35"

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader =
        p
          []
          "BadHeader"
          "11b26c4a3bd6acd6e0b589da85d5dbfb1c1b620e6b88315c3c628a773371b8fc"
      let badUrlDetails =
        p
          []
          "BadUrlDetails"
          "47b1f4a1322b5d5991f21099f0117c700174a4d56576c3fcb7f94fda7e7c438a"
      let requestError =
        p
          []
          "RequestError"
          "e13688733793d4bbd576b88c53c715c962e9b7fdf5a937432efd239eafd0d46d"
      let response =
        p
          []
          "Response"
          "99f8eb662f0e21aaccc1b94af28a39135b45cbc7a7d4b5a1b0b67ce841dbfb35"

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part =
            p
              [ "JsonPath"; "Part" ]
              "Part"
              "f5a5d19c87f1a94170babdbb35d80d07ece991d052354c4c7b61bbca2a9dbc80"
        let jsonPath =
          p
            []
            "JsonPath"
            "1ce582babd222b96883a9f8a06604a12fc1a89171a803a2ef9f82e891eb8"
        let parseError =
          p
            []
            "ParseError"
            "ee09c5c09c725fc4a044cbf7412aa4899d750d7f80a651849186682633ddc51b"

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError =
        p
          [ "ParseError" ]
          "ParseError"
          "47f130cf50b25ecd00ceca1f0dcfac826044faf85b3a49e9d25f5f5a9979ce16"
      let json =
        p
          []
          "Json"
          "d17b31fe2f131d01c61f98fcdbf8b9b8696aac398549bf97d4dfe1539e61f603"

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome =
        p
          []
          "ExecutionOutcome"
          "8549bfb4d8876e2dd3fdc00729f9d07237f2c835137d9d540d46831f6993ff7a"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os =
          p
            []
            "OS"
            "3bfa72b0325d348443aa38773c07117026c8826819e288cae059c00aebc1cc2c"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers =
          p
            [ "Modifiers" ]
            "Modifiers"
            "82febfbf48bde74688691c09e1482434abbd46ee0651c20fd6cb07bd836d5de8"
        let key =
          p
            [ "Key" ]
            "Key"
            "16ad74e8c94ee8ee33f101fe609b652a873e379e0500e817eca1ea298473570d"
        let keyRead =
          p
            [ "KeyRead" ]
            "KeyRead"
            "9c5b91901c3ca18fd880891d4a0e4f9412cf88cee5e69ef831028b408516c14b"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      p [] "Sign" "7c8a000af9b525376396cbee66c53cb5662edcb06d3a43d366db11a0660a58f7"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      p
        []
        "BuiltinValue"
        "5fa0c3b2f302601b676f543bb8560c842a3904488a099a6218ecac92962aa40d"
    let builtinFnParam =
      p
        []
        "BuiltinFunctionParameter"
        "d8546eefaf46da96917f56973c92d66a26c7e12cfca77f6f520e338cc9ced6ba"
    let builtinFn =
      p
        []
        "BuiltinFunction"
        "9ff1d24e5d03749ed0908bd7122dc0b75700220dca5174533c34c95fec4edd6a"


    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point =
        p
          []
          "Point"
          "b3d77f08cf1dfd12dc0c8f0786dc48e935f3c043f1bc55df0135a61b02ea73f6"
      let range =
        p
          []
          "Range"
          "733400e2c3ce7f39df3da02c4edb65fef6a4bd4d316de9b7974a02f12833683b"
      let parsedNode =
        p
          []
          "ParsedNode"
          "894aed4030ac9ca9be33b58b33140b285c992e5c3512cba23a885809df55f8d4"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule =
          p
            []
            "PTCliScriptModule"
            "af9793f49441a23ff0531ce5a22c244ea1de43c82361d776adcf272eaa7999f2"


    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      let nameResolverOnMissing =
        p
          []
          "OnMissing"
          "c6016893fc4c6e7c2257a6d873fe03f5102c22388227f8ddb56e1e574c0d3f9c"


    module WrittenTypes =
      let private p addl = p ("WrittenTypes" :: addl)
      let name =
        p
          []
          "Name"
          "81d7ec24bd1e0bec87c23bc62c75a3bea942a399a969bc2c3fb1705d88aea724"
      let range =
        p
          []
          "Range"
          "e015d465d31e2c845197c06d453ee38cff355065705972bfceafcb94962c12dc"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          p
            []
            "Package"
            "3122a85499ff9c6a6d8d0963e419eb0c376eb8307bfcb92ad5e95794891bb80f"
        let builtin =
          p
            []
            "Builtin"
            "0317989ec75f36cc9730a2d639661f55a0eff917bc90846782d98fed64abdc91" //TODO this isn't available in the db
        let fqTypeName =
          p
            []
            "FQTypeName"
            "378f3ae574a2eb47efaa9be909013682799181f4c438115bb0e4ec653c57009a"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let package =
          p
            []
            "Package"
            "3122a85499ff9c6a6d8d0963e419eb0c376eb8307bfcb92ad5e95794891bb80f"
        let builtin =
          p
            []
            "Builtin"
            "0317989ec75f36cc9730a2d639661f55a0eff917bc90846782d98fed64abdc91"
        let fqValueName =
          p
            []
            "FQValueName"
            "3f75d7df66419896c78cddc1bf7d79f7d05e2ad6e0237ee9e67010b072a9ab0b"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let package =
          p
            []
            "Package"
            "3122a85499ff9c6a6d8d0963e419eb0c376eb8307bfcb92ad5e95794891bb80f"
        let builtin =
          p
            []
            "Builtin"
            "0317989ec75f36cc9730a2d639661f55a0eff917bc90846782d98fed64abdc91"
        let fqFnName =
          p
            []
            "FQFnName"
            "3f75d7df66419896c78cddc1bf7d79f7d05e2ad6e0237ee9e67010b072a9ab0b"

      let nameResolutionError =
        p
          []
          "NameResolutionError"
          "98d13bcc9459d7d18e4d0a7aed7d99b1a5cf82087023f495cb651abe14d5ab44"
      let nameResolution =
        p
          []
          "NameResolution"
          "55fbda7e38772ec7473f472a23ca9449ed67d7a420c3b4003f44ad675cfdacbd"

      let typeReference =
        p
          []
          "TypeReference"
          "66259e9ec2767e01fe356dfa886d198677669b77171ceecf73f4f41d4f90e91c"
      let param =
        p
          []
          "Param"
          "33afa9180045d083fd9c20768952e0116099f76ef4a8ce545c3167bc2358e4e9" // todo not found in db
      let letPattern =
        p
          []
          "LetPattern"
          "c46100475dfd676a5369172fb9776ca8362c1f4f8fc3586e599e78d269e4211a"
      let matchPattern =
        p
          []
          "MatchPattern"
          "211bc8d5bb823a0731012a3bd444070f41dbbddf92a14389f7799517ad100c07"
      let matchCase = p [] "MatchCase" "5fb0f282-5f7c-4fb8-b107-b63429080e69" // todo not found in db
      let stringSegment =
        p
          []
          "StringSegment"
          "4aa15849bc684e71ab0e79a0e3d35d7064f141fa23c73f4a2275ecc87f66e246"

      let dval =
        p
          []
          "Dval"
          "6a2a4f136473a3fadc153cb9bb86ab5c53b851e779b32c535af8a40d8b51dc59"
      let knownType =
        p
          []
          "KnownType"
          "f71b115abbd5365944b23e34a2fcdbb9a9132a65cf6f74c3344c041d506f82d5"
      let valueType =
        p
          []
          "ValueType"
          "dbab70c683be63e3c2aec7194ec68f5f4bb7e7272a2b9fa0cfff686269b8b7b6"

      let applicableNamedFn =
        p
          []
          "ApplicableNamedFn"
          "130df184098de77d3c6e452b8b251ad1e27071aba8129163938877cd9b0aa16d"
      let applicableLambda =
        p
          []
          "ApplicableLambda"
          "6cde5c701541dc5fb5a339715f2f2add5ccf085311d95ba399b56ccd6a59e991"
      let applicable =
        p
          []
          "Applicable"
          "9d149b60b36a76d286a3bc8a9c24fb9ef9b0ee896d714fd3a0660df1fb31867d"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module TypeCheckers =
          let private p addl = p ("TypeCheckers" :: addl)
          let pathPart =
            p
              []
              "PathPart"
              "f15861b29b44f02f39fbe103c683e241673367ca0fd5c8314680a86e14f2d47e" // todo not found in db
          let path =
            p
              []
              "Path"
              "09ef9d9a3f9a8e573df0014ea0008f2a31c7b2785e2a02504002f1b713de91e7" // todo not found in db
          let error = p [] "Error" "c5dbd3b4-38d4-40e7-8147-48e3ec64b028" // todo not found in db

        module Bools =
          let error =
            p
              [ "Bools" ]
              "Error"
              "62814a04c98d41eda21fcea1181819c79b2d8400dfb6c2e2ff6d88776b590167"

        module Ints =
          let error =
            p
              [ "Ints" ]
              "Error"
              "e0f14b0d5edde4d6900e9d11e2b3e9a579077312f0cbfdc98d95a3790c4af528"

        module Strings =
          let error =
            p
              [ "Strings" ]
              "Error"
              "99e50b5abc9bcad9ea3ad95e2679b5148b7e037cdda3c86355f4ab83bf714838"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            p
              []
              "Error"
              "5e40d55dde689fc6dabd14dc4093f4946f014e9545e4afc17a90b42b4ee08e1c"

        module Dicts =
          let error =
            p
              [ "Dicts" ]
              "Error"
              "086363e865264d2702ce41c006672a1f754eb6e7efa277d7399681c49c3acf5e"

        module Lets =
          let error =
            p
              [ "Lets" ]
              "Error"
              "e8719cc07aec598923067b67c187c1f6ec7c58873961a2f9ca7509ac80a1ef1b"

        module Matches =
          let error =
            p
              [ "Matches" ]
              "Error"
              "88bf7366602831563d04b1350e03c468430c22afe05e4bf18fce0dc6756cbb91"

        module Enums =
          let error =
            p
              [ "Enums" ]
              "Error"
              "4dd0334fbbc8eb0b65dd562bd27da155b80e32cae050656b1987724cecdaaddc"

        module Records =
          let error =
            p
              [ "Records" ]
              "Error"
              "b9f34d326b7512d24952009dbc9603b9bff9fbe41d330f7fe4dab514b10e163e"

        module Applications =
          let error =
            p
              [ "Applications" ]
              "Error"
              "66b9cf05c82cca0206f3fd7b22810742fac80fb3b4d84685681e3b947bc34eb4"

        module Statements =
          let error =
            p
              [ "Statements" ]
              "Error"
              "897d09ae39f30d7a7210bd9784e31f254838334853f44d97e1fe27a625331914"

        module Unwraps =
          let error =
            p
              [ "Unwraps" ]
              "Error"
              "defafe0bf2833fb9641cfb03f3c1c5b68b7f5857f05cd8646288b86126de64e6"

        module Jsons =
          let error =
            p
              [ "Jsons" ]
              "Error"
              "9187ebd9d2fa4d45d1a26ae8903a1f14938ea4b2bf179382d29b89bfab113cb2"

        module CLIs =
          let error =
            p
              [ "CLIs" ]
              "Error"
              "f56dcf1d5005ef4c445be69a92b8ae068c7aaa16e408daa64103579ad6579c2a"

        let error =
          p
            []
            "Error"
            "8319c90d9cea59cb1c1512fe07a3c1cfde45d0a9b2a08ed5cad4dd816599b00f"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let nameResolutionError =
        p
          []
          "NameResolutionError"
          "98d13bcc9459d7d18e4d0a7aed7d99b1a5cf82087023f495cb651abe14d5ab44"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          p
            []
            "Package"
            "3122a85499ff9c6a6d8d0963e419eb0c376eb8307bfcb92ad5e95794891bb80f"
        let fqTypeName =
          p
            []
            "FQTypeName"
            "378f3ae574a2eb47efaa9be909013682799181f4c438115bb0e4ec653c57009a"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          p
            []
            "Builtin"
            "0317989ec75f36cc9730a2d639661f55a0eff917bc90846782d98fed64abdc91"
        let package =
          p
            []
            "Package"
            "3122a85499ff9c6a6d8d0963e419eb0c376eb8307bfcb92ad5e95794891bb80f"
        let fqValueName =
          p
            []
            "FQValueName"
            "3f75d7df66419896c78cddc1bf7d79f7d05e2ad6e0237ee9e67010b072a9ab0b"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          p
            []
            "Builtin"
            "0317989ec75f36cc9730a2d639661f55a0eff917bc90846782d98fed64abdc91"
        let package =
          p
            []
            "Package"
            "3122a85499ff9c6a6d8d0963e419eb0c376eb8307bfcb92ad5e95794891bb80f"
        let fqFnName =
          p
            []
            "FQFnName"
            "3f75d7df66419896c78cddc1bf7d79f7d05e2ad6e0237ee9e67010b072a9ab0b"

      let typeReference =
        p
          []
          "TypeReference"
          "95fb6587054f619a1c9a316830a34d612f8ce63bb788ac45c5844f4a9d7c3d60"

      let letPattern =
        p
          []
          "LetPattern"
          "094048cb62e318f2f1ed6d1f38945bd8d762799d1595e1919314cb69ab0667cb"
      let matchPattern =
        p
          []
          "MatchPattern"
          "a54d9cc58431ad601b10ce5d013eb289131c12d2b07b2cd6f9f9080d72ce3e5a"
      let matchCase =
        p
          []
          "MatchCase"
          "e891804665806c9bd51be27924efef10ad6b69ad8844987ce2b1564b3bb81b81"
      let stringSegment =
        p
          []
          "StringSegment"
          "2dc2ad443b25097599ce5cb818b0a58a9a12bb2f71abd3cca11495064dd0b1e2"
      let binaryOperation =
        p
          []
          "BinaryOperation"
          "056d634ccb90d0292da3151f7b1412eb1b05f62d88e1f316f7deee268bae8cf2"
      let infixFnName =
        p
          []
          "InfixFnName"
          "216acefc11fbe422a477aead2c2c5ac82352c9ca873a85fecb0420e74a436885"
      let infix =
        p
          []
          "Infix"
          "60c240b8cd47317ee1f6d127bc19f963fe168f98b82032019e62fd8a90c176a3"
      let pipeExpr =
        p
          []
          "PipeExpr"
          "5966f8857dcaec771f5701366242189497bd7cd37a046bf8a92c62b2fb9d46ae"
      let expr =
        p
          []
          "Expr"
          "b6e14a652f0d9dd0ae01092dc7e680fda5bff06fb316fafcfe199d0959f40b2b"

      let deprecation =
        p
          []
          "Deprecation"
          "8e512c81a5a11239c9e0291a94406574f3188e530ecb7fde7881673169631717"

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField =
          p
            []
            "RecordField"
            "b79487427ef2e0ea2725c33a276c35e46d90de59b0c728e64e8ef7cefdf36535"
        let enumField =
          p
            []
            "EnumField"
            "68bd970c1e750aa8e1900d43e5c3c1c11b0f0e9d8ed469f14829a0d4fa083b67"
        let enumCase =
          p
            []
            "EnumCase"
            "5d81b8110607ffc2fd791866f8fb16babb47d67c15f99596b565c7cf3ec2d97a"
        let definition =
          p
            []
            "Definition"
            "cdbe277ef2cf50d4ba92e75465a95c9cca84691e9282db4c954064f2f743c449"
        let typeDeclaration =
          p
            []
            "TypeDeclaration"
            "68daff95080e9e609211e9e5f07d7da627b6c950bb29e2b9aa8a7f43f278a8e9"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let name =
          p
            []
            "Name"
            "81d7ec24bd1e0bec87c23bc62c75a3bea942a399a969bc2c3fb1705d88aea724"
        let packageType =
          p
            []
            "PackageType"
            "2d6c70d93c78a0f14f6fb57414e268b72f924674ef790226b848bf0e9ebf218c"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let name =
          p
            []
            "Name"
            "81d7ec24bd1e0bec87c23bc62c75a3bea942a399a969bc2c3fb1705d88aea724"
        let packageValue =
          p
            []
            "PackageValue"
            "e49fce1dfafa21f4d057c3abe333542d474abbaf64b3edee794850703fe7a04f"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let name =
          p
            []
            "Name"
            "81d7ec24bd1e0bec87c23bc62c75a3bea942a399a969bc2c3fb1705d88aea724"
        let parameter =
          p
            []
            "Parameter"
            "b79487427ef2e0ea2725c33a276c35e46d90de59b0c728e64e8ef7cefdf36535"
        let packageFn =
          p
            []
            "PackageFn"
            "d6bbbd576a63a181c6ee29fdabb6538a96f3f6f5b9ef3566fc7821f278e069d1"

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType =
          p
            []
            "EntityType"
            "b6496bcbbde460dfe61d8705ffd8954d1743aee76b7cf85bff2035109fc6a272"
        let searchDepth =
          p
            []
            "SearchDepth"
            "e73c201c15569c33080f4d9092a1bceb886965ea1232f762705347776a1753a4"
        let searchQuery =
          p
            []
            "SearchQuery"
            "05c577256fd07273c4d89299772c894f3c9affda9140dfcb630d3351522351c3"
        let searchResults =
          p
            []
            "SearchResults"
            "09ba7274edd2755632db025213586867aec510c007f1c69acd818c939b83ecf8"

      let secret =
        p
          []
          "Secret"
          "234fb2be1a566f56244a9194437b64fada37dbc0d0505eeaac687fcc5f662a04"
      let db =
        p [] "DB" "c01ddb84f971caac0bf5d15f9af02a8a810b410dc93c158e952f0cb28d3cf357"

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval =
          p
            []
            "CronInterval"
            "70dc286b8c189a8b3b42520d3bc933f7b8f81f1367eda6ca766495a8b5e427c7"
        let spec =
          p
            []
            "Spec"
            "9f4a16ca74567e69603a3173de7896062bd5b349551fefa6aabaad3ac7f62aab"
        let handler =
          p
            []
            "Handler"
            "65fc91c85d9504b11507883d57c51e0fbefaf54bb6c14f7627aabdd4b42998c4"

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
            "d470d15f18d89cea5f4b05f861fb6efa8e36379d148a835c54949a6469b1adf1"

  module Cli =
    let executionError =
      p
        [ "Cli"; "ExecutionError" ]
        "ExecutionError"
        "6b41a1a6fcd5b66c1ff8f590db7b21d790e216f3633e3c6263ebe3d9127dad45"

    let script =
      p
        [ "Cli"; "Scripts" ]
        "Script"
        "3b7f319c02d37f73f5aa999d153f09bc07f870d4614bcf6c6f383f243c7b95ad"


  module DarkPackages =
    let stats =
      p
        [ "DarkPackages" ]
        "Stats"
        "2dd2d39aee25ff5737e2d1be193417c7a935a5e482d9b79ff74c707ccb98af14"



  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program =
        p
          []
          "Program"
          "b48d228166b107d45c9403f8af37fc992a3853abd52c6057b904c00d184190e9"
      let secret =
        p
          []
          "Secret"
          "234fb2be1a566f56244a9194437b64fada37dbc0d0505eeaac687fcc5f662a04"

    module Infra =
      let tableSize =
        p
          [ "Infra" ]
          "TableSize"
          "c707ed3f5df3d6cadc242deacbd78acdd6d8bf5c98a739f058751198cd61652b"

    module Worker =
      let scheduleRule =
        p
          [ "Worker" ]
          "SchedulingRule"
          "35873736303e95a4b2717e0b82dee5d186dc40a25478be216f5ff532559b72be"

    module Test =
      let private p addl = p ("Test" :: addl)
      let ptTest =
        p
          []
          "PTTest"
          "f99e5cd33669e5d5fc2e227a6cbb6533a7bbe553fdea1a40aa4f1dcb8dfacf05"

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
          "78e58f1747e107797fcbd780c88c2fcd6cde9bb0301136f45315d2c0f2e0b410"

    module HttpClient =
      let request =
        p
          [ "HttpClient" ]
          "request"
          "483ed8a0f8cebc5e694d53997e9b11133205c4bf79c2a3f7016bd9b991274473"


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
            "2c2706131ca722aef8a0098ad7bf51b667d47371c56ec4e9364e27cc74c08194"



    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr =
        p
          [ "TestParsing" ]
          "parsePTExpr"
          "65680c6cb0b74f2b939053da3356492a7d6a160b057aa66f68e356eb5d476557"

      let parseAndPrettyPrint =
        p
          [ "TestParsing" ]
          "parseAndPrettyPrint"
          "dc803dce95685c60cb1969b339b89a9b5a20da58372eb36ae16f322f71b99328"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseCliScript =
          p
            []
            "parse"
            "192bb8c40f263a368ed2fcb110434647d6e4da5827cae82b5a870beb2dc78787"

    module PackageManager =
      let private p addl = p ("PackageManager" :: addl)
      let pm =
        p [] "pm" "61f10038fd6136b3da8adffda2a16722c429cb67d4e10e86225a0053ebb06f28"

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
            "6b8dcd951707bd42fb298e4648ae9990853b7cb436d481e7a2f24b1a5f7fdb9a"
        let toErrorMessage =
          p
            []
            "toErrorMessage"
            "ace7680a9c8d4b9fe54f2f2927ddba350d4d9d6df45bb76cea8550547335e2d6"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let fullForReference =
          p
            []
            "fullForReference"
            "179fb10855b04a7575e944994155f431b1520b56dc30a3d3f49a804c19e5a990"

  module Cli =
    let executeCliCommand =
      p
        [ "Cli" ]
        "executeCliCommand"
        "cedb590f9650ab63dba2c77cac6c30fea48413daf8066632f07c1c4938eb6ed1"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        p
          []
          "parseSingleTestFromFile"
          "9e637955c70942b3407a3987ade92dc56c0f9200e8c47b4658a8cf2b5a135d77"

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
