/// All Darklang code exists in package space, referenced by a hash of the content.
/// In many places throughout our F# codebase, we reference these package items.
///
/// e.g. in order to return an `Option` from a Builtin, we need to know the hash of
/// the `Option` package type when constructing the `DEnum` value.
///
/// So, we record their hashes here, and reference via those hashes.
///
/// As we reference newer/updated Darklang code, we need to adjust this file.
/// Run `scripts/check-packagerefs.sh` to fetch current hashes from data.db and
/// compare against this file if any hashes seem off.
///
/// Note: all of these types are assumed to be owned by the Darklang user
///
/// TODO: some bindings share the same hash (e.g. floatParseError/uuidParseError,
/// Http.response/HttpClient.response, and many RT/PT pairs like fqTypeName,
/// fqValueName, nameResolution, builtin). Could consolidate with shared constants
/// like we did with intParseErrorHash.
///
/// TODO make a LocalExec script that produces a report for quicker updating:
/// it can look at ALL Package items referenced in PackageRefs,
/// query the DB for the current hash,
/// and identify/report which hashes need updating in the .fs file.
/// I suspect the _lookup mutables will need to  be exposed some way --
/// maybe a getAll() fn per each module, just for this use. then, we won't need the
/// .sh script. update this file to mention the ./scripts/run-local-exec script
/// w/ this option, at the end.
module LibExecution.PackageRefs

open Prelude


module Type =
  let mutable private _lookup = Map []
  let private p modules name (id : string) : ContentHash =
    let hash = ContentHash id
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result =
      "03549ecc7eb39c974d3f7181c87a3ee9337fc91307ecb2c24351b0df4809ce15"
      |> p [ "Result" ] "Result"
    let option =
      "c58cf283d0e5d6634a76fa30ea5b0d29e615e1c4fe1cc2bfca27077cd72bc072"
      |> p [ "Option" ] "Option"

    // (same shape for all of these)
    let intParseErrorHash =
      "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"

    let int8ParseError = intParseErrorHash |> p [ "Int8" ] "ParseError"
    let uint8ParseError =
      intParseErrorHash
      |> p [ "UInt8" ] "ParseError"
    let int16ParseError =
      intParseErrorHash
      |> p [ "Int16" ] "ParseError"
    let uint16ParseError =
      intParseErrorHash
      |> p [ "UInt16" ] "ParseError"
    let int32ParseError =
      intParseErrorHash
      |> p [ "Int32" ] "ParseError"
    let uint32ParseError =
      intParseErrorHash
      |> p [ "UInt32" ] "ParseError"
    let int64ParseError =
      intParseErrorHash
      |> p [ "Int64" ] "ParseError"
    let uint64ParseError =
      intParseErrorHash
      |> p [ "UInt64" ] "ParseError"
    let int128ParseError =
      intParseErrorHash
      |> p [ "Int128" ] "ParseError"
    let uint128ParseError =
      intParseErrorHash
      |> p [ "UInt128" ] "ParseError"
    let floatParseError =
      "e342cc59c22ca41ab61c5bb375c04c6dbb31592d5a79491593a3b2bc522fb86b"
      |> p [ "Float" ] "ParseError"
    let uuidParseError =
      "e342cc59c22ca41ab61c5bb375c04c6dbb31592d5a79491593a3b2bc522fb86b"
      |> p [ "Uuid" ] "ParseError"

    module Http =
      let request =
        "aa54e59a71fcf69e810b5be93a89f551be91bb2bf7e3fdb7cb6b237ea7abc07c"
        |> p [ "Http" ] "Request"
      let response =
        "1973e0642c38b5733e779ec2ef931de3b7f2c453d6bcd8401b1f4b1f6b0a0b11"
        |> p [ "Http" ] "Response"

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader =
        "dbfe4487bd474fdf0ffccaf4d8f546785e1037becc1f5527696e053e5199f1b8"
        |> p [] "BadHeader"
      let badUrlDetails =
        "1dbb355daa5a3c3531e71a0682e0aa20ccf6751d136abcfdcca5def779c21eb5"
        |> p [] "BadUrlDetails"
      let requestError =
        "4c8c5b2aeb3e71c865a106de626da3345a18094ca620bc12296b071cea5445f1"
        |> p [] "RequestError"
      let response =
        "1973e0642c38b5733e779ec2ef931de3b7f2c453d6bcd8401b1f4b1f6b0a0b11"
        |> p [] "Response"

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part =
            "cfa1c52ae5a0fc661c15db09bfb3e6c129904020cc6346a04399f24deac5c7e9"
            |> p [ "JsonPath"; "Part" ] "Part"
        let parseError =
          "843e851b01435213b27134405e9a8d988468ee2af1a06e622b0dc2229eaf6745"
          |> p [] "ParseError"

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError =
        "8dc77b20b13969bee07766b01d9afa75c78fbfe780443c621742fa2021831abe"
        |> p [ "ParseError" ] "ParseError"
      let json =
        "130be4b5b81501a911e1411dde62709f53baaf503287fca223ee30b8ad55e633"
        |> p [] "Json"

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome =
        "794afc09ab3850061cd00ede88ef1ef3add83a6d1146d6428d3fb03d164a8b06"
        |> p [] "ExecutionOutcome"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os =
          "b7c0e15d61d6652c5be3cdc9360637524332b72aec228c3a771e2f3f88a8a1fe"
          |> p [] "OS"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers =
          "5473100a8e7309f3438b5ef92a99fba6518d937b3de96d2c3e4169a21e097100"
          |> p [ "Modifiers" ] "Modifiers"
        let key =
          "788e35c44bf7b0546cfce602a673dcc38ee2a2e84680e6e8df39f5c281325085"
          |> p [ "Key" ] "Key"
        let keyRead =
          "216897425c81dd012bc812dd2a7238b327486f88cc3f615cb626d1b3d234e587"
          |> p [ "KeyRead" ] "KeyRead"

  module Builtins =
    let private p addl = p ("Builtins" :: addl)
    let purity =
      "871e4962a7f55a23f35c21043f6759a09aa382972f3b5ca48d279d764695920e"
      |> p [] "Purity"
    let paramInfo =
      "dab46d3abb6707d146e8fdd6904c6c91e7a73f7522cd081f7e32eccd31a2a725"
      |> p [] "ParamInfo"
    let functionInfo =
      "57dfcf233ecbe6b216f4a38e5d1bb852d34aece0232e7c71a1f17b281ed8a2da"
      |> p [] "FunctionInfo"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      "c8276aff8c52a06bbfacac24d654f3bc3d4a2a37bb0b9ba0ef98d2b99ca56dca"
      |> p [] "Sign"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      "ce73096c08e8f876c52dba1fd74e55dbb62a28852d6e60a2516c4c026207133d"
      |> p [] "BuiltinValue"
    let builtinFnParam =
      "afc1223c6c35e8d62f4db8c5b93fe343536b194f5ef6d1040e05f8d731459b38"
      |> p [] "BuiltinFunctionParameter"
    let builtinFn =
      "3ebb4c86a0a14cb3a84c81576908066b4a607329a12239e6d7ab510837443446"
      |> p [] "BuiltinFunction"


    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point =
        "1dfa2649bed131a5185d8de6289fa0122baf52aaa4103340fc42c2c8fa2d98f6"
        |> p [] "Point"
      let range =
        "181436ebc47d4bf0c43240038de88354ada449c9e0fef8ef03d15586c1c43996"
        |> p [] "Range"
      let parsedNode =
        "543545d4551cb753b14de434dbc4559105a697f76847ed9dbdadde2e1a95b8bd"
        |> p [] "ParsedNode"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule =
          "8a560d134ca3c70379667e004bf874ddb49bc623763490cc9f23a6c27cf138b1"
          |> p [] "PTCliScriptModule"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          |> p [] "Package"
        let fqTypeName =
          "593dbef1b81d45243efe5c73d94b13caaa337425de5884e2e2fd39ecaaf7d0c7"
          |> p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqValueName =
          "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          |> p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqFnName =
          "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          |> p [] "FQFnName"

      let nameResolutionError =
        "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        |> p [] "NameResolutionError"
      let nameResolution =
        "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        |> p [] "NameResolution"

      let typeReference =
        "3e01ed7c66abe2152f3d1f92cc5f7ad3d16dafda0b4222d8038e07615585203e"
        |> p [] "TypeReference"
      let letPattern =
        "490a27236fb548b21d2ddd743c1bdeada212a75b0c33dd1f8cc0eb5873fb9fe8"
        |> p [] "LetPattern"
      let matchPattern =
        "fe3783a863d78392faf65c588d8a03c074fa5d290113f60fb6933226ff09cfaa"
        |> p [] "MatchPattern"
      let stringSegment =
        "5e2fd40f810657a36daabb43d9519966924a23aa04b6cb5bbaecdc71c7c8886d"
        |> p [] "StringSegment"

      let dval =
        "45dd9d48f6c0e0364884ed1a3b85446ed9761f0bd95885c4d08b71d2b2963e1d"
        |> p [] "Dval"
      let knownType =
        "2faeeff43f773ba4b522ca9346961148e3f65134039c06e8f47cb927d5abc5dc"
        |> p [] "KnownType"
      let valueType =
        "58f6ec12f38c83245126422a4d81032ea743e07bb53acb3548c8ef2cdfa3154b"
        |> p [] "ValueType"

      let applicableNamedFn =
        "03410751eaa6d062191f2440b1b0a5a34e7c0d8a94aef504be66f314aa947b26"
        |> p [] "ApplicableNamedFn"
      let applicableLambda =
        "527ca58594e3389c02f00f9d7785f5b49e35db0d6b4eedadaa4d678b8ccc1ecf"
        |> p [] "ApplicableLambda"
      let applicable =
        "45aad3685f9e5913f68339e1c5c7c73cc13e53019adb675053d995713e7879f3"
        |> p [] "Applicable"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module Bools =
          let error =
            "2c4fec5380d7bd949189388a3a5b9268880e5252f378e559266f005b8731bbb6"
            |> p [ "Bools" ] "Error"

        module Ints =
          let error =
            "a3c5852ebe977e62763b25487608bce2ed1903a832e035993c99fb45b5a56fdf"
            |> p [ "Ints" ] "Error"

        module Strings =
          let error =
            "4065445cac4f03cc63d507749a2c9ca8bbe6f78415e9d00f847f729d3fa0e9d1"
            |> p [ "Strings" ] "Error"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            "14b116e589789e2effe97e598b98cb071a2f0d53cde1b80caf3c4a2ec3e9fafc"
            |> p [] "Error"

        module Dicts =
          let error =
            "011d37992ad30148858cf8c9eaf9a38cd709b020e144fa718e6ccd85b2352135"
            |> p [ "Dicts" ] "Error"

        module Lets =
          let error =
            "ada72d1d039cc23504898933b705da3d2537e8cd5618812e6105daaa2973eccb"
            |> p [ "Lets" ] "Error"

        module Matches =
          let error =
            "c31d81b343ba5debb1cfdc9977f8908c1280d6ad40bf09430142534f6bd9c981"
            |> p [ "Matches" ] "Error"

        module Enums =
          let error =
            "533b70051f6898c01e017f37b668be277818aa4d97321b71b958136fe89c6f69"
            |> p [ "Enums" ] "Error"

        module Records =
          let error =
            "71a94d192184bfe7350fbce77baf53ed319802c31f7722bf18c40b14ef934e88"
            |> p [ "Records" ] "Error"

        module Applications =
          let error =
            "45d69d587df92aba8ffb29f195aa4443287557c1e509518484911fb46214ab31"
            |> p [ "Applications" ] "Error"

        module Statements =
          let error =
            "5f27ed0633c2da02ce7209f502dd6e9b81cc26ad8153843dc7942475ae0544e4"
            |> p [ "Statements" ] "Error"

        module Unwraps =
          let error =
            "f26d39705defad33988746197a9e671c442dd167d10d62ebcb1968eb479945a4"
            |> p [ "Unwraps" ] "Error"

        module Jsons =
          let error =
            "4481deccfef0f84cbb36d6fd0223a866d68af44220c9f346da16ee8b4213c7a9"
            |> p [ "Jsons" ] "Error"

        module CLIs =
          let error =
            "ecc74d38cd91b77efe57dc67d3957ed361676bc89fc1e6c05a87bb655cff0594"
            |> p [ "CLIs" ] "Error"

        let error =
          "99ebdfc7df009829301853d2038c3cf5b4ddc5d1c69cd018487fe3cb7febdc58"
          |> p [] "Error"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let contentHash =
        "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
        |> p [] "ContentHash"

      let nameResolutionError =
        "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        |> p [] "NameResolutionError"

      let nameResolution =
        "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        |> p [] "NameResolution"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          |> p [] "Package"
        let fqTypeName =
          "593dbef1b81d45243efe5c73d94b13caaa337425de5884e2e2fd39ecaaf7d0c7"
          |> p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqValueName =
          "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          |> p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqFnName =
          "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          |> p [] "FQFnName"

      let typeReference =
        "7b125e2ee86f8eb978c01b2b59268f1d61c38f33f37154a8aa5462df19bd4af2"
        |> p [] "TypeReference"

      let letPattern =
        "376b83ed94797001fbc7349a5e38c92eae1d7a1b17f3897afe8d3acbf4a7f6cb"
        |> p [] "LetPattern"
      let matchPattern =
        "8cbb963c52eecc0ef51692d9991e7c010812c6448b172b4af8db70e3b38171fd"
        |> p [] "MatchPattern"
      let matchCase =
        "73647cea52ab37f274f8483cac1e4ad1545c41d9837dfe6431b9524bd1355bbc"
        |> p [] "MatchCase"
      let stringSegment =
        "b613711b5c977f9c4a3ddb48954283409640b69f4ca4fd2019257d9ed7576f56"
        |> p [] "StringSegment"
      let binaryOperation =
        "94a439540f7cd066316843e3124648fec75d8e650b7256bb08d428ec0f9d1b5e"
        |> p [] "BinaryOperation"
      let infixFnName =
        "85cdca8d06c978f066f0aaa59b920f53d2f16e2359d1021aebdd44d3b8a08523"
        |> p [] "InfixFnName"
      let infix =
        "5d7718518eae19f5a08888b7f211fddf2c4ac4f8822454ff789750d705e37979"
        |> p [] "Infix"
      let pipeExpr =
        "9577329df15f0255e738e24116847b85cf31217a7a1a98c39a0e8ff6ce03d917"
        |> p [] "PipeExpr"
      let expr =
        "e6cb6d2c1219dda2e0b06e641fee4370867f51f5ce72ade773813a267a26c513"
        |> p [] "Expr"

      let deprecation =
        "47b77911f56bc81a6a82dfe81c07d11c88d132efdbef9c4dc8e8807f23de59b2"
        |> p [] "Deprecation"

      let packageLocation =
        "952f921bae69c1df53ffde67755738bd3071fa97165be6d1e30f1d8c3ee9c706"
        |> p [] "PackageLocation"
      let locatedItem =
        "61fa6e787a2944d47e8e69b546ed3a2cffa7aa9b7e094db3f20205de6a9914e0"
        |> p [] "LocatedItem"

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField =
          "b1b77c46cf5a190630f3114037828193736fd2440dd2503cb3c5b3156b361453"
          |> p [] "RecordField"
        let enumField =
          "f6b386314c9d99715cf24ab0d9a543580302be25719e7123a92c77ce1eb1b490"
          |> p [] "EnumField"
        let enumCase =
          "f05132a9f1a01079e110f2dd716d5b805f5c9f8938e585873087bd157b852986"
          |> p [] "EnumCase"
        let definition =
          "f5346abc23fb2f9fcd41964b6695f79e7db0d1d0282be6816641ab3eda7b286d"
          |> p [] "Definition"
        let typeDeclaration =
          "7bf0edb27b606d9ef0a70339d0ab02a1fedc7ab6744551bd42fe7cca0f8deb9f"
          |> p [] "TypeDeclaration"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let packageType =
          "a7b1638022e3a52f99608824dc24b2d43b27b69a940c5705b5fa1c17ac9b7a97"
          |> p [] "PackageType"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let packageValue =
          "3e6690848e90934ec0b54a3d4836b256c290d155c1430ca194084e3af60d8b55"
          |> p [] "PackageValue"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let parameter =
          "b1b77c46cf5a190630f3114037828193736fd2440dd2503cb3c5b3156b361453"
          |> p [] "Parameter"
        let packageFn =
          "48badb6834c1160b15bab0e922643d64cd02d1cb8b1253a298bac57fcb7844a5"
          |> p [] "PackageFn"

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType =
          "7824e28c37f8b4f994c3f3752e682fb58229e66743c83ae7a6e4f29515cb4f6a"
          |> p [] "EntityType"
        let searchDepth =
          "364316241bbd776a630ac9c3f92927d3a634ef4446b9d45258a8436a254781ec"
          |> p [] "SearchDepth"
        let searchQuery =
          "854b4d7e83fd11abeada159881bda7adfc1b6596220da9f564779d0ff221e453"
          |> p [] "SearchQuery"
        let searchResults =
          "2c8aec01c7e9011b08965504bb9538a8e1b0fb5c89d882cbe2cf214dea0d2578"
          |> p [] "SearchResults"

      let packageOp =
        "d09c4e42fd19399047304a92e0127e0ad009e7989537200cd1ca1107cd7c184e"
        |> p [] "PackageOp"
      let itemKind =
        "642ec69245ff338e71dae980d8c7b1cac623b6b49fdd92375df3f90d06cc862f"
        |> p [] "ItemKind"
      let propagateRepoint =
        "e547cf6954711fccc03281d5eaaaa44695d3490f1a30b0fa84ba3b4c8cb53425"
        |> p [] "PropagateRepoint"

      let secret =
        "e48530853244d328a390b20a02e568c0782e0e65e6a9e78ae41cfa26c1461527"
        |> p [] "Secret"
      let db =
        "09b6f46f05cdfabc3c13fdcb2620e6e41642aa933e7f60f4b9c2e42b99d69d0a"
        |> p [] "DB"

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval =
          "5ccec4b619b5c44a34f85dcef3f37ec7bfdb23fe3b989087c406be91ca295429"
          |> p [] "CronInterval"
        let spec =
          "30d826f29e292f9e7db4c4ab184c619017f8fbd36463bea8f10b627f06aeae00"
          |> p [] "Spec"
        let handler =
          "04e1af2c3cc2c2555d61481ef3febe6cb393d96d60545a7dcb54629b07d7651c"
          |> p [] "Handler"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let errorMessage =
          "2314408c9017bab823414db19433005893a4d09e6235a41011e3ab1f5ae79818"
          |> p [] "ErrorMessage"

  module Cli =
    let executionError =
      "273caaa3cb6b098342f5c765f87fe87f03a38220d23d3d5a2ce43bc7613a68f0"
      |> p [ "Cli"; "ExecutionError" ] "ExecutionError"

    let script =
      "dae5fc6163a0ebe22c6ef1f55ec7b77d455041a914e6364191eec0f214644c88"
      |> p [ "Cli"; "Scripts" ] "Script"



  module DarkPackages =
    let stats =
      "3e1f75d46205cf5fc9515c4bfd3549c42396d10ab9d3333422e02806e7245894"
      |> p [ "DarkPackages" ] "Stats"


  module SCM =
    let private p addl = p ("SCM" :: addl)

    module Branch =
      let private p addl = p ("Branch" :: addl)
      let branch =
        "c0591dbfe3e3e85a30ed1d8d7a37609d6bde3fd395ab9ac2fb1db18a9c017316"
        |> p [] "Branch"

    module Merge =
      let private p addl = p ("Merge" :: addl)
      let mergeError =
        "e7fe21b0043e2ab84dc9ffe35fc802ff9ca9be52140c8e318c37eb94689de008"
        |> p [] "MergeError"

    module PackageOps =
      let private p addl = p ("PackageOps" :: addl)
      let commit =
        "4db1546d4553ff70bed6fad7512002e76d0490203eb0db039693837a20632300"
        |> p [] "Commit"


  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program =
        "13f07569930836064de1ea40e233c6aacc82ccfe80818cef96ca74a68a2c01ff"
        |> p [] "Program"

    module Infra =
      let tableSize =
        "e03ffd7442bd76f6e62f0d72da3be80cedf0371d55685eb88c5798186488053f"
        |> p [ "Infra" ] "TableSize"

module Fn =
  let mutable private _lookup = Map []

  let private p modules name (id : string) : ContentHash =
    let hash = ContentHash id
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module HttpClient =
      let request =
        "dfd9e14b5c6658a83dd41a68edb145766790b61182e6ac93d6618cd778bc5c7b"
        |> p [ "HttpClient" ] "request"


  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr =
        "38c347f32f7c4ab838bcd1509740a6452fee43695386917b72ba20187950e7ed"
        |> p [ "TestParsing" ] "parsePTExpr"

      let parsePTSourceFileWithOps =
        "2dc1c6242c880b763438e5892c33f710d855517c148c8443299579de0fdbaff9"
        |> p [ "TestParsing" ] "parsePTSourceFileWithOps"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseForCli =
          "a755d091ad133e5accb48c2dfa4da2f81c0ec0e3fd61b0bbb96e0155829f4d4a"
          |> p [] "parseForCli"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let dval =
        "3092e1e709803fcbfa38833c35755d161f385e530190fdbad12d99dc92f6761e"
        |> p [] "dval"
      let fnName =
        "7c43b7f750ee56883c61746a5f5cb42e2878d79c1998c25e5dc3f4089da14713"
        |> p [] "fnName"
      let typeReference =
        "283fc2f25df0a3c31f99ec07dcca86769513fd0bc6d424f2855d23dee3da9df2"
        |> p [] "typeReference"

      module Dval =
        let private p addl = p ("Dval" :: addl)
        let valueTypeName =
          "ba4236bae32639615f24b9c7a32be0999d2a8c6a15a924a14ce47723818832ac"
          |> p [] "valueTypeName"

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString =
          "cf5980dfd14c144a5e75fc4be0c1918b667d172700bb93ef9292fc18e5dba8a3"
          |> p [] "toString"
        let toErrorMessage =
          "35309aed7e05bdeac8e53b970078b47b13ddd5758157bd788b642ca5a1febfc2"
          |> p [] "toErrorMessage"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let sourceFile =
        "eb85a0913ca5686b50e3bce441e9e2f84dd5d7f4f1a93105c3b2f278ebe05160"
        |> p [] "sourceFile"

  module Cli =
    let executeCliCommand =
      "466960e20e328e3a7326d6931b2b2cbbb412abcb7a2792f09461bc2c907a6e3e"
      |> p [ "Cli" ] "executeCliCommand"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        "196585aa881800bd014c5f9650a72abb93d207a86d592cd0f1e144b45361a88a"
        |> p [] "parseSingleTestFromFile"
