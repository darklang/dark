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
  let mutable private _lookup : Map<string list * string, string> = Map []
  let private p modules name (id : string) : string =
    _lookup <- _lookup |> Map.add (modules, name) id
    id

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
    let uint8ParseError = intParseErrorHash |> p [ "UInt8" ] "ParseError"
    let int16ParseError = intParseErrorHash |> p [ "Int16" ] "ParseError"
    let uint16ParseError = intParseErrorHash |> p [ "UInt16" ] "ParseError"
    let int32ParseError = intParseErrorHash |> p [ "Int32" ] "ParseError"
    let uint32ParseError = intParseErrorHash |> p [ "UInt32" ] "ParseError"
    let int64ParseError = intParseErrorHash |> p [ "Int64" ] "ParseError"
    let uint64ParseError = intParseErrorHash |> p [ "UInt64" ] "ParseError"
    let int128ParseError = intParseErrorHash |> p [ "Int128" ] "ParseError"
    let uint128ParseError = intParseErrorHash |> p [ "UInt128" ] "ParseError"
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
          "64f7e8b4b24165eb2bbf3087252fef6fcd3c6d96469464ac7378372373bc82bc"
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
      "8948f9b0684b803df67eef9549b47e8a897cc201d126ceaf7c05ff7f95e7def3"
      |> p [] "ParamInfo"
    let functionInfo =
      "5d78771a5122b514e6d8df5d3b44bf0de8272b55dc203ba674543688553fb4c8"
      |> p [] "FunctionInfo"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      "c8276aff8c52a06bbfacac24d654f3bc3d4a2a37bb0b9ba0ef98d2b99ca56dca"
      |> p [] "Sign"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      "4db1f484f33f82cc05dcb127ab5b8e072a24da4bcd1518dcbf17877766a80ebb"
      |> p [] "BuiltinValue"
    let builtinFnParam =
      "d46e4b29a49af11900f74dd6b0a914a6740ee18ce7607df6929555e0be02bbab"
      |> p [] "BuiltinFunctionParameter"
    let builtinFn =
      "3400949ea65e7fa84c5a0d9ac17fb283228788a5bada0dcb6174732d5d204bfc"
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
          "81975320d7f3e48413227d8e7e0b5fb74a341a722ae1647c7b3966be772e8bd2"
          |> p [] "PTCliScriptModule"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)

      let hash =
        "596e678425be94c61f2bab9d1dd380691cd415167b4b9ed766af4df8ab0ccc2f"
        |> p [] "Hash"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          "8f54eb80b84cbbfde2315f3a2434096582533b9d8583db12db427c03427c3242"
          |> p [] "Package"
        let fqTypeName =
          "70c5a8edbd59fdf60836762eb87240924dc65606f68141b2c7d101f3e414a5a6"
          |> p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqValueName =
          "51f703030055ff1339fb948bc8abf4f95b884f33ec312cd12edde07f13ad0114"
          |> p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqFnName =
          "51f703030055ff1339fb948bc8abf4f95b884f33ec312cd12edde07f13ad0114"
          |> p [] "FQFnName"

      let nameResolutionError =
        "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        |> p [] "NameResolutionError"
      let nameResolution =
        "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        |> p [] "NameResolution"

      let typeReference =
        "f20d73ecaa7aa210f9e5e134802da58522ec2abfab1966edf74b3412fc7cd37d"
        |> p [] "TypeReference"
      let letPattern =
        "490a27236fb548b21d2ddd743c1bdeada212a75b0c33dd1f8cc0eb5873fb9fe8"
        |> p [] "LetPattern"
      let matchPattern =
        "fe3783a863d78392faf65c588d8a03c074fa5d290113f60fb6933226ff09cfaa"
        |> p [] "MatchPattern"
      let stringSegment =
        "eec82b6d269fe61be06f6f9f7997a91ff137a49ae708bb77b3cfcc82b051ef61"
        |> p [] "StringSegment"

      let dval =
        "d39836cc776d3d72f1b5a144df07f763305d7e3712db9b2f857e5d2d93cbc927"
        |> p [] "Dval"
      let knownType =
        "edf3a9a95ba24a64206ed750ba2b85d38f699114ee38fe2fc00f1bcfefa20061"
        |> p [] "KnownType"
      let valueType =
        "14ee95e64561274e4214440810e47cd9b236f301cb1a414f505e88064f78d93b"
        |> p [] "ValueType"

      let applicableNamedFn =
        "41573a3536c8e74fc112ce3206edc18a5625f877d91dac63bf5743fe984ae0ae"
        |> p [] "ApplicableNamedFn"
      let applicableLambda =
        "6caa4d71e00da44bf29271aef7ad9aa6c7b69b430c36d52cc0d0ff0fb8671d66"
        |> p [] "ApplicableLambda"
      let applicable =
        "3e5543055b4d0aa1e8ae6440f591d9d129d4d6104b7cf5e0885a315262ce76c7"
        |> p [] "Applicable"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module Bools =
          let error =
            "61acde1fc5fbc19046aaa17ae68e6453cfcb3b057c7195625fff7a8afc7eb345"
            |> p [ "Bools" ] "Error"

        module Ints =
          let error =
            "a3c5852ebe977e62763b25487608bce2ed1903a832e035993c99fb45b5a56fdf"
            |> p [ "Ints" ] "Error"

        module Strings =
          let error =
            "66a1993406dfa2c85983709dedcac5f1a946b214ac6f2a84c268985cabe074e9"
            |> p [ "Strings" ] "Error"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            "e223a08e5ce48826866fa763017ca4a13ed411e72adab3c60d2f96ecbd3ce8be"
            |> p [] "Error"

        module Dicts =
          let error =
            "05d0ed2de94343a48686b401875721f5a51aa63f3e9fbe66eeaec9d03cbcc13a"
            |> p [ "Dicts" ] "Error"

        module Lets =
          let error =
            "2c108ba6a33ef59a4c562b15446ea2e7f24ef7d53704b904a63fa0d80ca87194"
            |> p [ "Lets" ] "Error"

        module Matches =
          let error =
            "0657d002bd7a5377c592c407963111a6245d14a1b7b4946fa1b2bf71fbf8090d"
            |> p [ "Matches" ] "Error"

        module Enums =
          let error =
            "04f16905570741bdfd9fc43d9d282560d62db60e05f54199ab210991566d8172"
            |> p [ "Enums" ] "Error"

        module Records =
          let error =
            "52790daa6f6073856572b5175ff9e54663c684be3f45b81dabc9a9eb511621ef"
            |> p [ "Records" ] "Error"

        module Applications =
          let error =
            "a176210962b77fb0a1e78480f10bc503bd6ab10a39dd308185d5dfb50ab30f1e"
            |> p [ "Applications" ] "Error"

        module Statements =
          let error =
            "42646f6fb84665f29189489e7989a1da5ce210a3c87a4c46980f88f1c902bc04"
            |> p [ "Statements" ] "Error"

        module Unwraps =
          let error =
            "c43d4474effee5c4d0d199fdd4362b39859773c87af27fc474cb3c758ef417ca"
            |> p [ "Unwraps" ] "Error"

        module Jsons =
          let error =
            "aa8dab879f174c02429cca2896601ef46d2182eb66a2b993b2b784e4bf90d74d"
            |> p [ "Jsons" ] "Error"

        module CLIs =
          let error =
            "cd65de199c38ba1399b90194ea595cce6318ff70033912c41410ebe348966f6c"
            |> p [ "CLIs" ] "Error"

        let error =
          "3baf58d64d046311cd8db23136348d3bed67811c16cda899d8ed7ce0a1f20adb"
          |> p [] "Error"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let hash =
        "596e678425be94c61f2bab9d1dd380691cd415167b4b9ed766af4df8ab0ccc2f"
        |> p [] "Hash"

      let nameResolutionError =
        "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        |> p [] "NameResolutionError"

      let nameResolution =
        "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        |> p [] "NameResolution"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          "8f54eb80b84cbbfde2315f3a2434096582533b9d8583db12db427c03427c3242"
          |> p [] "Package"
        let fqTypeName =
          "70c5a8edbd59fdf60836762eb87240924dc65606f68141b2c7d101f3e414a5a6"
          |> p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqValueName =
          "51f703030055ff1339fb948bc8abf4f95b884f33ec312cd12edde07f13ad0114"
          |> p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqFnName =
          "51f703030055ff1339fb948bc8abf4f95b884f33ec312cd12edde07f13ad0114"
          |> p [] "FQFnName"

      let typeReference =
        "c9844a41899d6aee24655bd2c1dd5928eec4c5dbd764898fe7c0dbeb0cd896d5"
        |> p [] "TypeReference"

      let letPattern =
        "376b83ed94797001fbc7349a5e38c92eae1d7a1b17f3897afe8d3acbf4a7f6cb"
        |> p [] "LetPattern"
      let matchPattern =
        "8cbb963c52eecc0ef51692d9991e7c010812c6448b172b4af8db70e3b38171fd"
        |> p [] "MatchPattern"
      let matchCase =
        "3687bf92a26bd28f4fa20fcd41d96d7060bfc5adef5d6bfa71517f084e3bc608"
        |> p [] "MatchCase"
      let stringSegment =
        "eec82b6d269fe61be06f6f9f7997a91ff137a49ae708bb77b3cfcc82b051ef61"
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
        "a9fe1d58866aa2fc4dc6d1ab6508cad5c779efaca5a233c85989a3c48112c9d1"
        |> p [] "PipeExpr"
      let expr =
        "64527279d446e322195a0da531ebe8e90fbfad936a2553a30cfaf54b0f805b68"
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
          "b04bebfebf0d08738f5e9c56c66c121f9d7b426eeaaa9fa4b3d1fa43c2bb17fb"
          |> p [] "RecordField"
        let enumField =
          "1043846c41db210bec13e46525b675c8408ed5c8a600205d5e3946ed6db2fcd5"
          |> p [] "EnumField"
        let enumCase =
          "a53695c9de468d5937c5903d02b2e5e28e3bbc0cf5b4ac5e11d2e39a72c7572a"
          |> p [] "EnumCase"
        let definition =
          "a51c191b671003bda75a27ade58d0d9d5d35cf3d4b2ab1372f2d72e6de15eb01"
          |> p [] "Definition"
        let typeDeclaration =
          "f55b50f5638f9e38e9e57a8980a02df8cbefe1dc143d1a55389a9a435fc6967d"
          |> p [] "TypeDeclaration"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let packageType =
          "14a33b1b0034b6a7dc63d8c1255c96a8b912108cf783dc95d0ac644995944969"
          |> p [] "PackageType"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let packageValue =
          "b23ee5f3b5bfe2b17ac395ff08808fad3ce395007bf1e9dc1e8d2f3ee7cbd295"
          |> p [] "PackageValue"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let parameter =
          "b04bebfebf0d08738f5e9c56c66c121f9d7b426eeaaa9fa4b3d1fa43c2bb17fb"
          |> p [] "Parameter"
        let packageFn =
          "9719983d9e2f20d469e4dfbde142ca98729a4ea4df244c4242a7d9a5cc978953"
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
          "2bb7e2179d9e49f6fecc2e66131109b123cd8941bb8ba69868b31bd3017640a8"
          |> p [] "SearchResults"

      let packageOp =
        "d78a13382e001c01e0b90186ff6b63abce439e33dc5e5a1cba35a1b03afa4182"
        |> p [] "PackageOp"
      let itemKind =
        "642ec69245ff338e71dae980d8c7b1cac623b6b49fdd92375df3f90d06cc862f"
        |> p [] "ItemKind"
      let propagateRepoint =
        "c56effe3efb7d7e384bbb5fb4aa7e74b3cc883b97d2a693ab7c8a37f70d5e05f"
        |> p [] "PropagateRepoint"

      let secret =
        "e48530853244d328a390b20a02e568c0782e0e65e6a9e78ae41cfa26c1461527"
        |> p [] "Secret"
      let db =
        "aa0bef809c40a17743b00d9a9e5cb45efe32a1ab3e9bddffea87d7728b3cc634"
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
          "07b0431970b6530e22f744bb3559b9e8c0b1d027a0dd2ab58509a46d0c9d6fbc"
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
        "ad2679cd3151647838eb8b1295a8d4ce38b50335a240bdb533389abad4c24c01"
        |> p [] "Branch"

    module Merge =
      let private p addl = p ("Merge" :: addl)
      let mergeError =
        "e7fe21b0043e2ab84dc9ffe35fc802ff9ca9be52140c8e318c37eb94689de008"
        |> p [] "MergeError"

    module PackageOps =
      let private p addl = p ("PackageOps" :: addl)
      let commit =
        "b8ddbb62baba9216aa3783be7116671524ee3cd2ab65bf1073f3674f1540760c"
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
  let mutable private _lookup : Map<string list * string, string> = Map []

  let private p modules name (id : string) : string =
    _lookup <- _lookup |> Map.add (modules, name) id
    id

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
        "25a5a15779cb6ada348b8a4b450b7388b811075ef0b45cf2f1f6f5ee1ad0bfcb"
        |> p [ "TestParsing" ] "parsePTExpr"

      let parsePTSourceFileWithOps =
        "bd4747f43fe42c879580e8630b16c51dedb3906caa6468fa67751050f816f4a9"
        |> p [ "TestParsing" ] "parsePTSourceFileWithOps"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseForCli =
          "473bdf77bf3ad735eff2c54592db73e347ce95e101d957ffc239a6fbc585cb14"
          |> p [] "parseForCli"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let dval =
        "ac6840b1314d398c512de9feffda0187f32d5ac42a48878778e831d1a37831e9"
        |> p [] "dval"
      let fnName =
        "d75235b421f06cb1ac59d86a44962cf68344032f5e5588e8ab0f9e40c7786c8f"
        |> p [] "fnName"
      let typeReference =
        "e32301dc2086207f40ea7f57c2364e361434af53513877047ca55cd5fe65a343"
        |> p [] "typeReference"

      module Dval =
        let private p addl = p ("Dval" :: addl)
        let valueTypeName =
          "2c54e40e0925e41b7ec73d13e792e7048ebba088ab648086d905b660f4353fb9"
          |> p [] "valueTypeName"

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString =
          "4eac460acad676ec18b1b910420059b293a4d397066424addc4b97657f4b64bb"
          |> p [] "toString"
        let toErrorMessage =
          "fc5edae998b115d1cbcad014781a80027e7d7cdfd8b75fd441364101e6184b0a"
          |> p [] "toErrorMessage"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let sourceFile =
        "6bd389c5404c245fc931ddff85deafa61ccf00cfd8f684b6207f577a8205d7db"
        |> p [] "sourceFile"

  module Cli =
    let executeCliCommand =
      "ee03cd87e341868b4896fe66bca3b38c6b94f0b63b8fee3c93ec7e9b0cdeb66f"
      |> p [ "Cli" ] "executeCliCommand"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        "ded19b7ad869f74082c0d0e7adb471816b7688e0e2f84851acdbda5cbf18e67f"
        |> p [] "parseSingleTestFromFile"
