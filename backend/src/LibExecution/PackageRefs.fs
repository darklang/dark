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
          "019fd6f48d9dafa3c33f1b034c9d2df5a757ac14254b78497c04025f27e9825b"
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
      "1d06416e22df3d29b2b30d3af9772a89b7eeb21e0dafe841d3d9c9819c83aa70"
      |> p [] "ParamInfo"
    let functionInfo =
      "b24f53b16ba7b82cc84d4d710670ce34fc5f98a43ef1ef9151468c93aa2d2b44"
      |> p [] "FunctionInfo"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      "c8276aff8c52a06bbfacac24d654f3bc3d4a2a37bb0b9ba0ef98d2b99ca56dca"
      |> p [] "Sign"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      "ae2162370a6d0c94403866b64729510f6a2e042ec4d95dbb6bc4d20e9ab3cbed"
      |> p [] "BuiltinValue"
    let builtinFnParam =
      "6d231562692d8a5dd2753490a132dc8cc6e3194870c2b615ed0d6316e3c9dcef"
      |> p [] "BuiltinFunctionParameter"
    let builtinFn =
      "ef5d8a17ac2ad9943069758ee09fda3e1d3d5dd698373a1a3b005590eb74a109"
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
          "33181fd8d669fbf579ac6d0e4b174d99f2be3a93ab3071e8d725d48ffcd7431b"
          |> p [] "PTCliScriptModule"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)

      let contentHash =
        "647a721ce3543dd4763a1ffec5edc4aacaefd814d9edbc63a62e2f23a83e500c"
        |> p [] "ContentHash"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          "6c3786ef4c88c87f3adcaced1c7be56a3bab4441b68dea13ace4f49dd412b221"
          |> p [] "Package"
        let fqTypeName =
          "ea1a4c449ce36404904c4d025ffea5a6479b02894afebf63d3f53d2b2358edd7"
          |> p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqValueName =
          "d77fea1769eaea1113585d613124fbf8462e2b96fdd72962a2bf1e3740fe8dbf"
          |> p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqFnName =
          "d77fea1769eaea1113585d613124fbf8462e2b96fdd72962a2bf1e3740fe8dbf"
          |> p [] "FQFnName"

      let nameResolutionError =
        "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        |> p [] "NameResolutionError"
      let nameResolution =
        "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        |> p [] "NameResolution"

      let typeReference =
        "24d0357bf07607b1da5a6487dd0f7d0634064b5e1f3103e70b6f07d524ade4c9"
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
        "505c12170c2e57c167f35b02f404c5ff9c9fa786066bfbf741090b8fef3876b4"
        |> p [] "Dval"
      let knownType =
        "3167b287baedfa63d85529ce7cf4209a5eeade19650c061deececd715ed93b8f"
        |> p [] "KnownType"
      let valueType =
        "4ca559c40352d23747502e9895b89f361bec29078f4e20cda9a85ebb373c1fb3"
        |> p [] "ValueType"

      let applicableNamedFn =
        "c8f0fa40c69da74949e74f04a082fb8c5c93ed504f194ed7ce06c94f55c7bac7"
        |> p [] "ApplicableNamedFn"
      let applicableLambda =
        "1035e8af20c9ce12e9baa78593aabe30ec38f9d3abd7be5242fd629368e07c7d"
        |> p [] "ApplicableLambda"
      let applicable =
        "5a6fd2eeeaf169b7cdbe5b3c52a2c8f34837df05670d9bb422f6f442667a5b6c"
        |> p [] "Applicable"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module Bools =
          let error =
            "ce2b9beedab8cc8d2313a7505e34a6d5bfb2d6e56843ccaac45eaea943eae728"
            |> p [ "Bools" ] "Error"

        module Ints =
          let error =
            "a3c5852ebe977e62763b25487608bce2ed1903a832e035993c99fb45b5a56fdf"
            |> p [ "Ints" ] "Error"

        module Strings =
          let error =
            "5b9e9c4cdfef8fff6876677d0a562603021df6f34b7f96dbb8e050c9dc27d6a0"
            |> p [ "Strings" ] "Error"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            "996b557c0661d0658cdd0bfdcc115ab80b21d62fa6c7bf40642999d4fb99dec9"
            |> p [] "Error"

        module Dicts =
          let error =
            "20b0c8b2e2d303465a7c34589fe4ce588b51d5296c79284cba8cf57998c14245"
            |> p [ "Dicts" ] "Error"

        module Lets =
          let error =
            "6409a3289e80a52984bbc3ddee80a901351eb4acc700605b96f7e6a99e08f30d"
            |> p [ "Lets" ] "Error"

        module Matches =
          let error =
            "9eb3751ff348aef1eaa34849aaf42e92d66ca3b0fdd091b0efefbd33667bb090"
            |> p [ "Matches" ] "Error"

        module Enums =
          let error =
            "a0e83770db9a3ac712ba3f36f8518ac37914870e0adc3140d24a88bf19ce5fa0"
            |> p [ "Enums" ] "Error"

        module Records =
          let error =
            "05c6f433673cbb16c6b3a67853f5ec2d876d3b5954e35cbb49d2d2b249889ac2"
            |> p [ "Records" ] "Error"

        module Applications =
          let error =
            "e22dce3fa3419b82c15fcc64db9d71731ff77c69f52df80d6c3d7b60fabb3f30"
            |> p [ "Applications" ] "Error"

        module Statements =
          let error =
            "a9de23ae9af0ae26c3fe0f12552a8cd1c472eaef45c0714de1c529dbe7201279"
            |> p [ "Statements" ] "Error"

        module Unwraps =
          let error =
            "ef642be9c8aba0843257432d5c15c49c29ef5ea3b7c20d6230f6719c1019a96e"
            |> p [ "Unwraps" ] "Error"

        module Jsons =
          let error =
            "b2228404bf9dd81342e1703b901aaecd5ef7c5576a17836b0f54c87ab9c15848"
            |> p [ "Jsons" ] "Error"

        module CLIs =
          let error =
            "46129ff3f211b74493307bdc093c6cbff2ce72e1ff53b8414c8f042838b46a69"
            |> p [ "CLIs" ] "Error"

        let error =
          "6577110343da270c01b34bad15f5fd112478d42a6fd10c1702afdb5794e47fac"
          |> p [] "Error"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let contentHash =
        "647a721ce3543dd4763a1ffec5edc4aacaefd814d9edbc63a62e2f23a83e500c"
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
          "6c3786ef4c88c87f3adcaced1c7be56a3bab4441b68dea13ace4f49dd412b221"
          |> p [] "Package"
        let fqTypeName =
          "ea1a4c449ce36404904c4d025ffea5a6479b02894afebf63d3f53d2b2358edd7"
          |> p [] "FQTypeName"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqValueName =
          "d77fea1769eaea1113585d613124fbf8462e2b96fdd72962a2bf1e3740fe8dbf"
          |> p [] "FQValueName"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          |> p [] "Builtin"
        let fqFnName =
          "d77fea1769eaea1113585d613124fbf8462e2b96fdd72962a2bf1e3740fe8dbf"
          |> p [] "FQFnName"

      let typeReference =
        "f4e39ed39e48e03205117fd2d970055ac7e1de943efe32a7ae1e323dbb9ccd82"
        |> p [] "TypeReference"

      let letPattern =
        "376b83ed94797001fbc7349a5e38c92eae1d7a1b17f3897afe8d3acbf4a7f6cb"
        |> p [] "LetPattern"
      let matchPattern =
        "8cbb963c52eecc0ef51692d9991e7c010812c6448b172b4af8db70e3b38171fd"
        |> p [] "MatchPattern"
      let matchCase =
        "efcd4f3f42c628093e5c91a9a3a0e1848f0a41a7f9e8b2dfdbfe242a8f79f5cc"
        |> p [] "MatchCase"
      let stringSegment =
        "86eaa93bc696be49b5364f28639578af59bb87db370d5d9c58ac5e4824e9d0dc"
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
        "e239c8f1d20fdbfe704d21ed952beddf81b79cb1ad6c7c559b96bdeb62a205e0"
        |> p [] "PipeExpr"
      let expr =
        "09b8f8b1fcf41c11d185833930d46856044c7864b48d666813cb64a377e2b5c2"
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
          "86fde2d0b203cfd97ca8aa60eb480aac720870765875d956d3fad65260bde8d1"
          |> p [] "RecordField"
        let enumField =
          "0bfec4b32eabf055fd61d13695c1a6e81215101a9cc12dc962aa1ea2af1ac268"
          |> p [] "EnumField"
        let enumCase =
          "a610aaf85898b9abf1f04ee7146468c81b15ac29d0edd75242c9ee96539a2de5"
          |> p [] "EnumCase"
        let definition =
          "c47c39c9a35986b14268678e6d8618ae92670ba65c391d66b036a7b645fafada"
          |> p [] "Definition"
        let typeDeclaration =
          "0cd7cfb31660f202fd80fac0a16a9188e0cf7f42f69bc71479ba28026a0263e8"
          |> p [] "TypeDeclaration"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let packageType =
          "e40e85501105924d7f0ea2fd7be8aaf7de0e681ae8766a93ff23c63f96357ad1"
          |> p [] "PackageType"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let packageValue =
          "5720274623f8edee4c3603b4a4cfef5c9e9bc7cb683c0a7f91f5081f2a1ec818"
          |> p [] "PackageValue"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let parameter =
          "86fde2d0b203cfd97ca8aa60eb480aac720870765875d956d3fad65260bde8d1"
          |> p [] "Parameter"
        let packageFn =
          "8400cd7fc39b65267dab4165a953ad60ec0c89a6480d25208c14b6abf4a0583f"
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
          "dcc04f0f19fad37326849140b9b7d298bb7c23bfcf78b9bdb03405254edaef69"
          |> p [] "SearchResults"

      let packageOp =
        "574523dd30a02156f25e0e599adf2ee4327e50b6dfe635940fe19b83251ad932"
        |> p [] "PackageOp"
      let itemKind =
        "642ec69245ff338e71dae980d8c7b1cac623b6b49fdd92375df3f90d06cc862f"
        |> p [] "ItemKind"
      let propagateRepoint =
        "f27add8bc029c2dfec1d38e680fcafa0a8462785c4215fc4a6aa1d87f171711d"
        |> p [] "PropagateRepoint"

      let secret =
        "e48530853244d328a390b20a02e568c0782e0e65e6a9e78ae41cfa26c1461527"
        |> p [] "Secret"
      let db =
        "8959f06e26a32d744cf2604ae9757db5d2fb2ebe6e19211af948ec8eaf419de0"
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
          "3edf2e3b99307b6aa43f52cfa69c8d33282e295bc2bfb304342a696d4c18ee74"
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
        "c712857936387e79f146b921fc7a9e8d0c58f2580bb35cb46b24e12a04d0415b"
        |> p [ "TestParsing" ] "parsePTExpr"

      let parsePTSourceFileWithOps =
        "99010a449ffe3f11152ba3655180e28aa9f7429c48605a91967f3098b6a21bff"
        |> p [ "TestParsing" ] "parsePTSourceFileWithOps"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseForCli =
          "4f2ca4481bd02c833b6034e222d2c6b5cee8ef6a5da0b2fb3902f7586ea1f853"
          |> p [] "parseForCli"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let dval =
        "27dc43f1c0b447fc56a0758e5d1cc9a56e0b0b06524b31b32ae6da08ca278d9e"
        |> p [] "dval"
      let fnName =
        "9723e87fb0ad3a611482f57ca7aa09356c1700f905c35694d75a6e9a32e51b9a"
        |> p [] "fnName"
      let typeReference =
        "5a16babd9b658d67b37a998d92ba0d630688f350a9e0fd9bbccc50a361b4760a"
        |> p [] "typeReference"

      module Dval =
        let private p addl = p ("Dval" :: addl)
        let valueTypeName =
          "7ae9d57f7f4b8cf3e62a5bb9ddab530b8a26e52e1248745465b73f4ba144d876"
          |> p [] "valueTypeName"

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString =
          "2e93aa55061ff9c09c662a613df2307e31cb382adc97ce729212f4db7428eb5b"
          |> p [] "toString"
        let toErrorMessage =
          "679c2753ad53b18b4e925ddf89a6cebaed7583ed01b60132cb9b539e097b7c05"
          |> p [] "toErrorMessage"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let sourceFile =
        "5f51c40d0f71df49392b3436dcc754aa3c7e43bbcb748f417b317332c214f620"
        |> p [] "sourceFile"

  module Cli =
    let executeCliCommand =
      "73f808c6a73548884310442d871c9c0c7ae13df1010757680f7ae1bf1a7e4421"
      |> p [ "Cli" ] "executeCliCommand"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        "391e32a3d58dd7958ec4b5f08dc4fef03107d1e3ed28f395fd78b06f532a7811"
        |> p [] "parseSingleTestFromFile"
