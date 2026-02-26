/// All Darklang code exists in package space, referenced by content hash. In many
/// places throughout our F# codebase, we reference these hashes (i.e. in order to
/// return an `Option` from a function, we need to know the hash of the `Option`
/// package type).
///
/// So, we define their hashes here, and reference via those hashes. When parsing, we
/// have a lookup of name -> hash handy; if a parsed definition matches one of those
/// names, we ensure that we use the corresponding hash when saving it to the DB.
///
/// Some tests exist to ensure each of these hashes is unique.
///
/// When .dark package files change, hashes may shift. Run
///   scripts/check-packagerefs.sh
/// to fetch current hashes from data.db and compare against this file.
///
/// Note: all of these types are assumed to be owned by the Darklang user
module LibExecution.PackageRefs

open Prelude


// The way this is set up, we provide the name of the package item, with the ID.
// Doing this at once helps things to be legible, and makes sure you don't forget
// to add it to a separate lookup table. Maybe it's not ideal to use mutation so
// much, but it seems kinda reasonable.


module Type =
  let mutable private _lookup = Map []
  let private p modules name (id : string) : ContentHash =
    let hash = ContentHash id
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result =
      let hash = "03549ecc7eb39c974d3f7181c87a3ee9337fc91307ecb2c24351b0df4809ce15"
      p [ "Result" ] "Result" hash
    let option =
      let hash = "c58cf283d0e5d6634a76fa30ea5b0d29e615e1c4fe1cc2bfca27077cd72bc072"
      p [ "Option" ] "Option" hash

    let int8ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "Int8" ] "ParseError" hash
    let uint8ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "UInt8" ] "ParseError" hash
    let int16ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "Int16" ] "ParseError" hash
    let uint16ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "UInt16" ] "ParseError" hash
    let int32ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "Int32" ] "ParseError" hash
    let uint32ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "UInt32" ] "ParseError" hash
    let int64ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "Int64" ] "ParseError" hash
    let uint64ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "UInt64" ] "ParseError" hash
    let int128ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "Int128" ] "ParseError" hash
    let uint128ParseError =
      let hash = "ab0a98d41aabb3799b3209709a093f3e0ccc6ec79853bb333bfe7cafc8813333"
      p [ "UInt128" ] "ParseError" hash
    let floatParseError =
      let hash = "e342cc59c22ca41ab61c5bb375c04c6dbb31592d5a79491593a3b2bc522fb86b"
      p [ "Float" ] "ParseError" hash
    let uuidParseError =
      let hash = "e342cc59c22ca41ab61c5bb375c04c6dbb31592d5a79491593a3b2bc522fb86b"
      p [ "Uuid" ] "ParseError" hash

    module Http =
      let request =
        let hash = "aa54e59a71fcf69e810b5be93a89f551be91bb2bf7e3fdb7cb6b237ea7abc07c"
        p [ "Http" ] "Request" hash
      let response =
        let hash = "1973e0642c38b5733e779ec2ef931de3b7f2c453d6bcd8401b1f4b1f6b0a0b11"
        p [ "Http" ] "Response" hash

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader =
        let hash = "dbfe4487bd474fdf0ffccaf4d8f546785e1037becc1f5527696e053e5199f1b8"
        p [] "BadHeader" hash
      let badUrlDetails =
        let hash = "1dbb355daa5a3c3531e71a0682e0aa20ccf6751d136abcfdcca5def779c21eb5"
        p [] "BadUrlDetails" hash
      let requestError =
        let hash = "4c8c5b2aeb3e71c865a106de626da3345a18094ca620bc12296b071cea5445f1"
        p [] "RequestError" hash
      let response =
        let hash = "1973e0642c38b5733e779ec2ef931de3b7f2c453d6bcd8401b1f4b1f6b0a0b11"
        p [] "Response" hash

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part =
            let hash =
              "cfa1c52ae5a0fc661c15db09bfb3e6c129904020cc6346a04399f24deac5c7e9"
            p [ "JsonPath"; "Part" ] "Part" hash
        let jsonPath =
          let hash =
            "a9475a0792181ea56248a4fb7ad5a711dfcbb3931a543f46a041709d8cdc6133"
          p [] "JsonPath" hash
        let parseError =
          let hash =
            "843e851b01435213b27134405e9a8d988468ee2af1a06e622b0dc2229eaf6745"
          p [] "ParseError" hash

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError =
        let hash = "8dc77b20b13969bee07766b01d9afa75c78fbfe780443c621742fa2021831abe"
        p [ "ParseError" ] "ParseError" hash
      let json =
        let hash = "130be4b5b81501a911e1411dde62709f53baaf503287fca223ee30b8ad55e633"
        p [] "Json" hash

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome =
        let hash = "794afc09ab3850061cd00ede88ef1ef3add83a6d1146d6428d3fb03d164a8b06"
        p [] "ExecutionOutcome" hash

      module OS =
        let private p addl = p ("OS" :: addl)
        let os =
          let hash =
            "b7c0e15d61d6652c5be3cdc9360637524332b72aec228c3a771e2f3f88a8a1fe"
          p [] "OS" hash

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers =
          let hash =
            "5473100a8e7309f3438b5ef92a99fba6518d937b3de96d2c3e4169a21e097100"
          p [ "Modifiers" ] "Modifiers" hash
        let key =
          let hash =
            "788e35c44bf7b0546cfce602a673dcc38ee2a2e84680e6e8df39f5c281325085"
          p [ "Key" ] "Key" hash
        let keyRead =
          let hash =
            "216897425c81dd012bc812dd2a7238b327486f88cc3f615cb626d1b3d234e587"
          p [ "KeyRead" ] "KeyRead" hash

  module Builtins =
    let private p addl = p ("Builtins" :: addl)
    let purity =
      let hash = "871e4962a7f55a23f35c21043f6759a09aa382972f3b5ca48d279d764695920e"
      p [] "Purity" hash
    let paramInfo =
      let hash = "dab46d3abb6707d146e8fdd6904c6c91e7a73f7522cd081f7e32eccd31a2a725"
      p [] "ParamInfo" hash
    let functionInfo =
      let hash = "57dfcf233ecbe6b216f4a38e5d1bb852d34aece0232e7c71a1f17b281ed8a2da"
      p [] "FunctionInfo" hash

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      let hash = "c8276aff8c52a06bbfacac24d654f3bc3d4a2a37bb0b9ba0ef98d2b99ca56dca"
      p [] "Sign" hash

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      let hash = "ce73096c08e8f876c52dba1fd74e55dbb62a28852d6e60a2516c4c026207133d"
      p [] "BuiltinValue" hash
    let builtinFnParam =
      let hash = "afc1223c6c35e8d62f4db8c5b93fe343536b194f5ef6d1040e05f8d731459b38"
      p [] "BuiltinFunctionParameter" hash
    let builtinFn =
      let hash = "3ebb4c86a0a14cb3a84c81576908066b4a607329a12239e6d7ab510837443446"
      p [] "BuiltinFunction" hash


    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point =
        let hash = "1dfa2649bed131a5185d8de6289fa0122baf52aaa4103340fc42c2c8fa2d98f6"
        p [] "Point" hash
      let range =
        let hash = "181436ebc47d4bf0c43240038de88354ada449c9e0fef8ef03d15586c1c43996"
        p [] "Range" hash
      let parsedNode =
        let hash = "543545d4551cb753b14de434dbc4559105a697f76847ed9dbdadde2e1a95b8bd"
        p [] "ParsedNode" hash

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule =
          let hash =
            "03ebedc002f56014c02f681bd90b873968c4d8d38b59075411401c7a37d17b62"
          p [] "PTCliScriptModule" hash


    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      let nameResolverOnMissing =
        let hash = "d496dc2a14c8c7d25097db090c8b7f54027be4dc831a023df6c8084822d408a5"
        p [] "OnMissing" hash


    module WrittenTypes =
      let private p addl = p ("WrittenTypes" :: addl)
      let name =
        let hash = "754df89d6942b9625091bf0b80f54386e61a3392e44f4435fdbea9b975212440"
        p [] "Name" hash
      let range =
        let hash = "d4d21c6b436f82ac5b3ab3f5268d1dd7d42cc204814a3e648539cd3af6578559"
        p [] "Range" hash


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          let hash =
            "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          p [] "Package" hash
        let builtin =
          let hash =
            "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          p [] "Builtin" hash
        let fqTypeName =
          let hash =
            "593dbef1b81d45243efe5c73d94b13caaa337425de5884e2e2fd39ecaaf7d0c7"
          p [] "FQTypeName" hash

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let package =
          let hash =
            "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          p [] "Package" hash
        let builtin =
          let hash =
            "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          p [] "Builtin" hash
        let fqValueName =
          let hash =
            "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          p [] "FQValueName" hash

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let package =
          let hash =
            "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          p [] "Package" hash
        let builtin =
          let hash =
            "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          p [] "Builtin" hash
        let fqFnName =
          let hash =
            "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          p [] "FQFnName" hash

      let nameResolutionError =
        let hash = "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        p [] "NameResolutionError" hash
      let nameResolution =
        let hash = "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        p [] "NameResolution" hash

      let typeReference =
        let hash = "3e01ed7c66abe2152f3d1f92cc5f7ad3d16dafda0b4222d8038e07615585203e"
        p [] "TypeReference" hash
      let param =
        let hash = "2f53b3137b8c59ab62e57c6e6c4ea2c0b9463c18dc48ee644f5969fd53811eb0"
        p [] "Param" hash
      let letPattern =
        let hash = "490a27236fb548b21d2ddd743c1bdeada212a75b0c33dd1f8cc0eb5873fb9fe8"
        p [] "LetPattern" hash
      let matchPattern =
        let hash = "fe3783a863d78392faf65c588d8a03c074fa5d290113f60fb6933226ff09cfaa"
        p [] "MatchPattern" hash
      let matchCase =
        let hash = "89e4eb85ea5c04ad023ccca1f6689e916616828058dc6c4f4b21cbbcf398ae89"
        p [] "MatchCase" hash
      let stringSegment =
        let hash = "5e2fd40f810657a36daabb43d9519966924a23aa04b6cb5bbaecdc71c7c8886d"
        p [] "StringSegment" hash

      let dval =
        let hash = "45dd9d48f6c0e0364884ed1a3b85446ed9761f0bd95885c4d08b71d2b2963e1d"
        p [] "Dval" hash
      let knownType =
        let hash = "2faeeff43f773ba4b522ca9346961148e3f65134039c06e8f47cb927d5abc5dc"
        p [] "KnownType" hash
      let valueType =
        let hash = "58f6ec12f38c83245126422a4d81032ea743e07bb53acb3548c8ef2cdfa3154b"
        p [] "ValueType" hash

      let applicableNamedFn =
        let hash = "03410751eaa6d062191f2440b1b0a5a34e7c0d8a94aef504be66f314aa947b26"
        p [] "ApplicableNamedFn" hash
      let applicableLambda =
        let hash = "527ca58594e3389c02f00f9d7785f5b49e35db0d6b4eedadaa4d678b8ccc1ecf"
        p [] "ApplicableLambda" hash
      let applicable =
        let hash = "45aad3685f9e5913f68339e1c5c7c73cc13e53019adb675053d995713e7879f3"
        p [] "Applicable" hash


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module TypeCheckers =
          let private p addl = p ("TypeCheckers" :: addl)
          let pathPart =
            let hash =
              "27857bb74fd7bb9def0d1a7e58bdc858bbc56f6037a3665459b2189486a5309a"
            p [] "PathPart" hash
          let path =
            let hash =
              "735322d4b3cb0f40e936ed45e429d85d2f876062c5667bf01ea418457e543c27"
            p [] "Path" hash
          let error =
            let hash =
              "19e134caa3ecb61f20f8a7cf7f1ee4ac385e6417327f894d31179cb91a9c8e4a"
            p [] "Error" hash

        module Bools =
          let error =
            let hash =
              "2c4fec5380d7bd949189388a3a5b9268880e5252f378e559266f005b8731bbb6"
            p [ "Bools" ] "Error" hash

        module Ints =
          let error =
            let hash =
              "a3c5852ebe977e62763b25487608bce2ed1903a832e035993c99fb45b5a56fdf"
            p [ "Ints" ] "Error" hash

        module Strings =
          let error =
            let hash =
              "4065445cac4f03cc63d507749a2c9ca8bbe6f78415e9d00f847f729d3fa0e9d1"
            p [ "Strings" ] "Error" hash

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            let hash =
              "14b116e589789e2effe97e598b98cb071a2f0d53cde1b80caf3c4a2ec3e9fafc"
            p [] "Error" hash

        module Dicts =
          let error =
            let hash =
              "011d37992ad30148858cf8c9eaf9a38cd709b020e144fa718e6ccd85b2352135"
            p [ "Dicts" ] "Error" hash

        module Lets =
          let error =
            let hash =
              "ada72d1d039cc23504898933b705da3d2537e8cd5618812e6105daaa2973eccb"
            p [ "Lets" ] "Error" hash

        module Matches =
          let error =
            let hash =
              "c31d81b343ba5debb1cfdc9977f8908c1280d6ad40bf09430142534f6bd9c981"
            p [ "Matches" ] "Error" hash

        module Enums =
          let error =
            let hash =
              "533b70051f6898c01e017f37b668be277818aa4d97321b71b958136fe89c6f69"
            p [ "Enums" ] "Error" hash

        module Records =
          let error =
            let hash =
              "71a94d192184bfe7350fbce77baf53ed319802c31f7722bf18c40b14ef934e88"
            p [ "Records" ] "Error" hash

        module Applications =
          let error =
            let hash =
              "45d69d587df92aba8ffb29f195aa4443287557c1e509518484911fb46214ab31"
            p [ "Applications" ] "Error" hash

        module Statements =
          let error =
            let hash =
              "5f27ed0633c2da02ce7209f502dd6e9b81cc26ad8153843dc7942475ae0544e4"
            p [ "Statements" ] "Error" hash

        module Unwraps =
          let error =
            let hash =
              "f26d39705defad33988746197a9e671c442dd167d10d62ebcb1968eb479945a4"
            p [ "Unwraps" ] "Error" hash

        module Jsons =
          let error =
            let hash =
              "4481deccfef0f84cbb36d6fd0223a866d68af44220c9f346da16ee8b4213c7a9"
            p [ "Jsons" ] "Error" hash

        module CLIs =
          let error =
            let hash =
              "ecc74d38cd91b77efe57dc67d3957ed361676bc89fc1e6c05a87bb655cff0594"
            p [ "CLIs" ] "Error" hash

        let error =
          let hash =
            "99ebdfc7df009829301853d2038c3cf5b4ddc5d1c69cd018487fe3cb7febdc58"
          p [] "Error" hash

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let contentHash =
        let hash = "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
        p [] "ContentHash" hash

      let nameResolutionError =
        let hash = "f4d41a91096f025fe7d0db4123ef4d31bf47a0602c55da47036f0d5640434f53"
        p [] "NameResolutionError" hash

      let nameResolution =
        let hash = "478d97f39fdb45e063f5595e3198bbbe0c10ad14ea3f9e752507885717af80d1"
        p [] "NameResolution" hash

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          let hash =
            "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          p [] "Package" hash
        let fqTypeName =
          let hash =
            "593dbef1b81d45243efe5c73d94b13caaa337425de5884e2e2fd39ecaaf7d0c7"
          p [] "FQTypeName" hash

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          let hash =
            "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          p [] "Builtin" hash
        let package =
          let hash =
            "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          p [] "Package" hash
        let fqValueName =
          let hash =
            "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          p [] "FQValueName" hash

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          let hash =
            "bd575864526cda832d5e94fa3e827d98681766f1ecb8885e0b5dac1af0cde23a"
          p [] "Builtin" hash
        let package =
          let hash =
            "8092fe01b9ce31a49ce380fc2fe1ee6b6b7d5c15c5f1ecb6ad98c60c4273a435"
          p [] "Package" hash
        let fqFnName =
          let hash =
            "95bdec1ebde43fe053994ba303fc007a0d3255c4c1a34a72340ebe6c97df9204"
          p [] "FQFnName" hash

      let typeReference =
        let hash = "7b125e2ee86f8eb978c01b2b59268f1d61c38f33f37154a8aa5462df19bd4af2"
        p [] "TypeReference" hash

      let letPattern =
        let hash = "376b83ed94797001fbc7349a5e38c92eae1d7a1b17f3897afe8d3acbf4a7f6cb"
        p [] "LetPattern" hash
      let matchPattern =
        let hash = "8cbb963c52eecc0ef51692d9991e7c010812c6448b172b4af8db70e3b38171fd"
        p [] "MatchPattern" hash
      let matchCase =
        let hash = "73647cea52ab37f274f8483cac1e4ad1545c41d9837dfe6431b9524bd1355bbc"
        p [] "MatchCase" hash
      let stringSegment =
        let hash = "b613711b5c977f9c4a3ddb48954283409640b69f4ca4fd2019257d9ed7576f56"
        p [] "StringSegment" hash
      let binaryOperation =
        let hash = "94a439540f7cd066316843e3124648fec75d8e650b7256bb08d428ec0f9d1b5e"
        p [] "BinaryOperation" hash
      let infixFnName =
        let hash = "85cdca8d06c978f066f0aaa59b920f53d2f16e2359d1021aebdd44d3b8a08523"
        p [] "InfixFnName" hash
      let infix =
        let hash = "5d7718518eae19f5a08888b7f211fddf2c4ac4f8822454ff789750d705e37979"
        p [] "Infix" hash
      let pipeExpr =
        let hash = "9577329df15f0255e738e24116847b85cf31217a7a1a98c39a0e8ff6ce03d917"
        p [] "PipeExpr" hash
      let expr =
        let hash = "e6cb6d2c1219dda2e0b06e641fee4370867f51f5ce72ade773813a267a26c513"
        p [] "Expr" hash

      let deprecation =
        let hash = "47b77911f56bc81a6a82dfe81c07d11c88d132efdbef9c4dc8e8807f23de59b2"
        p [] "Deprecation" hash

      let packageLocation =
        let hash = "952f921bae69c1df53ffde67755738bd3071fa97165be6d1e30f1d8c3ee9c706"
        p [] "PackageLocation" hash
      let locatedItem =
        let hash = "61fa6e787a2944d47e8e69b546ed3a2cffa7aa9b7e094db3f20205de6a9914e0"
        p [] "LocatedItem" hash

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField =
          let hash =
            "b1b77c46cf5a190630f3114037828193736fd2440dd2503cb3c5b3156b361453"
          p [] "RecordField" hash
        let enumField =
          let hash =
            "f6b386314c9d99715cf24ab0d9a543580302be25719e7123a92c77ce1eb1b490"
          p [] "EnumField" hash
        let enumCase =
          let hash =
            "f05132a9f1a01079e110f2dd716d5b805f5c9f8938e585873087bd157b852986"
          p [] "EnumCase" hash
        let definition =
          let hash =
            "f5346abc23fb2f9fcd41964b6695f79e7db0d1d0282be6816641ab3eda7b286d"
          p [] "Definition" hash
        let typeDeclaration =
          let hash =
            "7bf0edb27b606d9ef0a70339d0ab02a1fedc7ab6744551bd42fe7cca0f8deb9f"
          p [] "TypeDeclaration" hash

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let name =
          let hash =
            "5ff0b8dd494650769dffcf327f6984b40759547a4b53320e800d2f83eec62028"
          p [] "Name" hash
        let packageType =
          let hash =
            "de0b2843a023665c137bf739443c5d78e8760cd13ba88458b3970dab809cf5a3"
          p [] "PackageType" hash

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let name =
          let hash =
            "b7601a971f5c9af203906fb0c73689e61406d38ad59e14e2b0b3c2a06b8c3e0b"
          p [] "Name" hash
        let packageValue =
          let hash =
            "7a0f278c9c6c4323d43fde4ba888913e5a89faf5003acaf4a804c0fd5226c273"
          p [] "PackageValue" hash

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let name =
          let hash =
            "c0c5a6430d1b022a614c712fe75c1a14f12fb56704c80ede29b3e5ab691f135c"
          p [] "Name" hash
        let parameter =
          let hash =
            "b1b77c46cf5a190630f3114037828193736fd2440dd2503cb3c5b3156b361453"
          p [] "Parameter" hash
        let packageFn =
          let hash =
            "d9d944903aba83c528200a64d5d57c6890e2debced634eb3c1a583b1445ae92e"
          p [] "PackageFn" hash

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType =
          let hash =
            "7824e28c37f8b4f994c3f3752e682fb58229e66743c83ae7a6e4f29515cb4f6a"
          p [] "EntityType" hash
        let searchDepth =
          let hash =
            "364316241bbd776a630ac9c3f92927d3a634ef4446b9d45258a8436a254781ec"
          p [] "SearchDepth" hash
        let searchQuery =
          let hash =
            "854b4d7e83fd11abeada159881bda7adfc1b6596220da9f564779d0ff221e453"
          p [] "SearchQuery" hash
        let searchResults =
          let hash =
            "7557575be405dc61e055c1d3ee751eee82b3a375134f21a39de083ea0cbb48ab"
          p [] "SearchResults" hash

      let packageOp =
        let hash = "e480777599d1cbe69e36517d1507bd1048f6322971f6b5a9bd8c60f1fbdebd4c"
        p [] "PackageOp" hash
      let packageOpBatch =
        let hash = "5735c5cf716a7b1e0133d4f1ac9e77b36680966eecad8a13bd9e1f8f4836f456"
        p [] "PackageOpBatch" hash
      let itemKind =
        let hash = "642ec69245ff338e71dae980d8c7b1cac623b6b49fdd92375df3f90d06cc862f"
        p [] "ItemKind" hash
      let propagateRepoint =
        let hash = "e547cf6954711fccc03281d5eaaaa44695d3490f1a30b0fa84ba3b4c8cb53425"
        p [] "PropagateRepoint" hash

      let secret =
        let hash = "e48530853244d328a390b20a02e568c0782e0e65e6a9e78ae41cfa26c1461527"
        p [] "Secret" hash
      let db =
        let hash = "09b6f46f05cdfabc3c13fdcb2620e6e41642aa933e7f60f4b9c2e42b99d69d0a"
        p [] "DB" hash

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval =
          let hash =
            "5ccec4b619b5c44a34f85dcef3f37ec7bfdb23fe3b989087c406be91ca295429"
          p [] "CronInterval" hash
        let spec =
          let hash =
            "30d826f29e292f9e7db4c4ab184c619017f8fbd36463bea8f10b627f06aeae00"
          p [] "Spec" hash
        let handler =
          let hash =
            "04e1af2c3cc2c2555d61481ef3febe6cb393d96d60545a7dcb54629b07d7651c"
          p [] "Handler" hash

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let errorMessage =
          let hash =
            "2314408c9017bab823414db19433005893a4d09e6235a41011e3ab1f5ae79818"
          p [] "ErrorMessage" hash

  module Cli =
    let executionError =
      let hash = "273caaa3cb6b098342f5c765f87fe87f03a38220d23d3d5a2ce43bc7613a68f0"
      p [ "Cli"; "ExecutionError" ] "ExecutionError" hash

    let script =
      let hash = "dae5fc6163a0ebe22c6ef1f55ec7b77d455041a914e6364191eec0f214644c88"
      p [ "Cli"; "Scripts" ] "Script" hash



  module DarkPackages =
    let stats =
      let hash = "3e1f75d46205cf5fc9515c4bfd3549c42396d10ab9d3333422e02806e7245894"
      p [ "DarkPackages" ] "Stats" hash


  module SCM =
    let private p addl = p ("SCM" :: addl)

    module Branch =
      let private p addl = p ("Branch" :: addl)
      let branch =
        let hash = "07ab66ad0375f0fcdc81c9112a737c1e143499f7a1acd3bba50b1dd5a5da8160"
        p [] "Branch" hash

    module Merge =
      let private p addl = p ("Merge" :: addl)
      let mergeError =
        let hash = "e7fe21b0043e2ab84dc9ffe35fc802ff9ca9be52140c8e318c37eb94689de008"
        p [] "MergeError" hash

    module PackageOps =
      let private p addl = p ("PackageOps" :: addl)
      let commit =
        let hash = "1fcb03395ef96353c26dcd00b3581b8f9885b5e9bbb90885695a22e6fcc5b7d5"
        p [] "Commit" hash

    module Instances =
      let private p addl = p ("Instances" :: addl)
      let instance =
        let hash = "d92c9ba4ed9ee47b468fe5207fd9deb90b3f99baff725f13db5542377f1c4a45"
        p [] "Instance" hash

    module Approvals =
      let private p addl = p ("Approvals" :: addl)
      let approvalStatus =
        let hash = "f54e8be072a3b85e55db6cadc83ce36b5320cfe48715370aea4bb357671667d2"
        p [] "ApprovalStatus" hash
      let approvalRequestStatus =
        let hash = "ad1ea5777bea5af81f162541430eff94dda22b41a7302392ab890f74c81ed49f"
        p [] "ApprovalRequestStatus" hash
      let approvalRequest =
        let hash = "91936c247d449a3b57f6a91d4af875aeeb81aa5f951775052ad4b06d3e995f45"
        p [] "ApprovalRequest" hash
      let requestItem =
        let hash = "2d35eaa09b47d0b31b63a31bc1f0112175dfdde1fe9024f4d56301f184c3f7c7"
        p [] "RequestItem" hash
      let pendingLocationDetails =
        let hash = "a5e6d4d40bb0d919c667529f0bfc511d35ef0df14fb1b81eb0fd48254f0f7199"
        p [] "PendingLocationDetails" hash
      let requestItemWithDetails =
        let hash = "c346af7a450d8409e9281427e59247ed58ea7b6c6d5f0c9f4d111cadd956c9dd"
        p [] "RequestItemWithDetails" hash


  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program =
        let hash = "13f07569930836064de1ea40e233c6aacc82ccfe80818cef96ca74a68a2c01ff"
        p [] "Program" hash
      let secret =
        let hash = "e48530853244d328a390b20a02e568c0782e0e65e6a9e78ae41cfa26c1461527"
        p [] "Secret" hash

    module Infra =
      let tableSize =
        let hash = "e03ffd7442bd76f6e62f0d72da3be80cedf0371d55685eb88c5798186488053f"
        p [ "Infra" ] "TableSize" hash

    module Worker =
      let scheduleRule =
        let hash = "ac7adfd78a92f8f3a98c5cd053b9e00fa2fa3f4c69bc0e0b288640cfcb296273"
        p [ "Worker" ] "ScheduleRule" hash

    module Test =
      let private p addl = p ("Test" :: addl)
      let ptTest =
        let hash = "2e999789ca7bc21bed2acf37046608125910b6c5bb6f886559e3abdae3c6b4d2"
        p [] "PTTest" hash

module Fn =
  let mutable private _lookup = Map []

  let private p modules name (id : string) : ContentHash =
    let hash = ContentHash id
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module List =
      let map =
        let hash = "39840eb85e41c0a11dd9e18d9225ac3850b50a881af27caac0a10b0e58c3a485"
        p [ "List" ] "map" hash

    module HttpClient =
      let request =
        let hash = "dfd9e14b5c6658a83dd41a68edb145766790b61182e6ac93d6618cd778bc5c7b"
        p [ "HttpClient" ] "request" hash


  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      module FnName =
        let private p addl = p ("FnName" :: addl)
        let resolve =
          let hash =
            "33b1abe1fc67c03de59bb86437e506fda91a445416568824899f058e4a86d81a"
          p [] "resolve" hash



    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr =
        let hash = "1de96dfbfda4c8c3a013791a85e6fd2f995a9b9d6dc948ea463d0e6ee4a7c5ed"
        p [ "TestParsing" ] "parsePTExpr" hash

      let parsePTSourceFileWithOps =
        let hash = "1ec354db0b01bec1197572ed7ede5ee7f4fc9ff7e23fa152d9d70851ae6616e5"
        p [ "TestParsing" ] "parsePTSourceFileWithOps" hash

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseCliScript =
          let hash =
            "6bac75ecd4d6a9bf8fce5a5f6304d568611d00e1ed4b7403e652b9b9e6d75c27"
          p [] "parse" hash
        let parseForCli =
          let hash =
            "c829f609c8eee4faeb7a37c168397bb027dfa00dad3cbf7fce40e0da5bfd3684"
          p [] "parseForCli" hash

    module PackageManager =
      let private p addl = p ("PackageManager" :: addl)
      let pm =
        let hash = "8cc83fbf658b8fc5b20dd966ec41f608161c0fb5bf7e698c18ed0a9198839dfb"
        p [] "pm" hash

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      let expr =
        let hash = "79df88862980fbf8992f8a02a032358f7e7223ca507771fca400f743eceda6b8"
        p [] "expr" hash
      let dval =
        let hash = "3092e1e709803fcbfa38833c35755d161f385e530190fdbad12d99dc92f6761e"
        p [] "dval" hash
      let fnName =
        let hash = "7c43b7f750ee56883c61746a5f5cb42e2878d79c1998c25e5dc3f4089da14713"
        p [] "fnName" hash
      let typeReference =
        let hash = "283fc2f25df0a3c31f99ec07dcca86769513fd0bc6d424f2855d23dee3da9df2"
        p [] "typeReference" hash

      module Dval =
        let private p addl = p ("Dval" :: addl)
        let valueTypeName =
          let hash =
            "ba4236bae32639615f24b9c7a32be0999d2a8c6a15a924a14ce47723818832ac"
          p [] "valueTypeName" hash

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString =
          let hash =
            "cf5980dfd14c144a5e75fc4be0c1918b667d172700bb93ef9292fc18e5dba8a3"
          p [] "toString" hash
        let toErrorMessage =
          let hash =
            "35309aed7e05bdeac8e53b970078b47b13ddd5758157bd788b642ca5a1febfc2"
          p [] "toErrorMessage" hash

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let sourceFile =
        let hash = "fcdd53a2514bc5184a361d711aa6efdcd6a1d1216d9f0a5383f5406c650f1360"
        p [] "sourceFile" hash

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let fullForReference =
          let hash =
            "3556d4ff043ae17b0af32f196ec10c21b97c995b273790640ae81535da2bd56e"
          p [] "fullForReference" hash

  module Cli =
    let executeCliCommand =
      let hash = "d482fe465ff73d9f2fec4b17c07509ed66c43eeea8b9b78bf283f0e6cf8467b0"
      p [ "Cli" ] "executeCliCommand" hash

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        let hash = "d7dea178e3d51a524f0492ec1dc95442ab964b183114ed924b2b70a673e87023"
        p [] "parseSingleTestFromFile" hash
