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
/// Package items are now identified by their content hashes instead of UUIDs.
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
  let private p modules name (hashStr : string) : Hash =
    let hash = Hash hashStr
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    let result =
      p
        [ "Result" ]
        "Result"
        "2e3af0033cfbc86c0e261bdd33ed0058330f3bc647a933c0c28dbb7beb454d7d"
    let option =
      p
        [ "Option" ]
        "Option"
        "daf8ab700bbabf454fa57efd2242a8a856b5423b53359e45fc8fc5b44ce288d2"

    let int8ParseError =
      p
        [ "Int8" ]
        "ParseError"
        "2e46958e373dce147263544a76f932ffb7e107a0af503dafb1778133c91b8a0d"
    let uint8ParseError =
      p
        [ "UInt8" ]
        "ParseError"
        "0bb44cc98b846f65db8f25ce107fb57f10280af908a4e22475eb14928e944bee"
    let int16ParseError =
      p
        [ "Int16" ]
        "ParseError"
        "a95c7babccc84af5b539ff04d60d5c5eb3987b7ab8c6e365791fb663412575cf"
    let uint16ParseError =
      p
        [ "UInt16" ]
        "ParseError"
        "17c5cac7912748307a7fffc0df14c05312acc10c973b286c7ab4159b7a699dda"
    let int32ParseError =
      p
        [ "Int32" ]
        "ParseError"
        "6e8159e7555752e260a003f2da0e97f95eb2b2e213d0e92346be8a2ac1203b8e"
    let uint32ParseError =
      p
        [ "UInt32" ]
        "ParseError"
        "e7b35446e5f951db8475ddde3106f7cfa25c29eaf4ae6d62c5f53c9e437bbd24"
    let int64ParseError =
      p
        [ "Int64" ]
        "ParseError"
        "fe57c19fa8852f7c5a75bc7352e5e79f2e819a97eba2954da21a69ff2b0eb492"
    let uint64ParseError =
      p
        [ "UInt64" ]
        "ParseError"
        "db3ea47e9e8724b0a257ebf10c51e6cfbf3c351ca5bd65397a0c2594b7c3e240"
    let int128ParseError =
      p
        [ "Int128" ]
        "ParseError"
        "0c3417a5c77a077dfef7070c9d631f1fe692a16f28e32e665ed76896928ba35d"
    let uint128ParseError =
      p
        [ "UInt128" ]
        "ParseError"
        "5b81e8526282465fbf209cab17dce4b3d436574dfb60520784abc000ef5c99c1"
    let floatParseError =
      p
        [ "Float" ]
        "ParseError"
        "dc0890261dd008abc6eaf962bf2677a68e57637e620b704d98538f1fa09829b9"
    let uuidParseError =
      p
        [ "Uuid" ]
        "ParseError"
        "ba534998448a1e2ba411d80b2e195f93ba903a8a4d8d9ff1cbd7e7b02db1b6d4"

    module Http =
      let request =
        p
          [ "Http" ]
          "Request"
          "aa3ead7085e100cd1be6a6c19e7109482bef15ff7e77d67ef248cfea722f4cba"
      let response =
        p
          [ "Http" ]
          "Response"
          "0af69f8d88bfe21b84b43b84942b608f909ac4122ea15893c3eaff650d6313ea"

    module HttpClient =
      let private p addl = p ("HttpClient" :: addl)
      let badHeader =
        p
          []
          "BadHeader"
          "a2bba4ca7dfa9ddf3fbcea04a689a30db600f15bd1c5959ae2d08bbe14b08492"
      let badUrlDetails =
        p
          []
          "BadUrlDetails"
          "a6796f85d4ae43b64f50409e776ea9c7894f88d4bca8aa4b56cb345eccff95a6"
      let requestError =
        p
          []
          "RequestError"
          "e75e626f743ec3b1c3e693f6385b601b6bda050e012834bc4c2e7d08a101d223"
      let response =
        p
          []
          "Response"
          "03f0e1b2dac46b61da2c39744579e674ae1b1ab5182382647e7ce64dac25a30d"

    module Json =
      module ParseError =
        let private p addl = p ("Json" :: "ParseError" :: addl)

        module JsonPath =
          let part =
            p
              [ "JsonPath"; "Part" ]
              "Part"
              "40c32a77a8e9c8848a670d2dc4985804915c660d7750a34daa8a6daac410892a"
        let jsonPath =
          p
            []
            "JsonPath"
            "55297e9f433f35383c4272e5ef83a312fc7911cde74634daea26e02c518da341"
        let parseError =
          p
            []
            "ParseError"
            "832b943038d7181b9c7ed3ea1cea2773d708e0e7e678e0415d5497206e38ecc7"

    module AltJson =
      let private p addl = p ("AltJson" :: addl)
      let parseError =
        p
          [ "ParseError" ]
          "ParseError"
          "c9c3abe3f6d38cad10f0f5c902367706cdde4cc0d94a1c7fd3513b1a1e30cc05"
      let json =
        p
          []
          "Json"
          "98bcd29921b522613c44499f277de319f9ece4ad8330e2ec71952ce578d9cfca"

    module Cli =
      let private p addl = p ("Cli" :: addl)
      let executionOutcome =
        p
          []
          "ExecutionOutcome"
          "ef9493927cdcf5e0e5a57206223371bdd71ebc5e73906bfddfd12e20b6318a80"

      module OS =
        let private p addl = p ("OS" :: addl)
        let os =
          p
            []
            "OS"
            "4788c4345c88eb606c9511cd4069fd19f5457060d9583a0de23f9646b2936f58"

      module Stdin =
        let private p addl = p ("Stdin" :: addl)
        let modifiers =
          p
            [ "Modifiers" ]
            "Modifiers"
            "783430d6509dc86cd741b156abf0eab25232953c890ce1dc1475ca775599de1f"
        let key =
          p
            [ "Key" ]
            "Key"
            "e6c3106cced222720e235bcaf5f1ec5bd9ddae230270359b4201042bd5b66945"
        let keyRead =
          p
            [ "Key" ]
            "KeyRead"
            "e7d5f7708985bd9a74ea134a88d4493a1a6bd65aa22ac63b1b27aad700333598"

  module LanguageTools =
    let private p addl = p ("LanguageTools" :: addl)
    let sign =
      p [] "Sign" "4ecce1451b44ff77dfef69450500a2299338024370bbfb92f71b2b5a3f057d9e"

    // TODO: where do these actually belong? are they used, even?
    let builtinValue =
      p
        []
        "BuiltinValue"
        "d4d24c92788aa583c3c995381a8a466a9415e321008057906ef974ed0ba1234c"
    let builtinFnParam =
      p
        []
        "BuiltinFunctionParameter"
        "5a1aef167cf50ffcf4a64e3da15c298216ec59893862d7006cae433299a40e83"
    let builtinFn =
      p
        []
        "BuiltinFunction"
        "f01a3888da2dc38b49473fa485e2bbceeb20be1bfa9a3166181522a22d599129"


    module Parser =
      let private p addl = p ("Parser" :: addl)
      let point =
        p
          []
          "Point"
          "43c7e325d72f8c4c0e057025d5cfb675d7424f22e17416868ee5ed8c8bc6821a"
      let range =
        p
          []
          "Range"
          "eba3e5dc13be2a3c5952498ed7aed0744218a19beed0b1bbd506922420fb09a0"
      let parsedNode =
        p
          []
          "ParsedNode"
          "03d32c1cd92c315c53a095c9f2603a7d6da5948240d36ba4be146f35114005f2"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let pTCliScriptModule =
          p
            []
            "PTCliScriptModule"
            "55551b19c31739a686aa5cdc4c51d94b9e1e986e37e8b410667607be922e002b"


    module NameResolver =
      let private p addl = p ("NameResolver" :: addl)
      let nameResolverOnMissing =
        p
          []
          "OnMissing"
          "01c794cfd8d981827c028f0f344a3d88b15ff2ecc00c412361665762c293d24c"


    module WrittenTypes =
      let private p addl = p ("WrittenTypes" :: addl)
      let name =
        p
          []
          "Name"
          "151a213da1a76c6de1bf44d41291808487e1a407ed288cd1c9f512b6ceaa3f23"
      let range =
        p
          []
          "Range"
          "571d3f56758ccbf02552e3a329d4a5e67f74da03c2f9722bc07a6ef4c1f411b3"


    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)
      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          p
            []
            "Package"
            "b11b1e0c18e8cb137f4c7781c98542dd492f18b9e443606f3d95bcff968d59ad"
        let builtin = p [] "Builtin" "67c7c385-91e9-4f37-91c8-117a0d6a1ace"
        let fqTypeName =
          p
            []
            "FQTypeName"
            "8b91304e4b5af8439370070a1dc0a0a93a348e5b402f8e023b0af55ae8031103"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let package =
          p
            []
            "Package"
            "9106bcaffaa5393302c51d5a392cc3d16f6d7b6e819b230fdbc7ccdeaeec9103"
        let builtin =
          p
            []
            "Builtin"
            "d15ec46e1362a2f051ef1a484f92669da90df8bd18e9779a6a8431cec350e825"
        let fqValueName =
          p
            []
            "FQValueName"
            "8496398d43991b1d8ca0593d1e3b10a5959ba16d1c4ab8ca74a823286624def6"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let package =
          p
            []
            "Package"
            "055685be7d03fa5a4b24b8dd039f98b71c77fc0434cfc0dfc28b56947ed5f20b"
        let builtin =
          p
            []
            "Builtin"
            "c42eda1a3f071071536f16a3447ba1d2e37194934905cf3fd0b1401ccdea4f4d"
        let fqFnName =
          p
            []
            "FQFnName"
            "efcd5d239b04f28bc0d9ec83043c2ae65dd1bd4a65a7da79cb0e7485f8384026"

      let nameResolutionError =
        p
          []
          "NameResolutionError"
          "35194b2d29ce57a2d68c8bdc1e7b948b437eed5d2a7f653cbed62f9715df49e1"
      let nameResolution =
        p
          []
          "NameResolution"
          "214bc85454dc2fb432c6dad6266b2cca29caf15b149733921718b6ed82d980e8"

      let typeReference =
        p
          []
          "TypeReference"
          "f0d0918a6656812e3093eb262557e0d93a86439b6a101bc703324b54d586e328"
      let param = // does this exist anywhere?
        p
          []
          "Param"
          "4c13515d44037b288f15fffa62873ea37a8241dc12993a82b8b6c88d73a9d79e"
      let letPattern =
        p
          []
          "LetPattern"
          "bb866778a86041d98385435b4b0ba13316b9497fbac9a441f55808e0a267e572"
      let matchPattern =
        p
          []
          "MatchPattern"
          "23ab809340b295fd84ab55121fc5475ba83d1c57203c99ca0eca4f95e96a6497"
      let matchCase = // does this exist anywhere?
        p
          []
          "MatchCase"
          "4491b4b6b69a986377a30896d05c47f2745ecedd2956b0bd148bbb646fb1b787"
      let stringSegment = // does this exist anywhere?
        p
          []
          "StringSegment"
          "019a62c69924d1029ab602d5c796b058bd61f356fa8a4257dcfee4ae186f1d42"

      let dval =
        p
          []
          "Dval"
          "7a42777dd1fb3aae43be6f9c9aad995ff3992a64c5fa623f96dd7c114afcb9a3"
      let knownType =
        p
          []
          "KnownType"
          "b881d41531634a682d6247fee7474f1bad518b2113fb2ce512c14b67d5115f5d"
      let valueType =
        p
          []
          "ValueType"
          "c6a6e0bc2d9d78c28f7b4473493722c522660f59967d44d628e8fe56ca0fe20f"

      let applicableNamedFn =
        p
          []
          "ApplicableNamedFn"
          "f1adf47552773555f08729632c9849ff4525e35f25c2f32952c217ac5596b925"
      let applicableLambda =
        p
          []
          "ApplicableLambda"
          "bb9883490f8130985379cd436d2069cd2a2d105bdb2b776825d27d651a4e3a5b"
      let applicable =
        p
          []
          "Applicable"
          "5f1634486ebd6f1c8581144a528f72162e59a3e9b1967c5d5b4635f267fbbccc"


      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)

        module TypeChecking =
          let private p addl = p ("TypeChecking" :: addl)
          let typeCheckPathPart =
            p
              []
              "TypeCheckPathPart"
              "1966c94432bfedffeec98e505736a518a0e8b391e10fd979c7104ae26c2d2e67"
          let reverseTypeCheckPath =
            p
              []
              "ReverseTypeCheckPath"
              "c4f4f6c122c1cbd9a29a9cf3a0b6f585e5f28bb1b21d7a9ce0517a090283860f"

        module Bools =
          let error =
            p
              [ "Bools" ]
              "Error"
              "2e996e5e2b2ab3e7ba69455816ea3a5c599cc2ea7bf3cda45cfbbc3e756ae58c"

        module Ints =
          let error =
            p
              [ "Ints" ]
              "Error"
              "9e764c84a1047fa4e9cd6a70ed72eff743f75af8b6764456055f6ab1f268246b"

        module Strings =
          let error =
            p
              [ "Strings" ]
              "Error"
              "8e80cdb02ad0aebaaf36537a8b07c02f9f1beaed74e5278668ab80db2f556a48"

        module Lists =
          let private p addl = p ("Lists" :: addl)
          let error =
            p
              []
              "Error"
              "8bb541612507cf602a5cfefc2069ee54a36ae3ea410a94fca5990c7aa6c71561"

        module Dicts =
          let error =
            p
              [ "Dicts" ]
              "Error"
              "e6c74a394f2bfd76e9a6f4445d56cae5d5605a798b2f5149b1991283ebd0352d"

        module Lets =
          let error =
            p
              [ "Lets" ]
              "Error"
              "070ced804587215a8385d1718713f2e689ef043fcb13fbc87fb5875c686dd4b9"

        module Matches =
          let error =
            p
              [ "Matches" ]
              "Error"
              "41ad583a0d6dd90c226fa142523a6f424eac87d3b9ff11ebbe5410c2cff909af"

        module Enums =
          let error =
            p
              [ "Enums" ]
              "Error"
              "ec4aa114e9c678c04d83e8a4da59622c6dc4f4bf0e681f515fb75b907a007af0"

        module Records =
          let error =
            p
              [ "Records" ]
              "Error"
              "8e448c4578a1b658dae9344a00c2127408895628d8aef98cdfa0345c9b83f246"

        module Applications =
          let error =
            p
              [ "Applications" ]
              "Error"
              "22fba83a2b39e78e9e6222c950d0ba4c585da2ae82ad8cdfabf7bd6fc700fc39"

        module Statements =
          let error =
            p
              [ "Statements" ]
              "Error"
              "70419c7ec277b4a8996e1dbbc3a158c75bec432d4ab4f2b59e716a24f015fe64"

        module Unwraps =
          let error =
            p
              [ "Unwraps" ]
              "Error"
              "4c3f10121944a3b12e2cea93ba4245f5303c0f63ead79b31358adaedb9289a5f"

        module Jsons =
          let error =
            p
              [ "Jsons" ]
              "Error"
              "3bb7d1e509d325e9dd8bb5ba7943e8565266e1af615777b36b4a6a8f6b623c58"

        module CLIs =
          let error =
            p
              [ "CLIs" ]
              "Error"
              "2a4259e2939acf1b3f18e3a231b7097c631a47a9899c1ebb127de0db2a288325"

        let error =
          p
            []
            "Error"
            "dbf3368afcd62d8c06f10490fe10829731a18e3f287fcf2a9513f48c81bcc62b"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)

      let nameResolutionError =
        p
          []
          "NameResolutionError"
          "6b7f125f28101601f11bd6c82f0b8d435ef3b241ab6277645c6742ab01249fc5"

      module FQTypeName =
        let private p addl = p ("FQTypeName" :: addl)
        let package =
          p
            []
            "Package"
            "6b56851992e45b58146529762d7ba18a9e131bdf524d1e5fa458b460de1b3628"
        let fqTypeName =
          p
            []
            "FQTypeName"
            "74a250b1814ba9a3f7c0d11636b1db15c4c30a786003d4294aa490c7e0f2911e"

      module FQValueName =
        let private p addl = p ("FQValueName" :: addl)
        let builtin =
          p
            []
            "Builtin"
            "27eb1ab8f827ccf2b47019300af46ce6067a56f91a612aff441246a943cd5250"
        let package =
          p
            []
            "Package"
            "027644714f38c9fb8e71845470e39c38b1a2c0e0626c6747d6dc9ff6624b4a7b"
        let fqValueName =
          p
            []
            "FQValueName"
            "1cbf38826e5f4fa0f1164d84dedefeec82d960f88c7baaf401b666017676703f"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let builtin =
          p
            []
            "Builtin"
            "8f4996154c9fd0acdb467718cc0c7fcb4f36a5c0fc3b96d792e0249cb7101e69"
        let package =
          p
            []
            "Package"
            "5366b04fc6123e9ed318d992338730e2cb1e105349f6519c6485da2a2745b45e"
        let fqFnName =
          p
            []
            "FQFnName"
            "aaaf999e90e61d59d6c7f71dfcc5eb52cffd65b26d743551f9202be899afb488"

      let typeReference =
        p
          []
          "TypeReference"
          "106676a5a5605c8a63f74f506431b7174ec150fc0ae46d20667b9b111218d328"

      let letPattern =
        p
          []
          "LetPattern"
          "aea6efe319b3550fc478261cb8baa5b7e94fdd96412e93f1606258715994f058"
      let matchPattern =
        p
          []
          "MatchPattern"
          "809284630038fe7393faebd58cf67e552656df995da45be6fb1913619e7a7459"
      let matchCase =
        p
          []
          "MatchCase"
          "12817d338fc6ece80c5a347f7d2af45e17058e360c67b10192657dee882fd31e"
      let stringSegment =
        p
          []
          "StringSegment"
          "cf953571a709bde5cc5382752358ef76450b64a79954bda6260d1c4cf9bc467a"
      let binaryOperation =
        p
          []
          "BinaryOperation"
          "358192ad3a174b19c0ccf997ad3b57990d3fda4d6de33412f68f94082f1364b3"
      let infixFnName =
        p
          []
          "InfixFnName"
          "129b4cd90cda8330f739ce14433716f6d6076b962a908670e818cb2ccbd9fa73"
      let infix =
        p
          []
          "Infix"
          "f5244cf1b6dcdf3246e14ba81a7993916c0615229a45e3ab611d1167f7d9474b"
      let pipeExpr =
        p
          []
          "PipeExpr"
          "6563f7c04a77ae9869ab7d18505eee4b5fb8ce2217a120c5c5a4dd92104bd92c"
      let expr =
        p
          []
          "Expr"
          "645263384a25f889119957050d3d8f1b32a9bddd1650303222ddf3e2dd888df9"

      let deprecation =
        p
          []
          "Deprecation"
          "0a1d48aba47e7ba24c4d7e80d692c40c6b3272b6c61150914e0d5b4a0e620a07"

      module TypeDeclaration =
        let private p addl = p ("TypeDeclaration" :: addl)
        let recordField =
          p
            []
            "RecordField"
            "5d20db7615c5c99b8f2e7176f1e215bc4899fb0ad835338ff657c57e2f5d008e"
        let enumField =
          p
            []
            "EnumField"
            "4443586cd013fa8f09ac89f834099605d62bfd12b2b04aec1c66c3909c28ec35"
        let enumCase =
          p
            []
            "EnumCase"
            "e10a26a2050bdaab5438f9b2200edf53bebaf2d1fab6d03b72715fb83cf047b1"
        let definition =
          p
            []
            "Definition"
            "06cf50ff0a5bac3e5bcb5e2eddc6f7a05cf2ba1c24a72b11b08c96bef6f3ad97"
        let typeDeclaration =
          p
            []
            "TypeDeclaration"
            "44c728d5a2d7dc01f28e652e4ec2d24e38faa7e9a1f54709d8a65ccb8c7c871d"

      module PackageType =
        let private p addl = p ("PackageType" :: addl)
        let name =
          p
            []
            "Name"
            "9f0d7b8821fda85a708a1e541446e15aa500d3b5157ba527b7c939ebf6aadba3"
        let packageType =
          p
            []
            "PackageType"
            "0d87bdb83d33dccb7fb7bc9fc04a629c09b1fbf240ad66d31750eab0c497e390"

      module PackageValue =
        let private p addl = p ("PackageValue" :: addl)
        let name =
          p
            []
            "Name"
            "cf6a194493c37781d28460c0a21f8757c4249a03ac1b7a12b89fd382b3f5f9cc"
        let packageValue =
          p
            []
            "PackageValue"
            "0b076aad5df43ca1c1797885716a51d4832a83612aded04b3cb2e61fa3959524"

      module PackageFn =
        let private p addl = p ("PackageFn" :: addl)
        let name =
          p
            []
            "Name"
            "d956ce5cdbaa26545cd7c5bfac18b1c00275f9095e958d815ad9eb4b99cfcff7"
        let parameter =
          p
            []
            "Parameter"
            "4e7750a6e01f60c5f988607af461f2c7fbf30a3ae75dba0eb70f3161890ab313"
        let packageFn =
          p
            []
            "PackageFn"
            "9893171d1dd87e840c5d34b16e79b6bef2ea1915fbcce2e554c83c56c8e6d75f"

      module Search =
        let private p addl = p ("Search" :: addl)
        let entityType =
          p
            []
            "EntityType"
            "1dd1eac217ed7d765b418c8496025805408e8ed8a4023d5aab146c661183fad8"
        let searchDepth =
          p
            []
            "SearchDepth"
            "fa10141e8a87f0f1677f2164fee4a086ca5e713ab90c011659a335ef2424c350"
        let searchQuery =
          p
            []
            "SearchQuery"
            "30161e1968462dc458d1bc253c68850e8fb2c545608727cc7407f237e7375478"
        let searchResults =
          p
            []
            "SearchResults"
            "7a3996dc9916a6266eca81e405cf2d9ee4a85a0eda16478a31e68a5d8e5932f5"

      let secret =
        p
          []
          "Secret"
          "f5bf9bac6f842bbaa498b0ef0cfa95f902889437d471ef7e2a393cf485db8bde"
      let db =
        p [] "DB" "cd1ffbe29ac38a520958b6e5d41af1e4ff8f5cacb95d78537718ba59dd5c3627"

      module Handler =
        let private p addl = p ("Handler" :: addl)
        let cronInterval =
          p
            []
            "CronInterval"
            "8a8f092db263f0de2d05da87a48fbd74e994d68d1650ff39266e9a90802e7c08"
        let spec =
          p
            []
            "Spec"
            "da02512bc78bdc9ed5c3a1797319d9746a056b37e7e1d1397a808c14fec17b1d"
        let handler =
          p
            []
            "Handler"
            "d9e98ef3b27cfdd305dc70272af96e9f882c5a40eb36efd8ead70db04661f4a5"

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
            "74428304ac84f1eabc79bb1450464b78bee7f77b1901c83a01b6fcc828351f95"

  module Cli =
    let executionError =
      p
        [ "Cli"; "ExecutionError" ]
        "ExecutionError"
        "f375a4fc5ed0c29ed1c5de4ca551272ba0ec13f11126263df7886cd58e4d8efd"

    let script =
      p
        [ "Cli"; "Scripts" ]
        "Script"
        "bbe14271ce9848d14e37ebcca8c164bf9a12d818b0fb6b7a9da3f7833314621f"


  module DarkPackages =
    let stats =
      p
        [ "DarkPackages" ]
        "Stats"
        "95ab7357ffe77fcf63da44a6ca0dcf2cd7e16fe22c0f89ce53495333b0d2a582"



  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Canvas =
      let private p addl = p ("Canvas" :: addl)
      let program =
        p
          []
          "Program"
          "700a1b2de5502deeae8c53c60cae9cfc90dab02ede423106093985740464fef9"
      let secret =
        p
          []
          "Secret"
          "e9700db9cea11363c90e919f88274a6ce79c5078f9cac4920d484c5251414b4c"

    module Infra =
      let tableSize =
        p
          [ "Infra" ]
          "TableSize"
          "30c94656822c95710d431fa5c46e880f48e851ce05e0f63d1db565f184660f1e"

    module Worker =
      let scheduleRule =
        p
          [ "Worker" ]
          "SchedulingRule"
          "dd7b810d8db47e4bb78d7bdd31e8d2c15425821c2992d9e05cdf2e6e3584d84b"

    module Test =
      let private p addl = p ("Test" :: addl)
      let ptTest =
        p
          []
          "PTTest"
          "e93d58403910af3345858b76898c9345a37dbdbdb79950da03a2e44fa0acd838"

  // what we expose to the outside world
  let hashForName (owner : string) (modules : List<string>) (name : string) : Hash =
    match owner with
    | "Darklang" ->
      match Map.get (modules, name) _lookup with
      | Some hash -> hash
      | None -> Hash "unknown-type"
    | _ -> Hash "external-type"


module Value =
  // There are no referenced Values at this point,
  // but we may be thankful later for hooking this up in the meantime.
  let hashForName
    (_owner : string)
    (_modules : List<string>)
    (_name : string)
    : Hash =
    Hash "external-value"



module Fn =
  let mutable private _lookup = Map []

  let private p modules name (hashStr : string) : Hash =
    let hash = Hash hashStr
    _lookup <- _lookup |> Map.add (modules, name) hash
    hash

  module Stdlib =
    let private p addl = p ("Stdlib" :: addl)

    module List =
      let map =
        p
          [ "List" ]
          "map"
          "fd12037bb415b46cbbe0d2f4da31030321dc1cf96746a5e3aa51afa26c61a973"

    module HttpClient =
      let request =
        p
          [ "HttpClient" ]
          "request"
          "fc0ae3d6c1709bd0186a98da1841827b6198ff2c1006370b8481b5bb2331755e"


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
            "8dd81543c21759a2ee62611d840d461ef9ef959f9306e46ffc32095a522a9536"



    module Parser =
      let private p addl = p ("Parser" :: addl)
      let parsePTExpr =
        p
          [ "TestParsing" ]
          "parsePTExpr"
          "36d5ea8d231f869717405d10fbe00ddb5f2194e54b341af7d7604f5f66073087"

      let parseAndPrettyPrint =
        p
          [ "TestParsing" ]
          "parseAndPrettyPrint"
          "a5923111527c9d8675dbb94b5f21b874b20b6fd4cdd0c2b4fa7bd42202b14f8d"

      module CliScript =
        let private p addl = p ("CliScript" :: addl)
        let parseCliScript =
          p
            []
            "parse"
            "80d1c351943ca1373189f3461b367b9ff8d9d4d7113749628c7d64dc79fbd3b4"

    module PackageManager =
      let private p addl = p ("PackageManager" :: addl)
      let pm =
        p [] "pm" "2ca2fa862cd74111dd4ea6ea0dfd2a746f37d026db0fce373789c1e3a8910e4c"

  module PrettyPrinter =
    let private p addl = p ("PrettyPrinter" :: addl)
    module RuntimeTypes =
      let private p addl = p ("RuntimeTypes" :: addl)

      module RuntimeError =
        let private p addl = p ("RuntimeError" :: addl)
        let toString =
          p
            []
            "toString"
            "2b77ebe969f7de844efefb7dd2d24191c3a9289572e9a85caf03ad20bf9da834"
        let toErrorMessage =
          p
            []
            "toErrorMessage"
            "2ae9c7533d45cf7ac9cb71e91eac5604e9f03da60794f9cae20f32923c173cee"

    module ProgramTypes =
      let private p addl = p ("ProgramTypes" :: addl)
      let expr =
        p
          []
          "expr"
          "09c274ec906b3ebdf45b153344c90ccd0e267bb6d223ef4d3f207ba1598ad701"

      module FQFnName =
        let private p addl = p ("FQFnName" :: addl)
        let fullForReference =
          p
            []
            "fullForReference"
            "ac886b31ae1060f1ed73b285a807a74123e8e5f4ca34df8cd7c86781d83b240a"

  module Cli =
    let executeCliCommand =
      p
        [ "Cli" ]
        "executeCliCommand"
        "7f6cfac064365af380aa69a93f1181872e672bf817501cafee5e73b8362ba925"

  module Internal =
    let private p addl = p ("Internal" :: addl)
    module Test =
      let private p addl = p ("Test" :: addl)
      let parseSingleTestFromFile =
        p
          []
          "parseSingleTestFromFile"
          "594fb3496f67541e2d537cccc42b4a454cb5fbd6e0c3ab253ca1ce71d5fcbbcc"

  // what we expose to the outside world
  let hashForName (owner : string) (modules : List<string>) (name : string) : Hash =
    match owner with
    | "Darklang" ->
      match Map.get (modules, name) _lookup with
      | Some hash -> hash
      | None ->
        // Generate a unique hash based on the function name for consistency
        let fullName = $"""{owner}.{String.concat "." modules}.{name}"""
        let hashBytes = System.Text.Encoding.UTF8.GetBytes(fullName)
        use sha256 = System.Security.Cryptography.SHA256.Create()
        let computedHash = sha256.ComputeHash(hashBytes)
        let hashString =
          System.BitConverter
            .ToString(computedHash)
            .Replace("-", "")
            .ToLowerInvariant()
        Hash hashString
    | _ ->
      let fullName = $"""{owner}.{String.concat "." modules}.{name}"""
      let hashBytes = System.Text.Encoding.UTF8.GetBytes(fullName)
      use sha256 = System.Security.Cryptography.SHA256.Create()
      let computedHash = sha256.ComputeHash(hashBytes)
      let hashString =
        System.BitConverter
          .ToString(computedHash)
          .Replace("-", "")
          .ToLowerInvariant()
      Hash hashString
