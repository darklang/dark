module Tests.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

open LibPackageManager

module PMPT = Types.ProgramTypes

module ParsesAndDecodesOk =
  let deprecation =
    test "assertFn" {
      let json = """{ "NotDeprecated": [] }"""
      let deserialized =
        SimpleJson.deserialize<PMPT.Deprecation<string>>
          (JsonDeserialization.ProgramTypes.Deprecation.decoder
            SimpleJson.Decoders.string)
          json

      let expected = Ok PMPT.Deprecation.NotDeprecated

      assertEq "msg" expected deserialized
    }

  let packageType =
    test "assertFn" {
      let json =
        """{
          "tlid": 112,
          "id": "f6345e32-f0c6-422a-a9f1-1039a63ce781",
          "name": { "modules": [ "Stdlib", "Result" ], "name": "Result", "owner": "Darklang" },
          "description": "",
          "declaration": {
            "definition": {
              "Enum": [
                [
                  {
                    "name": "Ok",
                    "fields": [ { "description": "", "label": { "None": [] }, "typ": { "TVariable": [ "Ok" ] } } ],
                    "description": ""
                  },
                  {
                    "name": "Error",
                    "fields": [ { "description": "", "label": { "None": [] }, "typ": { "TVariable": [ "Err" ] } } ],
                    "description": ""
                  }
                ]
              ]
            },
            "typeParams": [ "Ok", "Err" ]
          },
          "deprecated": { "NotDeprecated": [] }
        }"""

      let deserialized =
        SimpleJson.deserialize<PMPT.PackageType>
          JsonDeserialization.ProgramTypes.PackageType.decoder
          json

      let expected : PMPT.PackageType =
        { id = System.Guid.Parse "f6345e32-f0c6-422a-a9f1-1039a63ce781"
          name =
            { owner = "Darklang"; modules = [ "Stdlib"; "Result" ]; name = "Result" }
          description = ""
          declaration =
            { typeParams = [ "Ok"; "Err" ]
              definition =
                PMPT.TypeDeclaration.Definition.Enum(
                  NEList.ofListUnsafe
                    ""
                    []
                    [ { name = "Ok"
                        fields =
                          [ { description = ""
                              label = None
                              typ = PMPT.TypeReference.TVariable "Ok" } ]
                        description = "" }
                      { name = "Error"
                        fields =
                          [ { description = ""
                              label = None
                              typ = PMPT.TypeReference.TVariable "Err" } ]
                        description = "" } ]
                )

            }
          deprecated = PMPT.Deprecation.NotDeprecated }

      match deserialized with
      | Ok deserialized -> assertEq "not expected package type" expected deserialized

      | Error(SimpleJson.JsonDeserializationError.ParseError parseError) ->
        Exception.raiseInternal
          "Failed to parse package type"
          [ "json", json; "error", parseError ]
      | Error(SimpleJson.JsonDeserializationError.DecodeError decodeError) ->
        debuG "decodeError" decodeError
        Exception.raiseInternal
          "Failed to decode package type"
          [ "json", json; "error", decodeError ]
    }

let tests =
  testList
    "PackageManager"
    [ ParsesAndDecodesOk.deprecation; ParsesAndDecodesOk.packageType ]
