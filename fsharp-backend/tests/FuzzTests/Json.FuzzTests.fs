/// Generators and FuzzTests that ensure JSON serialization
/// of Runtime Dvals functionality is consistent across OCaml and F# backends
module FuzzTests.Json

open System.Threading.Tasks

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal
module G = Generators

/// Generators and FuzzTests ensuring Runtime Dvals are
/// consistently pretty-printed for machines (code) across
/// OCaml and F# backends.
module PrettyMachineJson =
  type Generator =
    inherit G.NodaTime.All

    static member String() : Arbitrary<string> =
      Arb.fromGen (Generators.ocamlSafeString)

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  /// Ensures Runtime types are consistently serialized
  /// to machine-friendly JSON across OCaml and F# backends
  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      dv
      |> DvalReprExternal.toPrettyMachineJsonStringV1
      |> Newtonsoft.Json.Linq.JToken.Parse
      |> string

    let expected =
      (OCamlInterop.toPrettyMachineJsonV1 dv).Result
      |> Newtonsoft.Json.Linq.JToken.Parse
      |> string

    actual .=. expected

  let tests =
    testList
      "prettyMachineJson"
      [ testProperty typeof<Generator> "roundtripping prettyMachineJson" equalsOCaml ]

module PrettyResponseJson =
  type Generator =
    inherit G.NodaTime.All

    static member String() : Arbitrary<string> =
      Arb.fromGen (Generators.ocamlSafeString)

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    // The json generated is not identical, so check that it parses to the same thing
    let actual =
      try
        dv
        |> LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0
        |> Newtonsoft.Json.Linq.JToken.Parse
        |> string
      with
      | e -> e.Message

    let expected =
      try
        (OCamlInterop.toPrettyResponseJson dv).Result
        |> Newtonsoft.Json.Linq.JToken.Parse
        |> string
      with
      // Task error
      | :? System.AggregateException as e -> e.InnerException.Message
      | e -> e.Message

    actual .=. expected

  let tests =
    testList
      "prettyResponseJson"
      [ testProperty typeof<Generator> "compare to ocaml" equalsOCaml ]

module PrettyRequestJson =
  type Generator =

    inherit G.NodaTime.All

    static member String() : Arbitrary<string> =
      Arb.fromGen (Generators.ocamlSafeString)

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      try
        dv |> BackendOnlyStdLib.LibHttpClient0.PrettyRequestJson.toPrettyRequestJson
      with
      | e -> e.Message

    let expected =
      try
        (OCamlInterop.toPrettyRequestJson dv).Result
      with
      // Task error
      | :? System.AggregateException as e -> e.InnerException.Message
      | e -> e.Message

    actual .=. expected

  let tests =
    testList
      "prettyRequestJson"
      [ testProperty typeof<Generator> "compare to ocaml" equalsOCaml ]

module LibJwtJson =
  type Generator =
    inherit G.NodaTime.All

    static member String() : Arbitrary<string> =
      Arb.fromGen (Generators.ocamlSafeString)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        // They're all printed as blocks, but the OCamlInterop doesn't work great - no point in fixing though
        | RT.DFnVal _ -> false
        | _ -> true)

  // This should produce absolutely identical JSON to the OCaml function or customers will have an unexpected change
  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      dv
      |> BackendOnlyStdLib.LibJwt.Legacy.toYojson
      |> BackendOnlyStdLib.LibJwt.Legacy.toString

    let expected = (OCamlInterop.toSafePrettyMachineYojsonV1 dv).Result

    actual .=. expected


  let roundtripV0 (dv : RT.Dval) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "jwt-roundtrip-v0"

        let privateKey =
          "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ\nd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN\nc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2\nuUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B\nCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW\nDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa\noNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq\np0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+\n1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ\nXUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv\nvz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua\n6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509\nrWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz\nNBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj\namEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr\nr48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5\nPJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J\nhpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g\nMcgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn\n4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV\nctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf\nn8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9\n5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb\nT/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I\nFW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=\n-----END RSA PRIVATE KEY-----"
        let publicKey =
          "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvxW2wuTTK2d0ob5mu/AS\nJ9vYDc/SXy06QAIepF9x9eoVZZVZd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqL\nn9/0Ag9ua4ml/ft7COprfEYA7klNc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGW\nCV7+3DF2RvDV2okk3x1ZKyBy2Rw2uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTw\nMlHbmVv9QMY5UetA9o05uPaAXH4BCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p\n4Ur22mtma+6ree45gsdnzlj1OASWDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ4\n4wIDAQAB\n-----END PUBLIC KEY-----"

        let callWithBoth code symtable =
          task {
            let ast = FSharpToExpr.parsePTExpr code
            let! expected =
              OCamlInterop.execute meta.owner meta.id ast symtable [] []

            let! state = executionStateFor meta Map.empty Map.empty
            let! actual =
              LibExecution.Execution.executeExpr state symtable (PT2RT.Expr.toRT ast)
            return (expected, actual)
          }

        // Encode it with v0 first
        let code = "JWT.signAndEncode_v0 priv payload"
        let symtable = Map [ "priv", RT.DStr privateKey; "payload", dv ]
        let! (expectedEncoded, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expectedEncoded) false "isn't an error"
        Expect.equalDval actual expectedEncoded "signed string matches"

        // check it can be read with v0 decode functions
        let code = "JWT.verifyAndExtract_v0_ster pub encoded"
        let symtable = Map [ "pub", RT.DStr publicKey; "encoded", expectedEncoded ]
        let! (expected, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expected) false "isn't on the error rail"
        Expect.equalDval actual expected "extracted matches ocaml"

        // For v0, the extracted often does not match the original
        // match actual with
        // | RT.DObj map ->
        //   Expect.equalDval map["payload"] dv "extracted matches original"
        // | _ -> Exception.raiseInternal "doesnt match" []

        return true
      }
    try
      if containsPassword dv || containsFakeDval dv then true else t.Result
    with
    | :? System.AggregateException as e ->
      print e.Message
      print e.StackTrace
      false


  let roundtripV1 (dv : RT.Dval) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "jwt-roundtrip-v1"

        let privateKey =
          "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ\nd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN\nc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2\nuUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B\nCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW\nDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa\noNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq\np0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+\n1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ\nXUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv\nvz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua\n6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509\nrWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz\nNBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj\namEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr\nr48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5\nPJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J\nhpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g\nMcgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn\n4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV\nctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf\nn8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9\n5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb\nT/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I\nFW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=\n-----END RSA PRIVATE KEY-----"
        let publicKey =
          "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvxW2wuTTK2d0ob5mu/AS\nJ9vYDc/SXy06QAIepF9x9eoVZZVZd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqL\nn9/0Ag9ua4ml/ft7COprfEYA7klNc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGW\nCV7+3DF2RvDV2okk3x1ZKyBy2Rw2uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTw\nMlHbmVv9QMY5UetA9o05uPaAXH4BCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p\n4Ur22mtma+6ree45gsdnzlj1OASWDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ4\n4wIDAQAB\n-----END PUBLIC KEY-----"

        let callWithBoth code symtable =
          task {
            let ast = FSharpToExpr.parsePTExpr code
            let! expected =
              OCamlInterop.execute meta.owner meta.id ast symtable [] []

            let! state = executionStateFor meta Map.empty Map.empty
            let! actual =
              LibExecution.Execution.executeExpr state symtable (PT2RT.Expr.toRT ast)
            return (expected, actual)
          }

        // Encode it with v1 first
        let code = "JWT.signAndEncode_v1_ster priv payload"
        let symtable = Map [ "priv", RT.DStr privateKey; "payload", dv ]
        let! (expectedEncoded, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expectedEncoded) false "isn't an error"
        Expect.equalDval actual expectedEncoded "signed string matches"

        // check it can be read with v1 decode function
        let code = "JWT.verifyAndExtract_v1_ster pub encoded"
        let symtable = Map [ "pub", RT.DStr publicKey; "encoded", expectedEncoded ]
        let! (expected, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expected) false "isn't on the error rail"
        Expect.equalDval actual expected "extracted matches ocaml"

        return true
      }
    try
      if containsPassword dv || containsFakeDval dv then true else t.Result
    with
    | :? System.AggregateException as e ->
      print e.Message
      print e.StackTrace
      false


  let tests =
    testList
      "jwtJson"
      [ testProperty typeof<Generator> "comparing jwt json" equalsOCaml
        testProperty typeof<Generator> "roundtrip jwt v0" roundtripV0
        testProperty typeof<Generator> "roundtrip jwt v1" roundtripV1 ]
