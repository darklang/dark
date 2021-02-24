module Tests.DvalRepr

open Expecto
open Prelude
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr


// module Resp = Cohttp_lwt_unix.Response
// module Req = Cohttp_lwt_unix.Request
// module Header = Cohttp.Header

let testInternalRoundtrippableDoesntCareAboutOrder =
  test "internal_roundtrippable doesn't care about key order" {
    Expect.equal
      (DvalRepr.ofInternalRoundtrippableV0
        "{
           \"type\": \"option\",
           \"value\": 5
          }")
      (DvalRepr.ofInternalRoundtrippableV0
        "{
           \"value\": 5,
           \"type\": \"option\"
          }")
      ""
  }


let testDvalRoundtrippableRoundtrips =
  testList
    "roundtrippable dvals roundtrip"
    (sampleDvals
     |> List.filter
          (function
          | _, RT.DFnVal _ -> false
          // | _, RT.DPassword _ -> false // FSTODO
          | _ -> true)
     |> List.map
          (fun (name, dv) ->
            test $"{name}: {dv}" {
              Expect.equalDval
                dv
                (dv
                 |> DvalRepr.toInternalRoundtrippableV0
                 |> DvalRepr.ofInternalRoundtrippableV0)
                "full"

              Expect.equal
                (dv |> DvalRepr.toInternalRoundtrippableV0)
                (dv
                 |> DvalRepr.toInternalRoundtrippableV0
                 |> DvalRepr.ofInternalRoundtrippableV0
                 |> DvalRepr.toInternalRoundtrippableV0)
                "extra"
            }))

// FSTODO: test that printable formats support all data types
// FSTODO: test nested objects

let testDvalOptionQueryableSpecialCase =
  test "dval Option Queryable Special Case" {
    let dvm = Map.ofList [ ("type", RT.DStr "option"); ("value", RT.DInt 5I) ]

    Expect.equal
      (RT.DObj dvm)
      (dvm |> DvalRepr.toInternalQueryableV1 |> DvalRepr.ofInternalQueryableV1)
      "extra"
  }

let testDvalUserDBV1Migration =
  let forward v =
    // Saved with old version, can be read with new version *)
    v |> DvalRepr.toInternalQueryableV0 |> DvalRepr.ofInternalQueryableV1

  let backwards v =
    // Saved with new version, can be read with old version
    v
    |> (function
    | RT.DObj dvm -> dvm
    | _ -> failwith "dobj only here")
    |> DvalRepr.toInternalQueryableV1
    |> DvalRepr.ofInternalQueryableV0

  sampleDvals
  // These are the field types allowed in the DB
  |> List.filter
       (fun (_, dv) ->
         match dv with
         | RT.DInt _
         | RT.DFloat _
         | RT.DBool _
         | RT.DNull
         | RT.DStr _
         | RT.DList _
         | RT.DDate _
         // | DPassword _
         | RT.DUuid _
         | RT.DObj _ -> true
         | RT.DChar _
         | RT.DDB _
         | RT.DError _
         | RT.DIncomplete _
         | RT.DErrorRail _
         | RT.DOption _
         | RT.DResult _
         | RT.DFnVal _
         | RT.DHttpResponse _
         | RT.DBytes _ -> false)
  |> fun fields ->
       test $"dbv1migration" {
         let dv = RT.DObj(Map.ofList fields)
         Expect.equalDval dv (forward dv) $"forward"
         Expect.equalDval dv (backwards dv) $"backwards"
       }

// let testDateMigrationHasCorrectFormats () =
//   let str = "2019-03-08T08:26:14Z" in
//   let date = RT.DDate(System.DateTime.ofIsoString str) in
//   let oldFormat = $"{{ \"type\": \"date\", \"value\": \"{str}\"}}"
//   Expect.equal (Legacy.toPrettyMachineJsonStringV0 date) oldFormat "old version"
//   Expect.equal (DvalRepr.toPrettyMachineJsonStringV1 date) $"\"{str}\"" "new version"
//

// let t_password_json_round_trip_forwards () =
//   let password = DPassword (Bytes.of_string "x") in
//   check_dval
//     "Passwords serialize and deserialize if there's no redaction."
//     password
//     ( password
//     |> Dval.to_internal_roundtrippable_v0
//     |> Dval.of_internal_roundtrippable_v0
//     |> Dval.to_internal_roundtrippable_v0
//     |> Dval.of_internal_roundtrippable_v0 )
//
//
// let t_password_serialization () =
//   let does_serialize name expected f =
//     let bytes = Bytes.of_string "encryptedbytes" in
//     let password = DPassword bytes in
//     AT.check
//       AT.bool
//       ("Passwords serialize in non-redaction function: " ^ name)
//       expected
//       (String.is_substring
//          ~substring:(B64.encode "encryptedbytes")
//          (f password))
//   in
//   let roundtrips name serialize deserialize =
//     let bytes = Bytes.of_string "encryptedbytes" in
//     let password = DPassword bytes in
//     AT.check
//       at_dval
//       ("Passwords serialize in non-redaction function: " ^ name)
//       password
//       (password |> serialize |> deserialize |> serialize |> deserialize)
//   in
//   (* doesn't redact *)
//   does_serialize
//     "to_internal_roundtrippable_v0"
//     true
//     Dval.to_internal_roundtrippable_v0 ;
//   (* roundtrips *)
//   roundtrips
//     "to_internal_roundtrippable_v0"
//     Dval.to_internal_roundtrippable_v0
//     Dval.of_internal_roundtrippable_v0 ;
//   (* redacting *)
//   does_serialize
//     "to_enduser_readable_text_v0"
//     false
//     Dval.to_enduser_readable_text_v0 ;
//   does_serialize
//     "to_enduser_readable_html_v0"
//     false
//     Dval.to_enduser_readable_html_v0 ;
//   does_serialize "to_developer_repr_v0" false Dval.to_developer_repr_v0 ;
//   does_serialize
//     "to_pretty_machine_json_v1"
//     false
//     Dval.to_pretty_machine_json_v1 ;
//   does_serialize
//     "to_pretty_request_json_v0"
//     false
//     Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 ;
//   does_serialize
//     "to_pretty_response_json_v1"
//     false
//     Libexecution.Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0 ;
//   ()
//
//
// (* put it in an object too *)
// let t_password_serialization2 () =
//   let roundtrips name serialize deserialize =
//     let bytes = Bytes.of_string "encryptedbytes" in
//     let password = DObj (DvalMap.singleton "x" (DPassword bytes)) in
//     let wrapped_serialize dval =
//       dval
//       |> (function
//            | DObj dval_map ->
//                dval_map
//            | _ ->
//                Exception.internal "dobj only here")
//       |> serialize
//     in
//     AT.check
//       at_dval
//       ("Passwords serialize in non-redaction function: " ^ name)
//       password
//       ( password
//       |> wrapped_serialize
//       |> deserialize
//       |> wrapped_serialize
//       |> deserialize )
//   in
//   (* roundtrips *)
//   roundtrips
//     "to_internal_queryable_v1"
//     Dval.to_internal_queryable_v1
//     Dval.of_internal_queryable_v1 ;
//   ()

let tests =
  testList
    "dvalRepr"
    [ testDvalRoundtrippableRoundtrips
      testInternalRoundtrippableDoesntCareAboutOrder
      testDvalOptionQueryableSpecialCase
      testDvalUserDBV1Migration ]
