/// Tests around our "Vanilla" JSON serializer used throughout the F# codebase
module Tests.VanillaSerialization

open Expecto
open System.Text.RegularExpressions

open Prelude

open TestUtils.TestUtils

module File = LibCloud.File
module Config = LibCloud.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module WorkerStates = LibCloud.QueueSchedulingRules.WorkerStates

module V = SerializationTestValues


// Throughout our F# codebase, we've annotated which types we `allow` to be
// serialized with our "Vanilla" serializer. Now we want to verify that the format
// doesn't change. We do this by maintaining at least one sample value of each type,
// and verifying the serialized sample value doesn't change. This gives us extremely
// high confidence that we're not breaking anything when we change the serializer.
// - If there's a change to the format, a new file will be generated.
//   What to do here will depend on the change, how much data is saved with the old
//   change, etc. You may want to save both formats and check that both are
//   deserializable.
// - If there's a change to the sample value, generate a new file.
//   Commit it after verifying that the format is appropriate.
module PersistedSerializations =
  type T =
    { TypeName : string
      CaseName : string
      SerializedData : string }

    member this.FileName =
      let original = $"vanilla_{this.TypeName}_{this.CaseName}"
      let replaced = Regex.Replace(original, "[^-_a-zA-Z0-9]", "-")
      Regex.Replace(replaced, "[-]+", "-") + ".json"

  // shortcut to make `data` more readable
  let v<'t> (caseName : string) (value : 't) =
    { TypeName = string typeof<'t>
      CaseName = caseName

      // we use prettySerialize even though we use serialize in practice, as the
      // non-pretty version is too hard to read and compare.
      SerializedData = Json.Vanilla.prettySerialize value }

  // the vales that we expect to match the ones persisted to disk
  let data =
    lazy
      [ v<Map<string, string>> "baseline" (Map.ofList [ "a", "b" ])

        // ------------------
        // Prelude
        // ------------------

        // ------------------
        // LibService
        // ------------------
        v<LibService.Rollbar.HoneycombJson>
          "simple"
          { filters =
              [ { column = "trace.trace_id"; op = "="; value = string V.uuid } ]
            limit = 100
            time_range = 604800 }

        // ------------------
        // LibExecution
        // ------------------
        v<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
          "complete"
          (V.RuntimeTypes.dval
           |> LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT)

        v<LibExecution.ProgramTypes.PackageType.T> "type" V.ProgramTypes.packageType

        v<LibExecution.ProgramTypes.PackageConstant.T>
          "constant"
          V.ProgramTypes.packageConstant

        v<LibExecution.ProgramTypes.PackageFn.T> "function" V.ProgramTypes.packageFn

        v<List<LibExecution.ProgramTypes.Toplevel.T>>
          "complete"
          V.ProgramTypes.toplevels

        v<LibExecution.ProgramTypes.Toplevel.T>
          "httphandler"
          (PT.Toplevel.TLHandler V.ProgramTypes.Handler.http)

        v<LibExecution.ProgramTypes.Toplevel.T>
          "db"
          (PT.Toplevel.TLDB V.ProgramTypes.userDB)

        // ------------------
        // LibCloud
        // ------------------
        v<LibCloud.Queue.NotificationData>
          "simple"
          { id = V.uuid; canvasID = V.uuid }

        v<LibCloud.TraceCloudStorage.CloudStorageFormat>
          "simple"
          { storageFormatVersion = 0
            input =
              [ "request",
                V.RuntimeTypes.dval
                |> LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT ]
            functionResults =
              [ (V.tlid,
                 7777772986753UL,
                 "testFn",
                 LibExecution.DvalReprInternalHash.currentHashVersion,
                 V.RuntimeTypes.dvals
                 |> NEList.ofListUnsafe "dvals" []
                 |> LibExecution.DvalReprInternalHash.hash
                   LibExecution.DvalReprInternalHash.currentHashVersion,
                 V.RuntimeTypes.dval
                 |> LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT) ] }



        // ------------------
        // Used by Pusher
        // ------------------
        v<LibClientTypes.Pusher.Payload.NewTrace> "simple" (V.uuid, V.tlids)
        v<LibClientTypes.Pusher.Payload.New404>
          "simple"
          ("HTTP", "/", "GET", V.instant, V.uuid)
        // v<LibClientTypes.Pusher.Payload.UpdateWorkerStates>
        //   "pusher-update-worker-states"
        //   CV.workerStates
        // v<LibClientTypes.Pusher.Payload.AddOpV1> "simple" CV.addOpEventV1
        // v<LibClientTypes.Pusher.Payload.AddOpV1PayloadTooBig> // this is so-far unused
        //   "simple"
        //   { tlids = testTLIDs }


        // ------------------
        // Tests
        // ------------------

        v<LibExecution.AnalysisTypes.TraceData>
          "testTraceData"
          { input = [ "var", V.RuntimeTypes.dval ]
            functionResults =
              [ ("fnName", 777777837205UL, "hash", 0, V.RuntimeTypes.dval) ] } ]


  let generateTestFiles () =
    // Enabled in dev so we can see changes as git diffs.
    // Disabled in CI so changes will fail the tests
    if Config.serializationGenerateTestData then
      data.Force()
      |> List.iter (fun record ->
        File.writefile Config.Serialization record.FileName record.SerializedData)

  let testAllAllowedTypesHaveARecord : Test =
    test "no extra or missing types tested by files" {
      let expected = Json.Vanilla.allowedTypes |> Dictionary.keys |> Set
      let actual = data.Force() |> List.map _.TypeName |> Set

      let missingTypes = Set.difference actual expected
      let extraTypes = Set.difference expected actual

      Expect.equal missingTypes Set.empty "missing types"
      Expect.equal extraTypes Set.empty "extra types"
    }

  let testNoMissingOrExtraOutputTestFiles : Test =
    test "no extra or missing test files" {
      let expectedFilenames = data.Force() |> List.map _.FileName |> Set
      let actualFilenames =
        File.lsPattern Config.Serialization "vanilla_*.json" |> Set

      let missingFiles = Set.difference expectedFilenames actualFilenames
      let extraFiles = Set.difference actualFilenames expectedFilenames

      Expect.equal missingFiles Set.empty "missing files"
      Expect.equal extraFiles Set.empty "extra files"
    }

  let testNoTwoRecordsTryToWriteToTheSameFile : Test =
    test "no two records would result in the same file name" {
      let duplicates =
        data.Force()
        |> List.groupBy _.FileName
        |> Map.toList
        |> List.filterMap (fun (fileName, records) ->
          if List.length records > 1 then
            Some(fileName, records |> List.map _.SerializedData)
          else
            None)

      Expect.equal duplicates [] ""
    }

  // Note: if you're confused by one of these failing, there's a chance that 2 cases
  // in the list are trying to write to the same file, potentially due to type
  // abbreviations confusing things, since they're compiled to the same eventual
  // type. If you encounter this, focus (--filter-test-case) on the adjacent test
  // which ensures no 2 records would result in the same file name, to see if that's
  // the issue.
  let testTestFilesForConsistentSerialization : Test =
    let tests =
      data.Force()
      |> List.map (fun expectedPersistedSerialization ->
        let item = expectedPersistedSerialization

        test
          $"check vanilla test files are correct for {item.TypeName}, {item.CaseName}" {
          let expected = File.readfile Config.Serialization item.FileName

          Expect.equal
            item.SerializedData
            expected
            $"Serialization for {item.CaseName} case of {item.TypeName} doesn't match file {item.FileName}"
        })

    testList "serializations match ones on persisted" tests

  let tests =
    testList
      "Persisted Serializations"
      [ testAllAllowedTypesHaveARecord
        testNoMissingOrExtraOutputTestFiles
        testNoTwoRecordsTryToWriteToTheSameFile
        testTestFilesForConsistentSerialization ]


let tests = testList "Vanilla Serialization" [ PersistedSerializations.tests ]
