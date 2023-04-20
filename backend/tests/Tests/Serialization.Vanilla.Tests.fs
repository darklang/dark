/// Tests around our "Vanilla" JSON serializer used throughout the F# codebase
module Tests.VanillaSerialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open TestUtils.TestUtils

module File = LibBackend.File
module Config = LibBackend.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module WorkerStates = LibBackend.QueueSchedulingRules.WorkerStates
module CPT = LibAnalysis.ClientProgramTypes
module CRT = LibAnalysis.ClientRuntimeTypes
module CAT = LibAnalysis.ClientAnalysisTypes

module V = SerializationTestValues

[<RequireQualifiedAccess>]
module ClientTestValues =
  let dval : CRT.Dval.T = CRT.Dval.toCT V.RuntimeTypes.dval

  let dtypes : List<CRT.TypeReference> =
    List.map CRT.TypeReference.toCT V.RuntimeTypes.dtypes

module CV = ClientTestValues

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
        v<CRT.Dval.T> "complete" CV.dval

        v<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
          "complete"
          (V.RuntimeTypes.dval
           |> LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT)

        v<LibExecution.ProgramTypes.Oplist> "complete" V.ProgramTypes.oplist
        v<CPT.Handler.T> "simple" (CPT.Handler.toCT V.ProgramTypes.Handler.http)

        // ------------------
        // LibBackend
        // ------------------
        v<LibBackend.Queue.NotificationData>
          "simple"
          { id = V.uuid; canvasID = V.uuid }

        v<LibBackend.PackageManager.ParametersDBFormat>
          "all"
          [ { name = "int"; typ = PT.TInt; description = "" }
            { name = "string"; typ = PT.TString; description = "" }
            { name = "List"; typ = PT.TList PT.TInt; description = "" } ]

        v<LibBackend.TraceCloudStorage.CloudStorageFormat>
          "simple"
          { storageFormatVersion = 0
            input =
              [ "request",
                V.RuntimeTypes.dval
                |> LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT ]
            functionResults =
              [ (V.tlid,
                 7UL,
                 "testFn",
                 LibExecution.DvalReprInternalHash.currentHashVersion,
                 V.RuntimeTypes.dvals
                 |> LibExecution.DvalReprInternalHash.hash
                      LibExecution.DvalReprInternalHash.currentHashVersion,
                 V.RuntimeTypes.dval
                 |> LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT) ] }



        // ------------------
        // Used by Pusher
        // ------------------
        v<ClientTypes.Pusher.Payload.NewTrace> "simple" (V.uuid, V.tlids)
        v<ClientTypes.Pusher.Payload.New404>
          "simple"
          ("HTTP", "/", "GET", V.instant, V.uuid)
        // v<ClientTypes.Pusher.Payload.UpdateWorkerStates>
        //   "pusher-update-worker-states"
        //   CV.workerStates
        // v<ClientTypes.Pusher.Payload.AddOpV1> "simple" CV.addOpEventV1
        // v<ClientTypes.Pusher.Payload.AddOpV1PayloadTooBig> // this is so-far unused
        //   "simple"
        //   { tlids = testTLIDs }



        // ------------------
        // LibAnalysis
        // ------------------
        v<CAT.AnalysisResult>
          "simple"
          (Ok(
            V.uuid,
            Dictionary.fromList (
              [ (7UL, CAT.ExecutionResult.ExecutedResult CV.dval)
                (7UL, CAT.ExecutionResult.NonExecutedResult CV.dval) ]
            ),
            1,
            NodaTime.Instant.UnixEpoch
          ))
        v<CAT.PerformAnalysisParams>
          "handler"
          (CAT.AnalyzeHandler
            { requestID = 2
              requestTime = NodaTime.Instant.UnixEpoch
              handler = CPT.Handler.toCT V.ProgramTypes.Handler.http
              traceID = V.uuid
              traceData =
                { input = [ "var", CV.dval ]
                  functionResults = [ ("fnName", 7UL, "hash", 0, CV.dval) ] }
              dbs =
                [ { tlid = V.tlid; name = "dbname"; typ = CPT.TInt; version = 1 } ]
              userFns = List.map CPT.UserFunction.toCT V.ProgramTypes.userFunctions
              userTypes = List.map CPT.UserType.toCT V.ProgramTypes.userTypes
              packageFns = [ V.ProgramTypes.packageFn |> CPT.Package.Fn.toCT ]
              secrets = [ { name = "z"; value = "y"; version = 1 } ] })
        v<CAT.PerformAnalysisParams>
          "function"
          (CAT.AnalyzeFunction
            { requestID = 3
              requestTime = NodaTime.Instant.UnixEpoch
              func = CPT.UserFunction.toCT V.ProgramTypes.userFunction
              traceID = V.uuid
              traceData =
                { input = [ "var", CV.dval ]
                  functionResults = [ ("fnName", 7UL, "hash", 0, CV.dval) ] }
              dbs =
                [ { tlid = V.tlid; name = "dbname"; typ = CPT.TInt; version = 1 } ]
              userFns = List.map CPT.UserFunction.toCT V.ProgramTypes.userFunctions
              userTypes = List.map CPT.UserType.toCT V.ProgramTypes.userTypes
              packageFns = [ V.ProgramTypes.packageFn |> CPT.Package.Fn.toCT ]
              secrets = [ { name = "z"; value = "y"; version = 2 } ] })


        // ------------------
        // Tests
        // ------------------

        v<LibExecution.AnalysisTypes.TraceData>
          "testTraceData"
          { input = [ "var", V.RuntimeTypes.dval ]
            functionResults = [ ("fnName", 7UL, "hash", 0, V.RuntimeTypes.dval) ] } ]


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
      let actual = data.Force() |> List.map (fun z -> z.TypeName) |> Set

      let missingTypes = Set.difference actual expected
      let extraTypes = Set.difference expected actual

      Expect.equal missingTypes Set.empty "missing types"
      Expect.equal extraTypes Set.empty "extra types"
    }

  let testNoMissingOrExtraOutputTestFiles : Test =
    test "no extra or missing test files" {
      let expectedFilenames = data.Force() |> List.map (fun z -> z.FileName) |> Set
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
        |> List.groupBy (fun z -> z.FileName)
        |> Map.toList
        |> List.choose (fun (fileName, records) ->
          if List.length records > 1 then
            Some(fileName, records |> List.map (fun r -> r.SerializedData))
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


// Ensure that converting between 'internal' and 'client' types is stable
module RoundtripTests =
  // for each type/value:
  // perform domain val -> CT -> domain -> CT -> domain
  // most of the time, it should end up being the same as the source.
  // if there are known exceptions, break down individual mappings as separate tests

  let testRoundtrip
    (typeName : string)
    (original : 'a)
    (toCT : 'a -> 'b)
    (fromCT : 'b -> 'a)
    =
    test typeName {
      let actual : 'a = original |> toCT |> fromCT |> toCT |> fromCT

      Expect.equal actual original $"{typeName} does not roundtrip successfully"
    }

  let testRoundtripList
    (typeName : string)
    (original : List<'a>)
    (toCT : 'a -> 'b)
    (fromCT : 'b -> 'a)
    (customEquality : Option<'a -> 'a -> unit>)
    =
    test typeName {
      let actual : List<'a> =
        original
        |> List.map toCT
        |> List.map fromCT
        |> List.map toCT
        |> List.map fromCT

      match customEquality with
      | None ->
        Expect.equal actual original $"{typeName} does not roundtrip successfully"
      | Some customEquality ->
        List.zip original actual
        |> List.iter (fun (original, actual) -> customEquality original actual)
    }


  module RuntimeTypes =
    let tests =
      [ testRoundtripList
          "RT.FqFnName"
          V.RuntimeTypes.fqFnNames
          CRT.FQFnName.toCT
          CRT.FQFnName.fromCT
          None
        testRoundtripList
          "RT.TypeReference"
          V.RuntimeTypes.dtypes
          CRT.TypeReference.toCT
          CRT.TypeReference.fromCT
          None
        testRoundtripList
          "RT.MatchPattern"
          V.RuntimeTypes.matchPatterns
          CRT.MatchPattern.toCT
          CRT.MatchPattern.fromCT
          None
        testRoundtripList
          "RT.Expr"
          V.RuntimeTypes.exprs
          CRT.Expr.toCT
          CRT.Expr.fromCT
          None
        testRoundtripList
          "RT.DvalSource"
          V.RuntimeTypes.dvalSources
          CRT.Dval.DvalSource.toCT
          CRT.Dval.DvalSource.fromCT
          None
        testRoundtripList
          "RT.Dval"
          V.RuntimeTypes.dvals
          CRT.Dval.toCT
          CRT.Dval.fromCT
          (Some (fun l r ->
            Expect.equalDval l r "dval does not roundtrip successfully")) ]

  module ProgramTypes =
    let tests =
      [ testRoundtripList
          "PT.FQFnName"
          V.ProgramTypes.fqFnNames
          CPT.FQFnName.toCT
          CPT.FQFnName.fromCT
          None
        testRoundtripList
          "PT.MatchPattern"
          V.ProgramTypes.matchPatterns
          CPT.MatchPattern.toCT
          CPT.MatchPattern.fromCT
          None
        testRoundtripList
          "PT.LetPattern"
          V.ProgramTypes.letPatterns
          CPT.LetPattern.toCT
          CPT.LetPattern.fromCT
          None
        testRoundtrip "PT.Expr" V.ProgramTypes.expr CPT.Expr.toCT CPT.Expr.fromCT
        testRoundtrip
          "PT.Dtype"
          V.ProgramTypes.dtype
          CPT.TypeReference.toCT
          CPT.TypeReference.fromCT
        testRoundtripList
          "PT.CronInterval"
          V.ProgramTypes.Handler.cronIntervals
          CPT.Handler.CronInterval.toCT
          CPT.Handler.CronInterval.fromCT
          None
        testRoundtripList
          "PT.HandlerSpec"
          V.ProgramTypes.Handler.specs
          CPT.Handler.Spec.toCT
          CPT.Handler.Spec.fromCT
          None
        testRoundtripList
          "PT.Handler"
          V.ProgramTypes.Handler.handlers
          CPT.Handler.toCT
          CPT.Handler.fromCT
          None
        testRoundtrip "PT.UserDB" V.ProgramTypes.userDB CPT.DB.toCT CPT.DB.fromCT
        testRoundtrip
          "PT.UserType"
          V.ProgramTypes.userRecordType
          CPT.UserType.toCT
          CPT.UserType.fromCT
        testRoundtrip
          "PT.UserFunction"
          V.ProgramTypes.userFunction
          CPT.UserFunction.toCT
          CPT.UserFunction.fromCT
        testRoundtripList
          "PT.Toplevel"
          V.ProgramTypes.toplevels
          CPT.Toplevel.toCT
          CPT.Toplevel.fromCT
          None
        testRoundtrip
          "PT.UserSecret"
          V.ProgramTypes.userSecret
          CPT.Secret.toCT
          CPT.Secret.fromCT
        testRoundtrip
          "PT.Package"
          V.ProgramTypes.packageFn
          CPT.Package.Fn.toCT
          CPT.Package.Fn.fromCT ]


let tests =
  testList
    "Vanilla Serialization"
    [ PersistedSerializations.tests

      testList
        "roundtrip RTs to and from client types"
        RoundtripTests.RuntimeTypes.tests
      testList
        "roundtrip PTs to and from client types"
        RoundtripTests.ProgramTypes.tests ]

// TODO: ensure (using reflection) we've covered all types within ClientTypes
// (many of which we'll have to explicity exclude, if they don't have exact
// equivalents in the 'domain' types)

// TODO: ensure (using reflection) that each of the DU cases _of_ those types
// are covered (so if we add another case to CTProgram.Expr, it's covered).
