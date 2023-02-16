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
module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis
module CTApi = ClientTypes.Api
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes
module CT2Ops = ClientTypes2BackendTypes.Ops
module CT2Worker = ClientTypes2BackendTypes.Worker

module V = SerializationTestValues

[<RequireQualifiedAccess>]
module ClientTestValues =
  let dval : CTRuntime.Dval.T = CT2Runtime.Dval.toCT V.RuntimeTypes.dval

  let staticDeploy : ClientTypes.Pusher.Payload.NewStaticDeploy =
    { deployHash = "zf2ttsgwln"
      url = "https://paul.darksa.com/nwtf5qhdku2untsc17quotrhffa/zf2ttsgwln"
      status = ClientTypes.StaticDeploy.Deployed
      lastUpdate = V.instant }

  let addOpResultV1 : ClientTypes.Ops.AddOpResultV1 =
    { handlers = V.ProgramTypes.Handler.handlers |> List.map CT2Program.Handler.toCT
      deletedHandlers =
        V.ProgramTypes.Handler.handlers |> List.map CT2Program.Handler.toCT
      dbs = [ V.ProgramTypes.userDB |> CT2Program.DB.toCT ]
      deletedDBs = [ V.ProgramTypes.userDB |> CT2Program.DB.toCT ]
      userFunctions =
        V.ProgramTypes.userFunctions |> List.map CT2Program.UserFunction.toCT
      deletedUserFunctions =
        V.ProgramTypes.userFunctions |> List.map CT2Program.UserFunction.toCT
      userTypes = V.ProgramTypes.userTypes |> List.map CT2Program.UserType.toCT
      deletedUserTypes =
        V.ProgramTypes.userTypes |> List.map CT2Program.UserType.toCT }

  let workerStates : ClientTypes.Worker.WorkerStates =
    Map.ofList [ "run", CT2Worker.WorkerState.toCT WorkerStates.Running
                 "blocked", CT2Worker.WorkerState.toCT WorkerStates.Blocked
                 "paused", CT2Worker.WorkerState.toCT WorkerStates.Paused ]

  let addOpEventV1 : ClientTypes.Pusher.Payload.AddOpV1 =
    { ``params`` =
        { ops = V.ProgramTypes.oplist |> List.map CT2Program.Op.toCT
          opCtr = 0
          clientOpCtrID = V.uuid.ToString() }
      result = addOpResultV1 }

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
        v<Prelude.pos> "simple" { x = 10; y = -16 }

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
        v<CTRuntime.Dval.T> "complete" CV.dval

        v<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
          "complete"
          (V.RuntimeTypes.dval
           |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT)

        v<LibExecution.ProgramTypes.Oplist> "complete" V.ProgramTypes.oplist
        v<ClientTypes.Program.Handler.T>
          "simple"
          (CT2Program.Handler.toCT V.ProgramTypes.Handler.http)

        // ------------------
        // LibBackend
        // ------------------
        v<LibBackend.EventQueueV2.NotificationData>
          "simple"
          { id = V.uuid; canvasID = V.uuid }

        v<LibBackend.Session.JsonData>
          "simple"
          { username = "paul"; csrf_token = "abcd1234abdc1234abcd1234abc1234" }

        v<LibBackend.PackageManager.ParametersDBFormat>
          "all"
          [ { name = "int"; tipe = LibBackend.PackageManager.TInt; description = "" }
            { name = "string"
              tipe = LibBackend.PackageManager.TStr
              description = "" }
            { name = "any"; tipe = LibBackend.PackageManager.TAny; description = "" }
            { name = "List"
              tipe = LibBackend.PackageManager.TList
              description = "" }
            { name = "obj"; tipe = LibBackend.PackageManager.TObj; description = "" } ]

        v<PT.Position> "simple" { x = 10; y = -16 }

        v<LibBackend.TraceCloudStorage.CloudStorageFormat>
          "simple"
          { storageFormatVersion = 0
            input =
              [ "request",
                V.RuntimeTypes.dval
                |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT ]
            functionArguments =
              [ V.tlid,
                [ "testParam",
                  V.RuntimeTypes.dval
                  |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT ] ]
            functionResults =
              [ (V.tlid,
                 7UL,
                 "testFn",
                 LibExecution.DvalReprInternalHash.currentHashVersion,
                 V.RuntimeTypes.dvals
                 |> LibExecution.DvalReprInternalHash.hash
                      LibExecution.DvalReprInternalHash.currentHashVersion,
                 V.RuntimeTypes.dval
                 |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT) ] }



        // ------------------
        // Used by Pusher
        // ------------------
        v<ClientTypes.Pusher.Payload.NewStaticDeploy> "simple" CV.staticDeploy
        v<ClientTypes.Pusher.Payload.NewTrace> "simple" (V.uuid, V.tlids)
        v<ClientTypes.Pusher.Payload.New404>
          "simple"
          ("HTTP", "/", "GET", V.instant, V.uuid)
        v<ClientTypes.Pusher.Payload.UpdateWorkerStates>
          "pusher-update-worker-states"
          CV.workerStates
        v<ClientTypes.Pusher.Payload.AddOpV1> "simple" CV.addOpEventV1
        // v<ClientTypes.Pusher.Payload.AddOpV1PayloadTooBig> // this is so-far unused
        //   "simple"
        //   { tlids = testTLIDs }



        // ------------------
        // ClientTypes
        // ------------------

        // AddOps
        // v<CTApi.Ops.AddOpV1.Request>
        //   "simple"
        //   (CT2Ops.AddOpParamsV1.toCT
        //     { ops = V.ProgramTypes.oplist
        //       opCtr = 0
        //       clientOpCtrID = V.uuid.ToString() })

        // v<CTApi.Ops.AddOpV1.Response> "simple" CV.addOpResultV1

        // // User DBs
        // v<CTApi.DB.StatsV1.Request> "simple" { tlids = V.tlids }
        // v<CTApi.DB.StatsV1.Response.T>
        //   "simple"
        //   (Map.ofList [ "db1", { count = 0; example = None }
        //                 "db2", { count = 5; example = Some(CV.dval, "myKey") } ])
        // v<CTApi.DB.Unlocked.Response> "simple" { unlocked_dbs = [ V.tlid ] }

        // // Execution
        // v<CTApi.Execution.FunctionV1.Request>
        //   "simple"
        //   { tlid = V.tlid
        //     trace_id = V.uuid
        //     caller_id = 7UL
        //     args = [ CV.dval ]
        //     fnname = "Int::mod_v0" }
        // v<CTApi.Execution.FunctionV1.Response>
        //   "simple"
        //   { result = CV.dval
        //     hash = "abcd"
        //     hashVersion = 0
        //     touched_tlids = [ V.tlid ]
        //     unlocked_dbs = [ V.tlid ] }
        // v<CTApi.Execution.HandlerV1.Request>
        //   "simple"
        //   { tlid = V.tlid; trace_id = V.uuid; input = [ "v", CV.dval ] }
        // v<CTApi.Execution.HandlerV1.Response> "simple" { touched_tlids = [ V.tlid ] }

        // // 404s
        // v<CTApi.F404.List.Response>
        //   "simple"
        //   { f404s = [ ("HTTP", "/", "GET", V.instant, V.uuid) ] }
        // v<CTApi.F404.Delete.Request>
        //   "simple"
        //   { space = "HTTP"; path = "/"; modifier = "POST" }
        // v<CTApi.F404.Delete.Response> "simple" { result = "success" }

        // // Functions
        // v<List<ClientTypes.UI.Functions.BuiltInFn>>
        //   "all"
        //   ([ { name = { module_ = "Int"; function_ = "mod"; version = 0 }
        //        parameters =
        //          [ { name = "a"
        //              ``type`` = CTRuntime.DType.TInt
        //              args = []
        //              description = "param description" } ]
        //        returnType = CTRuntime.DType.TList(CTRuntime.DType.TInt)
        //        description = "basic"
        //        isInfix = false
        //        previewable = ClientTypes.UI.Functions.Pure
        //        deprecated = ClientTypes.UI.Functions.NotDeprecated
        //        sqlSpec = ClientTypes.UI.Functions.NotQueryable }
        //      { name = { module_ = "Int"; function_ = "mod"; version = 0 }
        //        parameters = []
        //        returnType = CTRuntime.DType.TInt
        //        description = "impure"
        //        isInfix = false
        //        previewable = ClientTypes.UI.Functions.Impure
        //        deprecated = ClientTypes.UI.Functions.NotDeprecated
        //        sqlSpec = ClientTypes.UI.Functions.NotQueryable }
        //      { name = { module_ = "Int"; function_ = "mod"; version = 0 }
        //        parameters = []
        //        returnType = CTRuntime.DType.TInt
        //        description = "impurepreviewable"
        //        isInfix = false
        //        previewable = ClientTypes.UI.Functions.ImpurePreviewable
        //        deprecated = ClientTypes.UI.Functions.NotDeprecated
        //        sqlSpec = ClientTypes.UI.Functions.NotQueryable }
        //      { name = { module_ = "Int"; function_ = "mod"; version = 0 }
        //        parameters = []
        //        returnType = CTRuntime.DType.TInt
        //        description = "replacedBy"
        //        isInfix = false
        //        previewable = ClientTypes.UI.Functions.Pure
        //        deprecated =
        //          ClientTypes.UI.Functions.ReplacedBy(
        //            { module_ = "Int"; function_ = "mod"; version = 1 }
        //          )
        //        sqlSpec = ClientTypes.UI.Functions.NotQueryable }
        //      { name = { module_ = "Int"; function_ = "mod"; version = 0 }
        //        parameters = []
        //        returnType = CTRuntime.DType.TInt
        //        description = "renamedTo"
        //        isInfix = false
        //        previewable = ClientTypes.UI.Functions.Pure
        //        deprecated =
        //          ClientTypes.UI.Functions.RenamedTo(
        //            { module_ = "Int"; function_ = "mod"; version = 1 }
        //          )
        //        sqlSpec = ClientTypes.UI.Functions.NotQueryable }
        //      { name = { module_ = "Int"; function_ = "mod"; version = 0 }
        //        parameters = []
        //        returnType = CTRuntime.DType.TInt
        //        description = "deprecatedBecause"
        //        isInfix = false
        //        previewable = ClientTypes.UI.Functions.Pure
        //        deprecated = ClientTypes.UI.Functions.DeprecatedBecause "reason"
        //        sqlSpec = ClientTypes.UI.Functions.NotQueryable } ])

        // // InitialLoad
        // v<ClientTypes.Api.InitialLoad.V1.Response>
        //   "initial"
        //   { handlers =
        //       V.ProgramTypes.Handler.handlers |> List.map CT2Program.Handler.toCT
        //     deletedHandlers =
        //       V.ProgramTypes.Handler.handlers |> List.map CT2Program.Handler.toCT
        //     dbs = [ V.ProgramTypes.userDB |> CT2Program.DB.toCT ]
        //     deletedDBs = [ V.ProgramTypes.userDB |> CT2Program.DB.toCT ]
        //     userFunctions =
        //       V.ProgramTypes.userFunctions |> List.map CT2Program.UserFunction.toCT
        //     deletedUserFunctions =
        //       V.ProgramTypes.userFunctions |> List.map CT2Program.UserFunction.toCT
        //     unlockedDBs = [ V.tlid ]
        //     userTypes = V.ProgramTypes.userTypes |> List.map CT2Program.UserType.toCT
        //     deletedUserTypes =
        //       V.ProgramTypes.userTypes |> List.map CT2Program.UserType.toCT
        //     staticDeploys = [ CV.staticDeploy ]
        //     opCtrs = Map [ V.uuid, 7 ]
        //     canvasList = [ "test"; "test-canvas2" ]
        //     orgCanvasList = [ "testorg"; "testorg-canvas2" ]
        //     permission = Some(ClientTypes.Authorization.ReadWrite)
        //     orgs = [ "test"; "testorg" ]
        //     account =
        //       { username = "test"; name = "Test Name"; email = "test@darklang.com" }
        //     creationDate = V.instant
        //     workerSchedules = CV.workerStates
        //     secrets = [ { name = "test"; value = "secret" } ] }

        // // Tunnels
        // v<CTApi.Tunnels.Register.Request> "empty" { tunnelHost = None }
        // v<CTApi.Tunnels.Register.Request>
        //   "simple"
        //   { tunnelHost = Some "host.tunnel.com" }
        // v<CTApi.Tunnels.Register.Response> "simple" { success = false }

        // // Packages
        // v<CTApi.Packages.ListV1.Response>
        //   "simple"
        //   [ V.ProgramTypes.packageFn |> CT2Program.Package.Fn.toCT ]

        // // SecretsV1

        // v<CTApi.Secrets.DeleteV1.Request> "simple" { name = "test" }
        // v<CTApi.Secrets.DeleteV1.Response>
        //   "simple"
        //   { secrets = [ { name = "test"; value = "secret" } ] }

        // v<CTApi.Secrets.InsertV1.Request>
        //   "simple"
        //   { name = "test"; value = "secret" }
        // v<CTApi.Secrets.InsertV1.Response>
        //   "simple"
        //   { secrets = [ { name = "test"; value = "secret" } ] }

        // // Toplevels

        // v<CTApi.Toplevels.Delete.Request> "simple" { tlid = V.tlid }
        // v<CTApi.Toplevels.Delete.Response> "simple" { result = "success" }

        // // Traces

        // v<CTApi.Traces.GetAllTraces.Response>
        //   "simple"
        //   { traces = [ (V.tlid, V.uuid) ] }
        // v<CTApi.Traces.GetTraceDataV1.Request>
        //   "simple"
        //   { tlid = V.tlid; traceID = V.uuid }
        // v<CTApi.Traces.GetTraceDataV1.Response.T>
        //   "simple"
        //   { trace =
        //       (V.uuid,
        //        { input = [ "var", CV.dval ]
        //          timestamp = V.instant
        //          functionResults = [ ("fnName", 7UL, "hash", 0, CV.dval) ] }) }


        // // Workers

        // v<CTApi.Workers.Scheduler.Request>
        //   "simple"
        //   { name = "x"; schedule = "pause" }
        // v<CTApi.Workers.Scheduler.Response>
        //   "api-worker-scheduler-response"
        //   CV.workerStates

        // v<CTApi.Workers.WorkerStats.Request> "simple" { tlid = V.tlid }
        // v<CTApi.Workers.WorkerStats.Response> "simple" { count = 5 }

        // ------------------
        // LibAnalysis
        // ------------------
        v<ClientTypes.Analysis.AnalysisResult>
          "simple"
          (Ok(
            V.uuid,
            Dictionary.fromList (
              [ (7UL, CTAnalysis.ExecutionResult.ExecutedResult CV.dval)
                (7UL, CTAnalysis.ExecutionResult.NonExecutedResult CV.dval) ]
            ),
            1,
            NodaTime.Instant.UnixEpoch
          ))
        v<ClientTypes.Analysis.PerformAnalysisParams>
          "handler"
          (ClientTypes.Analysis.AnalyzeHandler
            { requestID = 2
              requestTime = NodaTime.Instant.UnixEpoch
              handler = CT2Program.Handler.toCT V.ProgramTypes.Handler.http
              traceID = V.uuid
              traceData =
                { input = [ "var", CV.dval ]
                  timestamp = V.instant
                  functionResults = [ ("fnName", 7UL, "hash", 0, CV.dval) ] }
              dbs =
                [ { tlid = V.tlid
                    name = "dbname"
                    nameID = 7UL
                    cols =
                      [ { name = Some("colname")
                          nameID = 8UL
                          typ = Some(CT2Program.DType.toCT PT.TInt)
                          typeID = 9UL } ]
                    version = 1 } ]
              userFns =
                List.map CT2Program.UserFunction.toCT V.ProgramTypes.userFunctions
              userTypes = List.map CT2Program.UserType.toCT V.ProgramTypes.userTypes
              packageFns = [ V.ProgramTypes.packageFn |> CT2Program.Package.Fn.toCT ]
              secrets = [ { name = "z"; value = "y" } ] })
        v<ClientTypes.Analysis.PerformAnalysisParams>
          "function"
          (ClientTypes.Analysis.AnalyzeFunction
            { requestID = 3
              requestTime = NodaTime.Instant.UnixEpoch
              func = CT2Program.UserFunction.toCT V.ProgramTypes.userFunction
              traceID = V.uuid
              traceData =
                { input = [ "var", CV.dval ]
                  timestamp = V.instant
                  functionResults = [ ("fnName", 7UL, "hash", 0, CV.dval) ] }
              dbs =
                [ { tlid = V.tlid
                    name = "dbname"
                    nameID = 7UL
                    cols =
                      [ { name = Some("colname")
                          nameID = 8UL
                          typ = Some(CT2Program.DType.toCT PT.TInt)
                          typeID = 9UL } ]
                    version = 1 } ]
              userFns =
                List.map CT2Program.UserFunction.toCT V.ProgramTypes.userFunctions
              userTypes = List.map CT2Program.UserType.toCT V.ProgramTypes.userTypes
              packageFns = [ V.ProgramTypes.packageFn |> CT2Program.Package.Fn.toCT ]
              secrets = [ { name = "z"; value = "y" } ] })


        // ------------------
        // Tests
        // ------------------

        v<LibExecution.AnalysisTypes.TraceData>
          "testTraceData"
          { input = [ "var", V.RuntimeTypes.dval ]
            timestamp = V.instant
            function_results = [ ("fnName", 7UL, "hash", 0, V.RuntimeTypes.dval) ] } ]


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
          CT2Runtime.FQFnName.toCT
          CT2Runtime.FQFnName.fromCT
          None
        testRoundtripList
          "RT.DType"
          V.RuntimeTypes.dtypes
          CT2Runtime.DType.toCT
          CT2Runtime.DType.fromCT
          None
        testRoundtripList
          "RT.MatchPattern"
          V.RuntimeTypes.matchPatterns
          CT2Runtime.MatchPattern.toCT
          CT2Runtime.MatchPattern.fromCT
          None
        testRoundtripList
          "RT.SendToRail"
          V.RuntimeTypes.sendToRails
          CT2Runtime.Expr.sterFromRT
          CT2Runtime.Expr.sterToRT
          None
        testRoundtripList
          "RT.IsInPipe"
          V.RuntimeTypes.isInPipes
          CT2Runtime.Expr.pipeFromRT
          CT2Runtime.Expr.pipeToRT
          None
        testRoundtripList
          "RT.Expr"
          V.RuntimeTypes.exprs
          CT2Runtime.Expr.toCT
          CT2Runtime.Expr.fromCT
          None
        testRoundtripList
          "RT.DvalSource"
          V.RuntimeTypes.dvalSources
          CT2Runtime.Dval.DvalSource.toCT
          CT2Runtime.Dval.DvalSource.fromCT
          None
        testRoundtripList
          "RT.DHTTP"
          V.RuntimeTypes.dvalHttpResponses
          CT2Runtime.Dval.httpResponseToCT
          CT2Runtime.Dval.httpResponseFromCT
          None
        testRoundtripList
          "RT.Dval"
          V.RuntimeTypes.dvals
          CT2Runtime.Dval.toCT
          CT2Runtime.Dval.fromCT
          (Some (fun l r ->
            Expect.equalDval l r "dval does not roundtrip successfully")) ]

  module ProgramTypes =
    let tests =
      [ testRoundtrip
          "PT.Position"
          V.ProgramTypes.pos
          CT2Program.Position.toCT
          CT2Program.Position.fromCT
        testRoundtripList
          "PT.FQFnName"
          V.ProgramTypes.fqFnNames
          CT2Program.FQFnName.toCT
          CT2Program.FQFnName.fromCT
          None
        testRoundtripList
          "PT.Pattern"
          V.ProgramTypes.matchPatterns
          CT2Program.MatchPattern.toCT
          CT2Program.MatchPattern.fromCT
          None
        testRoundtripList
          "PT.SendToRail"
          V.ProgramTypes.sendToRails
          CT2Program.SendToRail.toCT
          CT2Program.SendToRail.fromCT
          None
        testRoundtrip
          "PT.Expr"
          V.ProgramTypes.expr
          CT2Program.Expr.toCT
          CT2Program.Expr.fromCT
        testRoundtrip
          "PT.Dtype"
          V.ProgramTypes.dtype
          CT2Program.DType.toCT
          CT2Program.DType.fromCT
        testRoundtripList
          "PT.CronInterval"
          V.ProgramTypes.Handler.cronIntervals
          CT2Program.Handler.CronInterval.toCT
          CT2Program.Handler.CronInterval.fromCT
          None
        testRoundtripList
          "PT.HandlerSpec"
          V.ProgramTypes.Handler.specs
          CT2Program.Handler.Spec.toCT
          CT2Program.Handler.Spec.fromCT
          None
        testRoundtripList
          "PT.Handler"
          V.ProgramTypes.Handler.handlers
          CT2Program.Handler.toCT
          CT2Program.Handler.fromCT
          None
        testRoundtrip
          "PT.UserDB"
          V.ProgramTypes.userDB
          CT2Program.DB.toCT
          CT2Program.DB.fromCT
        testRoundtrip
          "PT.UserType"
          V.ProgramTypes.userType
          CT2Program.UserType.toCT
          CT2Program.UserType.fromCT
        testRoundtrip
          "PT.UserFunction"
          V.ProgramTypes.userFunction
          CT2Program.UserFunction.toCT
          CT2Program.UserFunction.fromCT
        testRoundtripList
          "PT.Toplevel"
          V.ProgramTypes.toplevels
          CT2Program.Toplevel.toCT
          CT2Program.Toplevel.fromCT
          None
        testRoundtripList
          "PT.Op"
          V.ProgramTypes.oplist
          CT2Program.Op.toCT
          CT2Program.Op.fromCT
          None
        testRoundtrip
          "PT.UserSecret"
          V.ProgramTypes.userSecret
          CT2Program.Secret.toCT
          CT2Program.Secret.fromCT
        testRoundtrip
          "PT.Package"
          V.ProgramTypes.packageFn
          CT2Program.Package.Fn.toCT
          CT2Program.Package.Fn.fromCT ]


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
