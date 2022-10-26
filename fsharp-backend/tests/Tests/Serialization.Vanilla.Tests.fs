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
module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis
module CTApi = ClientTypes.Api
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes
module CT2Ops = ClientTypes2BackendTypes.Ops

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

module SampleData =
  // Stores a list of sample outputs for `$"{serializerName}-{typeName}"
  let data = Dictionary.T<string, List<string * string>>()

  let keyFor (typeName : string) (serializerName : string) : string =
    $"{serializerName}-{typeName}"

  let get
    (typeName : string)
    (serializerName : string)
    (reason : string)
    : List<string * string> =
    data
    |> Dictionary.get (keyFor typeName serializerName)
    |> Exception.unwrapOptionInternal
         "testCases should exist for allowed serializable type"
         [ "typeName", typeName :> obj
           "serializer", serializerName
           "reason", reason ]


  let private generateSerializerData<'t>
    (serializerName : string)
    (serializerFn : 't -> string)
    (name : string)
    (value : 't)
    =
    let typeName = string typeof<'t>
    let key = keyFor typeName serializerName
    let currentValue = Dictionary.get key data |> Option.unwrap []
    let newValue = (name, serializerFn value) :: currentValue
    data[key] <- newValue

  let private generateVanillaData<'t> (dataName : string) (data : 't) =
    // Use prettySerialize even though we use serialize in practice, as the
    // non-pretty version is too hard to read and compare.
    generateSerializerData "vanilla" Json.Vanilla.prettySerialize dataName data

  // Shortcuts to make generateTestData more readable
  let private v<'t> = generateVanillaData<'t>


  let generateTestData () : unit =

    v<Map<string, string>> "simple" (Map.ofList [ "a", "b" ])

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
          [ { column = "trace.trace_id"; op = "="; value = string V.testUuid } ]
        limit = 100
        time_range = 604800 }

    // ------------------
    // LibExecution
    // ------------------

    v<CTRuntime.Dval.T> "complete" V.testClientDval

    v<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
      "complete"
      (V.testDval
       |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT)

    v<LibExecution.ProgramTypes.Oplist> "complete" V.ProgramTypes.testOplist
    v<LibExecution.ProgramTypes.Handler.T> "simple" V.ProgramTypes.testHttpHandler

    // ------------------
    // LibBackend
    // ------------------

    v<LibBackend.EventQueueV2.NotificationData>
      "simple"
      { id = V.testUuid; canvasID = V.testUuid }

    v<LibBackend.Session.JsonData>
      "simple"
      { username = "paul"; csrf_token = "abcd1234abdc1234abcd1234abc1234" }

    v<LibBackend.PackageManager.ParametersDBFormat>
      "all"
      [ { name = "int"; tipe = LibBackend.PackageManager.TInt; description = "" }
        { name = "string"; tipe = LibBackend.PackageManager.TStr; description = "" }
        { name = "any"; tipe = LibBackend.PackageManager.TAny; description = "" }
        { name = "List"; tipe = LibBackend.PackageManager.TList; description = "" }
        { name = "obj"; tipe = LibBackend.PackageManager.TObj; description = "" } ]

    v<PT.Position> "simple" { x = 10; y = -16 }

    // ------------------
    // Used by Pusher
    // ------------------
    v<ClientTypes.Pusher.Payload.NewStaticDeploy> "simple" V.testStaticDeploy

    v<ClientTypes.Pusher.Payload.NewTrace> "simple" (V.testUuid, V.testTLIDs)

    v<ClientTypes.Pusher.Payload.New404>
      "simple"
      ("HTTP", "/", "GET", V.testInstant, V.testUuid)

    v<ClientTypes.Pusher.Payload.UpdateWorkerStates> "simple" V.testWorkerStates

    v<ClientTypes.Pusher.Payload.AddOpV1> "simple" V.testAddOpEventV1
    // v<ClientTypes.Pusher.Payload.AddOpV1PayloadTooBig> // this is so-far unused
    //   "simple"
    //   { tlids = testTLIDs }



    // ------------------
    // ApiServer
    // ------------------

    // AddOps
    v<CTApi.Ops.AddOpV1.Request>
      "simple"
      (CT2Ops.AddOpParamsV1.toCT
        { ops = V.ProgramTypes.testOplist
          opCtr = 0
          clientOpCtrID = V.testUuid.ToString() })

    v<CTApi.Ops.AddOpV1.Response> "simple" V.testAddOpResultV1


    // User DBs
    v<CTApi.DB.StatsV1.Request> "simple" { tlids = V.testTLIDs }
    v<CTApi.DB.StatsV1.Response.T>
      "simple"
      (Map.ofList [ "db1", { count = 0; example = None }
                    "db2", { count = 5; example = Some(V.testClientDval, "myKey") } ])
    v<CTApi.DB.Unlocked.Response> "simple" { unlocked_dbs = [ V.testTLID ] }

    // Execution
    v<CTApi.Execution.FunctionV1.Request>
      "simple"
      { tlid = V.testTLID
        trace_id = V.testUuid
        caller_id = 7UL
        args = [ V.testClientDval ]
        fnname = "Int::mod_v0" }
    v<CTApi.Execution.FunctionV1.Response>
      "simple"
      { result = V.testClientDval
        hash = "abcd"
        hashVersion = 0
        touched_tlids = [ V.testTLID ]
        unlocked_dbs = [ V.testTLID ] }
    v<CTApi.Execution.HandlerV1.Request>
      "simple"
      { tlid = V.testTLID; trace_id = V.testUuid; input = [ "v", V.testClientDval ] }
    v<CTApi.Execution.HandlerV1.Response> "simple" { touched_tlids = [ V.testTLID ] }

    // 404s
    v<CTApi.F404.List.Response>
      "simple"
      { f404s = [ ("HTTP", "/", "GET", V.testInstant, V.testUuid) ] }
    v<CTApi.F404.Delete.Request>
      "simple"
      { space = "HTTP"; path = "/"; modifier = "POST" }
    v<CTApi.F404.Delete.Response> "simple" { result = "success" }

    // Functions
    v<List<ClientTypes.UI.Functions.BuiltInFn>>
      "all"
      ([ { name = { module_ = "Int"; function_ = "mod"; version = 0 }
           parameters =
             [ { name = "a"
                 ``type`` = CTRuntime.DType.TInt
                 args = []
                 description = "param description" } ]
           returnType = CTRuntime.DType.TList(CTRuntime.DType.TInt)
           description = "basic"
           isInfix = false
           previewable = ClientTypes.UI.Functions.Pure
           deprecated = ClientTypes.UI.Functions.NotDeprecated
           sqlSpec = ClientTypes.UI.Functions.NotQueryable }
         { name = { module_ = "Int"; function_ = "mod"; version = 0 }
           parameters = []
           returnType = CTRuntime.DType.TInt
           description = "impure"
           isInfix = false
           previewable = ClientTypes.UI.Functions.Impure
           deprecated = ClientTypes.UI.Functions.NotDeprecated
           sqlSpec = ClientTypes.UI.Functions.NotQueryable }
         { name = { module_ = "Int"; function_ = "mod"; version = 0 }
           parameters = []
           returnType = CTRuntime.DType.TInt
           description = "impurepreviewable"
           isInfix = false
           previewable = ClientTypes.UI.Functions.ImpurePreviewable
           deprecated = ClientTypes.UI.Functions.NotDeprecated
           sqlSpec = ClientTypes.UI.Functions.NotQueryable }
         { name = { module_ = "Int"; function_ = "mod"; version = 0 }
           parameters = []
           returnType = CTRuntime.DType.TInt
           description = "replacedBy"
           isInfix = false
           previewable = ClientTypes.UI.Functions.Pure
           deprecated =
             ClientTypes.UI.Functions.ReplacedBy(
               { module_ = "Int"; function_ = "mod"; version = 1 }
             )
           sqlSpec = ClientTypes.UI.Functions.NotQueryable }
         { name = { module_ = "Int"; function_ = "mod"; version = 0 }
           parameters = []
           returnType = CTRuntime.DType.TInt
           description = "renamedTo"
           isInfix = false
           previewable = ClientTypes.UI.Functions.Pure
           deprecated =
             ClientTypes.UI.Functions.RenamedTo(
               { module_ = "Int"; function_ = "mod"; version = 1 }
             )
           sqlSpec = ClientTypes.UI.Functions.NotQueryable }
         { name = { module_ = "Int"; function_ = "mod"; version = 0 }
           parameters = []
           returnType = CTRuntime.DType.TInt
           description = "deprecatedBecause"
           isInfix = false
           previewable = ClientTypes.UI.Functions.Pure
           deprecated = ClientTypes.UI.Functions.DeprecatedBecause "reason"
           sqlSpec = ClientTypes.UI.Functions.NotQueryable } ])

    // InitialLoad
    v<ClientTypes.Api.InitialLoad.V1.Response>
      "initial"
      { handlers = V.ProgramTypes.testHandlers |> List.map CT2Program.Handler.toCT
        deletedHandlers =
          V.ProgramTypes.testHandlers |> List.map CT2Program.Handler.toCT
        dbs = V.ProgramTypes.testDBs |> List.map CT2Program.DB.toCT
        deletedDBs = V.ProgramTypes.testDBs |> List.map CT2Program.DB.toCT
        userFunctions =
          V.ProgramTypes.testUserFunctions |> List.map CT2Program.UserFunction.toCT
        deletedUserFunctions =
          V.ProgramTypes.testUserFunctions |> List.map CT2Program.UserFunction.toCT
        unlockedDBs = [ V.testTLID ]
        userTypes = V.ProgramTypes.testUserTypes |> List.map CT2Program.UserType.toCT
        deletedUserTypes =
          V.ProgramTypes.testUserTypes |> List.map CT2Program.UserType.toCT
        staticDeploys = [ V.testStaticDeploy ]
        opCtrs = Map [ V.testUuid, 7 ]
        canvasList = [ "test"; "test-canvas2" ]
        orgCanvasList = [ "testorg"; "testorg-canvas2" ]
        permission = Some(ClientTypes.Authorization.ReadWrite)
        orgs = [ "test"; "testorg" ]
        account =
          { username = "test"; name = "Test Name"; email = "test@darklang.com" }
        creationDate = V.testInstant
        workerSchedules = V.testWorkerStates
        secrets = [ { name = "test"; value = "secret" } ] }

    // Tunnels
    v<CTApi.Tunnels.Register.Request> "empty" { tunnelHost = None }
    v<CTApi.Tunnels.Register.Request>
      "simple"
      { tunnelHost = Some "host.tunnel.com" }
    v<CTApi.Tunnels.Register.Response> "simple" { success = false }

    // Packages
    v<CTApi.Packages.ListV1.Response> "simple" [ V.ProgramTypes.testPackageFn ]

    // SecretsV1

    v<CTApi.Secrets.DeleteV1.Request> "simple" { name = "test" }
    v<CTApi.Secrets.DeleteV1.Response>
      "simple"
      { secrets = [ { name = "test"; value = "secret" } ] }

    v<CTApi.Secrets.InsertV1.Request> "simple" { name = "test"; value = "secret" }
    v<CTApi.Secrets.InsertV1.Response>
      "simple"
      { secrets = [ { name = "test"; value = "secret" } ] }

    // Toplevels

    v<CTApi.Toplevels.Delete.Request> "simple" { tlid = V.testTLID }
    v<CTApi.Toplevels.Delete.Response> "simple" { result = "success" }

    // Traces

    v<CTApi.Traces.GetAllTraces.Response>
      "simple"
      { traces = [ (V.testTLID, V.testUuid) ] }
    v<CTApi.Traces.GetTraceDataV1.Request>
      "simple"
      { tlid = V.testTLID; traceID = V.testUuid }
    v<CTApi.Traces.GetTraceDataV1.Response.T>
      "simple"
      { trace =
          (V.testUuid,
           { input = [ "var", V.testClientDval ]
             timestamp = V.testInstant
             functionResults = [ ("fnName", 7UL, "hash", 0, V.testClientDval) ] }) }


    // Workers

    v<CTApi.Workers.Scheduler.Request> "simple" { name = "x"; schedule = "pause" }
    v<CTApi.Workers.Scheduler.Response> "all" V.testWorkerStates

    v<CTApi.Workers.WorkerStats.Request> "simple" { tlid = V.testTLID }
    v<CTApi.Workers.WorkerStats.Response> "simple" { count = 5 }

    // ------------------
    // LibAnalysis
    // ------------------
    v<ClientTypes.Analysis.AnalysisResult>
      "simple"
      (Ok(
        V.testUuid,
        Dictionary.fromList (
          [ (7UL, CTAnalysis.ExecutionResult.ExecutedResult V.testClientDval)
            (7UL, CTAnalysis.ExecutionResult.NonExecutedResult V.testClientDval) ]
        ),
        1,
        NodaTime.Instant.UnixEpoch
      ))
    v<ClientTypes.Analysis.PerformAnalysisParams>
      "handler"
      (ClientTypes.Analysis.AnalyzeHandler
        { requestID = 2
          requestTime = NodaTime.Instant.UnixEpoch
          handler = CT2Program.Handler.toCT V.ProgramTypes.testHttpHandler
          traceID = V.testUuid
          traceData =
            { input = [ "var", V.testClientDval ]
              timestamp = V.testInstant
              functionResults = [ ("fnName", 7UL, "hash", 0, V.testClientDval) ] }
          dbs =
            [ { tlid = V.testTLID
                name = "dbname"
                nameID = 7UL
                pos = CT2Program.Position.toCT V.ProgramTypes.testPos
                cols =
                  [ { name = Some("colname")
                      nameID = 8UL
                      typ = Some(CT2Program.DType.toCT PT.TInt)
                      typeID = 9UL } ]
                version = 1 } ]
          userFns =
            List.map CT2Program.UserFunction.toCT V.ProgramTypes.testUserFunctions
          userTypes = List.map CT2Program.UserType.toCT V.ProgramTypes.testUserTypes
          packageFns = [ V.ProgramTypes.testPackageFn ]
          secrets = [ { name = "z"; value = "y" } ] })
    v<ClientTypes.Analysis.PerformAnalysisParams>
      "function"
      (ClientTypes.Analysis.AnalyzeFunction
        { requestID = 3
          requestTime = NodaTime.Instant.UnixEpoch
          func = CT2Program.UserFunction.toCT V.ProgramTypes.testUserFunction
          traceID = V.testUuid
          traceData =
            { input = [ "var", V.testClientDval ]
              timestamp = V.testInstant
              functionResults = [ ("fnName", 7UL, "hash", 0, V.testClientDval) ] }
          dbs =
            [ { tlid = V.testTLID
                name = "dbname"
                nameID = 7UL
                pos = CT2Program.Position.toCT V.ProgramTypes.testPos
                cols =
                  [ { name = Some("colname")
                      nameID = 8UL
                      typ = Some(CT2Program.DType.toCT PT.TInt)
                      typeID = 9UL } ]
                version = 1 } ]
          userFns =
            List.map CT2Program.UserFunction.toCT V.ProgramTypes.testUserFunctions
          userTypes = List.map CT2Program.UserType.toCT V.ProgramTypes.testUserTypes
          packageFns = [ V.ProgramTypes.testPackageFn ]
          secrets = [ { name = "z"; value = "y" } ] })


    // ------------------
    // Tests
    // ------------------

    v<LibExecution.AnalysisTypes.TraceData>
      "testTraceData"
      { input = [ "var", V.testDval ]
        timestamp = V.testInstant
        function_results = [ ("fnName", 7UL, "hash", 0, V.testDval) ] }


  generateTestData ()

let fileNameFor
  (serializerName : string)
  (reason : string)
  (typeName : string)
  (dataName : string)
  : string =
  let original = $"{serializerName}_{reason}_{typeName}_{dataName}"
  let replaced = Regex.Replace(original, "[^-_a-zA-Z0-9]", "-")
  Regex.Replace(replaced, "[-]+", "-") + ".json"

let testNoMissingOrExtraOutputTestFiles : Test =
  test "test no extra test files" {
    let filenamesFor dict serializerName =
      dict
      |> Dictionary.toList
      |> List.map (fun (typeName, reason) ->
        SampleData.get typeName serializerName reason
        |> List.map (fun (name, _) -> fileNameFor serializerName reason typeName name))
      |> List.concat
      |> Set

    let vanillaFilenames = filenamesFor Json.Vanilla.allowedTypes "vanilla"
    let vanillaActual = File.lsPattern Config.Serialization "vanilla_*.json" |> Set

    let vanillaMissingFiles = Set.difference vanillaFilenames vanillaActual
    let vanillaExtraFiles = Set.difference vanillaActual vanillaFilenames

    Expect.equal vanillaMissingFiles Set.empty "missing vanilla files"
    Expect.equal vanillaExtraFiles Set.empty "extra vanilla files"
  }

let testTestFiles : List<Test> =
  let testsFor (dict : Dictionary.T<string, string>) (serializerName : string) =
    dict
    |> Dictionary.toList
    |> List.map (fun (typeName, reason) ->
      // TODO find a roundtrip test
      test $"check {serializerName} test files are correct for {typeName}" {
        // For each type, compare the sample data to the file data
        SampleData.get typeName serializerName reason
        |> List.iter (fun (name, actualSerializedData) ->
          let filename = fileNameFor serializerName reason typeName name
          let expected = File.readfile Config.Serialization filename
          Expect.equal actualSerializedData expected "matches")
      })
  (testsFor Json.Vanilla.allowedTypes "vanilla")

let generateTestFiles () =
  // Enabled in dev so we can see changes as git diffs.
  // Disabled in CI so changes will fail the tests
  if Config.serializationGenerateTestData then
    let generate (dict : Dictionary.T<string, string>) (serializerName : string) =
      dict
      |> Dictionary.toList
      |> List.iter (fun (typeName, reason) ->
        // For each type, compare the sample data to the file data
        SampleData.get typeName serializerName reason
        |> List.iter (fun (name, serializedData) ->
          let filename = fileNameFor serializerName reason typeName name
          File.writefile Config.Serialization filename serializedData))

    generate Json.Vanilla.allowedTypes "vanilla"

let tests =
  testList
    "Serialization"
    [ testNoMissingOrExtraOutputTestFiles
      testList "vanilla test formats" testTestFiles ]
