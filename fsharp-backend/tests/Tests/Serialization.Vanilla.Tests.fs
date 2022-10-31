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
    Map.ofList [ "run", ClientTypes.Worker.Running
                 "blocked", ClientTypes.Worker.Blocked
                 "paused", ClientTypes.Worker.Paused ]

  let addOpEventV1 : ClientTypes.Pusher.Payload.AddOpV1 =
    { ``params`` =
        { ops = V.ProgramTypes.oplist |> List.map CT2Program.Op.toCT
          opCtr = 0
          clientOpCtrID = V.uuid.ToString() }
      result = addOpResultV1 }

module CV = ClientTestValues


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
      { filters = [ { column = "trace.trace_id"; op = "="; value = string V.uuid } ]
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
    v<LibExecution.ProgramTypes.Handler.T> "simple" V.ProgramTypes.Handler.http

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
        { name = "string"; tipe = LibBackend.PackageManager.TStr; description = "" }
        { name = "any"; tipe = LibBackend.PackageManager.TAny; description = "" }
        { name = "List"; tipe = LibBackend.PackageManager.TList; description = "" }
        { name = "obj"; tipe = LibBackend.PackageManager.TObj; description = "" } ]

    v<PT.Position> "simple" { x = 10; y = -16 }

    // ------------------
    // Used by Pusher
    // ------------------
    v<ClientTypes.Pusher.Payload.NewStaticDeploy> "simple" CV.staticDeploy
    v<ClientTypes.Pusher.Payload.NewTrace> "simple" (V.uuid, V.tlids)
    v<ClientTypes.Pusher.Payload.New404>
      "simple"
      ("HTTP", "/", "GET", V.instant, V.uuid)
    v<ClientTypes.Pusher.Payload.UpdateWorkerStates> "simple" CV.workerStates
    v<ClientTypes.Pusher.Payload.AddOpV1> "simple" CV.addOpEventV1
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
        { ops = V.ProgramTypes.oplist; opCtr = 0; clientOpCtrID = V.uuid.ToString() })

    v<CTApi.Ops.AddOpV1.Response> "simple" CV.addOpResultV1

    // User DBs
    v<CTApi.DB.StatsV1.Request> "simple" { tlids = V.tlids }
    v<CTApi.DB.StatsV1.Response.T>
      "simple"
      (Map.ofList [ "db1", { count = 0; example = None }
                    "db2", { count = 5; example = Some(CV.dval, "myKey") } ])
    v<CTApi.DB.Unlocked.Response> "simple" { unlocked_dbs = [ V.tlid ] }

    // Execution
    v<CTApi.Execution.FunctionV1.Request>
      "simple"
      { tlid = V.tlid
        trace_id = V.uuid
        caller_id = 7UL
        args = [ CV.dval ]
        fnname = "Int::mod_v0" }
    v<CTApi.Execution.FunctionV1.Response>
      "simple"
      { result = CV.dval
        hash = "abcd"
        hashVersion = 0
        touched_tlids = [ V.tlid ]
        unlocked_dbs = [ V.tlid ] }
    v<CTApi.Execution.HandlerV1.Request>
      "simple"
      { tlid = V.tlid; trace_id = V.uuid; input = [ "v", CV.dval ] }
    v<CTApi.Execution.HandlerV1.Response> "simple" { touched_tlids = [ V.tlid ] }

    // 404s
    v<CTApi.F404.List.Response>
      "simple"
      { f404s = [ ("HTTP", "/", "GET", V.instant, V.uuid) ] }
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
      { handlers =
          V.ProgramTypes.Handler.handlers |> List.map CT2Program.Handler.toCT
        deletedHandlers =
          V.ProgramTypes.Handler.handlers |> List.map CT2Program.Handler.toCT
        dbs = [ V.ProgramTypes.userDB |> CT2Program.DB.toCT ]
        deletedDBs = [ V.ProgramTypes.userDB |> CT2Program.DB.toCT ]
        userFunctions =
          V.ProgramTypes.userFunctions |> List.map CT2Program.UserFunction.toCT
        deletedUserFunctions =
          V.ProgramTypes.userFunctions |> List.map CT2Program.UserFunction.toCT
        unlockedDBs = [ V.tlid ]
        userTypes = V.ProgramTypes.userTypes |> List.map CT2Program.UserType.toCT
        deletedUserTypes =
          V.ProgramTypes.userTypes |> List.map CT2Program.UserType.toCT
        staticDeploys = [ CV.staticDeploy ]
        opCtrs = Map [ V.uuid, 7 ]
        canvasList = [ "test"; "test-canvas2" ]
        orgCanvasList = [ "testorg"; "testorg-canvas2" ]
        permission = Some(ClientTypes.Authorization.ReadWrite)
        orgs = [ "test"; "testorg" ]
        account =
          { username = "test"; name = "Test Name"; email = "test@darklang.com" }
        creationDate = V.instant
        workerSchedules = CV.workerStates
        secrets = [ { name = "test"; value = "secret" } ] }

    // Tunnels
    v<CTApi.Tunnels.Register.Request> "empty" { tunnelHost = None }
    v<CTApi.Tunnels.Register.Request>
      "simple"
      { tunnelHost = Some "host.tunnel.com" }
    v<CTApi.Tunnels.Register.Response> "simple" { success = false }

    // Packages
    v<CTApi.Packages.ListV1.Response>
      "simple"
      [ V.ProgramTypes.packageFn |> CT2Program.Package.Fn.toCT ]

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

    v<CTApi.Toplevels.Delete.Request> "simple" { tlid = V.tlid }
    v<CTApi.Toplevels.Delete.Response> "simple" { result = "success" }

    // Traces

    v<CTApi.Traces.GetAllTraces.Response> "simple" { traces = [ (V.tlid, V.uuid) ] }
    v<CTApi.Traces.GetTraceDataV1.Request>
      "simple"
      { tlid = V.tlid; traceID = V.uuid }
    v<CTApi.Traces.GetTraceDataV1.Response.T>
      "simple"
      { trace =
          (V.uuid,
           { input = [ "var", CV.dval ]
             timestamp = V.instant
             functionResults = [ ("fnName", 7UL, "hash", 0, CV.dval) ] }) }


    // Workers

    v<CTApi.Workers.Scheduler.Request> "simple" { name = "x"; schedule = "pause" }
    v<CTApi.Workers.Scheduler.Response> "all" CV.workerStates

    v<CTApi.Workers.WorkerStats.Request> "simple" { tlid = V.tlid }
    v<CTApi.Workers.WorkerStats.Response> "simple" { count = 5 }

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
                pos = CT2Program.Position.toCT V.ProgramTypes.pos
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
                pos = CT2Program.Position.toCT V.ProgramTypes.pos
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
        function_results = [ ("fnName", 7UL, "hash", 0, V.RuntimeTypes.dval) ] }


  generateTestData ()

let fileNameFor
  (serializerName : string)
  (typeName : string)
  (dataName : string)
  : string =
  let original = $"{serializerName}_{typeName}_{dataName}"
  let replaced = Regex.Replace(original, "[^-_a-zA-Z0-9]", "-")
  Regex.Replace(replaced, "[-]+", "-") + ".json"

let testNoMissingOrExtraOutputTestFiles : Test =
  test "test no extra test files" {
    let filenamesFor dict serializerName =
      dict
      |> Dictionary.toList
      |> List.map (fun (typeName, reason) ->
        SampleData.get typeName serializerName reason
        |> List.map (fun (name, _) -> fileNameFor serializerName typeName name))
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
          let filename = fileNameFor serializerName typeName name
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
          let filename = fileNameFor serializerName typeName name
          File.writefile Config.Serialization filename serializedData))

    generate Json.Vanilla.allowedTypes "vanilla"

module RoundtripToAndFromClientTypes =
  let tests = []

module RoundtripFromAndToClientTypes =
  let tests = []

let tests =
  // TODO: ensure (using reflection) we've covered all types within ClientTypes
  // (many of which we'll have to explicity exclude, if they don't have exact
  // equivalents in the 'domain' types)

  // TODO: ensure (using reflection) that each of the DU cases _of_ those types
  // are covered (so if we add another case to CTProgram.Expr, it's covered).

  testList
    "Vanilla Serialization"
    [ testNoMissingOrExtraOutputTestFiles
      testList "consistent serialization" testTestFiles
      testList
        "roundtrip to and from client types"
        RoundtripToAndFromClientTypes.tests
      testList
        "roundtrip from and to client types"
        RoundtripFromAndToClientTypes.tests ]
