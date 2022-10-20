module Tests.Serialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open TestUtils.TestUtils

module File = LibBackend.File
module Config = LibBackend.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes

module BinarySerialization = LibBinarySerialization.BinarySerialization


module Values =
  let testInstant = NodaTime.Instant.parse "2022-07-04T17:46:57Z"

  let testUuid = System.Guid.Parse "31d72f73-0f99-5a9b-949c-b95705ae7c4d"

  let testTLID : tlid = 7UL
  let testTLIDs : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

  module ProgramTypes =
    /// The test values below are used to check the exact output of test file. So we need
    /// the test inputs to be consistent, which is why we never use `gid ()` below, or
    /// FSharpToExpr functions.
    ///
    /// When updating this, also update FluidTestData.complexExpr in the client
    let testExpr =
      let e = PT.EInteger(34545UL, 5)
      PT.ELet(
        14219007199254740992UL,
        "x1",
        PT.EInteger(929452387UL, 5L),
        PT.ELet(
          620028536UL,
          "x2",
          PT.EInteger(452247642UL, 9223372036854775807L),
          PT.ELet(
            68205955UL,
            "bool",
            PT.EBool(43581311UL, true),
            PT.ELet(
              755798860UL,
              "bool",
              PT.EBool(97054530UL, false),
              PT.ELet(
                244891515UL,
                "str",
                PT.EString(446488682UL, "a string"),
                PT.ELet(
                  537517627UL,
                  "char",
                  PT.ECharacter(1031176330UL, "a"),
                  PT.ELet(
                    399526184UL,
                    "float",
                    PT.EFloat(770715427UL, Negative, "6", "5"),
                    PT.ELet(
                      975263310UL,
                      "n",
                      PT.ENull 923644248UL,
                      PT.ELet(
                        468988830UL,
                        "b",
                        PT.EBlank 133368677UL,
                        PT.ELet(
                          43886336UL,
                          "i",
                          PT.EIf(
                            46231874UL,
                            PT.EFnCall(
                              898531080UL,
                              PT.FQFnName.Stdlib
                                { module_ = "Bool"
                                  function_ = "isError"
                                  version = 0 },
                              [ PT.EInteger(160106123UL, 6L) ],
                              PT.Rail
                            ),
                            PT.EIf(
                              729246077UL,
                              PT.EBinOp(
                                94793109UL,
                                { module_ = None; function_ = "!=" },
                                PT.EInteger(264400705UL, 5L),
                                PT.EInteger(335743639UL, 6L),
                                PT.NoRail
                              ),
                              PT.EBinOp(
                                775118986UL,
                                { module_ = None; function_ = "+" },
                                PT.EInteger(803876589UL, 5L),
                                PT.EInteger(219131014UL, 2L),
                                PT.NoRail
                              ),
                              PT.ELambda(
                                947647446UL,
                                [ (180359194UL, "y") ],
                                PT.EBinOp(
                                  140609068UL,
                                  { module_ = None; function_ = "+" },
                                  PT.EInteger(450951790UL, 2L),
                                  PT.EVariable(402203255UL, "y"),
                                  PT.NoRail
                                )
                              )
                            ),
                            PT.EBinOp(
                              265463935UL,
                              { module_ = None; function_ = "+" },
                              PT.EBinOp(
                                312092282UL,
                                { module_ = None; function_ = "+" },
                                PT.EFieldAccess(
                                  974664608UL,
                                  PT.EVariable(1002893266UL, "x"),
                                  "y"
                                ),
                                PT.EFnCall(
                                  173079901UL,
                                  PT.FQFnName.Stdlib
                                    { module_ = "Int"
                                      function_ = "add"
                                      version = 0 },
                                  [ PT.EInteger(250221144UL, 6L)
                                    PT.EInteger(298149318UL, 2L) ],
                                  PT.NoRail
                                ),
                                PT.NoRail
                              ),
                              PT.EList(
                                539797095UL,
                                [ PT.EInteger(267797631UL, 5L)
                                  PT.EInteger(352138743UL, 6L)
                                  PT.EInteger(430871955UL, 7L) ]
                              ),
                              PT.NoRail
                            )
                          ),
                          PT.ELet(
                            831830073UL,
                            "r",
                            PT.ERecord(
                              109539183UL,
                              [ ("field",
                                 PT.EPipe(
                                   786862131UL,
                                   PT.EInteger(555880460UL, 5L),
                                   PT.EBinOp(
                                     1021880969UL,
                                     { module_ = None; function_ = "+" },
                                     PT.EPipeTarget 936577032UL,
                                     PT.EInteger(962393769UL, 2L),
                                     PT.NoRail
                                   ),
                                   []
                                 ))
                                ("constructor",
                                 PT.EConstructor(
                                   567764301UL,
                                   "Ok",
                                   [ PT.EConstructor(
                                       646107057UL,
                                       "Error",
                                       [ PT.EConstructor(
                                           689802831UL,
                                           "Just",
                                           [ PT.EConstructor(
                                               957916875UL,
                                               "Nothing",
                                               []
                                             ) ]
                                         ) ]
                                     ) ]
                                 )) ]
                            ),
                            PT.ELet(
                              745304029UL,
                              "m",
                              PT.EMatch(
                                889712088UL,
                                PT.EFnCall(
                                  203239466UL,
                                  PT.FQFnName.Stdlib
                                    { module_ = "Mod"
                                      function_ = "function"
                                      version = 2 },
                                  [],
                                  PT.NoRail
                                ),
                                [ (PT.PConstructor(
                                    1015986188UL,
                                    "Ok",
                                    [ PT.PVariable(334386852UL, "x") ]
                                   ),
                                   PT.EVariable(863810169UL, "v"))
                                  (PT.PInteger(928253813UL, 5L),
                                   PT.EInteger(342670561UL, -9223372036854775808L))
                                  (PT.PBool(435227293UL, true),
                                   PT.EInteger(232748650UL, 7L))
                                  (PT.PCharacter(387662539UL, "c"),
                                   PT.ECharacter(657848009UL, "c"))
                                  (PT.PString(491115870UL, "string"),
                                   PT.EString(820329949UL, "string"))
                                  (PT.PNull 701616052UL, PT.ENull 731162955UL)
                                  (PT.PVariable(722099983UL, "var"),
                                   PT.EBinOp(
                                     275666765UL,
                                     { module_ = None; function_ = "+" },
                                     PT.EInteger(739193732UL, 6L),
                                     PT.EVariable(880556562UL, "var"),
                                     PT.NoRail
                                   ))
                                  (PT.PFloat(409097457UL, Positive, "5", "6"),
                                   PT.EFloat(131187958UL, Positive, "5", "6"))
                                  (PT.PBlank 858594159UL,
                                   PT.EInteger(135348705UL, 6L))
                                  (PT.PTuple(
                                    1285610UL,
                                    PT.PVariable(17823641UL, "a"),
                                    PT.PVariable(58123641UL, "b"),
                                    [ PT.PVariable(95723641UL, "c") ]
                                   ),
                                   PT.EBool(123716747UL, true)) ]
                              ),
                              PT.ELet(
                                927055617UL,
                                "f",
                                PT.EFeatureFlag(
                                  882488977UL,
                                  "test",
                                  PT.EBool(349352147UL, true),
                                  PT.EInteger(578528886UL, 5L),
                                  PT.EInteger(562930224UL, 6L)
                                ),
                                PT.ELet(
                                  6345345UL,
                                  "partials",
                                  PT.EList(
                                    23423423UL,
                                    [ PT.EPartial(2949606UL, "some 🤬 string", e)
                                      PT.ERightPartial(
                                        9239755UL,
                                        "some 😭 string",
                                        e
                                      )
                                      PT.ELeftPartial(
                                        234885UL,
                                        "some 👨‍👩‍👧‍👦 string",
                                        e
                                      ) ]
                                  ),
                                  PT.ELet(
                                    883434UL,
                                    "tuples",
                                    PT.ETuple(72333UL, e, e, [ e ]),
                                    e
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    let testPos : PT.Position = { x = 6; y = 6 }

    let testHandlerIDs : PT.Handler.ids =
      { moduleID = 129952UL; nameID = 33052UL; modifierID = 10038562UL }

    let testHttpHandler : PT.Handler.T =
      let spec = PT.Handler.HTTP("/path", "GET", testHandlerIDs)
      { spec = spec; tlid = 92987663UL; ast = testExpr; pos = testPos }

    let testHttpBasicHandler : PT.Handler.T =
      let spec = PT.Handler.HTTPBasic("/path-bytes", "GET", testHandlerIDs)
      { spec = spec; tlid = 42280663UL; ast = testExpr; pos = testPos }

    let testWorker : PT.Handler.T =
      let spec = PT.Handler.Worker("name", testHandlerIDs)
      { spec = spec; tlid = 19930486UL; ast = testExpr; pos = testPos }

    let testOldWorker : PT.Handler.T =
      let spec = PT.Handler.OldWorker("MODULE", "name", testHandlerIDs)
      { spec = spec; tlid = 10438664321UL; ast = testExpr; pos = testPos }

    let testRepl : PT.Handler.T =
      let spec = PT.Handler.REPL("name", testHandlerIDs)
      { spec = spec; tlid = 10395769302UL; ast = testExpr; pos = testPos }

    let testCron1 : PT.Handler.T =
      let spec = PT.Handler.Cron("name", None, testHandlerIDs)
      { spec = spec; tlid = 294906673UL; ast = testExpr; pos = testPos }

    let testCron2 : PT.Handler.T =
      let spec =
        PT.Handler.Cron("name", Some PT.Handler.Every12Hours, testHandlerIDs)
      { spec = spec; tlid = 199385766UL; ast = testExpr; pos = testPos }

    let testUnknownHandler : PT.Handler.T =
      let spec = PT.Handler.UnknownHandler("name", "", testHandlerIDs)
      { spec = spec; tlid = 13633UL; ast = testExpr; pos = testPos }

    let testHandlersWithName : List<string * PT.Handler.T> =
      [ "Http", testHttpHandler
        "Worker", testWorker
        "Cron1", testCron1
        "Cron2", testCron2
        "REPL", testRepl
        "Unknown", testUnknownHandler
        "OldWorker", testOldWorker
        "HttpBasic", testHttpBasicHandler ]

    let testHandlers = List.map snd testHandlersWithName

    let testType =
      PT.TRecord [ ("nested",
                    PT.TList(
                      PT.TDict(
                        PT.TDB(
                          PT.THttpResponse(
                            PT.TOption(
                              PT.TDbList(
                                PT.TResult(PT.TInt, PT.TFn([ PT.TFloat ], PT.TNull))
                              )
                            )
                          )
                        )
                      )
                    ))
                   ("int", PT.TInt)
                   ("float", PT.TFloat)
                   ("bool", PT.TBool)
                   ("null", PT.TNull)
                   ("str", PT.TStr)
                   ("list", PT.TList(PT.TInt))
                   ("tuple", PT.TTuple(PT.TInt, PT.TStr, []))
                   ("dict", PT.TDict(PT.TInt))
                   ("incomplete", PT.TIncomplete)
                   ("error", PT.TError)
                   ("httpresponse", PT.THttpResponse(PT.TInt))
                   ("db", PT.TDB(PT.TInt))
                   ("date", PT.TDate)
                   ("char", PT.TChar)
                   ("password", PT.TPassword)
                   ("uuid", PT.TUuid)
                   ("option", PT.TOption(PT.TInt))
                   ("errorRail", PT.TErrorRail)
                   ("usertype", PT.TUserType("name", 0))
                   ("bytes", PT.TBytes)
                   ("result", PT.TResult(PT.TInt, PT.TStr))
                   ("variable", PT.TVariable "v")
                   ("fn", PT.TFn([ PT.TInt ], PT.TInt))
                   ("record", PT.TRecord([ "field1", PT.TInt ])) ]

    let testDBs : List<PT.DB.T> =
      [ { tlid = 0UL
          pos = testPos
          nameID = 2399545UL
          name = "User"
          version = 0
          cols =
            [ { name = None; typ = None; nameID = 2949054UL; typeID = 5929202UL }
              { name = None
                typ = Some PT.TInt
                nameID = 20109857UL
                typeID = 299063UL }
              { name = Some "name"
                typ = None
                nameID = 28234232UL
                typeID = 029985336UL }
              { name = Some "value"
                typ = Some testType
                nameID = 923982352UL
                typeID = 289429232UL } ] } ]

    let testUserFunction : PT.UserFunction.T =
      { tlid = 0UL
        name = "myFunc"
        nameID = 1828332UL
        parameters =
          [ { name = "myparam1"
              nameID = 23824935UL
              typ = None
              typeID = 38284244UL
              description = "param1" }
            { name = "myparam2"
              nameID = 92837232UL
              typ = Some testType
              typeID = 239232UL
              description = "param1" } ]
        returnType = testType
        returnTypeID = 23923423UL
        description = "function description"
        infix = false
        body = testExpr }

    let testUserFunctions : List<PT.UserFunction.T> = [ testUserFunction ]

    let testUserType : PT.UserType.T =
      { tlid = 0UL
        name = "User"
        nameID = 92930232UL
        version = 0
        definition =
          PT.UserType.Record [ { name = "prop1"
                                 typ = None
                                 nameID = 923942342UL
                                 typeID = 3452342UL }
                               { name = "prop1"
                                 typ = Some testType
                                 nameID = 0698978UL
                                 typeID = 93494534UL } ] }

    let testUserTypes : List<PT.UserType.T> = [ testUserType ]

    let testPackageFn : PT.Package.Fn =
      { name =
          { owner = "dark"
            package = "stdlib"
            module_ = "Int"
            function_ = "mod"
            version = 0 }
        body = testExpr
        parameters = [ { name = "param"; typ = testType; description = "desc" } ]
        returnType = testType
        description = "test"
        author = "test"
        deprecated = false
        tlid = testTLID }

    let testToplevels : List<PT.Toplevel.T> =
      [ List.map PT.Toplevel.TLHandler testHandlers
        List.map PT.Toplevel.TLDB testDBs
        List.map PT.Toplevel.TLFunction testUserFunctions
        List.map PT.Toplevel.TLType testUserTypes ]
      |> List.concat

    let testOplist : PT.Oplist =
      let id = 923832423UL
      let tlid = 94934534UL
      [ PT.SetHandler(testHttpHandler.tlid, testPos, testHttpHandler)
        PT.CreateDB(tlid, testPos, "name")
        PT.AddDBCol(tlid, id, id)
        PT.SetDBColName(tlid, id, "name")
        PT.SetDBColType(tlid, id, "int")
        PT.DeleteTL tlid
        PT.MoveTL(tlid, testPos)
        PT.SetFunction(testUserFunction)
        PT.ChangeDBColName(tlid, id, "name")
        PT.ChangeDBColType(tlid, id, "int")
        PT.UndoTL tlid
        PT.RedoTL tlid
        PT.SetExpr(tlid, id, testExpr)
        PT.TLSavepoint tlid
        PT.DeleteFunction tlid
        PT.DeleteDBCol(tlid, id)
        PT.RenameDBname(tlid, "newname")
        PT.CreateDBWithBlankOr(tlid, testPos, id, "User")
        PT.SetType(testUserType)
        PT.DeleteType tlid ]

  module RuntimeTypes =
    let testDval =
      sampleDvals
      |> List.filter (fun (name, dv) -> name <> "password")
      |> Map
      |> RT.DObj


  module ClientRuntime =
    let testClientDval : CTRuntime.Dval.T =
      CT2Runtime.Dval.toCT RuntimeTypes.testDval


  let testStaticDeploy : LibBackend.StaticAssets.StaticDeploy =
    { deployHash = "zf2ttsgwln"
      url = "https://paul.darksa.com/nwtf5qhdku2untsc17quotrhffa/zf2ttsgwln"
      status = LibBackend.StaticAssets.Deployed
      lastUpdate = testInstant }

  let testAddOpResultV1 : LibBackend.Op.AddOpResultV1 =
    { handlers = ProgramTypes.testHandlers
      deletedHandlers = ProgramTypes.testHandlers
      dbs = ProgramTypes.testDBs
      deletedDBs = ProgramTypes.testDBs
      userFunctions = ProgramTypes.testUserFunctions
      deletedUserFunctions = ProgramTypes.testUserFunctions
      userTypes = ProgramTypes.testUserTypes
      deletedUserTypes = ProgramTypes.testUserTypes }

  let testWorkerStates : LibBackend.QueueSchedulingRules.WorkerStates.T =
    (Map.ofList [ "run", LibBackend.QueueSchedulingRules.WorkerStates.Running
                  "blocked", LibBackend.QueueSchedulingRules.WorkerStates.Blocked
                  "paused", LibBackend.QueueSchedulingRules.WorkerStates.Paused ])


  module Pusher =
    let testAddOpEventV1 : LibBackend.Op.AddOpEventV1 =
      { ``params`` =
          { ops = ProgramTypes.testOplist
            opCtr = 0
            clientOpCtrID = testUuid.ToString() }
        result = testAddOpResultV1 }


module BinarySerializationRoundtripTests =
  let toplevelRoundtripTest =
    testMany
      "serializeToplevels"
      (fun tl ->
        let tlid = PT.Toplevel.toTLID tl
        tl
        |> BinarySerialization.serializeToplevel
        |> BinarySerialization.deserializeToplevel tlid
        |> (=) tl)
      (List.map (fun x -> x, true) Values.ProgramTypes.testToplevels)

  let oplistRoundtripTest =
    test "roundtrip oplists" {
      let actual =
        Values.ProgramTypes.testOplist
        |> BinarySerialization.serializeOplist 0UL
        |> BinarySerialization.deserializeOplist 0UL
      Expect.equal actual Values.ProgramTypes.testOplist ""
    }

module GenericSerializersTests =
  // We've annotated each type that we serialize in Dark, with the generic serializer
  // that it allows. Now we want to verify that the format doesn't change by having a
  // sample value of each type, and verifying the serialized sample value doesn't
  // change.  This gives us extremely high confidence that we're not breaking
  // anything when we change the serializer.
  // - If there's a change to the format, a new file will be generated.
  //   What to do here will depend on the change, how much data is saved with the old
  //   change, etc. You may want to save both formats and check that both are
  //   deserializable.
  // - If there's a change to the sample value, generate a new file.
  //   Commit it after verifying that the format is appropriate.

  module SampleData =
    open Values

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


    // TODO: stop serializing internal types
    // We're currently using many "internal" types in communication with external
    // parties such as APIs and Pusher.com payloads. All referenced types below
    // should reference _client_ types (currently in the ClientTypes project) rather
    // than internal types.
    // I think 'Prelude' types could probably stay as-is, though.
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
            [ { column = "trace.trace_id"; op = "="; value = string testUuid } ]
          limit = 100
          time_range = 604800 }

      // ------------------
      // LibExecution
      // ------------------

      v<CTRuntime.Dval.T> "complete" ClientRuntime.testClientDval

      v<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
        "complete"
        (RuntimeTypes.testDval
         |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT)

      v<LibExecution.ProgramTypes.Oplist> "complete" ProgramTypes.testOplist
      v<LibExecution.ProgramTypes.Handler.T> "simple" ProgramTypes.testHttpHandler

      // ------------------
      // LibBackend
      // ------------------

      v<LibBackend.EventQueueV2.NotificationData>
        "simple"
        { id = testUuid; canvasID = testUuid }

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
          { name = "List"; tipe = LibBackend.PackageManager.TList; description = "" }
          { name = "obj"; tipe = LibBackend.PackageManager.TObj; description = "" } ]

      v<PT.Position> "simple" { x = 10; y = -16 }

      // Used by Pusher
      v<LibBackend.Pusher.AddOpEventTooBigPayload> "simple" { tlids = testTLIDs }
      v<LibBackend.Op.AddOpEventV1> "simple" Pusher.testAddOpEventV1
      v<LibBackend.StaticAssets.StaticDeploy> "simple" testStaticDeploy
      v<LibBackend.Pusher.NewTraceID> "simple" (testUuid, testTLIDs)
      v<LibBackend.TraceInputs.F404>
        "simple"
        ("HTTP", "/", "GET", testInstant, testUuid)
      v<LibBackend.QueueSchedulingRules.WorkerStates.T> "simple" testWorkerStates



      // ------------------
      // ApiServer
      // ------------------

      // AddOps
      v<ApiServer.AddOps.V1.Params>
        "simple"
        { ops = ProgramTypes.testOplist
          opCtr = 0
          clientOpCtrID = testUuid.ToString() }
      v<ApiServer.AddOps.V1.T> "simple" testAddOpResultV1


      // User DBs
      v<ClientTypes.Api.DB.StatsV1.Request> "simple" { tlids = testTLIDs }
      v<ClientTypes.Api.DB.StatsV1.Response.T>
        "simple"
        (Map.ofList [ "db1", { count = 0; example = None }
                      "db2",
                      { count = 5
                        example = Some(ClientRuntime.testClientDval, "myKey") } ])
      v<ClientTypes.Api.DB.Unlocked.Response>
        "simple"
        { unlocked_dbs = [ testTLID ] }

      // Execution
      v<ClientTypes.Api.Execution.FunctionV1.Request>
        "simple"
        { tlid = testTLID
          trace_id = testUuid
          caller_id = 7UL
          args = [ ClientRuntime.testClientDval ]
          fnname = "Int::mod_v0" }
      v<ClientTypes.Api.Execution.FunctionV1.Response>
        "simple"
        { result = ClientRuntime.testClientDval
          hash = "abcd"
          hashVersion = 0
          touched_tlids = [ testTLID ]
          unlocked_dbs = [ testTLID ] }
      v<ClientTypes.Api.Execution.HandlerV1.Request>
        "simple"
        { tlid = testTLID
          trace_id = testUuid
          input = [ "v", ClientRuntime.testClientDval ] }
      v<ClientTypes.Api.Execution.HandlerV1.Response>
        "simple"
        { touched_tlids = [ testTLID ] }

      // 404s
      v<ApiServer.F404s.List.T>
        "simple"
        { f404s = [ ("HTTP", "/", "GET", testInstant, testUuid) ] }
      v<ApiServer.F404s.Delete.Params>
        "simple"
        { space = "HTTP"; path = "/"; modifier = "POST" }
      v<ApiServer.F404s.Delete.T> "simple" { result = "success" }

      // Functions
      v<List<ApiServer.Functions.BuiltInFn.T>>
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
             previewable = ApiServer.Functions.Previewable.Pure
             deprecated = ApiServer.Functions.Deprecation.NotDeprecated
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CTRuntime.DType.TInt
             description = "impure"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Impure
             deprecated = ApiServer.Functions.Deprecation.NotDeprecated
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CTRuntime.DType.TInt
             description = "impurepreviewable"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.ImpurePreviewable
             deprecated = ApiServer.Functions.Deprecation.NotDeprecated
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CTRuntime.DType.TInt
             description = "replacedBy"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Pure
             deprecated =
               ApiServer.Functions.Deprecation.ReplacedBy(
                 { module_ = "Int"; function_ = "mod"; version = 1 }
               )
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CTRuntime.DType.TInt
             description = "renamedTo"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Pure
             deprecated =
               ApiServer.Functions.Deprecation.RenamedTo(
                 { module_ = "Int"; function_ = "mod"; version = 1 }
               )
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CTRuntime.DType.TInt
             description = "deprecatedBecause"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Pure
             deprecated = ApiServer.Functions.Deprecation.DeprecatedBecause "reason"
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable } ])

      // InitialLoad
      v<ApiServer.InitialLoad.V1.T>
        "initial"
        { handlers = ProgramTypes.testHandlers
          deletedHandlers = ProgramTypes.testHandlers
          dbs = ProgramTypes.testDBs
          deletedDBs = ProgramTypes.testDBs
          userFunctions = ProgramTypes.testUserFunctions
          deletedUserFunctions = ProgramTypes.testUserFunctions
          unlockedDBs = [ testTLID ]
          userTypes = ProgramTypes.testUserTypes
          deletedUserTypes = ProgramTypes.testUserTypes
          staticDeploys =
            [ ApiServer.InitialLoad.V1.toApiStaticDeploys testStaticDeploy ]
          opCtrs = Map [ testUuid, 7 ]
          canvasList = [ "test"; "test-canvas2" ]
          orgCanvasList = [ "testorg"; "testorg-canvas2" ]
          permission = Some(LibBackend.Authorization.ReadWrite)
          orgs = [ "test"; "testorg" ]
          account =
            { username = "test"; name = "Test Name"; email = "test@darklang.com" }
          creationDate = testInstant
          workerSchedules = testWorkerStates
          secrets = [ { name = "test"; value = "secret" } ] }

      // Tunnels
      v<ClientTypes.Api.Tunnels.Register.Request> "empty" { tunnelHost = None }
      v<ClientTypes.Api.Tunnels.Register.Request>
        "simple"
        { tunnelHost = Some "host.tunnel.com" }
      v<ClientTypes.Api.Tunnels.Register.Response> "simple" { success = false }

      // Packages
      v<ApiServer.Packages.ListV1.T> "simple" [ ProgramTypes.testPackageFn ]

      // SecretsV1

      v<ClientTypes.Api.Secrets.DeleteV1.Request> "simple" { name = "test" }
      v<ClientTypes.Api.Secrets.DeleteV1.Response>
        "simple"
        { secrets = [ { name = "test"; value = "secret" } ] }

      v<ClientTypes.Api.Secrets.InsertV1.Request>
        "simple"
        { name = "test"; value = "secret" }
      v<ClientTypes.Api.Secrets.InsertV1.Response>
        "simple"
        { secrets = [ { name = "test"; value = "secret" } ] }

      // Toplevels

      v<ClientTypes.Api.Toplevels.Delete.Request> "simple" { tlid = testTLID }
      v<ClientTypes.Api.Toplevels.Delete.Response> "simple" { result = "success" }

      // Traces

      v<ClientTypes.Api.Traces.GetAllTraces.Response>
        "simple"
        { traces = [ (testTLID, testUuid) ] }
      v<ClientTypes.Api.Traces.GetTraceDataV1.Request>
        "simple"
        { tlid = testTLID; traceID = testUuid }
      v<ClientTypes.Api.Traces.GetTraceDataV1.Response.T>
        "simple"
        { trace =
            (testUuid,
             { input = [ "var", ClientRuntime.testClientDval ]
               timestamp = testInstant
               functionResults =
                 [ ("fnName", 7UL, "hash", 0, ClientRuntime.testClientDval) ] }) }


      // Workers

      v<ClientTypes.Api.Workers.Scheduler.Request>
        "simple"
        { name = "x"; schedule = "pause" }
      v<ApiServer.Workers.Scheduler.Response> "all" testWorkerStates

      v<ClientTypes.Api.Workers.WorkerStats.Request> "simple" { tlid = testTLID }
      v<ClientTypes.Api.Workers.WorkerStats.Response> "simple" { count = 5 }

      // ------------------
      // LibAnalysis
      // ------------------
      v<ClientTypes.Analysis.AnalysisResult>
        "simple"
        (Ok(
          testUuid,
          Dictionary.fromList (
            [ (7UL, CTAnalysis.ExecutionResult.ExecutedResult ClientRuntime.testClientDval)
              (7UL, CTAnalysis.ExecutionResult.NonExecutedResult ClientRuntime.testClientDval) ]
          ),
          1,
          NodaTime.Instant.UnixEpoch
        ))
      v<ClientTypes.Analysis.PerformAnalysisParams>
        "handler"
        (ClientTypes.Analysis.AnalyzeHandler
          { requestID = 2
            requestTime = NodaTime.Instant.UnixEpoch
            handler = CT2Program.Handler.toCT ProgramTypes.testHttpHandler
            traceID = testUuid
            traceData =
              { input = [ "var", ClientRuntime.testClientDval ]
                timestamp = testInstant
                functionResults =
                  [ ("fnName", 7UL, "hash", 0, ClientRuntime.testClientDval) ] }
            dbs =
              [ { tlid = testTLID
                  name = "dbname"
                  nameID = 7UL
                  pos = CT2Program.Position.toCT Values.ProgramTypes.testPos
                  cols =
                    [ { name = Some("colname")
                        nameID = 8UL
                        typ = Some(CT2Program.DType.toCT PT.TInt)
                        typeID = 9UL } ]
                  version = 1 } ]
            userFns =
              List.map CT2Program.UserFunction.toCT ProgramTypes.testUserFunctions
            userTypes = List.map CT2Program.UserType.toCT ProgramTypes.testUserTypes
            packageFns = [ CT2Program.Package.Fn.toCT ProgramTypes.testPackageFn ]
            secrets = [ { name = "z"; value = "y" } ] })
      v<ClientTypes.Analysis.PerformAnalysisParams>
        "function"
        (ClientTypes.Analysis.AnalyzeFunction
          { requestID = 3
            requestTime = NodaTime.Instant.UnixEpoch
            func = CT2Program.UserFunction.toCT ProgramTypes.testUserFunction
            traceID = testUuid
            traceData =
              { input = [ "var", ClientRuntime.testClientDval ]
                timestamp = testInstant
                functionResults =
                  [ ("fnName", 7UL, "hash", 0, ClientRuntime.testClientDval) ] }
            dbs =
              [ { tlid = testTLID
                  name = "dbname"
                  nameID = 7UL
                  pos = CT2Program.Position.toCT Values.ProgramTypes.testPos
                  cols =
                    [ { name = Some("colname")
                        nameID = 8UL
                        typ = Some(CT2Program.DType.toCT PT.TInt)
                        typeID = 9UL } ]
                  version = 1 } ]
            userFns =
              List.map CT2Program.UserFunction.toCT ProgramTypes.testUserFunctions
            userTypes = List.map CT2Program.UserType.toCT ProgramTypes.testUserTypes
            packageFns = [ CT2Program.Package.Fn.toCT ProgramTypes.testPackageFn ]
            secrets = [ { name = "z"; value = "y" } ] })


      // ------------------
      // Tests
      // ------------------

      v<LibExecution.AnalysisTypes.TraceData>
        "testTraceData"
        { input = [ "var", RuntimeTypes.testDval ]
          timestamp = testInstant
          function_results = [ ("fnName", 7UL, "hash", 0, RuntimeTypes.testDval) ] }


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
          |> List.map (fun (name, _) ->
            fileNameFor serializerName reason typeName name))
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

  let generateTestFiles () : unit =
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

module CustomSerializersTests =

  type Format =
    { name : string
      serializer : PT.Oplist -> byte array
      deserializer : byte array -> PT.Oplist
      prettyPrinter : Option<PT.Oplist -> string>
      prefix : string
      suffix : string
      prettyPrinterSuffix : string }

  let formats =
    [ { name = "BinarySerialization"
        serializer = BinarySerialization.serializeOplist 0UL
        deserializer = BinarySerialization.deserializeOplist 0UL
        prettyPrinter = Some(BinarySerialization.Test.serializeOplistToJson 0UL)
        prefix = "oplist-binary"
        suffix = ".bin"
        prettyPrinterSuffix = ".json" }
      // DvalReprExternal
      // - toEnduserReadableTextV0
      // - toPrettyMachineJsonV1
      // - toDeveloperReprV0
      // - unsafeOfUnknownJsonV0
      // - ofUnknownJsonV1
      // DvalReprInternalDeprecated
      // - unsafeDvalOfJsonV1
      // - unsafeDvalToJsonValueV0
      // - unsafeDvalToJsonValueV1
      // - toInternalRoundtrippableV0
      // - ofIntertnalRoundtrippableJsonV0
      // - toInternalQueryableV1
      // - ofInternalQueryableV1
      // - hash 0
      // - hash 1
      // DvalReprInternalNew
      // - toRoundtrippableJsonV0
      // - parseRoundtrippableJsonV0
      // Prelude.Json.Vanilla
      // Prelude.Json.OCamlCompatible
      // toStringPairs (LegacyBaseHttpClient)
      // toStringPairs (HttpClient)
      // LibJwt LegacySerializer.toYojson
      // LibJwt LegacySerializer.ofYojson
      ]

  let nameFor (f : Format) (pretty : bool) (version : string) =
    let (pretty, suffix) =
      if pretty then ("-pretty", f.prettyPrinterSuffix) else "", f.suffix
    $"{f.prefix}{pretty}-{version}{suffix}"

  /// Generates timestamped test files for binary serialization. These files are used
  /// to prove that the binary serialization format is compatible.  When we change the
  /// format, we should still be able to read the old files in addition to the new ones
  /// (though they will not necessarily have the same output). If we make changes to
  /// the binary serialization format (or to the test cases), we generate the files
  /// and commit them.
  let generateTestFiles () : unit =
    formats
    |> List.iter (fun f ->
      let output = f.serializer Values.ProgramTypes.testOplist

      let sha1 =
        System.Security.Cryptography.SHA1.HashData(System.ReadOnlySpan output)
        |> SimpleBase.Base16.LowerCase.Encode

      File.writefileBytes Config.Serialization (nameFor f false sha1) output
      File.writefileBytes Config.Serialization (nameFor f false "latest") output

      f.prettyPrinter
      |> Option.tap (fun s ->
        let jsonData = s Values.ProgramTypes.testOplist
        File.writefile Config.Serialization (nameFor f true sha1) jsonData
        File.writefile Config.Serialization (nameFor f true "latest") jsonData))


  let testTestFiles =
    formats
    |> List.map (fun f ->
      test "check test files are correct" {
        f.prettyPrinter
        |> Option.tap (fun s ->
          let expected = File.readfile Config.Serialization (nameFor f true "latest")
          let actual = s Values.ProgramTypes.testOplist
          Expect.equal actual expected "check generates the same json")

        // Check that the generated binary data matches what we have saved. This ensures
        // the format has not changed.
        let actual = f.serializer Values.ProgramTypes.testOplist
        let expected =
          File.readfileBytes Config.Serialization (nameFor f false "latest")
        Expect.equal actual expected "check can read the saved file"

        // Check that all .bin files can be parsed and give us the expected answer (this
        // might not be true as we get more formats, so this may need to be adapted)
        File.lsPattern Config.Serialization "{f.prefix}.*{f.suffix}"
        |> List.iter (fun filename ->
          let actual =
            File.readfileBytes Config.Serialization filename |> f.deserializer
          Expect.equal
            actual
            Values.ProgramTypes.testOplist
            "deserialize should  match latest format")
      })

let generateTestFiles () =
  // Enabled in dev so we can see changes as git diffs. Disabled in CI so changes
  // will fail the tests
  if Config.serializationGenerateTestData then
    CustomSerializersTests.generateTestFiles ()
    GenericSerializersTests.generateTestFiles ()

let tests =
  testList
    "Serialization"
    [ BinarySerializationRoundtripTests.toplevelRoundtripTest
      BinarySerializationRoundtripTests.oplistRoundtripTest
      GenericSerializersTests.testNoMissingOrExtraOutputTestFiles
      testList "customer test formats" CustomSerializersTests.testTestFiles
      testList "generic vanilla test formats" GenericSerializersTests.testTestFiles ]
