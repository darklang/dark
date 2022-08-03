module Tests.Serialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open TestUtils.TestUtils

module File = LibBackend.File
module Config = LibBackend.Config
module CT = ClientTypes
module CTA = CT.Analysis
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module OT = LibExecution.OCamlTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


module Values =

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
                                  { module_ = "Int"; function_ = "add"; version = 0 },
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
                                (PT.PBlank 858594159UL, PT.EInteger(135348705UL, 6L)) ]
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
                                    PT.ERightPartial(9239755UL, "some 😭 string", e)
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

  let testOCamlExpr = OT.Convert.pt2ocamlExpr testExpr

  let testPos : PT.Position = { x = 6; y = 6 }

  let testHandlerIDs : PT.Handler.ids =
    { moduleID = 129952UL; nameID = 33052UL; modifierID = 10038562UL }

  let testHttpHandler : PT.Handler.T =
    let spec = PT.Handler.HTTP("/path", "GET", testHandlerIDs)
    { spec = spec; tlid = 92987663UL; ast = testExpr; pos = testPos }

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
    let spec = PT.Handler.Cron("name", Some PT.Handler.Every12Hours, testHandlerIDs)
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
      "OldWorker", testOldWorker ]

  let testHandlers = List.map snd testHandlersWithName

  let testDval =
    sampleDvals
    |> List.filter (fun (name, dv) -> name <> "password")
    |> Map
    |> RT.DObj

  let testClientDval : CT.Dval.T = CT.Dval.fromRT testDval

  let testOCamlDval = LibExecution.OCamlTypes.Convert.rt2ocamlDval testDval

  let testInstant = NodaTime.Instant.parse "2022-07-04T17:46:57Z"

  let testUuid = System.Guid.Parse "31d72f73-0f99-5a9b-949c-b95705ae7c4d"

  let testTLID : tlid = 7UL
  let testTLIDs : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

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

  let testOCamlTipe = OT.Convert.pt2ocamlTipe testType

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


  let testToplevels : List<PT.Toplevel.T> =
    [ List.map PT.Toplevel.TLHandler testHandlers
      List.map PT.Toplevel.TLDB testDBs
      List.map PT.Toplevel.TLFunction testUserFunctions
      List.map PT.Toplevel.TLType testUserTypes ]
    |> List.concat

  let (testOCamlToplevels, testOCamlUserFns, testOCamlUserTipes) =
    testToplevels
    |> (List.map (fun tl -> PT.Toplevel.toTLID tl, tl))
    |> Map
    |> OT.Convert.pt2ocamlToplevels


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

  let testOCamlOplist : OT.oplist = OT.Convert.pt2ocamlOplist testOplist

  let testStaticDeploy : LibBackend.StaticAssets.StaticDeploy =
    { deployHash = "zf2ttsgwln"
      url = "https://paul.darksa.com/nwtf5qhdku2untsc17quotrhffa/zf2ttsgwln"
      status = LibBackend.StaticAssets.Deployed
      lastUpdate = testInstant }

  let testAddOpEventV0 : LibBackend.Op.AddOpEventV0 =
    { ``params`` = { ops = testOCamlOplist; opCtr = 0; clientOpCtrId = None }
      result =
        { toplevels = testOCamlToplevels
          deleted_toplevels = testOCamlToplevels
          user_functions = testOCamlUserFns
          deleted_user_functions = testOCamlUserFns
          user_tipes = testOCamlUserTipes
          deleted_user_tipes = testOCamlUserTipes } }


  let testAddOpResultV1 : LibBackend.Op.AddOpResultV1 =
    { handlers = testHandlers
      deletedHandlers = testHandlers
      dbs = testDBs
      deletedDBs = testDBs
      userFunctions = testUserFunctions
      deletedUserFunctions = testUserFunctions
      userTypes = testUserTypes
      deletedUserTypes = testUserTypes }

  let testAddOpEventV1 : LibBackend.Op.AddOpEventV1 =
    { ``params`` = { ops = testOplist; opCtr = 0; clientOpCtrID = None }
      result = testAddOpResultV1 }


  let testWorkerStates : LibBackend.QueueSchedulingRules.WorkerStates.T =
    (Map.ofList [ "run", LibBackend.QueueSchedulingRules.WorkerStates.Running
                  "blocked", LibBackend.QueueSchedulingRules.WorkerStates.Blocked
                  "paused", LibBackend.QueueSchedulingRules.WorkerStates.Paused ])


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
      (List.map (fun x -> x, true) Values.testToplevels)


  let oplistRoundtripTest =
    test "roundtrip oplists" {
      let actual =
        Values.testOplist
        |> BinarySerialization.serializeOplist 0UL
        |> BinarySerialization.deserializeOplist 0UL
      Expect.equal actual Values.testOplist ""
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

    let fileNameFor
      (typeName : string)
      (serializerName : string)
      (dataName : string)
      : string =
      let original = $"{serializerName}-{typeName}-{dataName}"
      let replaced = Regex.Replace(original, "[^-_a-zA-Z0-9]", "-")
      Regex.Replace(replaced, "[-]+", "-") + ".json"

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

    let generateOCamlCompatibleData<'t> (dataName : string) (data : 't) =
      // Use prettySerialize even though we use serialize in practice, as the
      // non-pretty version is too hard to read and compare.
      let serializer = Json.OCamlCompatible.prettySerialize
      generateSerializerData "ocaml" serializer dataName data

    let generateBoth<'t> (dataName) (data : 't) =
      generateVanillaData<'t> dataName data
      generateOCamlCompatibleData<'t> dataName data

    // Shortcuts to make generateTestData more readable
    let private v<'t> = generateVanillaData<'t>
    let private oc<'t> = generateOCamlCompatibleData<'t>
    let private both<'t> = generateBoth<'t>

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

      both<ORT.dval> "complete" testOCamlDval
      both<RT.Dval> "complete" testDval
      testHandlersWithName
      |> List.iter (fun (name, handler) -> oc<PT.Handler.T> name handler)
      // v<OT.oplist> "all" testOCamlOplist

      v<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
        "complete"
        (testDval
         |> LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT)

      // ------------------
      // LibBackend
      // ------------------

      v<LibBackend.EventQueueV2.NotificationData>
        "simple"
        { id = testUuid; canvasID = testUuid }

      v<LibBackend.Session.JsonData>
        "simple"
        { username = "paul"; csrf_token = "abcd1234abdc1234abcd1234abc1234" }

      oc<LibBackend.PackageManager.ParametersDBFormat>
        "simple"
        [ { name = "a"; tipe = OT.TInt; description = "param1" } ]

      v<PT.Position> "simple" { x = 10; y = -16 }
      v<OT.oplist> "simple" testOCamlOplist

      // Used by Pusher
      v<LibBackend.Pusher.AddOpEventTooBigPayload> "simple" { tlids = testTLIDs }
      oc<LibBackend.Op.AddOpEventV0> "simple" testAddOpEventV0
      v<LibBackend.Op.AddOpEventV1> "simple" testAddOpEventV1
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
        { ops = testOplist; opCtr = 0; clientOpCtrID = None }
      v<ApiServer.AddOps.V1.T> "simple" testAddOpResultV1
      oc<ApiServer.AddOps.V0.Params>
        "simple"
        { ops = testOCamlOplist; opCtr = 0; clientOpCtrId = None }
      oc<ApiServer.AddOps.V0.T> "simple" testAddOpEventV0


      // DBs

      v<ApiServer.DBs.DBStatsV1.Params> "simple" { tlids = testTLIDs }
      v<ApiServer.DBs.DBStatsV1.T>
        "simple"
        (Map.ofList [ "db1", { count = 0; example = None }
                      "db2", { count = 5; example = Some(testClientDval, "myKey") } ])
      oc<ApiServer.DBs.DBStatsV0.T>
        "simple"
        (Map.ofList [ "db1", { count = 0; example = None }
                      "db2", { count = 5; example = Some(testOCamlDval, "myKey") } ])
      v<ApiServer.DBs.Unlocked.T> "simple" { unlocked_dbs = [ testTLID ] }

      // Execution
      v<ApiServer.Execution.FunctionV1.Params>
        "simple"
        { tlid = testTLID
          trace_id = testUuid
          caller_id = 7UL
          args = [ testClientDval ]
          fnname = "Int::mod_v0" }
      oc<ApiServer.Execution.FunctionV0.Params>
        "simple"
        { tlid = testTLID
          trace_id = testUuid
          caller_id = 7UL
          args = [ testOCamlDval ]
          fnname = "Int::mod_v0" }
      v<ApiServer.Execution.FunctionV1.T>
        "simple"
        { result = testClientDval
          hash = "abcd"
          hashVersion = 0
          touched_tlids = [ testTLID ]
          unlocked_dbs = [ testTLID ] }
      oc<ApiServer.Execution.FunctionV0.T>
        "simple"
        { result = testOCamlDval
          hash = "abcd"
          hashVersion = 0
          touched_tlids = [ testTLID ]
          unlocked_dbs = [ testTLID ] }
      v<ApiServer.Execution.HandlerV1.Params>
        "simple"
        { tlid = testTLID; trace_id = testUuid; input = [ "v", testClientDval ] }
      oc<ApiServer.Execution.HandlerV0.Params>
        "simple"
        { tlid = testTLID; trace_id = testUuid; input = [ "v", testOCamlDval ] }
      v<ApiServer.Execution.HandlerV1.T> "simple" { touched_tlids = [ testTLID ] }

      // F404s
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
                   ``type`` = CT.DType.TInt
                   args = []
                   description = "param description" } ]
             returnType = CT.DType.TList(CT.DType.TInt)
             description = "basic"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Pure
             deprecated = ApiServer.Functions.Deprecation.NotDeprecated
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CT.DType.TInt
             description = "impure"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Impure
             deprecated = ApiServer.Functions.Deprecation.NotDeprecated
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CT.DType.TInt
             description = "impurepreviewable"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.ImpurePreviewable
             deprecated = ApiServer.Functions.Deprecation.NotDeprecated
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable }
           { name = { module_ = "Int"; function_ = "mod"; version = 0 }
             parameters = []
             returnType = CT.DType.TInt
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
             returnType = CT.DType.TInt
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
             returnType = CT.DType.TInt
             description = "deprecatedBecause"
             isInfix = false
             previewable = ApiServer.Functions.Previewable.Pure
             deprecated = ApiServer.Functions.Deprecation.DeprecatedBecause "reason"
             sqlSpec = ApiServer.Functions.SqlSpec.NotQueryable } ])

      // InitialLoad
      v<ApiServer.InitialLoad.V1.T>
        "initial"
        { handlers = testHandlers
          deletedHandlers = testHandlers
          dbs = testDBs
          deletedDBs = testDBs
          userFunctions = testUserFunctions
          deletedUserFunctions = testUserFunctions
          unlockedDBs = [ testTLID ]
          userTypes = testUserTypes
          deletedUserTypes = testUserTypes
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
      oc<ApiServer.InitialLoad.V0.T>
        "initial"
        { toplevels = testOCamlToplevels
          deleted_toplevels = testOCamlToplevels
          user_functions = testOCamlUserFns
          deleted_user_functions = testOCamlUserFns
          unlocked_dbs = [ testTLID ]
          user_tipes = testOCamlUserTipes
          deleted_user_tipes = testOCamlUserTipes
          assets = [ ApiServer.InitialLoad.V0.toApiStaticDeploys testStaticDeploy ]
          op_ctrs = [ testUuid, 7 ]
          canvas_list = [ "test"; "test-canvas2" ]
          org_canvas_list = [ "testorg"; "testorg-canvas2" ]
          permission = Some(LibBackend.Authorization.ReadWrite)
          orgs = [ "test"; "testorg" ]
          account =
            { username = "test"
              name = "Test Name"
              admin = false
              email = "test@darklang.com"
              id = testUuid }
          creation_date = testInstant
          worker_schedules = testWorkerStates
          secrets = [ { secret_name = "test"; secret_value = "secret" } ] }

      // Packages
      v<ApiServer.Packages.ListV1.T>
        "simple"
        [ { name =
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
            tlid = testTLID } ]
      oc<ApiServer.Packages.ListV0.T>
        "simple"
        [ { user = "dark"
            package = "stdlib"
            ``module`` = "Int"
            fnname = "mod"
            version = 0
            body = testOCamlExpr
            parameters =
              [ { name = "param"; tipe = testOCamlTipe; description = "desc" } ]
            return_type = testOCamlTipe
            description = "test"
            author = "test"
            deprecated = false
            tlid = testTLID } ]

      // SecretsV0

      v<ApiServer.Secrets.DeleteV0.Params> "simple" { secret_name = "test" }
      v<ApiServer.Secrets.DeleteV0.T>
        "simple"
        { secrets = [ { secret_name = "test"; secret_value = "secret" } ] }

      v<ApiServer.Secrets.InsertV0.Params>
        "simple"
        { secret_name = "test"; secret_value = "secret" }
      v<ApiServer.Secrets.InsertV0.T>
        "simple"
        { secrets = [ { secret_name = "test"; secret_value = "secret" } ] }


      // SecretsV1

      v<ApiServer.Secrets.DeleteV1.Params> "simple" { name = "test" }
      v<ApiServer.Secrets.DeleteV1.T>
        "simple"
        { secrets = [ { name = "test"; value = "secret" } ] }

      v<ApiServer.Secrets.InsertV1.Params>
        "simple"
        { name = "test"; value = "secret" }
      v<ApiServer.Secrets.InsertV1.T>
        "simple"
        { secrets = [ { name = "test"; value = "secret" } ] }

      // Toplevels

      v<ApiServer.Toplevels.Delete.Params> "simple" { tlid = testTLID }
      v<ApiServer.Toplevels.Delete.T> "simple" { result = "success" }

      // Traces

      v<ApiServer.Traces.AllTraces.T> "simple" { traces = [ (testTLID, testUuid) ] }
      v<ApiServer.Traces.TraceDataV0.Params>
        "simple"
        { tlid = testTLID; trace_id = testUuid }
      v<ApiServer.Traces.TraceDataV0.T>
        "simple"
        { trace =
            (testUuid,
             { input = [ "var", testOCamlDval ]
               timestamp = testInstant
               function_results = [ ("fnName", 7UL, "hash", 0, testOCamlDval) ] }) }
      v<ApiServer.Traces.TraceDataV1.Params>
        "simple"
        { tlid = testTLID; traceID = testUuid }
      v<ApiServer.Traces.TraceDataV1.T>
        "simple"
        { trace =
            (testUuid,
             { input = [ "var", testClientDval ]
               timestamp = testInstant
               functionResults = [ ("fnName", 7UL, "hash", 0, testClientDval) ] }) }


      // Workers

      v<ApiServer.Workers.Scheduler.Params>
        "simple"
        { name = "x"; schedule = "pause" }
      v<ApiServer.Workers.Scheduler.T> "all" testWorkerStates

      v<ApiServer.Workers.WorkerStats.Params> "simple" { tlid = testTLID }
      v<ApiServer.Workers.WorkerStats.T> "simple" { count = 5 }

      // ------------------
      // LibAnalysis
      // ------------------
      v<LibAnalysis.AnalysisResult>
        "simple"
        (Ok(
          testUuid,
          Dictionary.fromList (
            [ (7UL, CTA.ExecutionResult.ExecutedResult testClientDval)
              (7UL, CTA.ExecutionResult.NonExecutedResult testClientDval) ]
          )
        ))
      v<ClientTypes.Analysis.PerformAnalysisParams>
        "handler"
        (ClientTypes.Analysis.AnalyzeHandler
          { handler = testHttpHandler
            traceID = testUuid
            traceData =
              { input = [ "var", testClientDval ]
                timestamp = testInstant
                functionResults = [ ("fnName", 7UL, "hash", 0, testClientDval) ] }
            dbs =
              [ { tlid = testTLID
                  name = "dbname"
                  nameID = 7UL
                  pos = testPos
                  cols =
                    [ { name = Some("colname")
                        nameID = 8UL
                        typ = Some(PT.TInt)
                        typeID = 9UL } ]
                  version = 1 } ]
            userFns = testUserFunctions
            userTypes = testUserTypes
            secrets = [ { name = "z"; value = "y" } ] })
      v<ClientTypes.Analysis.PerformAnalysisParams>
        "function"
        (ClientTypes.Analysis.AnalyzeFunction
          { func = testUserFunction
            traceID = testUuid
            traceData =
              { input = [ "var", testClientDval ]
                timestamp = testInstant
                functionResults = [ ("fnName", 7UL, "hash", 0, testClientDval) ] }
            dbs =
              [ { tlid = testTLID
                  name = "dbname"
                  nameID = 7UL
                  pos = testPos
                  cols =
                    [ { name = Some("colname")
                        nameID = 8UL
                        typ = Some(PT.TInt)
                        typeID = 9UL } ]
                  version = 1 } ]
            userFns = testUserFunctions
            userTypes = testUserTypes
            secrets = [ { name = "z"; value = "y" } ] })


      // ------------------
      // Tests
      // ------------------

      oc<LibExecution.AnalysisTypes.TraceData>
        "testTraceData"
        { input = [ "var", testDval ]
          timestamp = testInstant
          function_results = [ ("fnName", 7UL, "hash", 0, testDval) ] }

    generateTestData ()

  let testNoExtraTestFiles : Test =
    test "test no extra test files" {
      let filenamesFor dict serializerName =
        dict
        |> Dictionary.toList
        |> List.map (fun (typeName, reason) ->
          SampleData.get typeName serializerName reason
          |> List.map (fun (name, _) ->
            SampleData.fileNameFor typeName serializerName name))
        |> List.concat
        |> Set

      let vanillaFilenames = filenamesFor Json.Vanilla.allowedTypes "vanilla"
      let vanillaActual = File.lsPattern Config.Serialization "vanilla-*.json" |> Set
      let vanillaMissingFiles = Set.difference vanillaFilenames vanillaActual
      let vanillaExtraFiles = Set.difference vanillaActual vanillaFilenames
      Expect.equal vanillaMissingFiles Set.empty "missing vanilla files"
      Expect.equal vanillaExtraFiles Set.empty "extra vanilla files"

      let ocamlFilenames = filenamesFor Json.OCamlCompatible.allowedTypes "ocaml"
      let ocamlActual = File.lsPattern Config.Serialization "ocaml-*.json" |> Set
      let ocamlMissingFiles = Set.difference ocamlFilenames ocamlActual
      let ocamlExtraFiles = Set.difference ocamlActual ocamlFilenames
      Expect.equal ocamlMissingFiles Set.empty "missing ocaml files"
      Expect.equal ocamlExtraFiles Set.empty "extra ocaml files"
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
            let filename = SampleData.fileNameFor typeName serializerName name
            let expected = File.readfile Config.Serialization filename
            Expect.equal actualSerializedData expected "matches")
        })
    (testsFor Json.Vanilla.allowedTypes "vanilla")
    @ (testsFor Json.OCamlCompatible.allowedTypes "ocaml")

  let generateTestFiles () : unit =
    let generate (dict : Dictionary.T<string, string>) (serializerName : string) =
      dict
      |> Dictionary.toList
      |> List.iter (fun (typeName, reason) ->
        // For each type, compare the sample data to the file data
        SampleData.get typeName serializerName reason
        |> List.iter (fun (name, serializedData) ->
          let filename = SampleData.fileNameFor typeName serializerName name
          File.writefile Config.Serialization filename serializedData))
    generate Json.Vanilla.allowedTypes "vanilla"
    generate Json.OCamlCompatible.allowedTypes "ocaml"

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
      let output = f.serializer Values.testOplist

      let sha1 =
        System.Security.Cryptography.SHA1.HashData(System.ReadOnlySpan output)
        |> SimpleBase.Base16.LowerCase.Encode

      File.writefileBytes Config.Serialization (nameFor f false sha1) output
      File.writefileBytes Config.Serialization (nameFor f false "latest") output

      f.prettyPrinter
      |> Option.tap (fun s ->
        let jsonData = s Values.testOplist
        File.writefile Config.Serialization (nameFor f true sha1) jsonData
        File.writefile Config.Serialization (nameFor f true "latest") jsonData))


  let testTestFiles =
    formats
    |> List.map (fun f ->
      test "check test files are correct" {
        f.prettyPrinter
        |> Option.tap (fun s ->
          let expected = File.readfile Config.Serialization (nameFor f true "latest")
          let actual = s Values.testOplist
          Expect.equal actual expected "check generates the same json")

        // Check that the generated binary data matches what we have saved. This ensures
        // the format has not changed.
        let actual = f.serializer Values.testOplist
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
            Values.testOplist
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
      GenericSerializersTests.testNoExtraTestFiles
      testList "customer test formats" CustomSerializersTests.testTestFiles
      testList "generic vanilla test formats" GenericSerializersTests.testTestFiles ]
