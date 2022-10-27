/// The test values within this module are used to verify the exact output of our
/// serializers against saved test files. So, we need the test inputs to be
/// consistent, which is why we never use `gid ()` below, or `FSharpToExpr`
/// functions.
[<RequireQualifiedAccess>]
module Tests.SerializationTestValues

open Prelude
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis
module CTApi = ClientTypes.Api
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes
module CT2Ops = ClientTypes2BackendTypes.Ops

module BinarySerialization = LibBinarySerialization.BinarySerialization

let instant = NodaTime.Instant.parse "2022-07-04T17:46:57Z"

let uuid = System.Guid.Parse "31d72f73-0f99-5a9b-949c-b95705ae7c4d"

let tlid : tlid = 7UL
let tlids : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

module RuntimeTypes =
  let dval =
    sampleDvals
    |> List.filter (fun (name, dv) -> name <> "password")
    |> Map
    |> RT.DObj

module ProgramTypes =
  // When updating this, also update `FluidTestData.complexExpr` in the client
  let expr =
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
                                (PT.PBlank 858594159UL, PT.EInteger(135348705UL, 6L))
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

  let pos : PT.Position = { x = 6; y = 6 }

  let handlerIDs : PT.Handler.ids =
    { moduleID = 129952UL; nameID = 33052UL; modifierID = 10038562UL }

  let httpHandler : PT.Handler.T =
    let spec = PT.Handler.HTTP("/path", "GET", handlerIDs)
    { spec = spec; tlid = 92987663UL; ast = expr; pos = pos }

  let httpBasicHandler : PT.Handler.T =
    let spec = PT.Handler.HTTPBasic("/path-bytes", "GET", handlerIDs)
    { spec = spec; tlid = 42280663UL; ast = expr; pos = pos }

  let worker : PT.Handler.T =
    let spec = PT.Handler.Worker("name", handlerIDs)
    { spec = spec; tlid = 19930486UL; ast = expr; pos = pos }

  let oldWorker : PT.Handler.T =
    let spec = PT.Handler.OldWorker("MODULE", "name", handlerIDs)
    { spec = spec; tlid = 10438664321UL; ast = expr; pos = pos }

  let repl : PT.Handler.T =
    let spec = PT.Handler.REPL("name", handlerIDs)
    { spec = spec; tlid = 10395769302UL; ast = expr; pos = pos }

  let cron1 : PT.Handler.T =
    let spec = PT.Handler.Cron("name", None, handlerIDs)
    { spec = spec; tlid = 294906673UL; ast = expr; pos = pos }

  let cron2 : PT.Handler.T =
    let spec = PT.Handler.Cron("name", Some PT.Handler.Every12Hours, handlerIDs)
    { spec = spec; tlid = 199385766UL; ast = expr; pos = pos }

  let unknownHandler : PT.Handler.T =
    let spec = PT.Handler.UnknownHandler("name", "", handlerIDs)
    { spec = spec; tlid = 13633UL; ast = expr; pos = pos }

  let handlersWithName : List<string * PT.Handler.T> =
    [ "Http", httpHandler
      "Worker", worker
      "Cron1", cron1
      "Cron2", cron2
      "REPL", repl
      "Unknown", unknownHandler
      "OldWorker", oldWorker
      "HttpBasic", httpBasicHandler ]

  let handlers = List.map snd handlersWithName

  let dtype =
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

  let dbs : List<PT.DB.T> =
    [ { tlid = 0UL
        pos = pos
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
              typ = Some dtype
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
            typ = Some dtype
            typeID = 239232UL
            description = "param1" } ]
      returnType = dtype
      returnTypeID = 23923423UL
      description = "function description"
      infix = false
      body = expr }

  let userFunctions : List<PT.UserFunction.T> = [ testUserFunction ]

  let userType : PT.UserType.T =
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
                               typ = Some dtype
                               nameID = 0698978UL
                               typeID = 93494534UL } ] }

  let userTypes : List<PT.UserType.T> = [ userType ]

  let packageFn : ClientTypes.Program.Package.Fn =
    { name =
        { owner = "dark"
          package = "stdlib"
          module_ = "Int"
          function_ = "mod"
          version = 0 }
      body = expr |> CT2Program.Expr.toCT
      parameters =
        [ { name = "param"
            typ = dtype |> CT2Program.DType.toCT
            description = "desc" } ]
      returnType = dtype |> CT2Program.DType.toCT
      description = "test"
      author = "test"
      deprecated = false
      tlid = tlid }

  let toplevels : List<PT.Toplevel.T> =
    [ List.map PT.Toplevel.TLHandler handlers
      List.map PT.Toplevel.TLDB dbs
      List.map PT.Toplevel.TLFunction userFunctions
      List.map PT.Toplevel.TLType userTypes ]
    |> List.concat

  let oplist : PT.Oplist =
    let id = 923832423UL
    let tlid = 94934534UL
    [ PT.SetHandler(httpHandler.tlid, pos, httpHandler)
      PT.CreateDB(tlid, pos, "name")
      PT.AddDBCol(tlid, id, id)
      PT.SetDBColName(tlid, id, "name")
      PT.SetDBColType(tlid, id, "int")
      PT.DeleteTL tlid
      PT.MoveTL(tlid, pos)
      PT.SetFunction(testUserFunction)
      PT.ChangeDBColName(tlid, id, "name")
      PT.ChangeDBColType(tlid, id, "int")
      PT.UndoTL tlid
      PT.RedoTL tlid
      PT.SetExpr(tlid, id, expr)
      PT.TLSavepoint tlid
      PT.DeleteFunction tlid
      PT.DeleteDBCol(tlid, id)
      PT.RenameDBname(tlid, "newname")
      PT.CreateDBWithBlankOr(tlid, pos, id, "User")
      PT.SetType(userType)
      PT.DeleteType tlid ]
