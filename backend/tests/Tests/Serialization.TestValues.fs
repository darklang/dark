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

module BinarySerialization = LibBinarySerialization.BinarySerialization

let instant = NodaTime.Instant.parse "2022-07-04T17:46:57Z"

let uuid = System.Guid.Parse "31d72f73-0f99-5a9b-949c-b95705ae7c4d"

let tlid : tlid = 7UL
let tlids : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

module RuntimeTypes =
  let fqFnNames : List<RT.FQFnName.T> =
    [ RT.FQFnName.User "user fn"
      RT.FQFnName.Stdlib { module_ = "a"; function_ = "b"; version = 1 }
      RT.FQFnName.Package
        { owner = "a"; package = "b"; module_ = "c"; function_ = "d"; version = 2 } ]

  let dtypes : List<RT.DType> =
    [ RT.TInt
      RT.TFloat
      RT.TBool
      RT.TNull
      RT.TStr
      RT.TList RT.TInt
      RT.TTuple(RT.TBool, RT.TBool, [ RT.TBool ])
      RT.TDict RT.TBool
      RT.TIncomplete
      RT.TError
      RT.THttpResponse RT.TBool
      RT.TDB RT.TBool
      RT.TErrorRail
      RT.TUserType("test", 1)
      RT.TBytes
      RT.TResult(RT.TBool, RT.TStr)
      RT.TVariable "test"
      RT.TFn([ RT.TBool ], RT.TBool)
      RT.TRecord["prop", RT.TBool] ]

  let matchPatterns : List<RT.MatchPattern> =
    [ RT.MPVariable(123UL, "test")
      RT.MPConstructor(1234UL, "Just", [ RT.MPVariable(746385UL, "var") ])
      RT.MPInteger(756385UL, 7857395)
      RT.MPBool(8759375UL, true)
      RT.MPCharacter(4875843UL, "8jgkdjsfg")
      RT.MPString(857395UL, "iklfijo13294")
      RT.MPBlank(71284374UL)
      RT.MPNull(812394UL)
      RT.MPTuple(
        487129457124UL,
        RT.MPNull(1234124UL),
        RT.MPString(128734857124UL, "1243sdfsadf"),
        [ RT.MPVariable(12748124UL, "var2") ]
      )
      RT.MPFloat(12385781243UL, 79375.847583) ]

  let isInPipes : List<RT.IsInPipe> = [ RT.NotInPipe; RT.InPipe(18274UL) ]

  let sendToRails : List<RT.SendToRail> = [ RT.Rail; RT.NoRail ]

  let exprs : List<RT.Expr> =
    [ RT.EInteger(124151234UL, 7)
      RT.EBool(158584UL, false)
      RT.EString(86749UL, "asdfasedf")
      RT.ECharacter(7683UL, "c")
      RT.EFloat(5495UL, 444.333)
      RT.ENull(59485UL)
      RT.EBlank(495839UL)
      RT.ELet(
        49583UL,
        RT.LPVariable(18274UL, "binding"),
        RT.ENull(12355555UL),
        RT.EVariable(68496UL, "binding")
      )
      RT.EIf(
        8975872314UL,
        RT.ENull(747123UL),
        RT.ENull(747123UL),
        RT.ENull(747123UL)
      )
      RT.ELambda(7587123UL, [ 758123UL, "var3" ], RT.ENull(17384UL))
      RT.EFieldAccess(74875UL, RT.ENull(737463UL), "field")
      RT.EVariable(8737583UL, "var4")
      RT.EApply(
        128384UL,
        RT.ENull(1235123UL),
        [ RT.ENull(7756UL) ],
        RT.NotInPipe,
        RT.Rail
      )
      RT.EFQFnValue(8737481UL, RT.FQFnName.User "sadflkjwerp")
      RT.EList(737481UL, [ RT.ENull(74618UL) ])
      RT.ETuple(
        73847UL,
        RT.ENull(8474UL),
        RT.ENull(84718341UL),
        [ RT.ENull(7167384UL) ]
      )
      RT.ERecord(8167384UL, [ "a9df8", RT.ENull(71631UL) ])
      RT.EConstructor(64617UL, "Just", [ RT.EBlank(8173UL) ])
      RT.EMatch(
        712743UL,
        RT.EInteger(712373UL, 123),
        [ RT.MPVariable(12738UL, "i"), RT.EVariable(1482374UL, "i") ]
      )
      RT.EFeatureFlag(
        1823UL,
        RT.EBool(81273UL, false),
        RT.EString(1283UL, "true"),
        RT.EString(18329472UL, "false")
      )
      RT.EAnd(9375723UL, RT.EBool(83645924UL, true), RT.EBool(385812673UL, false))
      RT.EOr(8375723UL, RT.EBool(83289473UL, true), RT.EBool(383674673UL, false)) ]

  let dvalSources : List<RT.DvalSource> =
    [ RT.SourceNone; RT.SourceID(123UL, 91293UL) ]

  let dvalHttpResponses : List<RT.DHTTP> =
    [ RT.Redirect "http://darklang.io"; RT.Response(8123, [ "a", "b" ], RT.DNull) ]

  let dvals : List<RT.Dval> =
    // TODO: is this exhaustive? I haven't checked.
    sampleDvals
    |> List.filter (fun (name, dv) -> name <> "password")
    |> List.map Tuple2.second

  let dvalFnValImpls : List<RT.FnValImpl> =
    [ RT.Lambda
        { parameters = []
          symtable = [ ("val1", RT.DNull); ("val2", RT.DInt(173648)) ] |> Map.ofList
          body = RT.ENull(1235123UL) } ]

  let dval : RT.Dval =
    sampleDvals
    |> List.filter (fun (name, dv) -> name <> "password")
    |> Map
    |> RT.DObj

module ProgramTypes =
  let fqFnNames : List<PT.FQFnName.T> =
    [ PT.FQFnName.User "test123"
      PT.FQFnName.Stdlib { module_ = "Int"; function_ = "increment"; version = 1 }
      PT.FQFnName.Package
        { owner = "Twilio"
          package = "twilio"
          module_ = "twilio"
          function_ = "sms"
          version = 1 } ]

  let pos : PT.Position = { x = 6; y = 6 }

  let matchPatterns : List<PT.MatchPattern> =
    [ PT.MPVariable(1234123UL, "var8481")
      PT.MPConstructor(7471263UL, "None", [])
      PT.MPInteger(74816UL, 84871728)
      PT.MPBool(66453UL, false)
      PT.MPCharacter(83749178UL, "w")
      PT.MPString(817201237UL, "testing testing 123")
      PT.MPFloat(012037123UL, Positive, "123", "456")
      PT.MPNull(9123871238UL)
      PT.MPBlank(8123818247123UL)
      PT.MPTuple(
        91298UL,
        PT.MPInteger(812831UL, 123),
        PT.MPBool(81871UL, true),
        [ PT.MPNull(17123UL) ]
      ) ]

  let sendToRails : List<PT.SendToRail> = [ PT.Rail; PT.NoRail ]

  // Note: this is aimed to contain all cases of `Expr`
  // When updating this, also update `FluidTestData.complexExpr` in the client
  let expr =
    let e = PT.EInteger(34545UL, 5)
    PT.ELet(
      14219007199254740992UL,
      PT.LPVariable(372643UL, "x1"),
      PT.EInteger(929452387UL, 5L),
      PT.ELet(
        620028536UL,
        PT.LPVariable(5524362UL, "x2"),
        PT.EInteger(452247642UL, 9223372036854775807L),
        PT.ELet(
          68205955UL,
          PT.LPVariable(927492UL, "bool"),
          PT.EBool(43581311UL, true),
          PT.ELet(
            755798860UL,
            PT.LPVariable(4473648UL, "bool"),
            PT.EBool(97054530UL, false),
            PT.ELet(
              244891515UL,
              PT.LPVariable(573659UL, "str"),
              PT.EString(446488682UL, "a string"),
              PT.ELet(
                537517627UL,
                PT.LPVariable(5565675UL, "char"),
                PT.ECharacter(1031176330UL, "a"),
                PT.ELet(
                  399526184UL,
                  PT.LPVariable(7481674UL, "float"),
                  PT.EFloat(770715427UL, Negative, "6", "5"),
                  PT.ELet(
                    975263310UL,
                    PT.LPVariable(23343UL, "n"),
                    PT.ENull 923644248UL,
                    PT.ELet(
                      468988830UL,
                      PT.LPVariable(81749UL, "b"),
                      PT.EBlank 133368677UL,
                      PT.ELet(
                        43886336UL,
                        PT.LPVariable(817381UL, "i"),
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
                            PT.EInfix(
                              94793109UL,
                              PT.InfixFnCall(
                                { module_ = None; function_ = "!=" },
                                PT.NoRail
                              ),
                              PT.EInteger(264400705UL, 5L),
                              PT.EInteger(335743639UL, 6L)
                            ),
                            PT.EInfix(
                              775118986UL,
                              PT.InfixFnCall(
                                { module_ = None; function_ = "+" },
                                PT.NoRail
                              ),
                              PT.EInteger(803876589UL, 5L),
                              PT.EInteger(219131014UL, 2L)
                            ),
                            PT.ELambda(
                              947647446UL,
                              [ (180359194UL, "y") ],
                              PT.EInfix(
                                140609068UL,
                                PT.InfixFnCall(
                                  { module_ = None; function_ = "+" },
                                  PT.NoRail
                                ),
                                PT.EInteger(450951790UL, 2L),
                                PT.EVariable(402203255UL, "y")
                              )
                            )
                          ),
                          PT.EInfix(
                            265463935UL,
                            PT.InfixFnCall(
                              { module_ = None; function_ = "+" },
                              PT.NoRail
                            ),
                            PT.EInfix(
                              312092282UL,
                              PT.InfixFnCall(
                                { module_ = None; function_ = "+" },
                                PT.NoRail
                              ),
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
                              )
                            ),
                            PT.EList(
                              539797095UL,
                              [ PT.EInteger(267797631UL, 5L)
                                PT.EInteger(352138743UL, 6L)
                                PT.EInteger(430871955UL, 7L) ]
                            )
                          )
                        ),
                        PT.ELet(
                          831830073UL,
                          PT.LPVariable(716741UL, "r"),
                          PT.ERecord(
                            109539183UL,
                            [ ("field",
                               PT.EPipe(
                                 786862131UL,
                                 PT.EInteger(555880460UL, 5L),
                                 PT.EInfix(
                                   1021880969UL,
                                   PT.InfixFnCall(
                                     { module_ = None; function_ = "+" },
                                     PT.NoRail
                                   ),
                                   PT.EPipeTarget 936577032UL,
                                   PT.EInteger(962393769UL, 2L)
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
                            PT.LPVariable(81739UL, "m"),
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
                              [ (PT.MPConstructor(
                                  1015986188UL,
                                  "Ok",
                                  [ PT.MPVariable(334386852UL, "x") ]
                                 ),
                                 PT.EVariable(863810169UL, "v"))
                                (PT.MPInteger(928253813UL, 5L),
                                 PT.EInteger(342670561UL, -9223372036854775808L))
                                (PT.MPBool(435227293UL, true),
                                 PT.EInteger(232748650UL, 7L))
                                (PT.MPCharacter(387662539UL, "c"),
                                 PT.ECharacter(657848009UL, "c"))
                                (PT.MPString(491115870UL, "string"),
                                 PT.EString(820329949UL, "string"))
                                (PT.MPNull 701616052UL, PT.ENull 731162955UL)
                                (PT.MPVariable(722099983UL, "var"),
                                 PT.EInfix(
                                   275666765UL,
                                   PT.InfixFnCall(
                                     { module_ = None; function_ = "+" },
                                     PT.NoRail
                                   ),
                                   PT.EInteger(739193732UL, 6L),
                                   PT.EVariable(880556562UL, "var")
                                 ))
                                (PT.MPFloat(409097457UL, Positive, "5", "6"),
                                 PT.EFloat(131187958UL, Positive, "5", "6"))
                                (PT.MPBlank 858594159UL, PT.EInteger(135348705UL, 6L))
                                (PT.MPTuple(
                                  1285610UL,
                                  PT.MPVariable(17823641UL, "a"),
                                  PT.MPVariable(58123641UL, "b"),
                                  [ PT.MPVariable(95723641UL, "c") ]
                                 ),
                                 PT.EBool(123716747UL, true)) ]
                            ),
                            PT.ELet(
                              927055617UL,
                              PT.LPVariable(8175917UL, "f"),
                              PT.EFeatureFlag(
                                882488977UL,
                                "test",
                                PT.EBool(349352147UL, true),
                                PT.EInteger(578528886UL, 5L),
                                PT.EInteger(562930224UL, 6L)
                              ),
                              PT.ELet(
                                6345345UL,
                                PT.LPVariable(192718UL, "partials"),
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
                                  PT.LPVariable(859173UL, "tuples"),
                                  PT.ETuple(72333UL, e, e, [ e ]),
                                  PT.ELet(
                                    47462UL,
                                    PT.LPVariable(9149127UL, "binopAnd"),
                                    PT.EInfix(
                                      234234UL,
                                      PT.BinOp(PT.BinOpAnd),
                                      PT.EBool(234234UL, true),
                                      PT.EBool(234234UL, false)
                                    ),
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
    )

  // Note: This is aimed to contain all cases of `DType`
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

  module Handler =
    let cronIntervals : List<PT.Handler.CronInterval> =
      [ PT.Handler.EveryDay
        PT.Handler.EveryWeek
        PT.Handler.EveryFortnight
        PT.Handler.Every12Hours
        PT.Handler.EveryHour
        PT.Handler.EveryMinute ]

    let ids : PT.Handler.ids =
      { moduleID = 129952UL; nameID = 33052UL; modifierID = 10038562UL }

    module Spec =
      let http = PT.Handler.HTTP("/path", "GET", ids)
      let httpBasic = PT.Handler.HTTPBasic("/path-bytes", "GET", ids)
      let worker = PT.Handler.Worker("name", ids)
      let oldWorker = PT.Handler.OldWorker("MODULE", "name", ids)
      let cronWithoutInterval = PT.Handler.Cron("name", None, ids)

      let cronWithInterval =
        PT.Handler.Cron("name", Some PT.Handler.Every12Hours, ids)

      let repl = PT.Handler.REPL("name", ids)
      let unknown = PT.Handler.UnknownHandler("name", "", ids)

    let specs : List<PT.Handler.Spec> =
      [ Spec.http
        Spec.httpBasic
        Spec.worker
        Spec.oldWorker
        Spec.cronWithoutInterval
        Spec.cronWithInterval
        Spec.repl
        Spec.unknown ]

    let http : PT.Handler.T =
      { spec = Spec.http; tlid = 92987663UL; ast = expr; pos = pos }

    let httpBasic : PT.Handler.T =
      { spec = Spec.httpBasic; tlid = 42280663UL; ast = expr; pos = pos }

    let worker : PT.Handler.T =
      { spec = Spec.worker; tlid = 19930486UL; ast = expr; pos = pos }

    let oldWorker : PT.Handler.T =
      { spec = Spec.oldWorker; tlid = 10438664321UL; ast = expr; pos = pos }

    let repl : PT.Handler.T =
      { spec = Spec.repl; tlid = 10395769302UL; ast = expr; pos = pos }

    let cronWithoutInterval : PT.Handler.T =
      { spec = Spec.cronWithoutInterval; tlid = 294906673UL; ast = expr; pos = pos }

    let cronWithInterval : PT.Handler.T =
      { spec = Spec.cronWithInterval; tlid = 199385766UL; ast = expr; pos = pos }

    let unknown : PT.Handler.T =
      { spec = Spec.unknown; tlid = 13633UL; ast = expr; pos = pos }

    let handlersWithName : List<string * PT.Handler.T> =
      [ "Http", http
        "Worker", worker
        "Cron1", cronWithoutInterval
        "Cron2", cronWithInterval
        "REPL", repl
        "Unknown", unknown
        "OldWorker", oldWorker
        "HttpBasic", httpBasic ]

    let handlers = List.map snd handlersWithName

  let userDB : PT.DB.T =
    { tlid = 0UL
      pos = pos
      nameID = 2399545UL
      name = "User"
      version = 0
      cols =
        [ { name = None; typ = None; nameID = 2949054UL; typeID = 5929202UL }
          { name = None; typ = Some PT.TInt; nameID = 20109857UL; typeID = 299063UL }
          { name = Some "name"
            typ = None
            nameID = 28234232UL
            typeID = 029985336UL }
          { name = Some "value"
            typ = Some dtype
            nameID = 923982352UL
            typeID = 289429232UL } ] }

  let userFunction : PT.UserFunction.T =
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

  let userFunctions : List<PT.UserFunction.T> = [ userFunction ]

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

  let packageFn : PT.Package.Fn =
    { name =
        { owner = "dark"
          package = "stdlib"
          module_ = "Int"
          function_ = "mod"
          version = 0 }
      body = expr
      parameters = [ { name = "param"; typ = dtype; description = "desc" } ]
      returnType = dtype
      description = "test"
      author = "test"
      deprecated = false
      tlid = tlid }

  let toplevels : List<PT.Toplevel.T> =
    [ List.map PT.Toplevel.TLHandler Handler.handlers
      List.map PT.Toplevel.TLDB [ userDB ]
      List.map PT.Toplevel.TLFunction userFunctions
      List.map PT.Toplevel.TLType userTypes ]
    |> List.concat

  let oplist : PT.Oplist =
    let id = 923832423UL
    let tlid = 94934534UL
    [ PT.SetHandler(Handler.http.tlid, pos, Handler.http)
      PT.CreateDB(tlid, pos, "name")
      PT.AddDBCol(tlid, id, id)
      PT.SetDBColName(tlid, id, "name")
      PT.SetDBColType(tlid, id, "int")
      PT.DeleteTL tlid
      PT.MoveTL(tlid, pos)
      PT.SetFunction(userFunction)
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

  let userSecret : PT.Secret.T = { name = "APIKEY"; value = "hunter2" }
