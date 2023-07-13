/// The test values within this module are used to verify the exact output of our
/// serializers against saved test files. So, we need the test inputs to be
/// consistent, which is why we never use `gid ()` below, or `Parser`
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
  let fqFnNames : List<RT.FnName.T> =
    [ RT.FQName.UserProgram
        { modules = [ "X" ]; name = RT.FnName.FnName "userfn"; version = 0 }
      RT.FQName.BuiltIn
        { modules = [ "A" ]; name = RT.FnName.FnName "b"; version = 1 }
      RT.FQName.Package
        { owner = "a"
          modules = NonEmptyList.ofList [ "b"; "C" ]
          name = RT.FnName.FnName "d"
          version = 2 } ]

  let dtypes : List<RT.TypeReference> =
    [ RT.TInt
      RT.TFloat
      RT.TBool
      RT.TUnit
      RT.TString
      RT.TList RT.TInt
      RT.TTuple(RT.TBool, RT.TBool, [ RT.TBool ])
      RT.TDict RT.TBool
      RT.TDB RT.TBool
      RT.TCustomType(
        RT.FQName.UserProgram
          { modules = []; name = RT.TypeName.TypeName "User"; version = 0 },
        [ RT.TBool ]
      )
      RT.TCustomType(
        RT.FQName.BuiltIn
          { modules = [ "Mod" ]; name = RT.TypeName.TypeName "User"; version = 0 },
        [ RT.TBool ]
      )
      RT.TCustomType(
        RT.FQName.Package
          { owner = "dark"
            modules = NonEmptyList.ofList [ "Mod1"; "Mod2" ]
            name = RT.TypeName.TypeName "Pack"
            version = 0 },
        [ RT.TBool ]
      )
      RT.TBytes
      RT.TVariable "test"
      RT.TFn([ RT.TBool ], RT.TBool) ]

  let letPatterns : List<RT.LetPattern> = [ RT.LPVariable(123UL, "test") ]

  let matchPatterns : List<RT.MatchPattern> =
    [ RT.MPVariable(123UL, "test")
      RT.MPEnum(1234UL, "Just", [ RT.MPVariable(746385UL, "var") ])
      RT.MPInt(756385UL, 7857395)
      RT.MPBool(8759375UL, true)
      RT.MPChar(4875843UL, "8jgkdjsfg")
      RT.MPString(857395UL, "iklfijo13294")
      RT.MPUnit(812394UL)
      RT.MPTuple(
        487129457124UL,
        RT.MPUnit(1234124UL),
        RT.MPString(128734857124UL, "1243sdfsadf"),
        [ RT.MPVariable(12748124UL, "var2") ]
      )
      RT.MPFloat(12385781243UL, 79375.847583)
      RT.MPList(12741471UL, [ RT.MPString(185712383UL, "1234") ])
      RT.MPListCons(
        596996239UL,
        RT.MPString(949735651UL, "val1"),
        RT.MPListCons(
          580612639UL,
          RT.MPString(191110189UL, "val2"),
          RT.MPList(448252771UL, [ RT.MPString(98945887UL, "val3") ])
        )
      ) ]

  let exprs : List<RT.Expr> =
    [ RT.EInt(124151234UL, 7)
      RT.EBool(158584UL, false)
      RT.EString(
        86749UL,
        [ RT.StringText "asdfasedf"
          RT.StringInterpolation(RT.EVariable(68496UL, "var")) ]
      )
      RT.EUnit(59485UL)
      RT.ELet(
        49583UL,
        RT.LPVariable(58172UL, "binding"),
        RT.EUnit(12355555UL),
        RT.EVariable(68496UL, "binding")
      )
      RT.EIf(
        8975872314UL,
        RT.EUnit(747123UL),
        RT.EUnit(747123UL),
        RT.EUnit(747123UL)
      )
      RT.ELambda(7587123UL, [ 758123UL, "var3" ], RT.EUnit(17384UL))
      RT.EFieldAccess(74875UL, RT.EUnit(737463UL), "field")
      RT.EVariable(8737583UL, "var4")
      RT.EApply(
        128384UL,
        RT.FnTargetExpr(RT.EUnit(1235123UL)),
        dtypes,
        [ RT.EUnit(7756UL) ]
      )
      RT.EApply(
        128384UL,
        RT.FnTargetName(
          RT.FQName.UserProgram
            { modules = []; name = RT.FnName.FnName "fn"; version = 0 }
        ),
        [],
        [ RT.EUnit(7756UL) ]
      )
      RT.EList(737481UL, [ RT.EUnit(74618UL) ])
      RT.ETuple(
        73847UL,
        RT.EUnit(8474UL),
        RT.EUnit(84718341UL),
        [ RT.EUnit(7167384UL) ]
      )
      RT.ERecord(
        8167384UL,
        RT.FQName.UserProgram(
          { modules = [ "MyModule"; "Name" ]
            name = RT.TypeName.TypeName "NonEmptyList"
            version = 0 }
        ),
        [ "a9df8", RT.EUnit(71631UL) ]
      )
      RT.ERecordUpdate(
        619623640UL,
        RT.EVariable(81036610UL, "myRec"),
        [ ("y", RT.EInt(401690270UL, 2L)) ]
      )
      RT.EEnum(
        64617UL,
        RT.FQName.BuiltIn(
          { modules = []; name = RT.TypeName.TypeName "Option"; version = 0 }
        ),
        "Just",
        [ RT.EUnit(8173UL) ]
      )
      RT.EMatch(
        712743UL,
        RT.EInt(712373UL, 123),
        [ RT.MPVariable(12738UL, "i"), RT.EVariable(1482374UL, "i") ]
      )
      RT.EAnd(9375723UL, RT.EBool(83645924UL, true), RT.EBool(385812673UL, false))
      RT.EOr(8375723UL, RT.EBool(83289473UL, true), RT.EBool(383674673UL, false))
      RT.EEnum(
        8375723UL,
        RT.FQName.UserProgram
          { modules = []; name = RT.TypeName.TypeName "MyEnum"; version = 0 },
        "A",
        [ RT.EUnit(81264012UL) ]
      ) ]

  let dvalSources : List<RT.DvalSource> =
    [ RT.SourceNone; RT.SourceID(123UL, 91293UL) ]

  let dvals : List<RT.Dval> =
    // TODO: is this exhaustive? I haven't checked.
    sampleDvals
    |> List.filter (fun (name, _dv) -> name <> "password")
    |> List.map (fun (name, (dv, t)) -> dv)

  let dval : RT.Dval =
    let typeName =
      RT.FQName.UserProgram
        { modules = []; name = RT.TypeName.TypeName "MyType"; version = 0 }
    sampleDvals
    |> List.filter (fun (name, _dv) -> name <> "password")
    |> List.map (fun (name, (dv, t)) -> name, dv)
    |> RT.Dval.record typeName

module ProgramTypes =
  let fqFnNames : List<PT.FnName.T> =
    [ PT.FQName.UserProgram
        { modules = []; name = PT.FnName.FnName "fn"; version = 0 }
      PT.FQName.BuiltIn
        { modules = [ "Int" ]; name = PT.FnName.FnName "increment"; version = 1 }
      PT.FQName.Package
        { owner = "twilio"
          modules = NonEmptyList.singleton "Twilio"
          name = PT.FnName.FnName "sms"
          version = 1 } ]

  let letPatterns : List<PT.LetPattern> = [ PT.LPVariable(123UL, "test") ]

  let matchPatterns : List<PT.MatchPattern> =
    [ PT.MPVariable(1234123UL, "var8481")
      PT.MPEnum(7471263UL, "Nothing", [])
      PT.MPInt(74816UL, 84871728)
      PT.MPBool(66453UL, false)
      PT.MPChar(83749178UL, "w")
      PT.MPString(817201237UL, "testing testing 123")
      PT.MPFloat(012037123UL, Positive, "123", "456")
      PT.MPUnit(9123871238UL)
      PT.MPTuple(
        91298UL,
        PT.MPInt(812831UL, 123),
        PT.MPBool(81871UL, true),
        [ PT.MPUnit(17123UL) ]
      )
      PT.MPList(57174UL, [ PT.MPInt(58175891783UL, 123) ])
      PT.MPListCons(
        59696239UL,
        PT.MPString(94973551UL, "val1"),
        PT.MPListCons(
          580612639UL,
          PT.MPString(19111089UL, "val2"),
          PT.MPList(44252771UL, [ PT.MPString(989487UL, "val3") ])
        )
      ) ]

  let dtypes : List<PT.TypeReference> =
    [ PT.TInt
      PT.TFloat
      PT.TBool
      PT.TUnit
      PT.TString
      PT.TList PT.TInt
      PT.TTuple(PT.TBool, PT.TBool, [ PT.TBool ])
      PT.TDict PT.TBool
      PT.TDB PT.TBool
      PT.TCustomType(
        PT.FQName.UserProgram
          { modules = [ "Mod" ]; name = PT.TypeName.TypeName "User"; version = 0 },

        [ PT.TBool ]
      )
      PT.TCustomType(
        PT.FQName.BuiltIn
          { modules = [ "Mod" ]; name = PT.TypeName.TypeName "User"; version = 0 },
        [ PT.TBool ]
      )
      PT.TCustomType(
        PT.FQName.Package
          { owner = "dark"
            modules = NonEmptyList.ofList [ "Mod1"; "Mod2" ]
            name = PT.TypeName.TypeName "Pack"
            version = 0 },
        [ PT.TBool ]
      )
      PT.TBytes
      PT.TVariable "test"
      PT.TFn([ PT.TBool ], PT.TBool) ]



  // Note: this is aimed to contain all cases of `Expr`
  let expr =
    let e = PT.EInt(34545UL, 5)
    PT.ELet(
      14219007199254740992UL,
      PT.LPTuple(
        189271UL,
        PT.LPVariable(18274132UL, "x0"),
        PT.LPTuple(
          189272UL,
          PT.LPVariable(18274133UL, "x1"),
          PT.LPVariable(27838183UL, "x2"),
          []
        ),
        [ PT.LPTuple(
            189273UL,
            PT.LPVariable(18274134UL, "x3"),
            PT.LPVariable(27838184UL, "x4"),
            []
          ) ]
      ),
      PT.EInt(929452387UL, 5L),
      PT.ELet(
        620028536UL,
        PT.LPVariable(5812673123UL, "x2"),
        PT.EInt(452247642UL, 9223372036854775807L),
        PT.ELet(
          68205955UL,
          PT.LPVariable(458172UL, "bool"),
          PT.EBool(43581311UL, true),
          PT.ELet(
            755798860UL,
            PT.LPVariable(457123UL, "bool"),
            PT.EBool(97054530UL, false),
            PT.ELet(
              244891515UL,
              PT.LPVariable(1856712UL, "str"),
              PT.EString(
                446488682UL,
                [ PT.StringText "a string"
                  PT.StringInterpolation(PT.EVariable(402203255UL, "var")) ]
              ),
              PT.ELet(
                537517627UL,
                PT.LPVariable(567161UL, "char"),
                PT.EChar(1031176330UL, "a"),
                PT.ELet(
                  399526184UL,
                  PT.LPVariable(578164UL, "float"),
                  PT.EFloat(770715427UL, Negative, "6", "5"),
                  PT.ELet(
                    975263310UL,
                    PT.LPVariable(55573UL, "n"),
                    PT.EUnit 923644248UL,
                    PT.ELet(
                      43886336UL,
                      PT.LPVariable(44124UL, "i"),
                      PT.EIf(
                        46231874UL,
                        PT.EApply(
                          898531080UL,
                          PT.FnTargetName(
                            Ok(
                              PT.FQName.BuiltIn
                                { modules = [ "Int" ]
                                  name = PT.FnName.FnName "toString"
                                  version = 0 }
                            )
                          ),
                          dtypes,
                          [ PT.EInt(160106123UL, 6L) ]
                        ),
                        PT.EIf(
                          729246077UL,
                          PT.EInfix(
                            94793109UL,
                            PT.InfixFnCall(PT.ComparisonNotEquals),
                            PT.EInt(264400705UL, 5L),
                            PT.EInt(335743639UL, 6L)
                          ),
                          PT.EInfix(
                            775118986UL,
                            PT.InfixFnCall(PT.ArithmeticPlus),
                            PT.EInt(803876589UL, 5L),
                            PT.EInt(219131014UL, 2L)
                          ),
                          PT.ELambda(
                            947647446UL,
                            [ (180359194UL, "y") ],
                            PT.EInfix(
                              140609068UL,
                              PT.InfixFnCall(PT.ArithmeticPlus),
                              PT.EInt(450951790UL, 2L),
                              PT.EVariable(402203255UL, "y")
                            )
                          )
                        ),
                        PT.EInfix(
                          265463935UL,
                          PT.InfixFnCall(PT.ArithmeticPlus),
                          PT.EInfix(
                            312092282UL,
                            PT.InfixFnCall(PT.ArithmeticPlus),
                            PT.EFieldAccess(
                              974664608UL,
                              PT.EVariable(1002893266UL, "x"),
                              "y"
                            ),
                            PT.EApply(
                              173079901UL,
                              PT.FnTargetName(
                                Ok(
                                  PT.FQName.BuiltIn
                                    { modules = [ "Int" ]
                                      name = PT.FnName.FnName "add"
                                      version = 0 }
                                )
                              ),
                              [],
                              [ PT.EInt(250221144UL, 6L); PT.EInt(298149318UL, 2L) ]
                            )
                          ),
                          PT.EList(
                            539797095UL,
                            [ PT.EInt(267797631UL, 5L)
                              PT.EInt(352138743UL, 6L)
                              PT.EInt(430871955UL, 7L) ]
                          )
                        )
                      ),
                      PT.ELet(
                        831830073UL,
                        PT.LPVariable(7567123UL, "r"),
                        PT.ERecord(
                          109539183UL,
                          Ok(
                            PT.FQName.UserProgram(
                              { modules = [ "dark"; "stdlib" ]
                                name = PT.TypeName.TypeName "NonEmptyList"
                                version = 0 }
                            )
                          ),
                          [ ("field",
                             PT.EPipe(
                               786862131UL,
                               PT.EInt(555880460UL, 5L),
                               PT.EPipeInfix(
                                 1021880969UL,
                                 PT.InfixFnCall(PT.ArithmeticPlus),
                                 PT.EInt(962393769UL, 2L)
                               ),
                               []
                             ))
                            ("enum",
                             PT.EEnum(
                               567764301UL,
                               Ok(
                                 PT.FQName.BuiltIn(
                                   { modules = []
                                     name = PT.TypeName.TypeName "Result"
                                     version = 0 }
                                 )
                               ),
                               "Ok",
                               [ PT.EEnum(
                                   646107057UL,
                                   Ok(
                                     PT.FQName.Package(
                                       { owner = "Darklang"
                                         modules =
                                           NonEmptyList.ofList [ "Stdlib"; "Result" ]
                                         name = PT.TypeName.TypeName "Result"
                                         version = 0 }
                                     )
                                   ),
                                   "Error",
                                   [ PT.EEnum(
                                       689802831UL,
                                       Ok(
                                         PT.FQName.BuiltIn(
                                           { modules = []
                                             name = PT.TypeName.TypeName "Option"
                                             version = 0 }
                                         )
                                       ),
                                       "Just",
                                       [ PT.EEnum(
                                           957916875UL,
                                           Ok(
                                             PT.FQName.BuiltIn(
                                               { modules = []
                                                 name = PT.TypeName.TypeName "Option"
                                                 version = 0 }
                                             )
                                           ),
                                           "Nothing",
                                           []
                                         ) ]
                                     ) ]
                                 ) ]
                             )) ]
                        ),
                        PT.ELet(
                          1039625813UL,
                          PT.LPVariable(169324087UL, "updatedR"),
                          PT.ERecordUpdate(
                            619623640UL,
                            PT.EVariable(81036610UL, "r"),
                            [ ("field", PT.EInt(401690270UL, 42L)) ]
                          ),
                          PT.ELet(
                            745304029UL,
                            PT.LPVariable(17461UL, "m"),
                            PT.EMatch(
                              889712088UL,
                              PT.EApply(
                                203239466UL,
                                PT.FnTargetName(
                                  Ok(
                                    PT.FQName.BuiltIn
                                      { modules = [ "Mod" ]
                                        name = PT.FnName.FnName "function"
                                        version = 2 }
                                  )
                                ),
                                [],
                                []
                              ),
                              [ (PT.MPEnum(
                                  1015986188UL,
                                  "Ok",
                                  [ PT.MPVariable(334386852UL, "x") ]
                                 ),
                                 PT.EVariable(863810169UL, "v"))
                                (PT.MPInt(928253813UL, 5L),
                                 PT.EInt(342670561UL, -9223372036854775808L))
                                (PT.MPBool(435227293UL, true),
                                 PT.EInt(232748650UL, 7L))
                                (PT.MPChar(387662539UL, "c"),
                                 PT.EChar(657848009UL, "c"))
                                (PT.MPList(
                                  387662539UL,
                                  [ PT.MPBool(435227293UL, true) ]
                                 ),
                                 PT.EList(
                                   657848009UL,
                                   [ PT.EBool(435227293UL, true) ]
                                 ))
                                (PT.MPListCons(
                                  596996239UL,
                                  PT.MPString(949735651UL, "val1"),
                                  PT.MPListCons(
                                    580612639UL,
                                    PT.MPString(191110189UL, "val2"),
                                    PT.MPList(
                                      448252771UL,
                                      [ PT.MPString(98945887UL, "val3") ]
                                    )
                                  )
                                 ),
                                 PT.EList(
                                   657848009UL,
                                   [ PT.EBool(435227293UL, true) ]
                                 ))
                                (PT.MPString(491115870UL, "string"),
                                 PT.EString(
                                   820329949UL,
                                   [ PT.StringText "string"
                                     PT.StringInterpolation(
                                       PT.EVariable(1002893266UL, "var")
                                     ) ]
                                 ))
                                (PT.MPUnit 701616052UL, PT.EUnit 731162955UL)
                                (PT.MPVariable(722099983UL, "var"),
                                 PT.EInfix(
                                   275666765UL,
                                   PT.InfixFnCall(PT.ArithmeticPlus),
                                   PT.EInt(739193732UL, 6L),
                                   PT.EVariable(880556562UL, "var")
                                 ))
                                (PT.MPFloat(409097457UL, Positive, "5", "6"),
                                 PT.EFloat(131187958UL, Positive, "5", "6"))
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
                              PT.LPVariable(1782674UL, "f"),
                              PT.EIf(
                                882488977UL,
                                PT.EBool(349352147UL, true),
                                PT.EInt(578528886UL, 5L),
                                PT.EInt(562930224UL, 6L)
                              ),
                              PT.ELet(
                                6345345UL,
                                PT.LPVariable(17274UL, "partials"),
                                PT.EList(23423423UL, []),
                                PT.ELet(
                                  883434UL,
                                  PT.LPVariable(12894671UL, "tuples"),
                                  PT.ETuple(72333UL, e, e, [ e ]),
                                  PT.ELet(
                                    47462UL,
                                    PT.LPVariable(123471UL, "binopAnd"),
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

  // Note: This is aimed to contain all cases of `TypeReference`
  let dtype =
    PT.TTuple(
      PT.TList(PT.TDict(PT.TDB(PT.TFn([ PT.TFloat ], PT.TUnit)))),
      PT.TInt,
      [ PT.TFloat
        PT.TBool
        PT.TUnit
        PT.TString
        PT.TList(PT.TInt)
        PT.TTuple(PT.TInt, PT.TString, [])
        PT.TDict(PT.TInt)
        PT.TDB(PT.TInt)
        PT.TDateTime
        PT.TChar
        PT.TPassword
        PT.TUuid
        PT.TCustomType(
          PT.FQName.UserProgram
            { modules = [ "Mod" ]; name = PT.TypeName.TypeName "name"; version = 0 },
          []
        )
        PT.TBytes
        PT.TVariable "v"
        PT.TFn([ PT.TInt ], PT.TInt) ]
    )

  module Handler =
    let cronIntervals : List<PT.Handler.CronInterval> =
      [ PT.Handler.EveryDay
        PT.Handler.EveryWeek
        PT.Handler.EveryFortnight
        PT.Handler.Every12Hours
        PT.Handler.EveryHour
        PT.Handler.EveryMinute ]

    module Spec =
      let http = PT.Handler.HTTP("/path-bytes", "GET")
      let worker = PT.Handler.Worker("name")

      let cron = PT.Handler.Cron("name", PT.Handler.Every12Hours)

      let repl = PT.Handler.REPL("name")

    let specs : List<PT.Handler.Spec> =
      [ Spec.http; Spec.worker; Spec.cron; Spec.repl ]

    let http : PT.Handler.T = { spec = Spec.http; tlid = 42280663UL; ast = expr }

    let worker : PT.Handler.T = { spec = Spec.worker; tlid = 19930486UL; ast = expr }

    let repl : PT.Handler.T = { spec = Spec.repl; tlid = 10395769302UL; ast = expr }

    let cron : PT.Handler.T = { spec = Spec.cron; tlid = 199385766UL; ast = expr }

    let handlersWithName : List<string * PT.Handler.T> =
      [ "Worker", worker; "Cron", cron; "REPL", repl; "Http", http ]

    let handlers = List.map snd handlersWithName

  let userDB : PT.DB.T =
    { tlid = 0UL
      name = "User"
      version = 0
      typ =
        PT.TCustomType(
          PT.FQName.UserProgram
            { modules = []; name = PT.TypeName.TypeName "User"; version = 0 },
          []
        ) }

  let userFunction : PT.UserFunction.T =
    { tlid = 0UL
      name = { modules = []; name = PT.FnName.FnName "User"; version = 0 }
      typeParams = [ "a" ]
      parameters = [ { name = "myparam1"; typ = dtype; description = "param1" } ]
      returnType = dtype
      description = "function description"
      deprecated = PT.DeprecatedBecause "some reason"
      body = expr }

  let userFunctions : List<PT.UserFunction.T> = [ userFunction ]

  let userRecordType : PT.UserType.T =
    { tlid = 0UL
      name = { modules = []; name = PT.TypeName.TypeName "User"; version = 0 }
      description = "test description"
      deprecated = PT.NotDeprecated
      declaration =
        { typeParams = [ "a" ]
          definition =
            let firstField : PT.TypeDeclaration.RecordField =
              { name = "prop1"; typ = dtype; description = "desc1" }
            PT.TypeDeclaration.Record(firstField, []) } }

  let userEnumType : PT.UserType.T =
    { tlid = 0UL
      name = { modules = []; name = PT.TypeName.TypeName "User"; version = 0 }
      deprecated = PT.NotDeprecated
      description = "test description"
      declaration =
        { typeParams = [ "a" ]
          definition =
            PT.TypeDeclaration.Enum(
              { name = "caseA"; fields = []; description = "" },
              [ { name = "caseB"
                  fields = [ { typ = dtype; label = Some "i"; description = "" } ]
                  description = "" } ]
            ) } }

  let userTypes : List<PT.UserType.T> = [ userRecordType; userEnumType ]


  // TODO: serialize stdlib types?
  // (also make sure we roundtrip test them)

  let packageFn : PT.PackageFn.T =
    { name =
        { owner = "dark"
          modules = NonEmptyList.ofList [ "stdlib"; "Int"; "Int64" ]
          name = PT.FnName.FnName "mod"
          version = 0 }
      body = expr
      typeParams = [ "a" ]
      parameters = [ { name = "param"; typ = dtype; description = "desc" } ]
      returnType = dtype
      description = "test"
      deprecated = PT.NotDeprecated
      id = uuid
      tlid = tlid }

  let packageType : PT.PackageType.T =
    { name =
        { owner = "darklang"
          modules = NonEmptyList.ofList [ "stdlib"; "Int"; "Int64" ]
          name = PT.TypeName.TypeName "T"
          version = 0 }
      declaration =
        { typeParams = [ "a" ]
          definition =
            PT.TypeDeclaration.Enum(
              { name = "caseA"; fields = []; description = "" },
              [ { name = "caseB"
                  fields = [ { typ = dtype; label = Some "i"; description = "" } ]
                  description = "" } ]
            ) }
      id = uuid
      description = "test"
      deprecated = PT.NotDeprecated
      tlid = tlid }

  let toplevels : List<PT.Toplevel.T> =
    [ List.map PT.Toplevel.TLHandler Handler.handlers
      List.map PT.Toplevel.TLDB [ userDB ]
      List.map PT.Toplevel.TLFunction userFunctions
      List.map PT.Toplevel.TLType userTypes ]
    |> List.concat

  let userSecret : PT.Secret.T = { name = "APIKEY"; value = "hunter2"; version = 0 }
