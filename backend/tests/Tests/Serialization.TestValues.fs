/// The test values within this module are used to verify the exact output of our
/// serializers against saved test files. So, we need the test inputs to be
/// consistent, which is why we never use `gid ()` below, or `Parser`
/// functions.
[<RequireQualifiedAccess>]
module Tests.SerializationTestValues

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module Dval = LibExecution.Dval
module RT = LibExecution.RuntimeTypes

module BinarySerialization = LibBinarySerialization.BinarySerialization

let instant = NodaTime.Instant.parse "2022-07-04T17:46:57Z"

let uuid = System.Guid.Parse "31d72f73-0f99-5a9b-949c-b95705ae7c4d"

let tlid : tlid = 777777928475UL
let tlids : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

module RuntimeTypes =
  let fqTypeNames : List<RT.FQTypeName.FQTypeName> =
    [ RT.FQTypeName.Package { owner = "a"; modules = [ "b"; "C" ]; name = "d" } ]

  let fqFnNames : List<RT.FQFnName.FQFnName> =
    [ RT.FQFnName.Builtin { name = "aB"; version = 1 }
      RT.FQFnName.Package { owner = "a"; modules = [ "b"; "C" ]; name = "d" } ]

  let fqConstantNames : List<RT.FQConstantName.FQConstantName> =
    [ RT.FQConstantName.Builtin { name = "aB"; version = 1 }
      RT.FQConstantName.Package { owner = "a"; modules = [ "b"; "C" ]; name = "d" } ]

  let typeReferences : List<RT.TypeReference> =
    [ RT.TInt64
      RT.TUInt64
      RT.TInt8
      RT.TUInt8
      RT.TInt16
      RT.TUInt16
      RT.TInt32
      RT.TUInt32
      RT.TInt128
      RT.TUInt128
      RT.TFloat
      RT.TBool
      RT.TUnit
      RT.TString
      RT.TList RT.TInt64
      RT.TTuple(RT.TBool, RT.TBool, [ RT.TBool ])
      RT.TDict RT.TBool
      RT.TDB RT.TBool
      RT.TCustomType(
        Ok(RT.FQTypeName.Package { owner = "test"; modules = []; name = "User" }),
        [ RT.TBool ]
      )
      RT.TCustomType(
        Ok(
          RT.FQTypeName.Package
            { owner = "dark"; modules = [ "Mod1"; "Mod2" ]; name = "Pack" }
        ),
        [ RT.TBool ]
      )
      RT.TVariable "test"
      RT.TFn(NEList.singleton RT.TBool, RT.TBool) ]

  let letPatterns : List<RT.LetPattern> =
    [ RT.LPVariable(123UL, "test")
      RT.LPUnit(12345UL)
      RT.LPTuple(948UL, RT.LPUnit 234UL, RT.LPUnit 8473UL, []) ]

  let matchPatterns : List<RT.MatchPattern> =
    [ RT.MPVariable(123UL, "test")
      RT.MPEnum(1234UL, "Some", [ RT.MPVariable(746385UL, "var") ])
      RT.MPInt64(756385UL, 7857395)
      RT.MPUInt64(756386UL, 756386UL)
      RT.MPInt8(756386UL, 127y)
      RT.MPUInt8(756387UL, 255uy)
      RT.MPInt16(756388UL, 32767s)
      RT.MPUInt16(756389UL, 65535us)
      RT.MPInt32(756389UL, 2147483647l)
      RT.MPUInt32(756389UL, 4294967295ul)
      RT.MPInt128(756390UL, 170141183460Q)
      RT.MPUInt128(756391UL, 340282366920938Z)
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
    [ RT.EInt64(124151234UL, 7L)
      RT.EUInt64(124151235UL, 7UL)
      RT.EInt8(124151235UL, 7y)
      RT.EUInt8(124151236UL, 7uy)
      RT.EInt16(124151237UL, 7s)
      RT.EUInt16(124151238UL, 7us)
      RT.EInt32(124151239UL, 7l)
      RT.EUInt32(124151238UL, 7ul)
      RT.EInt128(124151239UL, 7Q)
      RT.EUInt128(124151238UL, 7Z)
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
        Some(RT.EUnit(747123UL))
      )
      RT.EIf(747843UL, RT.EUnit(749549UL), RT.EUnit(6739745UL), None)
      RT.ELambda(
        7587123UL,
        NEList.singleton (RT.LPVariable(758123UL, "var3")),
        RT.EUnit(17384UL)
      )
      RT.ELambda(
        7587124UL,
        NEList.singleton (
          RT.LPTuple(
            758123UL,
            RT.LPVariable(758133UL, "var11"),
            RT.LPVariable(758433UL, "var12"),
            []
          )
        ),
        RT.EUnit(17384UL)
      )
      RT.EFieldAccess(74875UL, RT.EUnit(737463UL), "field")
      RT.EVariable(8737583UL, "var4")
      RT.EApply(
        128384UL,
        RT.EUnit(1235123UL),
        typeReferences,
        NEList.singleton (RT.EUnit(7756UL))
      )
      RT.EApply(
        128384UL,
        RT.EFnName(
          2236UL,
          RT.FQFnName.Package { owner = "test"; modules = []; name = "fn" }
        ),
        [],
        (NEList.singleton (RT.EUnit(7756UL)))
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
        RT.FQTypeName.Package(
          { owner = "owner"
            modules = [ "MyModule"; "Name" ]
            name = "NonEmptyList" }
        ),
        NEList.singleton ("a9df8", RT.EUnit(71631UL))
      )
      RT.ERecordUpdate(
        619623640UL,
        RT.EVariable(81036610UL, "myRec"),
        NEList.singleton ("y", RT.EInt64(401690270UL, 2L))
      )
      RT.EMatch(
        712743UL,
        RT.EInt64(712373UL, 123),
        NEList.singleton
          { pat = RT.MPVariable(12738UL, "i")
            whenCondition = None
            rhs = RT.EVariable(1482374UL, "i") }
      )
      RT.EAnd(9375723UL, RT.EBool(83645924UL, true), RT.EBool(385812673UL, false))
      RT.EOr(8375723UL, RT.EBool(83289473UL, true), RT.EBool(383674673UL, false))
      RT.EEnum(
        8375723UL,
        RT.FQTypeName.Package { owner = "owner"; modules = []; name = "MyEnum" },
        "A",
        [ RT.EUnit(81264012UL) ]
      ) ]


  let valueTypes : List<RT.ValueType> =
    let known kt = RT.ValueType.Known kt
    let ktUnit = known RT.KnownType.KTUnit

    [ RT.ValueType.Unknown
      ktUnit
      known RT.KnownType.KTBool
      known RT.KnownType.KTInt64
      known RT.KnownType.KTUInt64
      known RT.KnownType.KTInt8
      known RT.KnownType.KTUInt8
      known RT.KnownType.KTInt16
      known RT.KnownType.KTUInt16
      known RT.KnownType.KTInt32
      known RT.KnownType.KTUInt32
      known RT.KnownType.KTInt128
      known RT.KnownType.KTUInt128
      known RT.KnownType.KTFloat
      known RT.KnownType.KTChar
      known RT.KnownType.KTString
      known RT.KnownType.KTUuid
      known RT.KnownType.KTDateTime
      known (RT.KnownType.KTList ktUnit)
      known (RT.KnownType.KTTuple(ktUnit, ktUnit, []))
      known (RT.KnownType.KTFn(NEList.singleton ktUnit, ktUnit))
      known (RT.KnownType.KTDB ktUnit)
      known (RT.KnownType.KTDict ktUnit) ]

  let dvals : List<RT.Dval> =
    // TODO: is this exhaustive? I haven't checked.
    sampleDvals |> List.map (fun (_, (dv, _)) -> dv)

  let dval : RT.Dval =
    let typeName =
      RT.FQTypeName.Package { owner = "owner"; modules = []; name = "MyType" }
    sampleDvals
    |> List.map (fun (name, (dv, _)) -> name, dv)
    |> fun fields -> RT.DRecord(typeName, typeName, [], Map fields)


module ProgramTypes =

  let signs = [ Sign.Positive; Sign.Negative ]

  let fqFnNames : List<PT.FQFnName.FQFnName> =
    [ PT.FQFnName.Builtin { name = "int64Increment"; version = 1 }
      PT.FQFnName.Package { owner = "twilio"; modules = [ "Twilio" ]; name = "sms" } ]


  let letPatterns : List<PT.LetPattern> =
    [ PT.LPVariable(123UL, "test")
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
      ) ]


  let matchPatterns : List<PT.MatchPattern> =
    [ PT.MPVariable(1234123UL, "var8481")
      PT.MPEnum(7471263UL, "None", [])
      PT.MPInt64(74816UL, 84871728L)
      PT.MPUInt64(74817UL, 84871728UL)
      PT.MPInt8(74817UL, 127y)
      PT.MPUInt8(74818UL, 255uy)
      PT.MPInt16(74819UL, 32767s)
      PT.MPUInt16(74820UL, 65535us)
      PT.MPInt32(74821UL, 2147483647l)
      PT.MPUInt32(74822UL, 4294967295ul)
      PT.MPInt128(74821UL, 170141183460469231731687303715884105727Q)
      PT.MPUInt128(74822UL, 340282366920938463463374607431768211455Z)
      PT.MPBool(66453UL, false)
      PT.MPChar(83749178UL, "w")
      PT.MPString(817201237UL, "testing testing 123")
      PT.MPFloat(012037123UL, Positive, "123", "456")
      PT.MPUnit(9123871238UL)
      PT.MPTuple(
        91298UL,
        PT.MPInt64(812831UL, 123),
        PT.MPBool(81871UL, true),
        [ PT.MPUnit(17123UL) ]
      )
      PT.MPList(57174UL, [ PT.MPInt64(58175891783UL, 123) ])
      PT.MPListCons(
        59696239UL,
        PT.MPString(94973551UL, "val1"),
        PT.MPListCons(
          580612639UL,
          PT.MPString(19111089UL, "val2"),
          PT.MPList(44252771UL, [ PT.MPString(989487UL, "val3") ])
        )
      ) ]


  // Note: This is aimed to contain all cases of `TypeReference`
  let typeReference : PT.TypeReference =
    PT.TTuple(
      PT.TInt64,
      PT.TFloat,
      [ PT.TBool
        PT.TUnit
        PT.TUInt64
        PT.TInt8
        PT.TUInt8
        PT.TInt16
        PT.TUInt16
        PT.TInt32
        PT.TUInt32
        PT.TInt128
        PT.TUInt128
        PT.TString
        PT.TList PT.TInt64
        PT.TTuple(PT.TBool, PT.TBool, [ PT.TBool ])
        PT.TDict PT.TBool
        PT.TDB PT.TBool
        PT.TCustomType(
          Ok(
            PT.FQTypeName.Package
              { owner = "owner"; modules = [ "Mod" ]; name = "User" }
          ),
          [ PT.TBool ]
        )
        PT.TCustomType(
          Ok(
            PT.FQTypeName.Package
              { owner = "dark"; modules = [ "Mod1"; "Mod2" ]; name = "Pack" }
          ),
          [ PT.TBool ]
        )
        PT.TVariable "test"
        PT.TFn(NEList.singleton PT.TBool, PT.TBool) ]
    )



  // Note: this is aimed to contain all cases of `Expr`
  let expr =
    let e = PT.EInt64(34545UL, 5)
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
      PT.EInt64(929452387UL, 5L),
      PT.ELet(
        620028536UL,
        PT.LPVariable(5812673123UL, "x2"),
        PT.EInt64(452247642UL, 9223372036854775807L),
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
                          PT.EFnName(
                            63953UL,
                            Ok(
                              PT.FQFnName.Builtin
                                { name = "int64ToString"; version = 0 }
                            )
                          ),
                          [ typeReference ],
                          NEList.singleton (PT.EInt64(160106123UL, 6L))
                        ),
                        PT.EIf(
                          729246077UL,
                          PT.EInfix(
                            94793109UL,
                            PT.InfixFnCall(PT.ComparisonNotEquals),
                            PT.EInt64(264400705UL, 5L),
                            PT.EInt64(335743639UL, 6L)
                          ),
                          PT.EInfix(
                            775118986UL,
                            PT.InfixFnCall(PT.ArithmeticPlus),
                            PT.EInt64(803876589UL, 5L),
                            PT.EInt64(219131014UL, 2L)
                          ),
                          Some(
                            PT.ELambda(
                              947647446UL,
                              NEList.singleton (PT.LPVariable(180359194UL, "y")),
                              PT.EInfix(
                                140609068UL,
                                PT.InfixFnCall(PT.ArithmeticPlus),
                                PT.EInt64(450951790UL, 2L),
                                PT.EVariable(402203255UL, "y")
                              )
                            )
                          )
                        ),
                        Some(
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
                                PT.EFnName(
                                  638434UL,
                                  Ok(
                                    PT.FQFnName.Builtin
                                      { name = "int64Add"; version = 0 }
                                  )
                                ),
                                [],
                                NEList.doubleton
                                  (PT.EInt64(250221144UL, 6L))
                                  (PT.EInt64(298149318UL, 2L))
                              )
                            ),
                            PT.EList(
                              539797095UL,
                              [ PT.EInt64(267797631UL, 5L)
                                PT.EInt64(352138743UL, 6L)
                                PT.EInt64(430871955UL, 7L) ]
                            )
                          )
                        )
                      ),
                      PT.ELet(
                        831830073UL,
                        PT.LPVariable(7567123UL, "r"),
                        PT.ERecord(
                          109539183UL,
                          Ok(
                            PT.FQTypeName.Package(
                              { owner = "dark"
                                modules = [ "stdlib" ]
                                name = "NonEmptyList" }
                            )
                          ),
                          [ ("field",
                             PT.EPipe(
                               786862131UL,
                               PT.EInt64(555880460UL, 5L),
                               [ PT.EPipeVariable(
                                   1021880969UL,
                                   "fn",
                                   [ PT.EVariable(962393769UL, "x") ]
                                 )
                                 PT.EPipeLambda(
                                   1021880969UL,
                                   NEList.singleton (PT.LPVariable(180359194UL, "y")),
                                   PT.EInfix(
                                     140609068UL,
                                     PT.InfixFnCall(PT.ArithmeticPlus),
                                     PT.EInt64(450951790UL, 2L),
                                     PT.EVariable(402203255UL, "y")
                                   )
                                 )
                                 PT.EPipeInfix(
                                   1021880969UL,
                                   PT.InfixFnCall(PT.ArithmeticPlus),
                                   PT.EInt64(962393769UL, 2L)
                                 )
                                 PT.EPipeFnCall(
                                   1021880969UL,
                                   Ok(
                                     PT.FQFnName.Builtin
                                       { name = "int64Add"; version = 0 }
                                   ),
                                   [],
                                   [ (PT.EInt64(250221144UL, 6L))
                                     (PT.EInt64(298149318UL, 2L)) ]
                                 ) ]
                             ))
                            ("enum",
                             PT.EEnum(
                               646107057UL,
                               Ok(
                                 PT.FQTypeName.Package(
                                   { owner = "Darklang"
                                     modules = [ "Stdlib"; "Result" ]
                                     name = "Result" }
                                 )
                               ),
                               "Error",
                               []
                             )) ]
                        ),
                        PT.ELet(
                          1039625813UL,
                          PT.LPVariable(169324087UL, "updatedR"),
                          PT.ERecordUpdate(
                            619623640UL,
                            PT.EVariable(81036610UL, "r"),
                            NEList.singleton ("field", PT.EInt64(401690270UL, 42L))
                          ),
                          PT.ELet(
                            745304029UL,
                            PT.LPVariable(17461UL, "m"),
                            PT.EMatch(
                              889712088UL,
                              PT.EApply(
                                203239466UL,
                                PT.EFnName(
                                  342954UL,
                                  Ok(
                                    PT.FQFnName.Builtin
                                      { name = "modFunction"; version = 2 }
                                  )
                                ),
                                [],
                                (NEList.singleton (PT.EInt64(1015986188UL, 5L)))
                              ),
                              [ { pat =
                                    PT.MPEnum(
                                      1015986188UL,
                                      "Ok",
                                      [ PT.MPVariable(334386852UL, "x") ]
                                    )
                                  whenCondition = None
                                  rhs = PT.EVariable(863810169UL, "v") }
                                { pat = PT.MPInt64(928253813UL, 5L)
                                  whenCondition = None
                                  rhs = PT.EInt64(342670561UL, -9223372036854775808L) }
                                { pat = PT.MPBool(435227293UL, true)
                                  whenCondition = None
                                  rhs = PT.EInt64(232748650UL, 7L) }
                                { pat = PT.MPChar(387662539UL, "c")
                                  whenCondition = None
                                  rhs = PT.EChar(657848009UL, "c") }
                                { pat =
                                    PT.MPList(
                                      387662539UL,
                                      [ PT.MPBool(435227293UL, true) ]
                                    )
                                  whenCondition = None
                                  rhs =
                                    PT.EList(
                                      657848009UL,
                                      [ PT.EBool(435227293UL, true) ]
                                    ) }
                                { pat =
                                    PT.MPListCons(
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
                                    )
                                  whenCondition = None
                                  rhs =
                                    PT.EList(
                                      657848009UL,
                                      [ PT.EBool(435227293UL, true) ]
                                    ) }
                                { pat = PT.MPString(491115870UL, "string")
                                  whenCondition = None
                                  rhs =
                                    PT.EString(
                                      820329949UL,
                                      [ PT.StringText "string"
                                        PT.StringInterpolation(
                                          PT.EVariable(1002893266UL, "var")
                                        ) ]
                                    ) }
                                { pat = PT.MPUnit 701616052UL
                                  whenCondition = None
                                  rhs = PT.EUnit 731162955UL }
                                { pat = PT.MPVariable(722099983UL, "var")
                                  whenCondition = None
                                  rhs =
                                    PT.EInfix(
                                      275666765UL,
                                      PT.InfixFnCall(PT.ArithmeticPlus),
                                      PT.EInt64(739193732UL, 6L),
                                      PT.EVariable(880556562UL, "var")
                                    ) }
                                { pat = PT.MPFloat(409097457UL, Positive, "5", "6")
                                  whenCondition = None
                                  rhs = PT.EFloat(131187958UL, Positive, "5", "6") }
                                { pat =
                                    PT.MPTuple(
                                      1285610UL,
                                      PT.MPVariable(17823641UL, "a"),
                                      PT.MPVariable(58123641UL, "b"),
                                      [ PT.MPVariable(95723641UL, "c") ]
                                    )
                                  whenCondition = None
                                  rhs = PT.EBool(123716747UL, true) }
                                { pat =
                                    PT.MPTuple(
                                      1285610UL,
                                      PT.MPVariable(17823641UL, "a"),
                                      PT.MPVariable(58123641UL, "b"),
                                      [ PT.MPVariable(95723641UL, "c") ]
                                    )
                                  whenCondition = Some(PT.EBool(123716747UL, true))
                                  rhs = PT.EBool(123716747UL, true) } ]
                            ),
                            PT.ELet(
                              927055617UL,
                              PT.LPVariable(1782674UL, "f"),
                              PT.EIf(
                                882488977UL,
                                PT.EBool(349352147UL, true),
                                PT.EInt64(578528886UL, 5L),
                                Some(PT.EInt64(562930224UL, 6L))
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
                                    PT.ELet(
                                      831830073UL,
                                      PT.LPVariable(7567123UL, "dict"),
                                      PT.EDict(
                                        109539183UL,
                                        [ ("a string", PT.EInt64(450951790UL, 2L)) ]
                                      ),
                                      PT.ELet(
                                        831830074UL,
                                        PT.LPVariable(7567124UL, "int8"),
                                        PT.EInt8(7567124UL, 127y),
                                        PT.ELet(
                                          831830075UL,
                                          PT.LPVariable(7567125UL, "uint8"),
                                          PT.EUInt8(7567125UL, 255uy),
                                          PT.ELet(
                                            831830076UL,
                                            PT.LPVariable(7567126UL, "int16"),
                                            PT.EInt16(7567126UL, 32767s),
                                            PT.ELet(
                                              831830077UL,
                                              PT.LPVariable(7567127UL, "uint16"),
                                              PT.EUInt16(7567128UL, 65535us),
                                              PT.ELet(
                                                831830078UL,
                                                PT.LPVariable(7567128UL, "int32"),
                                                PT.EInt32(7567128UL, 2147483647l),
                                                PT.ELet(
                                                  831830079UL,
                                                  PT.LPVariable(7567129UL, "uint32"),
                                                  PT.EUInt32(7567129UL, 4294967295ul),
                                                  PT.ELet(
                                                    831830080UL,
                                                    PT.LPVariable(
                                                      7567130UL,
                                                      "int128"
                                                    ),
                                                    PT.EInt128(
                                                      7567130UL,
                                                      170141183460469231731687303715884105727Q
                                                    ),
                                                    PT.ELet(
                                                      831830081UL,
                                                      PT.LPVariable(
                                                        756730UL,
                                                        "uint128"
                                                      ),
                                                      PT.EUInt128(
                                                        7567130UL,
                                                        340282366920938463463374607431768211455Z
                                                      ),
                                                      PT.ELet(
                                                        831830082UL,
                                                        PT.LPVariable(
                                                          756731UL,
                                                          "uint64"
                                                        ),
                                                        PT.EUInt64(
                                                          7567132UL,
                                                          18446744073709551615UL
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


  let constValue : PT.Const =
    PT.Const.CTuple(
      PT.Const.CInt64(314L),
      PT.Const.CBool(true),
      [ PT.Const.CString("string")
        PT.Const.CUnit
        PT.Const.CFloat(Positive, "3", "14")
        PT.Const.CChar("c")
        PT.Const.CUnit
        PT.Const.CUInt64(3UL)
        PT.Const.CInt8(4y)
        PT.Const.CUInt8(3uy)
        PT.Const.CInt16(4s)
        PT.Const.CUInt16(3us)
        PT.Const.CInt32(4l)
        PT.Const.CUInt32(3ul)
        PT.Const.CInt128(-1Q)
        PT.Const.CUInt128(1Z) ]
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
    { tlid = 0UL; name = "User"; version = 0; typ = typeReference }

  let userDBs : List<PT.DB.T> = [ userDB ]

  // TODO: serialize stdlib types?
  // (also make sure we roundtrip test them)

  let packageFn : PT.PackageFn.T =
    { id = uuid
      name =
        { owner = "dark"; modules = [ "stdlib"; "Int64"; "Int64" ]; name = "mod" }
      body = expr
      typeParams = [ "a" ]
      parameters =
        NEList.singleton
          { name = "param"; typ = typeReference; description = "desc" }
      returnType = typeReference
      description = "test"
      deprecated = PT.NotDeprecated }

  let packageFns = [ packageFn ]

  let packageType : PT.PackageType.T =
    { id = uuid
      name =
        { owner = "darklang"
          modules = [ "stdlib"; "Int64"; "Int64" ]
          name = "Int64" }
      declaration =
        { typeParams = [ "a" ]
          definition =
            PT.TypeDeclaration.Enum(
              NEList.ofList
                { name = "caseA"; fields = []; description = "" }
                [ { name = "caseB"
                    fields =
                      [ { typ = typeReference; label = Some "i"; description = "" } ]
                    description = "" } ]
            ) }

      description = "test"
      deprecated = PT.NotDeprecated }

  let packageTypes = [ packageType ]

  let packageConstant : PT.PackageConstant.T =
    { id = uuid
      name =
        { owner = "dark"
          modules = [ "stdlib"; "Int64"; "Int64" ]
          name = "testConstant" }
      body = constValue
      description = "test"
      deprecated = PT.NotDeprecated }

  let packageConstants = [ packageConstant ]

  let toplevels : List<PT.Toplevel.T> =
    [ List.map PT.Toplevel.TLHandler Handler.handlers
      List.map PT.Toplevel.TLDB [ userDB ] ]
    |> List.concat

  let userSecret : PT.Secret.T = { name = "APIKEY"; value = "hunter2"; version = 0 }

  let userSecrets = [ userSecret ]
