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

let id : id = 123UL
let tlid : tlid = 777777928475UL
let tlids : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

module RuntimeTypes =
  let fqTypeNames : List<RT.FQTypeName.FQTypeName> = [ RT.FQTypeName.Package uuid ]

  let fqFnNames : List<RT.FQFnName.FQFnName> =
    [ RT.FQFnName.Builtin { name = "aB"; version = 1 }; RT.FQFnName.Package uuid ]

  let fqConstantNames : List<RT.FQConstantName.FQConstantName> =
    [ RT.FQConstantName.Builtin { name = "aB"; version = 1 }
      RT.FQConstantName.Package uuid ]

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
      RT.TCustomType(Ok(RT.FQTypeName.Package uuid), [ RT.TBool ])
      RT.TCustomType(Ok(RT.FQTypeName.Package uuid), [ RT.TBool ])
      RT.TVariable "test"
      RT.TFn(NEList.singleton RT.TBool, RT.TBool) ]

  let letPatterns : List<RT.LetPattern> =
    [ RT.LPVariable(id, "test")
      RT.LPUnit(id)
      RT.LPTuple(id, RT.LPUnit id, RT.LPUnit id, []) ]

  let matchPatterns : List<RT.MatchPattern> =
    [ RT.MPVariable(id, "test")
      RT.MPEnum(id, "Some", [ RT.MPVariable(id, "var") ])
      RT.MPInt64(id, 7857395)
      RT.MPUInt64(id, 756386UL)
      RT.MPInt8(id, 127y)
      RT.MPUInt8(id, 255uy)
      RT.MPInt16(id, 32767s)
      RT.MPUInt16(id, 65535us)
      RT.MPInt32(id, 2147483647l)
      RT.MPUInt32(id, 4294967295ul)
      RT.MPInt128(id, 170141183460Q)
      RT.MPUInt128(id, 340282366920938Z)
      RT.MPBool(id, true)
      RT.MPChar(id, "8jgkdjsfg")
      RT.MPString(id, "iklfijo13294")
      RT.MPUnit(id)
      RT.MPTuple(
        id,
        RT.MPUnit(id),
        RT.MPString(id, "1243sdfsadf"),
        [ RT.MPVariable(id, "var2") ]
      )
      RT.MPFloat(id, 79375.847583)
      RT.MPList(id, [ RT.MPString(id, "1234") ])
      RT.MPListCons(
        id,
        RT.MPString(id, "val1"),
        RT.MPListCons(
          id,
          RT.MPString(id, "val2"),
          RT.MPList(id, [ RT.MPString(id, "val3") ])
        )
      ) ]

  let exprs : List<RT.Expr> =
    [ RT.EInt64(id, 7L)
      RT.EUInt64(id, 7UL)
      RT.EInt8(id, 7y)
      RT.EUInt8(id, 7uy)
      RT.EInt16(id, 7s)
      RT.EUInt16(id, 7us)
      RT.EInt32(id, 7l)
      RT.EUInt32(id, 7ul)
      RT.EInt128(id, 7Q)
      RT.EUInt128(id, 7Z)
      RT.EBool(id, false)
      RT.EString(
        id,
        [ RT.StringText "asdfasedf"
          RT.StringInterpolation(RT.EVariable(id, "var")) ]
      )
      RT.EUnit(id)
      RT.ELet(
        id,
        RT.LPVariable(id, "binding"),
        RT.EUnit(id),
        RT.EVariable(id, "binding")
      )
      RT.EIf(id, RT.EUnit(id), RT.EUnit(id), Some(RT.EUnit(id)))
      RT.EIf(id, RT.EUnit(id), RT.EUnit(id), None)
      RT.ELambda(id, NEList.singleton (RT.LPVariable(id, "var3")), RT.EUnit(id))
      RT.ELambda(
        id,
        NEList.singleton (
          RT.LPTuple(id, RT.LPVariable(id, "var11"), RT.LPVariable(id, "var12"), [])
        ),
        RT.EUnit(id)
      )
      RT.ERecordFieldAccess(id, RT.EUnit(id), "field")
      RT.EVariable(id, "var4")
      RT.EApply(id, RT.EUnit(id), typeReferences, NEList.singleton (RT.EUnit(id)))
      RT.EApply(
        id,
        RT.EFnName(id, RT.FQFnName.Package uuid),
        [],
        (NEList.singleton (RT.EUnit(id)))
      )
      RT.EList(id, [ RT.EUnit(id) ])
      RT.ETuple(id, RT.EUnit(id), RT.EUnit(id), [ RT.EUnit(id) ])
      RT.ERecord(
        id,
        RT.FQTypeName.Package uuid,
        NEList.singleton ("a9df8", RT.EUnit(id))
      )
      RT.ERecordUpdate(
        id,
        RT.EVariable(id, "myRec"),
        NEList.singleton ("y", RT.EInt64(id, 2L))
      )
      RT.EMatch(
        id,
        RT.EInt64(id, 123),
        NEList.singleton
          { pat = RT.MPVariable(id, "i")
            whenCondition = None
            rhs = RT.EVariable(id, "i") }
      )
      RT.EAnd(id, RT.EBool(id, true), RT.EBool(id, false))
      RT.EOr(id, RT.EBool(id, true), RT.EBool(id, false))
      RT.EEnum(id, RT.FQTypeName.Package uuid, "A", [ RT.EUnit(id) ]) ]


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
    let typeName = RT.FQTypeName.Package uuid
    sampleDvals
    |> List.map (fun (name, (dv, _)) -> name, dv)
    |> fun fields -> RT.DRecord(typeName, typeName, [], Map fields)


module ProgramTypes =
  let signs = [ Sign.Positive; Sign.Negative ]

  let fqFnNames : List<PT.FQFnName.FQFnName> =
    [ PT.FQFnName.Builtin { name = "int64Increment"; version = 1 }
      PT.FQFnName.Package uuid ]


  let letPatterns : List<PT.LetPattern> =
    [ PT.LPVariable(id, "test")
      PT.LPTuple(
        id,
        PT.LPVariable(id, "x0"),
        PT.LPTuple(id, PT.LPVariable(id, "x1"), PT.LPVariable(id, "x2"), []),
        [ PT.LPTuple(id, PT.LPVariable(id, "x3"), PT.LPVariable(id, "x4"), []) ]
      ) ]


  let matchPatterns : List<PT.MatchPattern> =
    [ PT.MPVariable(id, "var8481")
      PT.MPEnum(id, "None", [])
      PT.MPInt64(id, 84871728L)
      PT.MPUInt64(id, 84871728UL)
      PT.MPInt8(id, 127y)
      PT.MPUInt8(id, 255uy)
      PT.MPInt16(id, 32767s)
      PT.MPUInt16(id, 65535us)
      PT.MPInt32(id, 2147483647l)
      PT.MPUInt32(id, 4294967295ul)
      PT.MPInt128(id, 170141183460469231731687303715884105727Q)
      PT.MPUInt128(id, 340282366920938463463374607431768211455Z)
      PT.MPBool(id, false)
      PT.MPChar(id, "w")
      PT.MPString(id, "testing testing 123")
      PT.MPFloat(id, Positive, "123", "456")
      PT.MPUnit(id)
      PT.MPTuple(id, PT.MPInt64(id, 123), PT.MPBool(id, true), [ PT.MPUnit(id) ])
      PT.MPList(id, [ PT.MPInt64(id, 123) ])
      PT.MPListCons(
        id,
        PT.MPString(id, "val1"),
        PT.MPListCons(
          id,
          PT.MPString(id, "val2"),
          PT.MPList(id, [ PT.MPString(id, "val3") ])
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
        PT.TCustomType(Ok(PT.FQTypeName.Package uuid), [ PT.TBool ])
        PT.TCustomType(Ok(PT.FQTypeName.Package uuid), [ PT.TBool ])
        PT.TVariable "test"
        PT.TFn(NEList.singleton PT.TBool, PT.TBool) ]
    )



  // Note: this is aimed to contain all cases of `Expr`
  let expr =
    let e = PT.EInt64(id, 5)
    PT.ELet(
      id,
      PT.LPTuple(
        id,
        PT.LPVariable(id, "x0"),
        PT.LPTuple(id, PT.LPVariable(id, "x1"), PT.LPVariable(id, "x2"), []),
        [ PT.LPTuple(id, PT.LPVariable(id, "x3"), PT.LPVariable(id, "x4"), []) ]
      ),
      PT.EInt64(id, 5L),
      PT.ELet(
        id,
        PT.LPVariable(id, "x2"),
        PT.EInt64(id, 9223372036854775807L),
        PT.ELet(
          id,
          PT.LPVariable(id, "bool"),
          PT.EBool(id, true),
          PT.ELet(
            id,
            PT.LPVariable(id, "bool"),
            PT.EBool(id, false),
            PT.ELet(
              id,
              PT.LPVariable(id, "str"),
              PT.EString(
                id,
                [ PT.StringText "a string"
                  PT.StringInterpolation(PT.EVariable(id, "var")) ]
              ),
              PT.ELet(
                id,
                PT.LPVariable(id, "char"),
                PT.EChar(id, "a"),
                PT.ELet(
                  id,
                  PT.LPVariable(id, "float"),
                  PT.EFloat(id, Negative, "6", "5"),
                  PT.ELet(
                    id,
                    PT.LPVariable(id, "n"),
                    PT.EUnit id,
                    PT.ELet(
                      id,
                      PT.LPVariable(id, "i"),
                      PT.EIf(
                        id,
                        PT.EApply(
                          id,
                          PT.EFnName(
                            id,
                            Ok(
                              PT.FQFnName.Builtin
                                { name = "int64ToString"; version = 0 }
                            )
                          ),
                          [ typeReference ],
                          NEList.singleton (PT.EInt64(id, 6L))
                        ),
                        PT.EIf(
                          id,
                          PT.EInfix(
                            id,
                            PT.InfixFnCall(PT.ComparisonNotEquals),
                            PT.EInt64(id, 5L),
                            PT.EInt64(id, 6L)
                          ),
                          PT.EInfix(
                            id,
                            PT.InfixFnCall(PT.ArithmeticPlus),
                            PT.EInt64(id, 5L),
                            PT.EInt64(id, 2L)
                          ),
                          Some(
                            PT.ELambda(
                              id,
                              NEList.singleton (PT.LPVariable(id, "y")),
                              PT.EInfix(
                                id,
                                PT.InfixFnCall(PT.ArithmeticPlus),
                                PT.EInt64(id, 2L),
                                PT.EVariable(id, "y")
                              )
                            )
                          )
                        ),
                        Some(
                          PT.EInfix(
                            id,
                            PT.InfixFnCall(PT.ArithmeticPlus),
                            PT.EInfix(
                              id,
                              PT.InfixFnCall(PT.ArithmeticPlus),
                              PT.ERecordFieldAccess(id, PT.EVariable(id, "x"), "y"),
                              PT.EApply(
                                id,
                                PT.EFnName(
                                  id,
                                  Ok(
                                    PT.FQFnName.Builtin
                                      { name = "int64Add"; version = 0 }
                                  )
                                ),
                                [],
                                NEList.doubleton
                                  (PT.EInt64(id, 6L))
                                  (PT.EInt64(id, 2L))
                              )
                            ),
                            PT.EList(
                              id,
                              [ PT.EInt64(id, 5L)
                                PT.EInt64(id, 6L)
                                PT.EInt64(id, 7L) ]
                            )
                          )
                        )
                      ),
                      PT.ELet(
                        id,
                        PT.LPVariable(id, "r"),
                        PT.ERecord(
                          id,
                          Ok(PT.FQTypeName.Package uuid),
                          [ ("field",
                             PT.EPipe(
                               id,
                               PT.EInt64(id, 5L),
                               [ PT.EPipeVariable(
                                   id,
                                   "fn",
                                   [ PT.EVariable(id, "x") ]
                                 )
                                 PT.EPipeLambda(
                                   id,
                                   NEList.singleton (PT.LPVariable(id, "y")),
                                   PT.EInfix(
                                     id,
                                     PT.InfixFnCall(PT.ArithmeticPlus),
                                     PT.EInt64(id, 2L),
                                     PT.EVariable(id, "y")
                                   )
                                 )
                                 PT.EPipeInfix(
                                   id,
                                   PT.InfixFnCall(PT.ArithmeticPlus),
                                   PT.EInt64(id, 2L)
                                 )
                                 PT.EPipeFnCall(
                                   id,
                                   Ok(
                                     PT.FQFnName.Builtin
                                       { name = "int64Add"; version = 0 }
                                   ),
                                   [],
                                   [ (PT.EInt64(id, 6L)); (PT.EInt64(id, 2L)) ]
                                 ) ]
                             ))
                            ("enum",
                             PT.EEnum(
                               id,
                               Ok(PT.FQTypeName.Package uuid),
                               "Error",
                               []
                             )) ]
                        ),
                        PT.ELet(
                          id,
                          PT.LPVariable(id, "updatedR"),
                          PT.ERecordUpdate(
                            id,
                            PT.EVariable(id, "r"),
                            NEList.singleton ("field", PT.EInt64(id, 42L))
                          ),
                          PT.ELet(
                            id,
                            PT.LPVariable(id, "m"),
                            PT.EMatch(
                              id,
                              PT.EApply(
                                id,
                                PT.EFnName(
                                  id,
                                  Ok(
                                    PT.FQFnName.Builtin
                                      { name = "modFunction"; version = 2 }
                                  )
                                ),
                                [],
                                (NEList.singleton (PT.EInt64(id, 5L)))
                              ),
                              [ { pat =
                                    PT.MPEnum(id, "Ok", [ PT.MPVariable(id, "x") ])
                                  whenCondition = None
                                  rhs = PT.EVariable(id, "v") }
                                { pat = PT.MPInt64(id, 5L)
                                  whenCondition = None
                                  rhs = PT.EInt64(id, -9223372036854775808L) }
                                { pat = PT.MPBool(id, true)
                                  whenCondition = None
                                  rhs = PT.EInt64(id, 7L) }
                                { pat = PT.MPChar(id, "c")
                                  whenCondition = None
                                  rhs = PT.EChar(id, "c") }
                                { pat = PT.MPList(id, [ PT.MPBool(id, true) ])
                                  whenCondition = None
                                  rhs = PT.EList(id, [ PT.EBool(id, true) ]) }
                                { pat =
                                    PT.MPListCons(
                                      id,
                                      PT.MPString(id, "val1"),
                                      PT.MPListCons(
                                        id,
                                        PT.MPString(id, "val2"),
                                        PT.MPList(id, [ PT.MPString(id, "val3") ])
                                      )
                                    )
                                  whenCondition = None
                                  rhs = PT.EList(id, [ PT.EBool(id, true) ]) }
                                { pat = PT.MPString(id, "string")
                                  whenCondition = None
                                  rhs =
                                    PT.EString(
                                      id,
                                      [ PT.StringText "string"
                                        PT.StringInterpolation(
                                          PT.EVariable(id, "var")
                                        ) ]
                                    ) }
                                { pat = PT.MPUnit id
                                  whenCondition = None
                                  rhs = PT.EUnit id }
                                { pat = PT.MPVariable(id, "var")
                                  whenCondition = None
                                  rhs =
                                    PT.EInfix(
                                      id,
                                      PT.InfixFnCall(PT.ArithmeticPlus),
                                      PT.EInt64(id, 6L),
                                      PT.EVariable(id, "var")
                                    ) }
                                { pat = PT.MPFloat(id, Positive, "5", "6")
                                  whenCondition = None
                                  rhs = PT.EFloat(id, Positive, "5", "6") }
                                { pat =
                                    PT.MPTuple(
                                      id,
                                      PT.MPVariable(id, "a"),
                                      PT.MPVariable(id, "b"),
                                      [ PT.MPVariable(id, "c") ]
                                    )
                                  whenCondition = None
                                  rhs = PT.EBool(id, true) }
                                { pat =
                                    PT.MPTuple(
                                      id,
                                      PT.MPVariable(id, "a"),
                                      PT.MPVariable(id, "b"),
                                      [ PT.MPVariable(id, "c") ]
                                    )
                                  whenCondition = Some(PT.EBool(id, true))
                                  rhs = PT.EBool(id, true) } ]
                            ),
                            PT.ELet(
                              id,
                              PT.LPVariable(id, "f"),
                              PT.EIf(
                                id,
                                PT.EBool(id, true),
                                PT.EInt64(id, 5L),
                                Some(PT.EInt64(id, 6L))
                              ),
                              PT.ELet(
                                id,
                                PT.LPVariable(id, "partials"),
                                PT.EList(id, []),
                                PT.ELet(
                                  id,
                                  PT.LPVariable(id, "tuples"),
                                  PT.ETuple(id, e, e, [ e ]),
                                  PT.ELet(
                                    id,
                                    PT.LPVariable(id, "binopAnd"),
                                    PT.EInfix(
                                      id,
                                      PT.BinOp(PT.BinOpAnd),
                                      PT.EBool(id, true),
                                      PT.EBool(id, false)
                                    ),
                                    PT.ELet(
                                      id,
                                      PT.LPVariable(id, "dict"),
                                      PT.EDict(
                                        id,
                                        [ ("a string", PT.EInt64(id, 2L)) ]
                                      ),
                                      PT.ELet(
                                        id,
                                        PT.LPVariable(id, "int8"),
                                        PT.EInt8(id, 127y),
                                        PT.ELet(
                                          id,
                                          PT.LPVariable(id, "uint8"),
                                          PT.EUInt8(id, 255uy),
                                          PT.ELet(
                                            id,
                                            PT.LPVariable(id, "int16"),
                                            PT.EInt16(id, 32767s),
                                            PT.ELet(
                                              id,
                                              PT.LPVariable(id, "uint16"),
                                              PT.EUInt16(id, 65535us),
                                              PT.ELet(
                                                id,
                                                PT.LPVariable(id, "int32"),
                                                PT.EInt32(id, 2147483647l),
                                                PT.ELet(
                                                  id,
                                                  PT.LPVariable(id, "uint32"),
                                                  PT.EUInt32(id, 4294967295ul),
                                                  PT.ELet(
                                                    id,
                                                    PT.LPVariable(id, "int128"),
                                                    PT.EInt128(
                                                      id,
                                                      170141183460469231731687303715884105727Q
                                                    ),
                                                    PT.ELet(
                                                      id,
                                                      PT.LPVariable(id, "uint128"),
                                                      PT.EUInt128(
                                                        id,
                                                        340282366920938463463374607431768211455Z
                                                      ),
                                                      PT.ELet(
                                                        id,
                                                        PT.LPVariable(id, "uint64"),
                                                        PT.EUInt64(
                                                          id,
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

    let http : PT.Handler.T = { spec = Spec.http; tlid = tlid; ast = expr }

    let worker : PT.Handler.T = { spec = Spec.worker; tlid = tlid; ast = expr }

    let repl : PT.Handler.T = { spec = Spec.repl; tlid = tlid; ast = expr }

    let cron : PT.Handler.T = { spec = Spec.cron; tlid = tlid; ast = expr }

    let handlersWithName : List<string * PT.Handler.T> =
      [ "Worker", worker; "Cron", cron; "REPL", repl; "Http", http ]

    let handlers = List.map snd handlersWithName

  let userDB : PT.DB.T =
    { tlid = 0UL; name = "User"; version = 0; typ = typeReference }

  let userDBs : List<PT.DB.T> = [ userDB ]

  // TODO: serialize stdlib types?
  // (also make sure we roundtrip test them)

  let packageFn : PT.PackageFn.PackageFn =
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

  let packageType : PT.PackageType.PackageType =
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

  let packageConstant : PT.PackageConstant.PackageConstant =
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
