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
    [ RT.TUnit
      RT.TBool

      RT.TInt8
      RT.TUInt8
      RT.TInt16
      RT.TUInt16
      RT.TInt32
      RT.TUInt32
      RT.TInt64
      RT.TUInt64
      RT.TInt128
      RT.TUInt128

      RT.TFloat

      RT.TString

      RT.TTuple(RT.TBool, RT.TBool, [ RT.TBool ])
      RT.TList RT.TInt64
      RT.TDict RT.TBool

      RT.TFn(NEList.singleton RT.TBool, RT.TBool)

      RT.TCustomType(Ok(RT.FQTypeName.Package uuid), [ RT.TBool ])

      RT.TDB RT.TBool

      RT.TVariable "test" ]


  let valueTypes : List<RT.ValueType> =
    let known kt = RT.ValueType.Known kt
    let ktUnit = known RT.KnownType.KTUnit

    [ RT.ValueType.Unknown

      ktUnit

      known RT.KnownType.KTBool

      known RT.KnownType.KTInt8
      known RT.KnownType.KTUInt8
      known RT.KnownType.KTInt16
      known RT.KnownType.KTUInt16
      known RT.KnownType.KTInt32
      known RT.KnownType.KTUInt32
      known RT.KnownType.KTInt64
      known RT.KnownType.KTUInt64
      known RT.KnownType.KTInt128
      known RT.KnownType.KTUInt128
      known RT.KnownType.KTFloat
      known RT.KnownType.KTChar
      known RT.KnownType.KTString
      known RT.KnownType.KTUuid
      known RT.KnownType.KTDateTime

      known (RT.KnownType.KTTuple(ktUnit, ktUnit, []))
      known (RT.KnownType.KTList ktUnit)
      known (RT.KnownType.KTDict ktUnit)

      known (RT.KnownType.KTFn(NEList.singleton ktUnit, ktUnit))

      known (RT.KnownType.KTDB ktUnit) ]

  let dvals : List<RT.Dval> =
    // TODO: is this exhaustive? I haven't checked.
    sampleDvals |> List.map (fun (_, (dv, _)) -> dv)

  let dval : RT.Dval =
    let typeName = RT.FQTypeName.Package uuid
    sampleDvals
    |> List.map (fun (name, (dv, _)) -> name, dv)
    |> fun fields -> RT.DRecord(typeName, typeName, [], Map fields)


module ProgramTypes =
  open PT

  let signs = [ Sign.Positive; Sign.Negative ]

  let fqFnNames : List<FQFnName.FQFnName> =
    [ FQFnName.Builtin { name = "int64Increment"; version = 1 }
      FQFnName.Package uuid ]


  let letPatterns : List<LetPattern> =
    [ LPVariable(id, "test")
      LPTuple(
        id,
        LPVariable(id, "x0"),
        LPTuple(id, LPVariable(id, "x1"), LPVariable(id, "x2"), []),
        [ LPTuple(id, LPVariable(id, "x3"), LPVariable(id, "x4"), []) ]
      ) ]


  let matchPatterns : List<MatchPattern> =
    [ MPVariable(id, "var8481")
      MPEnum(id, "None", [])
      MPInt64(id, 84871728L)
      MPUInt64(id, 84871728UL)
      MPInt8(id, 127y)
      MPUInt8(id, 255uy)
      MPInt16(id, 32767s)
      MPUInt16(id, 65535us)
      MPInt32(id, 2147483647l)
      MPUInt32(id, 4294967295ul)
      MPInt128(id, 170141183460469231731687303715884105727Q)
      MPUInt128(id, 340282366920938463463374607431768211455Z)
      MPBool(id, false)
      MPChar(id, "w")
      MPString(id, "testing testing 123")
      MPFloat(id, Positive, "123", "456")
      MPUnit(id)
      MPTuple(id, MPInt64(id, 123), MPBool(id, true), [ MPUnit(id) ])
      MPList(id, [ MPInt64(id, 123) ])
      MPListCons(
        id,
        MPString(id, "val1"),
        MPListCons(id, MPString(id, "val2"), MPList(id, [ MPString(id, "val3") ]))
      ) ]


  // Note: This is aimed to contain all cases of `TypeReference`
  let typeReference : TypeReference =
    TTuple(
      TInt64,
      TFloat,
      [ TBool
        TUnit
        TUInt64
        TInt8
        TUInt8
        TInt16
        TUInt16
        TInt32
        TUInt32
        TInt128
        TUInt128
        TString
        TList TInt64
        TTuple(TBool, TBool, [ TBool ])
        TDict TBool
        TDB TBool
        TCustomType(Ok(FQTypeName.Package uuid), [ TBool ])
        TCustomType(Ok(FQTypeName.Package uuid), [ TBool ])
        TVariable "test"
        TFn(NEList.singleton TBool, TBool) ]
    )



  // Note: this is aimed to contain all cases of `Expr`
  let expr =
    let e = EInt64(id, 5)
    ELet(
      id,
      LPTuple(
        id,
        LPVariable(id, "x0"),
        LPTuple(id, LPVariable(id, "x1"), LPVariable(id, "x2"), []),
        [ LPTuple(id, LPVariable(id, "x3"), LPVariable(id, "x4"), []) ]
      ),
      EInt64(id, 5L),
      ELet(
        id,
        LPVariable(id, "x2"),
        EInt64(id, 9223372036854775807L),
        ELet(
          id,
          LPVariable(id, "bool"),
          EBool(id, true),
          ELet(
            id,
            LPVariable(id, "bool"),
            EBool(id, false),
            ELet(
              id,
              LPVariable(id, "str"),
              EString(
                id,
                [ StringText "a string"; StringInterpolation(EVariable(id, "var")) ]
              ),
              ELet(
                id,
                LPVariable(id, "char"),
                EChar(id, "a"),
                ELet(
                  id,
                  LPVariable(id, "float"),
                  EFloat(id, Negative, "6", "5"),
                  ELet(
                    id,
                    LPVariable(id, "n"),
                    EUnit id,
                    ELet(
                      id,
                      LPVariable(id, "i"),
                      EIf(
                        id,
                        EApply(
                          id,
                          EFnName(
                            id,
                            Ok(
                              FQFnName.Builtin
                                { name = "int64ToString"; version = 0 }
                            )
                          ),
                          [ typeReference ],
                          NEList.singleton (EInt64(id, 6L))
                        ),
                        EIf(
                          id,
                          EInfix(
                            id,
                            InfixFnCall(ComparisonNotEquals),
                            EInt64(id, 5L),
                            EInt64(id, 6L)
                          ),
                          EInfix(
                            id,
                            InfixFnCall(ArithmeticPlus),
                            EInt64(id, 5L),
                            EInt64(id, 2L)
                          ),
                          Some(
                            ELambda(
                              id,
                              NEList.singleton (LPVariable(id, "y")),
                              EInfix(
                                id,
                                InfixFnCall(ArithmeticPlus),
                                EInt64(id, 2L),
                                EVariable(id, "y")
                              )
                            )
                          )
                        ),
                        Some(
                          EInfix(
                            id,
                            InfixFnCall(ArithmeticPlus),
                            EInfix(
                              id,
                              InfixFnCall(ArithmeticPlus),
                              ERecordFieldAccess(id, EVariable(id, "x"), "y"),
                              EApply(
                                id,
                                EFnName(
                                  id,
                                  Ok(
                                    FQFnName.Builtin
                                      { name = "int64Add"; version = 0 }
                                  )
                                ),
                                [],
                                NEList.doubleton (EInt64(id, 6L)) (EInt64(id, 2L))
                              )
                            ),
                            EList(
                              id,
                              [ EInt64(id, 5L); EInt64(id, 6L); EInt64(id, 7L) ]
                            )
                          )
                        )
                      ),
                      ELet(
                        id,
                        LPVariable(id, "r"),
                        ERecord(
                          id,
                          Ok(FQTypeName.Package uuid),
                          [ TUnit ],
                          [ ("field",
                             EPipe(
                               id,
                               EInt64(id, 5L),
                               [ EPipeVariable(id, "fn", [ EVariable(id, "x") ])
                                 EPipeLambda(
                                   id,
                                   NEList.singleton (LPVariable(id, "y")),
                                   EInfix(
                                     id,
                                     InfixFnCall(ArithmeticPlus),
                                     EInt64(id, 2L),
                                     EVariable(id, "y")
                                   )
                                 )
                                 EPipeInfix(
                                   id,
                                   InfixFnCall(ArithmeticPlus),
                                   EInt64(id, 2L)
                                 )
                                 EPipeFnCall(
                                   id,
                                   Ok(
                                     FQFnName.Builtin
                                       { name = "int64Add"; version = 0 }
                                   ),
                                   [],
                                   [ (EInt64(id, 6L)); (EInt64(id, 2L)) ]
                                 ) ]
                             ))
                            ("enum",
                             EEnum(
                               id,
                               Ok(FQTypeName.Package uuid),
                               [ TUnit ],
                               "Error",
                               []
                             )) ]
                        ),
                        ELet(
                          id,
                          LPVariable(id, "updatedR"),
                          ERecordUpdate(
                            id,
                            EVariable(id, "r"),
                            NEList.singleton ("field", EInt64(id, 42L))
                          ),
                          ELet(
                            id,
                            LPVariable(id, "m"),
                            EMatch(
                              id,
                              EApply(
                                id,
                                EFnName(
                                  id,
                                  Ok(
                                    FQFnName.Builtin
                                      { name = "modFunction"; version = 2 }
                                  )
                                ),
                                [],
                                (NEList.singleton (EInt64(id, 5L)))
                              ),
                              [ { pat = MPEnum(id, "Ok", [ MPVariable(id, "x") ])
                                  whenCondition = None
                                  rhs = EVariable(id, "v") }
                                { pat = MPInt64(id, 5L)
                                  whenCondition = None
                                  rhs = EInt64(id, -9223372036854775808L) }
                                { pat = MPBool(id, true)
                                  whenCondition = None
                                  rhs = EInt64(id, 7L) }
                                { pat = MPChar(id, "c")
                                  whenCondition = None
                                  rhs = EChar(id, "c") }
                                { pat = MPList(id, [ MPBool(id, true) ])
                                  whenCondition = None
                                  rhs = EList(id, [ EBool(id, true) ]) }
                                { pat =
                                    MPListCons(
                                      id,
                                      MPString(id, "val1"),
                                      MPListCons(
                                        id,
                                        MPString(id, "val2"),
                                        MPList(id, [ MPString(id, "val3") ])
                                      )
                                    )
                                  whenCondition = None
                                  rhs = EList(id, [ EBool(id, true) ]) }
                                { pat = MPString(id, "string")
                                  whenCondition = None
                                  rhs =
                                    EString(
                                      id,
                                      [ StringText "string"
                                        StringInterpolation(EVariable(id, "var")) ]
                                    ) }
                                { pat = MPUnit id
                                  whenCondition = None
                                  rhs = EUnit id }
                                { pat = MPVariable(id, "var")
                                  whenCondition = None
                                  rhs =
                                    EInfix(
                                      id,
                                      InfixFnCall(ArithmeticPlus),
                                      EInt64(id, 6L),
                                      EVariable(id, "var")
                                    ) }
                                { pat = MPFloat(id, Positive, "5", "6")
                                  whenCondition = None
                                  rhs = EFloat(id, Positive, "5", "6") }
                                { pat =
                                    MPTuple(
                                      id,
                                      MPVariable(id, "a"),
                                      MPVariable(id, "b"),
                                      [ MPVariable(id, "c") ]
                                    )
                                  whenCondition = None
                                  rhs = EBool(id, true) }
                                { pat =
                                    MPTuple(
                                      id,
                                      MPVariable(id, "a"),
                                      MPVariable(id, "b"),
                                      [ MPVariable(id, "c") ]
                                    )
                                  whenCondition = Some(EBool(id, true))
                                  rhs = EBool(id, true) } ]
                            ),
                            ELet(
                              id,
                              LPVariable(id, "f"),
                              EIf(
                                id,
                                EBool(id, true),
                                EInt64(id, 5L),
                                Some(EInt64(id, 6L))
                              ),
                              ELet(
                                id,
                                LPVariable(id, "partials"),
                                EList(id, []),
                                ELet(
                                  id,
                                  LPVariable(id, "tuples"),
                                  ETuple(id, e, e, [ e ]),
                                  ELet(
                                    id,
                                    LPVariable(id, "binopAnd"),
                                    EInfix(
                                      id,
                                      BinOp(BinOpAnd),
                                      EBool(id, true),
                                      EBool(id, false)
                                    ),
                                    ELet(
                                      id,
                                      LPVariable(id, "dict"),
                                      EDict(id, [ ("a string", EInt64(id, 2L)) ]),
                                      ELet(
                                        id,
                                        LPVariable(id, "int8"),
                                        EInt8(id, 127y),
                                        ELet(
                                          id,
                                          LPVariable(id, "uint8"),
                                          EUInt8(id, 255uy),
                                          ELet(
                                            id,
                                            LPVariable(id, "int16"),
                                            EInt16(id, 32767s),
                                            ELet(
                                              id,
                                              LPVariable(id, "uint16"),
                                              EUInt16(id, 65535us),
                                              ELet(
                                                id,
                                                LPVariable(id, "int32"),
                                                EInt32(id, 2147483647l),
                                                ELet(
                                                  id,
                                                  LPVariable(id, "uint32"),
                                                  EUInt32(id, 4294967295ul),
                                                  ELet(
                                                    id,
                                                    LPVariable(id, "int128"),
                                                    EInt128(
                                                      id,
                                                      170141183460469231731687303715884105727Q
                                                    ),
                                                    ELet(
                                                      id,
                                                      LPVariable(id, "uint128"),
                                                      EUInt128(
                                                        id,
                                                        340282366920938463463374607431768211455Z
                                                      ),
                                                      ELet(
                                                        id,
                                                        LPVariable(id, "uint64"),
                                                        EUInt64(
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


  let constValue : Const =
    CTuple(
      CInt64(314L),
      CBool(true),
      [ CString("string")
        CUnit
        CFloat(Positive, "3", "14")
        CChar("c")
        CUnit
        CUInt64(3UL)
        CInt8(4y)
        CUInt8(3uy)
        CInt16(4s)
        CUInt16(3us)
        CInt32(4l)
        CUInt32(3ul)
        CInt128(-1Q)
        CUInt128(1Z) ]
    )


  module Handler =
    let cronIntervals : List<Handler.CronInterval> =
      [ Handler.EveryDay
        Handler.EveryWeek
        Handler.EveryFortnight
        Handler.Every12Hours
        Handler.EveryHour
        Handler.EveryMinute ]

    module Spec =
      let http = Handler.HTTP("/path-bytes", "GET")
      let worker = Handler.Worker("name")

      let cron = Handler.Cron("name", Handler.Every12Hours)

      let repl = Handler.REPL("name")

    let specs : List<Handler.Spec> = [ Spec.http; Spec.worker; Spec.cron; Spec.repl ]

    let http : Handler.T = { spec = Spec.http; tlid = tlid; ast = expr }

    let worker : Handler.T = { spec = Spec.worker; tlid = tlid; ast = expr }

    let repl : Handler.T = { spec = Spec.repl; tlid = tlid; ast = expr }

    let cron : Handler.T = { spec = Spec.cron; tlid = tlid; ast = expr }

    let handlersWithName : List<string * Handler.T> =
      [ "Worker", worker; "Cron", cron; "REPL", repl; "Http", http ]

    let handlers = List.map snd handlersWithName

  let userDB : DB.T = { tlid = 0UL; name = "User"; version = 0; typ = typeReference }

  let userDBs : List<DB.T> = [ userDB ]

  // TODO: serialize stdlib types?
  // (also make sure we roundtrip test them)

  let packageFn : PackageFn.PackageFn =
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
      deprecated = NotDeprecated }

  let packageFns = [ packageFn ]

  let packageType : PackageType.PackageType =
    { id = uuid
      name =
        { owner = "darklang"
          modules = [ "stdlib"; "Int64"; "Int64" ]
          name = "Int64" }
      declaration =
        { typeParams = [ "a" ]
          definition =
            TypeDeclaration.Enum(
              NEList.ofList
                { name = "caseA"; fields = []; description = "" }
                [ { name = "caseB"
                    fields =
                      [ { typ = typeReference; label = Some "i"; description = "" } ]
                    description = "" } ]
            ) }

      description = "test"
      deprecated = NotDeprecated }

  let packageTypes = [ packageType ]

  let packageConstant : PackageConstant.PackageConstant =
    { id = uuid
      name =
        { owner = "dark"
          modules = [ "stdlib"; "Int64"; "Int64" ]
          name = "testConstant" }
      body = constValue
      description = "test"
      deprecated = NotDeprecated }

  let packageConstants = [ packageConstant ]

  let toplevels : List<Toplevel.T> =
    [ List.map Toplevel.TLHandler Handler.handlers
      List.map Toplevel.TLDB [ userDB ] ]
    |> List.concat

  let userSecret : Secret.T = { name = "APIKEY"; value = "hunter2"; version = 0 }

  let userSecrets = [ userSecret ]
