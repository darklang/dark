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
let hash = Hash "test-hash"

let id : id = 123UL
let tlid : tlid = 777777928475UL
let tlids : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

module RuntimeTypes =
  let fqTypeNames : List<RT.FQTypeName.FQTypeName> = [ RT.FQTypeName.Package hash ]

  let fqFnNames : List<RT.FQFnName.FQFnName> =
    [ RT.FQFnName.Builtin { name = "aB"; version = 1 }; RT.FQFnName.Package hash ]

  let fqValueNames : List<RT.FQValueName.FQValueName> =
    [ RT.FQValueName.Builtin { name = "aB"; version = 1 }
      RT.FQValueName.Package hash ]

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

      RT.TCustomType(Ok(RT.FQTypeName.Package hash), [ RT.TBool ])

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
    let typeName = RT.FQTypeName.Package hash
    sampleDvals
    |> List.map (fun (name, (dv, _)) -> name, dv)
    |> fun fields -> RT.DRecord(typeName, typeName, [], Map fields)

  let typeDeclarations : List<RT.TypeDeclaration.T> =
    [ // Alias type
      { typeParams = []; definition = RT.TypeDeclaration.Alias RT.TString }
      { typeParams = [ "T" ]
        definition = RT.TypeDeclaration.Alias(RT.TVariable "T") }

      // Record type
      { typeParams = []
        definition =
          RT.TypeDeclaration.Record(
            NEList.ofList
              { name = "name"; typ = RT.TString }
              [ { name = "age"; typ = RT.TInt64 } ]
          ) }

      // Enum type
      { typeParams = []
        definition =
          RT.TypeDeclaration.Enum(
            NEList.ofList
              { name = "None"; fields = [] }
              [ { name = "Some"; fields = [ RT.TVariable "T" ] } ]
          ) } ]

  // RT test values for binary serialization
  let packageTypes : List<RT.PackageType.PackageType> =
    [ { hash = hash; declaration = typeDeclarations[0] }
      { hash = Hash "another-test-hash"; declaration = typeDeclarations[1] } ]

  let packageValues : List<RT.PackageValue.PackageValue> =
    [ { hash = hash; body = RT.DString "Hello RT PackageValue" }
      { hash = Hash "another-test-hash"; body = RT.DInt64 42L }
      { hash = Hash "yet-another-test-hash"; body = RT.DBool true } ]

  let instructions : List<RT.Instructions> =
    [ { registerCount = 1; instructions = [ RT.CopyVal(0, 1) ]; resultIn = 0 }
      { registerCount = 3
        instructions =
          [ RT.LoadVal(0, RT.DUnit)
            RT.CreateString(1, [ RT.Text "hello" ])
            RT.JumpBy 2 ]
        resultIn = 1 } ]

  let packageFns : List<RT.PackageFn.PackageFn> =
    [ { hash = hash
        typeParams = []
        parameters = NEList.singleton { name = "x"; typ = RT.TInt64 }
        returnType = RT.TInt64
        body = instructions[0] }
      { hash = Hash "another-test-hash"
        typeParams = [ "T" ]
        parameters =
          NEList.ofList
            { name = "param1"; typ = RT.TVariable "T" }
            [ { name = "param2"; typ = RT.TString } ]
        returnType = RT.TString
        body = instructions[0] } ]


  let vals : List<RT.Dval> =
    [ RT.DUnit
      RT.DBool true
      RT.DBool false
      RT.DInt8 127y
      RT.DUInt8 255uy
      RT.DInt16 32767s
      RT.DUInt16 65535us
      RT.DInt32 2147483647l
      RT.DUInt32 4294967295ul
      RT.DInt64 9223372036854775807L
      RT.DUInt64 18446744073709551615UL
      RT.DInt128 170141183460469231731687303715884105727Q
      RT.DUInt128 340282366920938463463374607431768211455Z
      RT.DFloat(3.14159)
      RT.DChar "A"
      RT.DString "Hello, World!"
      // RT.CUuid and RT.CDateTime don't exist in RT.Const
      // RT.CUuid uuid
      // RT.CDateTime instant
      ]

module ProgramTypes =
  open PT

  let signs = [ Sign.Positive; Sign.Negative ]

  let fqFnNames : List<FQFnName.FQFnName> =
    [ FQFnName.Builtin { name = "int64Increment"; version = 1 }
      FQFnName.Package hash ]


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
      )
      MPOr(id, NEList.ofList (MPBool(id, true)) [ MPBool(id, false) ]) ]


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
        TCustomType(Ok(FQTypeName.Package hash), [ TBool ])
        TCustomType(Ok(FQTypeName.Package hash), [ TBool ])
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
                          Ok(FQTypeName.Package hash),
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
                               Ok(FQTypeName.Package hash),
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
                                  rhs = EBool(id, true) }
                                { pat =
                                    MPOr(
                                      id,
                                      NEList.ofList
                                        (MPBool(id, true))
                                        [ MPBool(id, false) ]
                                    )
                                  whenCondition = None
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
                                                        ELet(
                                                          id,
                                                          LPVariable(id, "statement"),
                                                          EStatement(
                                                            id,
                                                            EUnit id,
                                                            EInt64(id, 1L)
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
    )


  let constValue : PT.Expr =
    PT.ETuple(
      id,
      PT.EInt64(id, 314L),
      PT.EBool(id, true),
      [ PT.EString(id, [ PT.StringText("string") ])
        PT.EUnit(id)
        PT.EFloat(id, Positive, "3", "14")
        PT.EChar(id, "c")
        PT.EUnit(id)
        PT.EUInt64(id, 3UL)
        PT.EInt8(id, 4y)
        PT.EUInt8(id, 3uy)
        PT.EInt16(id, 4s)
        PT.EUInt16(id, 3us)
        PT.EInt32(id, 4l)
        PT.EUInt32(id, 3ul)
        PT.EInt128(id, -1Q)
        PT.EUInt128(id, 1Z) ]
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
    { hash = Hash "test-hash" // TODO: generate real hash
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
    { hash = Hash "test-hash" // TODO: generate real hash
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

  let packageValue : PT.PackageValue.PackageValue =
    { hash = Hash "test-hash" // TODO: generate real hash
      name = PT.PackageValue.name "dark" [ "stdlib"; "Int64"; "Int64" ] "testValue"
      body = constValue
      description = "test"
      deprecated = PT.NotDeprecated }

  let packageValues = [ packageValue ]

  let toplevels : List<Toplevel.T> =
    [ List.map Toplevel.TLHandler Handler.handlers
      List.map Toplevel.TLDB [ userDB ] ]
    |> List.concat

  let userSecret : Secret.T = { name = "APIKEY"; value = "hunter2"; version = 0 }

  let userSecrets = [ userSecret ]
