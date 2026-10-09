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
module RTNR = LibExecution.RuntimeTypes.NameResolution

module BS = LibSerialization.Binary.Serialization

let private hashStr =
  "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"

let hashRT = RT.Hash hashStr
let hashPT = PT.Hash hashStr

let instant = NodaTime.Instant.parse "2022-07-04T17:46:57Z"

let uuid = System.Guid.Parse "31d72f73-0f99-5a9b-949c-b95705ae7c4d"

let id : id = 123UL
let tlid : tlid = 777777928475UL
let tlids : List<tlid> = [ 1UL; 0UL; uint64 -1L ]

module RuntimeTypes =
  let fqTypeNames : List<RT.FQTypeName.FQTypeName> = [ RT.FQTypeName.Package hashRT ]

  let fqFnNames : List<RT.FQFnName.FQFnName> =
    [ RT.FQFnName.Builtin { name = "aB"; version = 1 }; RT.FQFnName.Package hashRT ]

  let fqValueNames : List<RT.FQValueName.FQValueName> =
    [ RT.FQValueName.Builtin { name = "aB"; version = 1 }
      RT.FQValueName.Package hashRT ]

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
      RT.TInt

      RT.TFloat

      RT.TString

      RT.TTuple(RT.TBool, RT.TBool, [ RT.TBool ])
      RT.TList RT.TInt64
      RT.TDict(RT.TString, RT.TBool)

      RT.TFn(NEList.singleton RT.TBool, RT.TBool)

      RT.TCustomType(RTNR.ok (RT.FQTypeName.Package hashRT), [ RT.TBool ])

      RT.TDB RT.TBool

      RT.TBlob

      RT.TVariable "test"

      RT.TChar
      RT.TUuid
      RT.TDateTime
      RT.TStream RT.TInt64
      // a failed resolution, so the NameResolution error tag is written too (via `Apply` below)
      RT.TCustomType({ originalName = [ "Nope" ]; resolved = Error RT.NotFound }, []) ]


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
      known RT.KnownType.KTInt
      known RT.KnownType.KTFloat
      known RT.KnownType.KTChar
      known RT.KnownType.KTString
      known RT.KnownType.KTUuid
      known RT.KnownType.KTDateTime

      known (RT.KnownType.KTTuple(ktUnit, ktUnit, []))
      known (RT.KnownType.KTList ktUnit)
      known (RT.KnownType.KTDict(ktUnit, ktUnit))

      known (RT.KnownType.KTFn(NEList.singleton ktUnit, ktUnit))

      known (RT.KnownType.KTDB ktUnit)
      known RT.KnownType.KTBlob
      known (RT.KnownType.KTCustomType(RT.FQTypeName.Package hashRT, [ ktUnit ]))
      known (RT.KnownType.KTStream ktUnit) ]

  let dvals () : List<RT.Dval> =
    // TODO: is this exhaustive? I haven't checked.
    sampleDvals () |> List.map (fun (_, (dv, _)) -> dv)

  let dval () : RT.Dval =
    let typeName = RT.FQTypeName.Package hashRT
    sampleDvals ()
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
    [ { hash = RT.Hash "abc123"; declaration = typeDeclarations[0] }
      { hash = RT.Hash "def456"; declaration = typeDeclarations[1] }
      { hash = RT.Hash "rec789"; declaration = typeDeclarations[2] }
      { hash = RT.Hash "enum012"; declaration = typeDeclarations[3] } ]

  let packageValues : List<RT.PackageValue.PackageValue> =
    [ { hash = RT.Hash "val1"; body = RT.DString "Hello RT PackageValue" }
      { hash = RT.Hash "val2"; body = RT.DInt64 42L }
      { hash = RT.Hash "val3"; body = RT.DBool true } ]

  let instructions : List<RT.Instructions> =
    [ { registerCount = 1; instructions = [ RT.CopyVal(0, 1) ]; resultIn = 0 }
      { registerCount = 3
        instructions =
          [ RT.LoadVal(0, RT.DUnit)
            RT.CreateString(1, [ RT.Text "hello" ])
            RT.JumpBy 2
            RT.Unwrap(2, 0, None)
            RT.Unwrap(
              2,
              0,
              // A fixed hash, not the stdlib Option's: that one comes from the gitignored pins file
              // and moves whenever Option does, which would turn this golden red with no format change.
              Some(RT.FQTypeName.Package hashRT)
            ) ]
        resultIn = 1 }

      // Every instruction the two above miss, and through them every let and match pattern, so the
      // golden corpus covers each tag the instruction writer can emit. Registers are arbitrary.
      { registerCount = 4
        instructions =
          [ RT.Or(0, 1, 2)
            RT.And(0, 1, 2)
            RT.CreateString(0, [ RT.Interpolated 1 ])
            RT.CheckLetPatternAndExtractVars(
              0,
              RT.LPTuple(RT.LPVariable 1, RT.LPUnit, [ RT.LPWildcard ])
            )
            RT.JumpByIfFalse(2, 0)
            RT.CheckMatchPatternAndExtractVars(
              0,
              RT.MPOr(
                NEList.ofList
                  RT.MPUnit
                  [ RT.MPBool true
                    RT.MPInt8 127y
                    RT.MPUInt8 255uy
                    RT.MPInt16 32767s
                    RT.MPUInt16 65535us
                    RT.MPInt32 2147483647l
                    RT.MPUInt32 4294967295ul
                    RT.MPInt64 9223372036854775807L
                    RT.MPUInt64 18446744073709551615UL
                    RT.MPInt128 170141183460469231731687303715884105727Q
                    RT.MPUInt128 340282366920938463463374607431768211455Z
                    RT.MPInt(
                      System.Numerics.BigInteger.Parse
                        "123456789012345678901234567890"
                    )
                    RT.MPFloat 1.5
                    RT.MPChar "c"
                    RT.MPString "s"
                    RT.MPList [ RT.MPUnit ]
                    RT.MPListCons(RT.MPVariable 1, RT.MPList [])
                    RT.MPTuple(RT.MPUnit, RT.MPUnit, [ RT.MPUnit ])
                    RT.MPEnum("Some", [ RT.MPVariable 2 ])
                    RT.MPVariable 3 ]
              ),
              5
            )
            RT.MatchUnmatched 0
            RT.CreateTuple(0, 1, 2, [ 3 ])
            RT.CreateList(0, [ 1; 2 ])
            RT.CreateDict(0, [ (1, 2) ])
            RT.CreateRecord(
              0,
              RT.FQTypeName.Package hashRT,
              [ RT.TInt64 ],
              [ ("f", 1) ]
            )
            RT.CloneRecordWithUpdates(0, 1, [ ("f", 2) ])
            RT.GetRecordField(0, 1, "f")
            RT.CreateEnum(0, RT.FQTypeName.Package hashRT, [], "Some", [ 1 ])
            RT.LoadValue(0, RT.FQValueName.Builtin { name = "pi"; version = 0 })
            RT.LoadValue(0, RT.FQValueName.Package hashRT)
            RT.CreateLambda(
              0,
              { exprId = 9UL
                patterns = NEList.singleton (RT.LPVariable 1)
                registersToCloseOver = [ (2, 3) ]
                selfRegister = Some 0
                instructions =
                  { registerCount = 1
                    instructions = [ RT.CopyVal(0, 0) ]
                    resultIn = 0 } }
            )
            // Every type reference, through the one instruction that carries a list of them.
            RT.Apply(0, 1, typeReferences, NEList.ofList 2 [ 3 ])
            RT.RaiseNRE([ "Nope" ], RT.NotFound)
            RT.RaiseNRE([ "not a name" ], RT.InvalidName)
            RT.VarNotFound(0, "x")
            RT.CheckIfFirstExprIsUnit 0
            RT.TraceExpr(11UL, 0) ]
        resultIn = 0 } ]

  let packageFns : List<RT.PackageFn.PackageFn> =
    [ { hash = RT.Hash "fn1"
        typeParams = []
        parameters = NEList.singleton { name = "x"; typ = RT.TInt64 }
        returnType = RT.TInt64
        body = instructions[0]
        symbols = RT.DebugSymbols.emptyLazy
        permissionCeiling = None
        bounds = [] }
      { hash = RT.Hash "fn2"
        typeParams = [ "T" ]
        parameters =
          NEList.ofList
            { name = "param1"; typ = RT.TVariable "T" }
            [ { name = "param2"; typ = RT.TString } ]
        returnType = RT.TString
        body = instructions[0]
        symbols = RT.DebugSymbols.emptyLazy
        permissionCeiling = Some(Set.singleton LibExecution.Effects.Effect.Clock)
        bounds =
          [ { param = "T"
              trait_ =
                { trait_ = RTNR.ok (RT.FQTraitName.Package hashRT)
                  typeArgs = [ RT.TInt64 ] } } ] } ]


  /// A function's symbol table, which travels in its own column. Index 300 takes a varint of two
  /// bytes, so the golden pins the multi-byte form too.
  let debugSymbols : List<RT.DebugSymbols> =
    [ { exprAt = Map [ 300, struct (7UL, 1) ]
        lambdas = Map [ 7UL, Map [ 0, struct (8UL, 2) ] ] } ]


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
      // the default Int, both Finite (Int64 range) and Infinite (past Int64)
      RT.Dval.int (bigint System.Int64.MaxValue)
      RT.Dval.int (System.Numerics.BigInteger.Parse "123456789012345678901234567890")
      RT.DFloat(3.14159)
      RT.DChar "A"
      RT.DString "Hello, World!"
      RT.DApplicable(
        RT.AppLambda
          { exprId = 7UL
            closedRegisters = []
            typeSymbolTable = RT.TST.empty
            access =
              LibExecution.Permissions.Access.start
                LibExecution.Permissions.Policy.denyAll
            argsSoFar = [] }
      )
      RT.DUuid uuid
      RT.DDateTime(LibExecution.DarkDateTime.fromInstant instant)
      RT.DList(RT.ValueType.Known RT.KnownType.KTInt64, [ RT.DInt64 1L ])
      RT.DTuple(RT.DUnit, RT.DBool true, [ RT.DString "t" ])
      RT.DDict(
        RT.ValueType.Known RT.KnownType.KTString,
        RT.ValueType.Known(
          RT.KnownType.KTList(RT.ValueType.Known RT.KnownType.KTUuid)
        ),
        Map [ RT.DictKey(RT.DString "k"), RT.DList(RT.ValueType.Unknown, []) ]
      )
      RT.DRecord(
        RT.FQTypeName.Package hashRT,
        RT.FQTypeName.Package hashRT,
        [ RT.ValueType.Known(
            RT.KnownType.KTDict(RT.ValueType.Unknown, RT.ValueType.Unknown)
          ) ],
        Map [ "f", RT.DInt64 1L ]
      )
      RT.DEnum(
        RT.FQTypeName.Package hashRT,
        RT.FQTypeName.Package hashRT,
        [ RT.ValueType.Known(
            RT.KnownType.KTFn(
              NEList.singleton RT.ValueType.Unknown,
              RT.ValueType.Unknown
            )
          )
          RT.ValueType.Known(
            RT.KnownType.KTCustomType(RT.FQTypeName.Package hashRT, [])
          )
          RT.ValueType.Known(
            RT.KnownType.KTTuple(RT.ValueType.Unknown, RT.ValueType.Unknown, [])
          )
          RT.ValueType.Known(RT.KnownType.KTDB RT.ValueType.Unknown)
          RT.ValueType.Known RT.KnownType.KTBlob
          RT.ValueType.Known(RT.KnownType.KTStream RT.ValueType.Unknown)
          RT.ValueType.Known RT.KnownType.KTUnit
          RT.ValueType.Known RT.KnownType.KTBool
          RT.ValueType.Known RT.KnownType.KTInt8
          RT.ValueType.Known RT.KnownType.KTUInt8
          RT.ValueType.Known RT.KnownType.KTInt16
          RT.ValueType.Known RT.KnownType.KTUInt16
          RT.ValueType.Known RT.KnownType.KTInt32
          RT.ValueType.Known RT.KnownType.KTUInt32
          RT.ValueType.Known RT.KnownType.KTUInt64
          RT.ValueType.Known RT.KnownType.KTInt128
          RT.ValueType.Known RT.KnownType.KTUInt128
          RT.ValueType.Known RT.KnownType.KTInt
          RT.ValueType.Known RT.KnownType.KTFloat
          RT.ValueType.Known RT.KnownType.KTChar
          RT.ValueType.Known RT.KnownType.KTString
          RT.ValueType.Known RT.KnownType.KTDateTime ],
        "Some",
        [ RT.DUnit ]
      )
      RT.DDB "db"
      RT.DBlob(RT.Persistent("blobhash", 3L))
      RT.DFloat System.Double.PositiveInfinity
      RT.DFloat System.Double.NegativeInfinity
      // NaN is not here: it never equals itself, so it cannot read back equal.
      RT.DApplicable(
        RT.AppLambda
          { exprId = 8UL
            closedRegisters = [ (1, RT.DInt64 2L) ]
            typeSymbolTable =
              RT.TST.empty
              |> RT.TST.add "a" (RT.ValueType.Known RT.KnownType.KTInt64)
            access =
              LibExecution.Permissions.Access.start
                LibExecution.Permissions.Policy.denyAll
            argsSoFar = [ RT.DUnit ] }
      )
      RT.DApplicable(
        RT.AppNamedFn
          { name = RT.FQFnName.Builtin { name = "someFn"; version = 0 }
            typeSymbolTable = RT.TST.empty
            typeArgs = [ RT.TInt64 ]
            access = None
            argsSoFar = [ RT.DUnit ]
            boundImpls =
              [ struct ("a", hashRT, "show", RT.FQFnName.Chosen hashRT)
                struct ("a", hashRT, "show", RT.FQFnName.FromTypeParam "b")
                struct ("a", hashRT, "show", RT.FQFnName.Unknown) ] }
      )
      RT.DApplicable(
        RT.AppNamedFn
          { name = RT.FQFnName.Package hashRT
            typeSymbolTable = RT.TST.empty
            typeArgs = []
            access = None
            argsSoFar = []
            boundImpls = [] }
      )
      // A trait method per implementation choice: its `implFn` has a writer of its own.
      RT.DApplicable(
        RT.AppNamedFn
          { name =
              RT.FQFnName.TraitMethod
                { trait_ = hashRT; method_ = "show"; implFn = RT.FQFnName.Unknown }
            typeSymbolTable = RT.TST.empty
            typeArgs = []
            access = None
            argsSoFar = []
            boundImpls = [] }
      )
      RT.DApplicable(
        RT.AppNamedFn
          { name =
              RT.FQFnName.TraitMethod
                { trait_ = hashRT
                  method_ = "show"
                  implFn = RT.FQFnName.Chosen hashRT }
            typeSymbolTable = RT.TST.empty
            typeArgs = []
            access = None
            argsSoFar = []
            boundImpls = [] }
      )
      RT.DApplicable(
        RT.AppNamedFn
          { name =
              RT.FQFnName.TraitMethod
                { trait_ = hashRT
                  method_ = "show"
                  implFn = RT.FQFnName.FromTypeParam "a" }
            typeSymbolTable = RT.TST.empty
            typeArgs = []
            access = None
            argsSoFar = []
            boundImpls = [] }
      ) ]

module ProgramTypes =
  open PT

  let signs = [ Sign.Positive; Sign.Negative ]

  let fqFnNames : List<FQFnName.FQFnName> =
    [ FQFnName.Builtin { name = "int64Increment"; version = 1 }
      FQFnName.Package hashPT ]


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
      MPInt(id, System.Numerics.BigInteger.Parse "123456789012345678901234567890")
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
        TInt
        TString
        TList TInt64
        TTuple(TBool, TBool, [ TBool ])
        TDict(TString, TBool)
        TDB TBool
        TCustomType(NameResolution.ok (FQTypeName.Package hashPT), [ TBool ])
        TCustomType(NameResolution.ok (FQTypeName.Package hashPT), [ TBool ])
        TVariable "test"
        TFn(NEList.singleton TBool, TBool) ]
    )



  // Note: this is aimed to contain all cases of `Expr`
  let expr =
    let e = EUnwrap(id, EInt64(id, 5))
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
                            NameResolution.ok (
                              FQFnName.Builtin
                                { name = "int64ToString"; version = 0 }
                            ),
                            []
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
                            EInt64(id, 6L),
                            FQFnName.Unknown
                          ),
                          EInfix(
                            id,
                            InfixFnCall(ArithmeticPlus),
                            EInt64(id, 5L),
                            EInt64(id, 2L),
                            FQFnName.Unknown
                          ),
                          Some(
                            ELambda(
                              id,
                              NEList.singleton (LPVariable(id, "y")),
                              EInfix(
                                id,
                                InfixFnCall(ArithmeticPlus),
                                EVariable(id, "y"),
                                EArg(id, 0),
                                FQFnName.Unknown
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
                                  NameResolution.ok (
                                    FQFnName.Builtin
                                      { name = "int64Add"; version = 0 }
                                  ),
                                  []
                                ),
                                [],
                                NEList.doubleton (EInt64(id, 6L)) (EInt64(id, 2L))
                              ),
                              FQFnName.Unknown
                            ),
                            EList(
                              id,
                              [ EInt64(id, 5L); EInt64(id, 6L); EInt64(id, 7L) ]
                            ),
                            FQFnName.Unknown
                          )
                        )
                      ),
                      ELet(
                        id,
                        LPVariable(id, "r"),
                        ERecord(
                          id,
                          NameResolution.ok (FQTypeName.Package hashPT),
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
                                     EVariable(id, "y"),
                                     FQFnName.Unknown
                                   )
                                 )
                                 EPipeInfix(
                                   id,
                                   InfixFnCall(ArithmeticPlus),
                                   EInt64(id, 2L),
                                   FQFnName.Unknown
                                 )
                                 EPipeFnCall(
                                   id,
                                   NameResolution.ok (
                                     FQFnName.Builtin
                                       { name = "int64Add"; version = 0 }
                                   ),
                                   [],
                                   [ (EInt64(id, 6L)); (EInt64(id, 2L)) ],
                                   []
                                 ) ]
                             ))
                            ("enum",
                             EEnum(
                               id,
                               NameResolution.ok (FQTypeName.Package hashPT),
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
                                  NameResolution.ok (
                                    FQFnName.Builtin
                                      { name = "modFunction"; version = 2 }
                                  ),
                                  []
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
                                      EVariable(id, "var"),
                                      FQFnName.Unknown
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
                                      EBool(id, false),
                                      FQFnName.Unknown
                                    ),
                                    ELet(
                                      id,
                                      LPVariable(id, "dict"),
                                      EDict(
                                        id,
                                        [ (EString(id, [ StringText "a string" ]),
                                           EInt64(id, 2L)) ]
                                      ),
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
        PT.EUInt128(id, 1Z)
        PT.EInt(id, 5I)
        PT.EInt(
          id,
          System.Numerics.BigInteger.Parse "123456789012345678901234567890"
        ) ]
    )


  // Handler test values are gone with the Handler delete.
  let userDB : DB.T = { tlid = 0UL; name = "User"; version = 0; typ = typeReference }

  let userDBs : List<DB.T> = [ userDB ]

  // TODO: serialize stdlib types?
  // (also make sure we roundtrip test them)

  let packageFn : PackageFn.PackageFn =
    { hash = hashPT
      body = expr
      typeParams = [ "a" ]
      parameters =
        NEList.singleton
          { name = "param"; typ = typeReference; description = "desc" }
      returnType = typeReference
      description = "test"
      permissionCeiling = Some(Set.singleton LibExecution.Effects.Effect.Clock)
      bounds = [] }

  /// `let f<'a: Show + Equal<Int>> ...`: two bounds on one param, one with a type arg,
  /// and a body that calls a trait method.
  let boundedPackageFn : PackageFn.PackageFn =
    let showRef : TraitRef =
      { trait_ = NameResolution.ok (FQTraitName.Package(Hash "trait-show"))
        typeArgs = [] }
    let eqRef : TraitRef =
      { trait_ = NameResolution.ok (FQTraitName.Package(Hash "trait-eq"))
        typeArgs = [ TInt ] }
    let traitCall =
      EApply(
        7001UL,
        EFnName(
          7002UL,
          NameResolution.ok (
            // With an implementation chosen, which is what a saved call carries.
            FQFnName.TraitMethod
              { trait_ = Hash "trait-show"
                method_ = "show"
                implFn =
                  FQFnName.Chosen
                    { name = Hash "impl-show-fn"
                      location =
                        Some { owner = "Tests"; modules = [ "Show" ]; name = "show" } } }
          ),
          // and what the CALLER worked out for the callee's bound
          [ { param = "a"
              trait_ = Hash "trait-show"
              method_ = "show"
              choice =
                FQFnName.Chosen
                  { name = Hash "impl-show-fn"
                    location =
                      Some { owner = "Tests"; modules = [ "Show" ]; name = "show" } } } ]
        ),
        [],
        NEList.singleton (EArg(7003UL, 0))
      )
    { hash = Hash "bounded-fn"
      body = traitCall
      typeParams = [ "a" ]
      parameters =
        NEList.singleton { name = "value"; typ = TVariable "a"; description = "" }
      returnType = TString
      description = "bounded"
      permissionCeiling = None
      bounds = [ { param = "a"; trait_ = showRef }; { param = "a"; trait_ = eqRef } ] }

  /// The cases `expr`, `typeReference` and the lists above do not reach, in one function, so the golden
  /// corpus covers every tag a live PT writer can emit (the `Deprecation` writer has no caller). Found by auditing each writer against these values;
  /// add here rather than to `expr`, whose own golden bytes would otherwise move.
  let coveragePackageFn : PackageFn.PackageFn =
    let at = Some { owner = "Tests"; modules = [ "Cover" ]; name = "it" }
    let failed
      (originalName : List<string>)
      (e : NameResolutionError)
      : NameResolution<'a> =
      { originalName = originalName; resolved = Error e }
    let infixes =
      [ ArithmeticPlus
        ArithmeticMinus
        ArithmeticMultiply
        ArithmeticDivide
        ArithmeticModulo
        ArithmeticPower
        BitwiseAnd
        BitwiseOr
        BitwiseXor
        ShiftLeft
        ShiftRight
        ComparisonGreaterThan
        ComparisonGreaterThanOrEqual
        ComparisonLessThan
        ComparisonLessThanOrEqual
        ComparisonEquals
        ComparisonNotEquals
        StringConcat ]
      |> List.map (fun f ->
        EInfix(id, InfixFnCall f, EInt64(id, 1L), EInt64(id, 2L), FQFnName.Unknown))
    let body =
      EList(
        id,
        infixes
        @ [ EInfix(
              id,
              BinOp BinOpAnd,
              EBool(id, true),
              EBool(id, false),
              FQFnName.Unknown
            )
            EInfix(
              id,
              BinOp BinOpOr,
              EBool(id, true),
              EBool(id, false),
              FQFnName.FromTypeParam "a"
            )
            ELet(
              id,
              LPUnit id,
              EUnit id,
              ELet(id, LPWildcard id, EUnit id, EUnit id)
            )
            EMatch(
              id,
              EUnit id,
              matchPatterns
              |> List.map (fun p ->
                { pat = p; whenCondition = None; rhs = EUnit id })
            )
            EIf(id, EBool(id, true), EUnit id, None)
            EValue(
              id,
              NameResolution.ok (FQValueName.Builtin { name = "pi"; version = 0 })
            )
            EValue(
              id,
              { originalName = [ "Tests"; "v" ]
                resolved = Ok { name = FQValueName.Package hashPT; location = at } }
            )
            ESelf id
            EFnName(id, failed [ "Nope"; "missing" ] NotFound, [])
            EFnName(id, failed [ "not a name" ] InvalidName, [])
            EPipe(
              id,
              EUnit id,
              [ EPipeEnum(
                  id,
                  NameResolution.ok (FQTypeName.Package hashPT),
                  "Some",
                  [ EUnit id ]
                )
                EPipeFnCall(
                  id,
                  NameResolution.ok (FQFnName.Package hashPT),
                  [],
                  [],
                  [ { param = "a"
                      trait_ = Hash "trait-show"
                      method_ = "show"
                      choice =
                        FQFnName.Chosen
                          { name = Hash "impl-show-fn"; location = None } } ]
                ) ]
            ) ]
      )
    { hash = Hash "coverage-fn"
      body = body
      typeParams = [ "a" ]
      parameters =
        NEList.singleton
          { name = "x"
            typ = TTuple(TDateTime, TChar, [ TUuid; TBlob; TStream TInt64 ])
            description = "" }
      returnType = TUnit
      description = "every PT tag the other values miss"
      permissionCeiling = None
      bounds = [] }

  let packageFns = [ packageFn; boundedPackageFn; coveragePackageFn ]

  let packageType : PackageType.PackageType =
    { hash = hashPT
      declaration =
        { typeParams = [ "a" ]
          bounds = []
          definition =
            TypeDeclaration.Enum(
              NEList.ofList
                { name = "caseA"; fields = []; description = "" }
                [ { name = "caseB"
                    fields =
                      [ { typ = typeReference; label = Some "i"; description = "" } ]
                    description = "" } ]
            ) }

      description = "test" }

  /// `type Set<'a: Compare> = List<'a>`
  let boundedPackageType : PackageType.PackageType =
    { hash = Hash "bounded-type"
      declaration =
        { typeParams = [ "a" ]
          bounds =
            [ { param = "a"
                trait_ =
                  { trait_ =
                      NameResolution.ok (FQTraitName.Package(Hash "trait-ord"))
                    typeArgs = [] } } ]
          definition = TypeDeclaration.Alias(TList(TVariable "a")) }
      description = "bounded" }

  /// A record, and an enum field with no label: the two type shapes the others miss.
  let recordPackageType : PackageType.PackageType =
    { hash = Hash "record-type"
      declaration =
        { typeParams = []
          bounds = []
          definition =
            TypeDeclaration.Record(
              NEList.singleton { name = "f"; typ = TInt64; description = "" }
            ) }
      description = "record" }

  let unlabelledEnumPackageType : PackageType.PackageType =
    { hash = Hash "unlabelled-enum-type"
      declaration =
        { typeParams = []
          bounds = []
          definition =
            TypeDeclaration.Enum(
              NEList.singleton
                { name = "only"
                  fields = [ { typ = TInt64; label = None; description = "" } ]
                  description = "" }
            ) }
      description = "unlabelled" }

  let packageTypes =
    [ packageType; boundedPackageType; recordPackageType; unlabelledEnumPackageType ]

  let packageValue : PT.PackageValue.PackageValue =
    { hash = Hash ""; body = constValue; description = "test" }

  let packageValues = [ packageValue ]

  /// `trait Convert<'a, 'b: Equal> = let convert (v: 'a) :{} 'b`, with a doc.
  let trait_ : Trait.Trait =
    { hash = Hash "trait-convert"
      typeParams = NEList.doubleton "a" "b"
      bounds =
        [ { param = "b"
            trait_ =
              { trait_ = NameResolution.ok (FQTraitName.Package(Hash "trait-eq"))
                typeArgs = [] } } ]
      methods =
        NEList.singleton
          { name = "convert"
            typeParams = []
            // a method-level bound, so the round trip has to carry one
            bounds =
              [ { param = "b"
                  trait_ =
                    { trait_ =
                        NameResolution.ok (FQTraitName.Package(Hash "trait-equal"))
                      typeArgs = [] } } ]
            parameters =
              NEList.singleton { name = "v"; typ = TVariable "a"; description = "" }
            returnType = TVariable "b"
            permissionCeiling = Some Set.empty
            description = "the method" }
      description = "a trait" }

  let traits = [ trait_ ]

  /// `impl<'a: Show> Convert<Int> for List<'a>` naming one method fn.
  let impl : TraitImpl.TraitImpl =
    { hash = Hash "impl-convert-list"
      trait_ = NameResolution.ok (FQTraitName.Package(Hash "trait-convert"))
      traitTypeArgs = [ TInt ]
      self = TList(TVariable "a")
      typeParams = [ "a" ]
      bounds =
        [ { param = "a"
            trait_ =
              { trait_ = NameResolution.ok (FQTraitName.Package(Hash "trait-show"))
                typeArgs = [] } } ]
      methods =
        [ ("convert", NameResolution.ok (FQFnName.Package(Hash "fn-convert"))) ]
      description = "an impl" }

  let impls = [ impl ]

  let packageLocation : PackageLocation =
    { owner = "Darklang"; modules = [ "Stdlib"; "List" ]; name = "map" }

  let packageLocations : List<PackageLocation> =
    [ packageLocation
      { owner = "MyOrg"; modules = []; name = "helper" }
      { owner = "Test"; modules = [ "Nested"; "Module" ]; name = "value" } ]

  /// Every `PackageOp` case, and every shape within a case that the writer branches on.
  ///
  /// This is the format two machines must agree on byte for byte to converge, and the fold covers it
  /// only indirectly: a case the fold never reaches (a `Decision` kind, a `BranchEvent`, a `SetName`
  /// carrying a predecessor) needs its value here or it is asserted nowhere.
  ///
  /// The shapes that matter, beyond one of each case: `SetName`/`Unbind` with and without `previous`,
  /// which is a presence byte rather than a sentinel hash; all three `DecisionKind`s; both
  /// `BranchEventKind`s, one of which carries a list of ids; and a `Hash` that is not 64 hex
  /// characters, which takes the writer's fallback branch.
  let packageOps : List<PackageOp> =
    let loc = packageLocation
    let otherLoc : PackageLocation =
      { owner = "MyOrg"; modules = []; name = "helper" }
    let shortHash = Hash "beef"
    let branchId =
      PT.BranchId.Id(System.Guid.Parse "3f2504e0-4f89-11d3-9a0c-0305e82c3301")

    [ AddType packageTypes[0]
      AddType boundedPackageType
      AddFn boundedPackageFn
      AddValue packageValues[0]
      AddFn packageFns[0]
      AddTrait trait_
      AddTraitImpl impl

      SetName(loc, Reference.PackageFn hashPT, None)
      SetName(otherLoc, Reference.PackageTrait hashPT, None)
      SetName(otherLoc, Reference.PackageTraitImpl shortHash, Some hashPT)
      SetName(loc, Reference.PackageFn hashPT, Some hashPT)
      SetName(otherLoc, Reference.PackageType hashPT, Some shortHash)
      SetName(otherLoc, Reference.PackageValue shortHash, None)

      Unbind(loc, None)
      Unbind(otherLoc, Some hashPT)

      Deprecate(Reference.PackageFn hashPT, DeprecationKind.Obsolete, "gone", None)
      Deprecate(Reference.PackageFn hashPT, DeprecationKind.Harmful, "", None)
      Deprecate(
        Reference.PackageType hashPT,
        DeprecationKind.SupersededBy(Reference.PackageType shortHash),
        "use the other one",
        None
      )
      // Restatements: the same deprecation said again after an undeprecate, and the undeprecate
      // said again after that, each made a distinct op by its stamp. The stamp rides on the
      // existing tags as a trailing field, so these two also pin that the reader finds it.
      Deprecate(
        Reference.PackageFn hashPT,
        DeprecationKind.Obsolete,
        "gone",
        Some "2026-01-01T00:00:00.000Z-0001"
      )
      Undeprecate(Reference.PackageValue hashPT, None)
      Undeprecate(
        Reference.PackageValue hashPT,
        Some "2026-01-01T00:00:00.000Z-0002"
      )
      UpdateDoc(loc, DocPart.WholeItem, "What it says about itself.", None, None)
      // The empty text is a real op: it is how a doc is cleared.
      UpdateDoc(loc, DocPart.WholeItem, "", Some shortHash, None)
      // A restatement: the same text said again, made a distinct op by its stamp.
      UpdateDoc(
        loc,
        DocPart.WholeItem,
        "What it says about itself.",
        None,
        Some "2026-09-08T22:00:00.000Z"
      )
      // One per nested part, because each carries a name or an index the whole-item case does not,
      // and a serializer that dropped it would still round-trip the ones above.
      UpdateDoc(
        loc,
        DocPart.RecordField "theField",
        "what the field is for",
        Some hashPT,
        None
      )
      UpdateDoc(
        loc,
        DocPart.EnumCase "TheCase",
        "when this case applies",
        None,
        None
      )
      UpdateDoc(loc, DocPart.Parameter 2, "what to pass", Some shortHash, None)

      Decision(
        "d1",
        loc,
        "kept mine",
        DecisionKind.Override(Reference.PackageFn hashPT)
      )
      Decision("d2", loc, "", DecisionKind.Ack "finding-7")
      BranchEvent(branchId, BranchEventKind.Archived, "2026-01-01T00:00:00.000Z")
      BranchEvent(branchId, BranchEventKind.Merged [], "2026-01-01T00:00:00.000Z")
      BranchEvent(
        branchId,
        BranchEventKind.Merged
          [ System.Guid.Parse "7c9e6679-7425-40de-944b-e07fc1f90ae7"
            System.Guid.Parse "3f2504e0-4f89-11d3-9a0c-0305e82c3300" ],
        "2026-01-01T00:00:00.000Z"
      )
      // The retired pin/follow decisions, one per policy: real stores hold them, so they must decode.
      Decision("p1", loc, "", DecisionKind.Propagation PropagationPolicy.Pin)
      Decision("p2", loc, "", DecisionKind.Propagation PropagationPolicy.Follow)
      Decision("p3", loc, "", DecisionKind.Propagation PropagationPolicy.Unset) ]

  let toplevels : List<DB.T> = [ userDB ]
