module BuiltinExecution.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageIDs = LibExecution.PackageIDs
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module RTE = RuntimeError


/// Note that type errors should be handled by the caller,
/// by using `Dval.toValueType`, attempting to merge the types,
/// and raising an RTE if the merge fails.
/// Any type mis-matches found in this fn will just return `false`.
let rec equals (a : Dval) (b : Dval) : bool =
  let r = equals

  match a, b with
  | DUnit, DUnit -> true

  | DBool a, DBool b -> a = b

  | DInt8 a, DInt8 b -> a = b
  | DUInt8 a, DUInt8 b -> a = b
  | DInt16 a, DInt16 b -> a = b
  | DUInt16 a, DUInt16 b -> a = b
  | DInt32 a, DInt32 b -> a = b
  | DUInt32 a, DUInt32 b -> a = b
  | DInt64 a, DInt64 b -> a = b
  | DUInt64 a, DUInt64 b -> a = b
  | DInt128 a, DInt128 b -> a = b
  | DUInt128 a, DUInt128 b -> a = b

  | DFloat a, DFloat b -> a = b

  | DChar a, DChar b -> a = b
  | DString a, DString b -> a = b

  | DDateTime a, DDateTime b -> a = b

  | DUuid a, DUuid b -> a = b

  | DList(typA, a), DList(typB, b) ->
    Result.isOk (ValueType.merge typA typB)
    && a.Length = b.Length
    && List.forall2 r a b

  | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
    if a3.Length <> b3.Length then // special case - this is a type error
      false
    else
      r a1 b1 && r a2 b2 && List.forall2 r a3 b3

  | DDict(_vtTODO1, a), DDict(_vtTODO2, b) ->
    Map.count a = Map.count b
    && Map.forall
      (fun k v -> Map.find k b |> Option.map (r v) |> Option.defaultValue false)
      a

  | DRecord(tn1, _, _typeArgsTODO1, a), DRecord(tn2, _, _typeArgsTODO2, b) ->
    tn1 = tn2 // these should be the fully resolved type
    && Map.count a = Map.count b
    && Map.forall
      (fun k v -> Map.find k b |> Option.map (r v) |> Option.defaultValue false)
      a

  | DEnum(a1, _, _typeArgsTODO1, a2, a3), DEnum(b1, _, _typeArgsTODO2, b2, b3) -> // these should be the fully resolved type
    a1 = b1 && a2 = b2 && a3.Length = b3.Length && List.forall2 r a3 b3

  // | DApplicable a, DApplicable b ->
  //   match a, b with
  //   | Lambda _a, Lambda _b ->
  //     //equalsLambdaImpl a b
  //     // TODO
  //     true
  //   | NamedFn _a, NamedFn _b ->
  //     //a = b
  //     // TODO
  //     true
  //   | Lambda _, _

  //   | NamedFn _, _ -> false

  // | DDB a, DDB b -> a = b

  // exhaustiveness check
  | DUnit, _
  | DBool _, _
  | DInt8 _, _
  | DUInt8 _, _
  | DInt16 _, _
  | DUInt16 _, _
  | DInt32 _, _
  | DUInt32 _, _
  | DInt64 _, _
  | DUInt64 _, _
  | DInt128 _, _
  | DUInt128 _, _
  | DFloat _, _
  | DChar _, _
  | DString _, _
  | DDateTime _, _
  | DUuid _, _
  | DList _, _
  | DTuple _, _
  | DDict _, _
  | DRecord _, _
  | DEnum _, _
  //| DApplicable _, _
  // | DDB _, _
   ->
    // type errors; should be caught above by the caller
    false

// and equalsLambdaImpl (impl1 : LambdaImpl) (impl2 : LambdaImpl) : bool =
//   // TODO what to do for TypeSymbolTable
//   NEList.length impl1.parameters = NEList.length impl2.parameters
//   && NEList.forall2
//     (fun p1 p2 -> equalsLetPattern p1 p2)
//     impl1.parameters
//     impl2.parameters
//   && equalsSymtable impl1.symtable impl2.symtable
//   && equalsExpr impl1.body impl2.body

// and equalsSymtable (a : Symtable) (b : Symtable) : bool =
//   Map.count a = Map.count b
//   && Map.forall
//     (fun k v -> Map.find k b |> Option.map (equals v) |> Option.defaultValue false)
//     a

// and equalsExpr (expr1 : Expr) (expr2 : Expr) : bool =
//   match expr1, expr2 with
//   | EInt64(_, int1), EInt64(_, int2) -> int1 = int2
//   | EUInt64(_, int1), EUInt64(_, int2) -> int1 = int2
//   | EInt8(_, int1), EInt8(_, int2) -> int1 = int2
//   | EUInt8(_, int1), EUInt8(_, int2) -> int1 = int2
//   | EInt16(_, int1), EInt16(_, int2) -> int1 = int2
//   | EUInt16(_, int1), EUInt16(_, int2) -> int1 = int2
//   | EInt32(_, int1), EInt32(_, int2) -> int1 = int2
//   | EUInt32(_, int1), EUInt32(_, int2) -> int1 = int2
//   | EInt128(_, int1), EInt128(_, int2) -> int1 = int2
//   | EUInt128(_, int1), EUInt128(_, int2) -> int1 = int2
//   | EBool(_, bool1), EBool(_, bool2) -> bool1 = bool2
//   | EString(_, segments1), EString(_, segments2) ->
//     equalsStringSegments segments1 segments2
//   | EChar(_, char1), EChar(_, char2) -> char1 = char2
//   | EFloat(_, float1), EFloat(_, float2) -> float1 = float2
//   | EUnit _, EUnit _ -> true
//   | EConstant(_, name1), EConstant(_, name2) -> name1 = name2
//   | ELet(_, pattern1, expr1, body1), ELet(_, pattern2, expr2, body2) ->
//     equalsLetPattern pattern1 pattern2
//     && equalsExpr expr1 expr2
//     && equalsExpr body1 body2
//   | EIf(_, cond1, then1, else1), EIf(_, cond2, then2, else2) ->
//     let equalsElseExpr else1 else2 =
//       match else1, else2 with
//       | Some else1, Some else2 -> equalsExpr else1 else2
//       | None, None -> true
//       | _, _ -> false
//     equalsExpr cond1 cond2 && equalsExpr then1 then2 && equalsElseExpr else1 else2

//   // | ELambda(_, pats1, body1), ELambda(_, pats2, body2) ->
//   //   NEList.length pats1 = NEList.length pats2
//   //   && NEList.forall2 (fun p1 p2 -> equalsLetPattern p1 p2) pats1 pats2
//   //   && equalsExpr body1 body2
//   // | ERecordFieldAccess(_, target1, fieldName1),
//   //   ERecordFieldAccess(_, target2, fieldName2) ->
//   //   equalsExpr target1 target2 && fieldName1 = fieldName2
//   | EVariable(_, name1), EVariable(_, name2) -> name1 = name2
//   | EApply(_, name1, typeArgs1, args1), EApply(_, name2, typeArgs2, args2) ->
//     equalsExpr name1 name2
//     && List.forall2 (=) typeArgs1 typeArgs2
//     && NEList.forall2 equalsExpr args1 args2
//   | EFnName(_, name1), EFnName(_, name2) -> name1 = name2
//   | EList(_, elems1), EList(_, elems2) ->
//     elems1.Length = elems2.Length && List.forall2 equalsExpr elems1 elems2
//   | ETuple(_, elem1_1, elem2_1, elems1), ETuple(_, elem1_2, elem2_2, elems2) ->
//     equalsExpr elem1_1 elem1_2
//     && equalsExpr elem2_1 elem2_2
//     && elems1.Length = elems2.Length
//     && List.forall2 equalsExpr elems1 elems2
//   // | ERecord(_, typeName, fields1), ERecord(_, typeName', fields2) ->
//   //   typeName = typeName'
//   //   && NEList.length fields1 = NEList.length fields2
//   //   && NEList.forall2
//   //     (fun (name1, expr1) (name2, expr2) -> name1 = name2 && equalsExpr expr1 expr2)
//   //     fields1
//   //     fields2
//   // | ERecordUpdate(_, record1, updates1), ERecordUpdate(_, record2, updates2) ->
//   //   record1 = record2
//   //   && NEList.length updates1 = NEList.length updates2
//   //   && NEList.forall2
//   //     (fun (name1, expr1) (name2, expr2) -> name1 = name2 && equalsExpr expr1 expr2)
//   //     updates1
//   //     updates2
//   // | EEnum(_, typeName, caseName, fields), EEnum(_, typeName', caseName', fields') ->
//   //   typeName = typeName'
//   //   && caseName = caseName'
//   //   && fields.Length = fields'.Length
//   //   && List.forall2 equalsExpr fields fields'
//   | EMatch(_, target1, cases1), EMatch(_, target2, cases2) ->
//     equalsExpr target1 target2
//     && NEList.length cases1 = NEList.length cases2
//     && NEList.forall2
//       (fun case1 case2 ->
//         let equalsWhenCondition when1 when2 =
//           match when1, when2 with
//           | Some when1, Some when2 -> equalsExpr when1 when2
//           | None, None -> true
//           | _, _ -> false
//         equalsMatchPattern case1.pat case2.pat
//         && equalsWhenCondition case1.whenCondition case2.whenCondition
//         && equalsExpr case1.rhs case2.rhs)
//       cases1
//       cases2
//   | EAnd(_, lhs1, rhs1), EAnd(_, lhs2, rhs2) ->
//     equalsExpr lhs1 lhs2 && equalsExpr rhs1 rhs2
//   | EOr(_, lhs1, rhs1), EOr(_, lhs2, rhs2) ->
//     equalsExpr lhs1 lhs2 && equalsExpr rhs1 rhs2
//   | EDict(_, fields1), EDict(_, fields2) ->
//     fields1.Length = fields2.Length
//     && List.forall2
//       (fun (k1, v1) (k2, v2) -> k1 = k2 && equalsExpr v1 v2)
//       fields1
//       fields2
//   | EError(_, msg, exprs), EError(_, msg2, exprs2) ->
//     msg = msg2 && List.forall2 equalsExpr exprs exprs2

//   // exhaustiveness check
//   | EInt64 _, _
//   | EUInt64 _, _
//   | EInt8 _, _
//   | EUInt8 _, _
//   | EInt16 _, _
//   | EUInt16 _, _
//   | EInt32 _, _
//   | EUInt32 _, _
//   | EInt128 _, _
//   | EUInt128 _, _
//   | EBool _, _
//   | EString _, _
//   | EChar _, _
//   | EFloat _, _
//   | EUnit _, _
//   // | EConstant _, _
//   | ELet _, _
//   | EIf _, _
//   // | ELambda _, _
//   // | ERecordFieldAccess _, _
//   | EVariable _, _
//   | EApply _, _
//   | EFnName _, _
//   | EList _, _
//   | ETuple _, _
//   // | ERecord _, _
//   // | ERecordUpdate _, _
//   // | EEnum _, _
//   | EMatch _, _
//   | EAnd _, _
//   | EOr _, _
//   | EDict _, _
//   // | EEnum _, _
//   | EError _, _ -> false


// and equalsLetPattern (pattern1 : LetPattern) (pattern2 : LetPattern) : bool =
//   match pattern1, pattern2 with
//   | LPVariable(_, name1), LPVariable(_, name2) -> name1 = name2
//   | LPUnit _, LPUnit _ -> true

//   | LPTuple(_, first, second, theRest), LPTuple(_, first', second', theRest') ->
//     let all = first :: second :: theRest
//     let all' = first' :: second' :: theRest'
//     all.Length = all'.Length && List.forall2 equalsLetPattern all all'

//   | LPTuple _, _
//   | LPUnit _, _
//   | LPVariable _, _ -> false

// and equalsStringSegments
//   (segments1 : List<StringSegment>)
//   (segments2 : List<StringSegment>)
//   : bool =
//   segments1.Length = segments2.Length
//   && List.forall2 equalsStringSegment segments1 segments2

// and equalsStringSegment
//   (segment1 : StringSegment)
//   (segment2 : StringSegment)
//   : bool =
//   match segment1, segment2 with
//   | StringText text1, StringText text2 -> text1 = text2
//   | StringInterpolation expr1, StringInterpolation expr2 -> equalsExpr expr1 expr2
//   // exhaustiveness check
//   | StringText _, _
//   | StringInterpolation _, _ -> false

// and equalsMatchPattern (pattern1 : MatchPattern) (pattern2 : MatchPattern) : bool =
//   match pattern1, pattern2 with
//   | MPVariable(_, name1), MPVariable(_, name2) -> name1 = name2
//   // | MPEnum(_, tag1, args1), MPEnum(_, tag2, args2) ->
//   //   tag1 = tag2
//   //   && args1.Length = args2.Length
//   //   && List.forall2 equalsMatchPattern args1 args2
//   | MPInt64(_, int1), MPInt64(_, int2) -> int1 = int2
//   | MPUInt64(_, int1), MPUInt64(_, int2) -> int1 = int2
//   | MPInt8(_, int1), MPInt8(_, int2) -> int1 = int2
//   | MPUInt8(_, int1), MPUInt8(_, int2) -> int1 = int2
//   | MPInt16(_, int1), MPInt16(_, int2) -> int1 = int2
//   | MPUInt16(_, int1), MPUInt16(_, int2) -> int1 = int2
//   | MPInt32(_, int1), MPInt32(_, int2) -> int1 = int2
//   | MPUInt32(_, int1), MPUInt32(_, int2) -> int1 = int2
//   | MPInt128(_, int1), MPInt128(_, int2) -> int1 = int2
//   | MPUInt128(_, int1), MPUInt128(_, int2) -> int1 = int2
//   | MPBool(_, bool1), MPBool(_, bool2) -> bool1 = bool2
//   | MPChar(_, char1), MPChar(_, char2) -> char1 = char2
//   | MPString(_, str1), MPString(_, str2) -> str1 = str2
//   | MPFloat(_, float1), MPFloat(_, float2) -> float1 = float2
//   | MPUnit _, MPUnit _ -> true
//   | MPTuple(_, elem1_1, elem2_1, elems1), MPTuple(_, elem1_2, elem2_2, elems2) ->
//     equalsMatchPattern elem1_1 elem1_2
//     && equalsMatchPattern elem2_1 elem2_2
//     && elems1.Length = elems2.Length
//     && List.forall2 equalsMatchPattern elems1 elems2
//   | MPList(_, elems1), MPList(_, elems2) ->
//     elems1.Length = elems2.Length && List.forall2 equalsMatchPattern elems1 elems2
//   | MPListCons(_, head, tail), MPListCons(_, head', tail') ->
//     equalsMatchPattern head head' && equalsMatchPattern tail tail'
//   // exhaustiveness check
//   | MPVariable _, _
//   // | MPEnum _, _
//   | MPInt64 _, _
//   | MPUInt64 _, _
//   | MPInt8 _, _
//   | MPUInt8 _, _
//   | MPInt16 _, _
//   | MPUInt16 _, _
//   | MPInt32 _, _
//   | MPUInt32 _, _
//   | MPInt128 _, _
//   | MPUInt128 _, _
//   | MPBool _, _
//   | MPChar _, _
//   | MPString _, _
//   | MPFloat _, _
//   | MPUnit _, _
//   | MPTuple _, _
//   | MPListCons _, _
//   | MPList _, _ -> false


let varA = TVariable "a"

let fns : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varA "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, vm, _, [ a; b ] ->
          let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
          match ValueType.merge vtA vtB with
          | Error _ ->
            raiseRTE vm.threadID (RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB))
          | Ok _ -> equals a b |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "notEquals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varA "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | _, vm, _, [ a; b ] ->
          let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
          match ValueType.merge vtA vtB with
          | Error _ ->
            raiseRTE vm.threadID (RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB))
          | Ok _ -> equals a b |> not |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated }


    // { name = fn "unwrap" 0
    //   typeParams = []
    //   parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
    //   returnType = TVariable "a"
    //   description =
    //     "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
    //   fn =
    //     (function
    //     | _, _, [] -> incorrectArgs ()
    //     | _, _, [ dval ] ->
    //       match dval with

    //       // success: extract `Some` out of an Option
    //       | DEnum(FQTypeName.Package id, _, _, "Some", [ value ]) when
    //         id = PackageIDs.Type.Stdlib.option
    //         ->
    //         Ply value

    //       // success: extract `Ok` out of a Result
    //       | DEnum(FQTypeName.Package id, _, _, "Ok", [ value ]) when
    //         id = PackageIDs.Type.Stdlib.result
    //         ->
    //         Ply value

    //       // Error: expected Some, got None
    //       | DEnum(FQTypeName.Package id, _, _, "None", []) when
    //         id = PackageIDs.Type.Stdlib.option
    //         ->
    //         "expected Some, got None" |> RuntimeError.oldError |> raiseUntargetedRTE

    //       // Error: expected Ok, got Error
    //       | DEnum(FQTypeName.Package id, _, _, "Error", [ value ]) when
    //         id = PackageIDs.Type.Stdlib.result
    //         ->
    //         $"expected Ok, got Error:\n{value |> DvalReprDeveloper.toRepr}"
    //         |> RuntimeError.oldError
    //         |> raiseUntargetedRTE


    //       // Error: single dval, but not an Option or Result
    //       | otherDval ->
    //         $"Unwrap called with non-Option/non-Result {otherDval}"
    //         |> RuntimeError.oldError
    //         |> raiseUntargetedRTE

    //     | _, _, multipleArgs ->
    //       $"unwrap called with multiple arguments: {multipleArgs}"
    //       |> RuntimeError.oldError
    //       |> raiseUntargetedRTE)

    //   sqlSpec = NotQueryable
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    // { name = fn "debug" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "label" TString "The label to be printed."
    //       Param.make "value" (TVariable "a") "The value to be printed." ]
    //   returnType = TUnit
    //   description = "Prints the given <param value> to the standard output"
    //   fn =
    //     (function
    //     | _, _, _, [ DString label; value ] ->
    //       // TODO: call upon the Dark equivalent fn instead of rlying on DvalReprDeveloper
    //       print $"DEBUG: {label} - {DvalReprDeveloper.toRepr value}"
    //       Ply DUnit
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotQueryable
    //   previewable = Impure
    //   deprecated = NotDeprecated }


    // { name = fn "debugSymbolTable" 0
    //   typeParams = []
    //   parameters = [ Param.make "unit" TUnit "" ]
    //   returnType = TUnit
    //   description = "Prints the current symbol table to the standard output"
    //   fn =
    //     (function
    //     | state, _, [ DUnit ] ->
    //       state.symbolTable
    //       |> Map.toList
    //       |> List.map (fun (key, dv) -> $"- {key}: {DvalReprDeveloper.toRepr dv}")
    //       |> String.concat "\n"
    //       |> fun lines -> print $"DEBUG: symTable\n{lines}"

    //       Ply DUnit
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotQueryable
    //   previewable = Impure
    //   deprecated = NotDeprecated }
    ]


let builtins = LibExecution.Builtin.make [] fns
