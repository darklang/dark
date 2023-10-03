module BuiltinExecution.Libs.NoModule

open Prelude
open System

module DvalReprDeveloper = LibExecution.DvalReprDeveloper

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module Dval = LibExecution.Dval


let rec equals (a : Dval) (b : Dval) : bool =
  match a, b with
  | DInt a, DInt b -> a = b
  | DFloat a, DFloat b -> a = b
  | DBool a, DBool b -> a = b
  | DUnit, DUnit -> true
  | DString a, DString b -> a = b
  | DChar a, DChar b -> a = b
  | DList(typA, a), DList(typB, b) ->
    Result.isOk (Dval.mergeValueTypes typA typB)
    && a.Length = b.Length
    && List.forall2 equals a b
  | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
    if a3.Length <> b3.Length then // special case - this is a type error
      raiseString "tuples must be the same length"
    else
      equals a1 b1 && equals a2 b2 && List.forall2 equals a3 b3
  | DDict(_vtTODO1, a), DDict(_vtTODO2, b) ->
    Map.count a = Map.count b
    && Map.forall
      (fun k v -> Map.find k b |> Option.map (equals v) |> Option.defaultValue false)
      a
  | DRecord(tn1, _, _typeArgsTODO1, a), DRecord(tn2, _, _typeArgsTODO2, b) ->
    tn1 = tn2 // these should be the fully resolved type
    && Map.count a = Map.count b
    && Map.forall
      (fun k v -> Map.find k b |> Option.map (equals v) |> Option.defaultValue false)
      a
  | DFnVal a, DFnVal b ->
    match a, b with
    | Lambda a, Lambda b -> equalsLambdaImpl a b
    | NamedFn a, NamedFn b -> a = b
    | Lambda _, _
    | NamedFn _, _ -> false
  | DDateTime a, DDateTime b -> a = b
  | DUuid a, DUuid b -> a = b
  | DBytes a, DBytes b -> a = b
  | DDB a, DDB b -> a = b
  | DEnum(a1, _, _typeArgsTODO1, a2, a3), DEnum(b1, _, _typeArgsTODO2, b2, b3) -> // these should be the fully resolved type
    a1 = b1 && a2 = b2 && a3.Length = b3.Length && List.forall2 equals a3 b3

  // exhaustiveness check
  | DInt _, _
  | DFloat _, _
  | DBool _, _
  | DUnit, _
  | DString _, _
  | DChar _, _
  | DList _, _
  | DTuple _, _
  | DDict _, _
  | DRecord _, _
  | DFnVal _, _
  | DDateTime _, _
  | DUuid _, _
  | DBytes _, _
  | DDB _, _
  | DEnum _, _ -> raiseString "Both values must be the same type"

and equalsLambdaImpl (impl1 : LambdaImpl) (impl2 : LambdaImpl) : bool =
  // TODO what to do for TypeSymbolTable
  NEList.length impl1.parameters = NEList.length impl2.parameters
  && NEList.forall2
    (fun (_, str1) (_, str2) -> str1 = str2)
    impl1.parameters
    impl2.parameters
  && equalsSymtable impl1.symtable impl2.symtable
  && equalsExpr impl1.body impl2.body

and equalsSymtable (a : Symtable) (b : Symtable) : bool =
  Map.count a = Map.count b
  && Map.forall
    (fun k v -> Map.find k b |> Option.map (equals v) |> Option.defaultValue false)
    a

and equalsExpr (expr1 : Expr) (expr2 : Expr) : bool =
  match expr1, expr2 with
  | EInt(_, int1), EInt(_, int2) -> int1 = int2
  | EBool(_, bool1), EBool(_, bool2) -> bool1 = bool2
  | EString(_, segments1), EString(_, segments2) ->
    equalsStringSegments segments1 segments2
  | EChar(_, char1), EChar(_, char2) -> char1 = char2
  | EFloat(_, float1), EFloat(_, float2) -> float1 = float2
  | EUnit _, EUnit _ -> true
  | EConstant(_, name1), EConstant(_, name2) -> name1 = name2
  | ELet(_, pattern1, expr1, body1), ELet(_, pattern2, expr2, body2) ->
    equalsLetPattern pattern1 pattern2
    && equalsExpr expr1 expr2
    && equalsExpr body1 body2
  | EIf(_, cond1, then1, else1), EIf(_, cond2, then2, else2) ->
    equalsExpr cond1 cond2
    && equalsExpr then1 then2
    && match else1, else2 with
       | Some el1, Some el2 -> equalsExpr el1 el2
       | None, None -> true
       | _, _ -> false

  | ELambda(_, parameters1, body1), ELambda(_, parameters2, body2) ->
    NEList.length parameters1 = NEList.length parameters2
    && NEList.forall2
      (fun (_, str1) (_, str2) -> str1 = str2)
      parameters1
      parameters2
    && equalsExpr body1 body2
  | EFieldAccess(_, target1, fieldName1), EFieldAccess(_, target2, fieldName2) ->
    equalsExpr target1 target2 && fieldName1 = fieldName2
  | EVariable(_, name1), EVariable(_, name2) -> name1 = name2
  | EApply(_, name1, typeArgs1, args1), EApply(_, name2, typeArgs2, args2) ->
    equalsExpr name1 name2
    && List.forall2 (=) typeArgs1 typeArgs2
    && NEList.forall2 equalsExpr args1 args2
  | EFnName(_, name1), EFnName(_, name2) -> name1 = name2
  | EList(_, elems1), EList(_, elems2) ->
    elems1.Length = elems2.Length && List.forall2 equalsExpr elems1 elems2
  | ETuple(_, elem1_1, elem2_1, elems1), ETuple(_, elem1_2, elem2_2, elems2) ->
    equalsExpr elem1_1 elem1_2
    && equalsExpr elem2_1 elem2_2
    && elems1.Length = elems2.Length
    && List.forall2 equalsExpr elems1 elems2
  | ERecord(_, typeName, fields1), ERecord(_, typeName', fields2) ->
    typeName = typeName'
    && NEList.length fields1 = NEList.length fields2
    && NEList.forall2
      (fun (name1, expr1) (name2, expr2) -> name1 = name2 && equalsExpr expr1 expr2)
      fields1
      fields2
  | ERecordUpdate(_, record1, updates1), ERecordUpdate(_, record2, updates2) ->
    record1 = record2
    && NEList.length updates1 = NEList.length updates2
    && NEList.forall2
      (fun (name1, expr1) (name2, expr2) -> name1 = name2 && equalsExpr expr1 expr2)
      updates1
      updates2
  | EEnum(_, typeName, caseName, fields), EEnum(_, typeName', caseName', fields') ->
    typeName = typeName'
    && caseName = caseName'
    && fields.Length = fields'.Length
    && List.forall2 equalsExpr fields fields'
  | EMatch(_, target1, cases1), EMatch(_, target2, cases2) ->
    equalsExpr target1 target2
    && NEList.length cases1 = NEList.length cases2
    && NEList.forall2
      (fun (p1, e1) (p2, e2) -> equalsMatchPattern p1 p2 && equalsExpr e1 e2)
      cases1
      cases2
  | EAnd(_, lhs1, rhs1), EAnd(_, lhs2, rhs2) ->
    equalsExpr lhs1 lhs2 && equalsExpr rhs1 rhs2
  | EOr(_, lhs1, rhs1), EOr(_, lhs2, rhs2) ->
    equalsExpr lhs1 lhs2 && equalsExpr rhs1 rhs2
  | EDict(_, fields1), EDict(_, fields2) ->
    fields1.Length = fields2.Length
    && List.forall2
      (fun (k1, v1) (k2, v2) -> k1 = k2 && equalsExpr v1 v2)
      fields1
      fields2
  | EError(_, msg, exprs), EError(_, msg2, exprs2) ->
    msg = msg2 && List.forall2 equalsExpr exprs exprs2

  // exhaustiveness check
  | EInt _, _
  | EBool _, _
  | EString _, _
  | EChar _, _
  | EFloat _, _
  | EUnit _, _
  | EConstant _, _
  | ELet _, _
  | EIf _, _
  | ELambda _, _
  | EFieldAccess _, _
  | EVariable _, _
  | EApply _, _
  | EFnName _, _
  | EList _, _
  | ETuple _, _
  | ERecord _, _
  | ERecordUpdate _, _
  | EEnum _, _
  | EMatch _, _
  | EAnd _, _
  | EOr _, _
  | EDict _, _
  | EEnum _, _
  | EError _, _ -> false


and equalsLetPattern (pattern1 : LetPattern) (pattern2 : LetPattern) : bool =
  match pattern1, pattern2 with
  | LPVariable(_, name1), LPVariable(_, name2) -> name1 = name2
  | LPUnit _, LPUnit _ -> true

  | LPTuple(_, first, second, theRest), LPTuple(_, first', second', theRest') ->
    let all = first :: second :: theRest
    let all' = first' :: second' :: theRest'
    all.Length = all'.Length && List.forall2 equalsLetPattern all all'

  | LPTuple _, _
  | LPUnit _, _
  | LPVariable _, _ -> false

and equalsStringSegments
  (segments1 : List<StringSegment>)
  (segments2 : List<StringSegment>)
  : bool =
  segments1.Length = segments2.Length
  && List.forall2 equalsStringSegment segments1 segments2

and equalsStringSegment
  (segment1 : StringSegment)
  (segment2 : StringSegment)
  : bool =
  match segment1, segment2 with
  | StringText text1, StringText text2 -> text1 = text2
  | StringInterpolation expr1, StringInterpolation expr2 -> equalsExpr expr1 expr2
  // exhaustiveness check
  | StringText _, _
  | StringInterpolation _, _ -> false

and equalsMatchPattern (pattern1 : MatchPattern) (pattern2 : MatchPattern) : bool =
  match pattern1, pattern2 with
  | MPVariable(_, name1), MPVariable(_, name2) -> name1 = name2
  | MPEnum(_, tag1, args1), MPEnum(_, tag2, args2) ->
    tag1 = tag2
    && args1.Length = args2.Length
    && List.forall2 equalsMatchPattern args1 args2
  | MPInt(_, int1), MPInt(_, int2) -> int1 = int2
  | MPBool(_, bool1), MPBool(_, bool2) -> bool1 = bool2
  | MPChar(_, char1), MPChar(_, char2) -> char1 = char2
  | MPString(_, str1), MPString(_, str2) -> str1 = str2
  | MPFloat(_, float1), MPFloat(_, float2) -> float1 = float2
  | MPUnit _, MPUnit _ -> true
  | MPTuple(_, elem1_1, elem2_1, elems1), MPTuple(_, elem1_2, elem2_2, elems2) ->
    equalsMatchPattern elem1_1 elem1_2
    && equalsMatchPattern elem2_1 elem2_2
    && elems1.Length = elems2.Length
    && List.forall2 equalsMatchPattern elems1 elems2
  | MPList(_, elems1), MPList(_, elems2) ->
    elems1.Length = elems2.Length && List.forall2 equalsMatchPattern elems1 elems2
  | MPListCons(_, head, tail), MPListCons(_, head', tail') ->
    equalsMatchPattern head head' && equalsMatchPattern tail tail'
  // exhaustiveness check
  | MPVariable _, _
  | MPEnum _, _
  | MPInt _, _
  | MPBool _, _
  | MPChar _, _
  | MPString _, _
  | MPFloat _, _
  | MPUnit _, _
  | MPTuple _, _
  | MPListCons _, _
  | MPList _, _ -> false


let varA = TVariable "a"

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn []

let fns : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varA "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, _, [ a; b ] -> equals a b |> DBool |> Ply
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
        | _, _, [ a; b ] -> equals a b |> not |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _,
          _,
          [ DEnum(FQName.Package({ owner = "Darklang"
                                   modules = [ "Stdlib"; "Option" ]
                                   name = TypeName.TypeName "Option"
                                   version = 0 }),
                  _,
                  _typeArgsDEnumTODO,
                  caseName,
                  [ value ]) ] ->
          uply {
            match caseName with
            | "Some" -> return value
            | "None" ->
              return
                RuntimeError.oldError (DvalReprDeveloper.toRepr value)
                |> raiseUntargetedRTE
            | _ -> return raiseUntargetedRTE (RuntimeError.oldError "Invalid Option")
          }
        | _,
          _,
          [ DEnum(FQName.Package({ owner = "Darklang"
                                   modules = [ "Stdlib"; "Result" ]
                                   name = TypeName.TypeName "Result"
                                   version = 0 }),
                  _,
                  _typeArgsDEnumTODO,
                  caseName,
                  [ value ]) ] ->
          uply {
            match caseName with
            | "Ok" -> return value
            | "Error" ->
              return
                RuntimeError.oldError (DvalReprDeveloper.toRepr value)
                |> raiseUntargetedRTE
            | _ -> return raiseUntargetedRTE (RuntimeError.oldError "Invalid Option")
          }
        | _ ->
          RuntimeError.oldError "Unwrap called with non-Option/non-Result"
          |> raiseUntargetedRTE)
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let contents = (fns, types, constants)
