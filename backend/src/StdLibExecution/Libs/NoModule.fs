module StdLibExecution.Libs.NoModule

open Prelude
open System

module DvalReprDeveloper = LibExecution.DvalReprDeveloper

open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts


let rec equals (types : Types) (a : Dval) (b : Dval) : bool =
  let equals = equals types
  match a, b with
  | DInt a, DInt b -> a = b
  | DFloat a, DFloat b -> a = b
  | DBool a, DBool b -> a = b
  | DUnit, DUnit -> true
  | DString a, DString b -> a = b
  | DChar a, DChar b -> a = b
  | DList a, DList b -> a.Length = b.Length && List.forall2 equals a b
  | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
    if a3.Length <> b3.Length then // special case - this is a type error
      Exception.raiseCode "tuples must be the same length"
    else
      equals a1 b1 && equals a2 b2 && List.forall2 equals a3 b3
  | DDict a, DDict b ->
    Map.count a = Map.count b
    && Map.forall
      (fun k v ->
        Map.tryFind k b |> Option.map (equals v) |> Option.defaultValue false)
      a
  | DRecord(tn1, a), DRecord(tn2, b) ->
    // CLEANUP: use the resolved type, not a type that may be an alias of something else
    tn1 = tn2
    && Map.count a = Map.count b
    && Map.forall
      (fun k v ->
        Map.tryFind k b |> Option.map (equals v) |> Option.defaultValue false)
      a
  | DFnVal a, DFnVal b ->
    match a, b with
    | Lambda a, Lambda b -> equalsLambdaImpl types a b
  | DDateTime a, DDateTime b -> a = b
  | DPassword _, DPassword _ -> false
  | DUuid a, DUuid b -> a = b
  | DBytes a, DBytes b -> a = b
  | DDB a, DDB b -> a = b
  | DEnum(a1, a2, a3), DEnum(b1, b2, b3) ->
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
  | DPassword _, _
  | DUuid _, _
  | DBytes _, _
  | DDB _, _
  | DEnum _, _
  | DError _, _
  | DIncomplete _, _ -> Exception.raiseCode "Both values must be the same type"

and equalsLambdaImpl
  (types : Types)
  (impl1 : LambdaImpl)
  (impl2 : LambdaImpl)
  : bool =
  impl1.parameters.Length = impl2.parameters.Length
  && List.forall2
    (fun (_, str1) (_, str2) -> str1 = str2)
    impl1.parameters
    impl2.parameters
  && equalsSymtable types impl1.symtable impl2.symtable
  && equalsExpr impl1.body impl2.body

and equalsSymtable (types : Types) (a : Symtable) (b : Symtable) : bool =
  Map.count a = Map.count b
  && Map.forall
    (fun k v ->
      Map.tryFind k b |> Option.map (equals types v) |> Option.defaultValue false)
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
  | ELet(_, pattern1, expr1, body1), ELet(_, pattern2, expr2, body2) ->
    equalsLetPattern pattern1 pattern2
    && equalsExpr expr1 expr2
    && equalsExpr body1 body2
  | EIf(_, cond1, then1, else1), EIf(_, cond2, then2, else2) ->
    equalsExpr cond1 cond2 && equalsExpr then1 then2 && equalsExpr else1 else2
  | ELambda(_, parameters1, body1), ELambda(_, parameters2, body2) ->
    parameters1.Length = parameters2.Length
    && List.forall2 (fun (_, str1) (_, str2) -> str1 = str2) parameters1 parameters2
    && equalsExpr body1 body2
  | EFieldAccess(_, target1, fieldName1), EFieldAccess(_, target2, fieldName2) ->
    equalsExpr target1 target2 && fieldName1 = fieldName2
  | EVariable(_, name1), EVariable(_, name2) -> name1 = name2
  | EApply(_, name1, typeArgs1, args1), EApply(_, name2, typeArgs2, args2) ->
    name1 = name2
    && List.forall2 (=) typeArgs1 typeArgs2
    && List.forall2 equalsExpr args1 args2
  | EList(_, elems1), EList(_, elems2) ->
    elems1.Length = elems2.Length && List.forall2 equalsExpr elems1 elems2
  | ETuple(_, elem1_1, elem2_1, elems1), ETuple(_, elem1_2, elem2_2, elems2) ->
    equalsExpr elem1_1 elem1_2
    && equalsExpr elem2_1 elem2_2
    && elems1.Length = elems2.Length
    && List.forall2 equalsExpr elems1 elems2
  | ERecord(_, typeName, fields1), ERecord(_, typeName', fields2) ->
    typeName = typeName'
    && fields1.Length = fields2.Length
    && List.forall2
      (fun (name1, expr1) (name2, expr2) -> name1 = name2 && equalsExpr expr1 expr2)
      fields1
      fields2
  | ERecordUpdate(_, record1, updates1), ERecordUpdate(_, record2, updates2) ->
    record1 = record2
    && updates1.Length = updates2.Length
    && List.forall2
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
    && cases1.Length = cases2.Length
    && List.forall2
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
  | ELet _, _
  | EIf _, _
  | ELambda _, _
  | EFieldAccess _, _
  | EVariable _, _
  | EApply _, _
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

  | LPTuple(_, first, second, theRest), LPTuple(_, first', second', theRest') ->
    let all = first :: second :: theRest
    let all' = first' :: second' :: theRest'
    all.Length = all'.Length && List.forall2 equalsLetPattern all all'

  | LPTuple _, LPVariable _
  | LPVariable _, LPTuple _ -> false

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

let fn = fn []

let fns : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varA "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | state, _, [ a; b ] ->
          let types = ExecutionState.availableTypes state
          equals types a b |> DBool |> Ply
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
        | state, _, [ a; b ] ->
          let availableTypes = ExecutionState.availableTypes state
          equals availableTypes a b |> not |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or a DError if Nothing"
      fn =
        (function
        | _,
          _,
          [ DEnum(FQName.Package({ owner = "Darklang"
                                   modules = { Head = "Stdlib"; Tail = [ "Option" ] }
                                   name = TypeName.TypeName "Option"
                                   version = 0 }),
                  caseName,
                  [ value ]) ] ->
          uply {
            match caseName with
            | "Just" -> return value
            | "Nothing" ->
              return DError(SourceNone, LibExecution.DvalReprDeveloper.toRepr value)
            | _ -> return (DError(SourceNone, "Invalid Result"))
          }
        | _,
          _,
          [ DEnum(FQName.Package({ owner = "Darklang"
                                   modules = { Head = "Stdlib"; Tail = [ "Result" ] }
                                   name = TypeName.TypeName "Result"
                                   version = 0 }),
                  caseName,
                  [ value ]) ] ->
          uply {
            match caseName with
            | "Ok" -> return value
            | "Error" ->
              return DError(SourceNone, LibExecution.DvalReprDeveloper.toRepr value)
            | _ -> return (DError(SourceNone, "Invalid Result"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    // { name = fn "AWS" "urlencode" 0
    //   typeParams = []
    //   parameters = [ Param.make "str" TString "" ]
    //   returnType = TString
    //   description = "Url encode a string per AWS' requirements"
    //   fn =
    //     (function
    //     | _, _, [ DString s ] ->
    //       // Based on the original OCaml implementation which was slightly modified from
    //       // https://github.com/mirage/ocaml-cohttp/pull/294/files (to use
    //       // Buffer.add_string instead of add_bytes); see also
    //       // https://github.com/mirage/ocaml-uri/issues/65. It's pretty much a straight
    //       // up port from the Java example at
    //       // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html,
    //       // which calls it UriEncode
    //       let sb = new Text.StringBuilder()

    //       // Percent encode the path as s3 wants it. Uri doesn't
    //       // encode $, or the other sep characters in a path.
    //       // If upstream allows that we can nix this function
    //       let bytes = UTF8.toBytes s
    //       let n = Array.length bytes

    //       let is_hex (ch : byte) =
    //         (ch >= byte 'A' && ch <= byte 'Z')
    //         || (ch >= byte 'a' && ch <= byte 'z')
    //         || (ch >= byte '0' && ch <= byte '9')

    //       let is_special (ch : byte) =
    //         ch = byte '_'
    //         || ch = byte '-'
    //         || ch = byte '~'
    //         || ch = byte '.'
    //         || ch = byte '/'


    //       for i = 0 to n - 1 do
    //         let (c : byte) = bytes[i]

    //         if ((is_hex c) || (is_special c)) then
    //           sb.Append(char c) |> ignore<Text.StringBuilder>
    //         elif (bytes[i] = byte '%') then
    //           // We're expecting already escaped strings so ignore the escapes
    //           if i + 2 < n then
    //             if is_hex bytes[i + 1] && is_hex bytes[i + 2] then
    //               sb.Append(char c) |> ignore<Text.StringBuilder>
    //             else
    //               sb.Append "%25" |> ignore<Text.StringBuilder>
    //         else
    //           sb.Append(c |> char |> int |> sprintf "%%%X")
    //           |> ignore<Text.StringBuilder>

    //       sb |> string |> DString |> Ply
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    // { name = fn "Twitter" "urlencode" 0
    //   typeParams = []
    //   parameters = [ Param.make "s" TString "" ]
    //   returnType = TString
    //   description = "Url encode a string per Twitter's requirements"
    //   fn =
    //     (function
    //     | _, _, [ DString s ] -> s |> Uri.EscapeDataString |> DString |> Ply
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }
    ]

let contents = (fns, types)
