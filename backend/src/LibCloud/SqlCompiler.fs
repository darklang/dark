module LibCloud.SqlCompiler

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Npgsql.FSharp
open Npgsql
open Db

open Prelude

open LibExecution.RuntimeTypes

module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable
module TypeChecker = LibExecution.TypeChecker

module RuntimeTypesAst = LibExecution.RuntimeTypesAst

let errorTemplate =
  "You're using our new experimental Datastore query compiler. It compiles your lambdas into optimized (and partially indexed) Datastore queries, which should be reasonably fast.\n\nUnfortunately, we hit a snag while compiling your lambda. We only support a subset of Darklang's functionality, but will be expanding it in the future.\n\nSome Darklang code is not supported in DB::query lambdas for now, and some of it won't be supported because it's an odd thing to do in a datastore query. If you think your operation should be supported, let us know in #general in Discord.\n\n  Error: "

exception SqlCompilerException of string
exception SqlCompilerRuntimeError of internalError : RuntimeError

let error (str : string) : 'a = raise (SqlCompilerException(errorTemplate + str))

let error2 (msg : string) (v : string) : 'a = error $"{msg}: {v}"

let error3 (msg : string) (v1 : string) (v2 : string) : 'a =
  error $"{msg}:\n  {v1}\n  {v2}"

let uint64 (v : System.UInt64) : SqlValue =
  let typ = NpgsqlTypes.NpgsqlDbType.Numeric
  let idParam = NpgsqlParameter("uint64", typ)
  idParam.Value <- System.Numerics.BigInteger.op_Implicit (v)
  Sql.parameter idParam

let int128 (v : System.Int128) : SqlValue =
  let typ = NpgsqlTypes.NpgsqlDbType.Numeric
  let idParam = NpgsqlParameter("int128", typ)
  idParam.Value <- System.Numerics.BigInteger.op_Implicit (v)
  Sql.parameter idParam

let uint128 (v : System.UInt128) : SqlValue =
  let typ = NpgsqlTypes.NpgsqlDbType.Numeric
  let idParam = NpgsqlParameter("uint128", typ)
  idParam.Value <- System.Numerics.BigInteger.op_Implicit (v)
  Sql.parameter idParam


// This canonicalizes an expression, meaning it removes multiple ways of
// representing the same thing. Currently nothing needs to be canonicalized.
let rec canonicalize (expr : Expr) : Expr = expr

// Returns a typeReference since we don't always know what type it should have (eg is
// a polymorphic function is being called)
let rec dvalToSql
  (callStack : CallStack)
  (types : Types)
  (expectedType : TypeReference)
  (dval : Dval)
  : Ply<SqlValue * TypeReference> =
  uply {
    match expectedType, dval with
    | _, DFnVal _
    | _, DDB _
    | _, DDict _ // CLEANUP allow
    | _, DTuple _ -> // CLEANUP allow
      return error2 "This value is not yet supported" (DvalReprDeveloper.toRepr dval)

    // TYPEARGSTODO handle non-empty type args fpr all 4 branches below
    | TVariable _, DRecord(typeName, _, [], _)
    | TVariable _, DEnum(typeName, _, [], _, _) ->
      let! jsonString =
        DvalReprInternalQueryable.toJsonStringV0 callStack types expectedType dval
      return Sql.jsonb jsonString, TCustomType(Ok typeName, [])

    | TCustomType(_, []), DEnum _
    | TCustomType(_, []), DRecord _ ->
      let! jsonString =
        DvalReprInternalQueryable.toJsonStringV0 callStack types expectedType dval
      return Sql.jsonb jsonString, expectedType

    | TVariable _, DDateTime date
    | TDateTime, DDateTime date ->
      return
        (date |> LibExecution.DarkDateTime.toDateTimeUtc |> Sql.timestamptz,
         TDateTime)

    | TVariable _, DInt64 i
    | TInt64, DInt64 i -> return Sql.int64 i, TInt64

    | TVariable _, DUInt64 i
    | TUInt64, DUInt64 i -> return uint64 i, TUInt64

    | TVariable _, DInt8 i
    | TInt8, DInt8 i -> return Sql.int16 (int16 i), TInt8

    | TVariable _, DUInt8 i
    | TUInt8, DUInt8 i -> return Sql.int16 (int16 i), TUInt8

    | TVariable _, DInt16 i
    | TInt16, DInt16 i -> return Sql.int16 (int16 i), TInt16

    | TVariable _, DUInt16 i
    | TUInt16, DUInt16 i -> return Sql.int64 (int i), TUInt16

    | TVariable _, DInt32 i
    | TInt32, DInt32 i -> return Sql.int i, TInt32

    | TVariable _, DUInt32 i
    | TUInt32, DUInt32 i -> return Sql.int64 (int i), TUInt32

    | TVariable _, DInt128 i
    | TInt128, DInt128 i -> return int128 i, TInt128

    | TVariable _, DUInt128 i
    | TUInt128, DUInt128 i -> return uint128 i, TUInt128

    | TVariable _, DFloat v
    | TFloat, DFloat v -> return Sql.double v, TFloat

    | TVariable _, DBool b
    | TBool, DBool b -> return Sql.bool b, TBool

    | TVariable _, DString s
    | TString, DString s -> return Sql.string s, TString

    | TVariable _, DChar c
    | TChar, DChar c -> return Sql.string c, TChar

    | TVariable _, DUuid id
    | TUuid, DUuid id -> return Sql.uuid id, TUuid

    | TVariable _, DUnit
    | TUnit, DUnit -> return Sql.int64 0, TUnit

    // CLEANUP: add test first
    // | TList typ, DList l ->
    //   let typeName = DvalReprDeveloper.typeName typ
    //   let typeToNpgSqlType (t : DType) : NpgsqlTypes.NpgsqlDbType =
    //     match t with
    //     | TString -> NpgsqlTypes.NpgsqlDbType.Text
    //     | TInt64 -> NpgsqlTypes.NpgsqlDbType.Bigint
    //     | TFloat -> NpgsqlTypes.NpgsqlDbType.Double
    //     | TBool -> NpgsqlTypes.NpgsqlDbType.Boolean
    //     | TDateTime -> NpgsqlTypes.NpgsqlDbType.TimestampTz
    //     | TChar -> NpgsqlTypes.NpgsqlDbType.Text
    //     | TUuid -> NpgsqlTypes.NpgsqlDbType.Uuid
    //     | _ -> error $"We do not support this type of DB field yet: {t}"
    //   l
    //   |> List.map (fun v ->
    //     match v with
    //     | DString s -> s : obj
    //     | DInt64 i -> i
    //     | DFloat f -> f
    //     | DBool b -> b
    //     | DDateTime date -> string date
    //     | DChar c -> c
    //     | DUuid uuid -> string uuid
    //     | _ ->
    //       error2
    //         $"{typeName} list should contain {typeName} but contains"
    //         (DvalReprDeveloper.toRepr v))
    //   |> List.toArray
    //   |> Sql.array (typeToNpgSqlType typ)

    // exhaustiveness check
    | _, DInt64 _
    | _, DUInt64 _
    | _, DInt8 _
    | _, DUInt8 _
    | _, DInt16 _
    | _, DUInt16 _
    | _, DInt32 _
    | _, DUInt32 _
    | _, DInt128 _
    | _, DUInt128 _
    | _, DFloat _
    | _, DBool _
    | _, DString _
    | _, DChar _
    | _, DUuid _
    | _, DUnit
    | _, DList _
    | _, DRecord _
    | _, DEnum _
    | _, DDateTime _ ->
      return
        error3
          "This value is not of the expected type"
          (DvalReprDeveloper.toRepr dval)
          (DvalReprDeveloper.typeName expectedType)
  }




// TYPESTODO: combine with TypeChecker.unify, which needs to add unification
let rec typecheck
  (name : string)
  (actualType : TypeReference)
  (expectedType : TypeReference)
  : unit =
  match actualType, expectedType with
  | _, TVariable _ -> ()
  | TVariable _, _ -> () // actual can be variable, eg [] is (TList (TVariable "a"))
  | TList actualType, TList expectedType -> typecheck name actualType expectedType
  | _ ->
    if actualType = expectedType then
      ()
    else
      let actual = DvalReprDeveloper.typeName actualType
      let expected = DvalReprDeveloper.typeName expectedType
      error $"Incorrect type in {name}, expected {expected}, but got a {actual}"

let typecheckDval
  (name : string)
  (types : Types)
  (dval : Dval)
  (expectedType : TypeReference)
  =
  uply {
    let context = TypeChecker.DBQueryVariable(name, expectedType)
    match! TypeChecker.unify context types Map.empty expectedType dval with
    | Ok() -> return ()
    | Error err -> raise (SqlCompilerRuntimeError err)
  }

let escapeFieldname (str : string) : string =
  // Allow underscore, numbers, letters, only
  // CLEANUP: error on bad field name
  System.Text.RegularExpressions.Regex.Replace(str, "[^a-zA-Z0-9_]", "")


//  Inline `let` statements directly into where they are used. Replaces
//
//    let y = 10
//    DB.query Person \value ->
//      let x = 5
//      value.age < x
//
//   with
//    let y = 10
//    DB.query Person \value ->
//      value.age < 5
//
//  The main purpose of inlining is to get `value.fieldname` inlined. Other
//  expressions are handled by a later partialEvaluation pass (which relies on
//  this pass having run first).
//
//  We need to inline value.fieldname because it can't be computed at
//  compile-time, (value here is the variable whose name symbolizes the DB rows
//  eg value.name, value.age, etc).
//
//  It's possible that we over-inline here, and introduce duplicate code that
//  has slightly different behaviour. For example, converting
//
//   DB.query Person \value ->
//     let x = launch_the_missiles ()
//     value.person < x + x
//
//  into
//
//   DB.query Person \value ->
//     value.person < launch_the_missiles () + launch_the_missiles ()
//
//  As a first attempt, this is fine, as it's hard to avoid without making a
//  more sophisticated algorithm, or iterating between inlining and partial
//  evaluation. We expect users to write pure code in here, and if they don't
//  we can solve that in the next version.
let rec inline'
  (fns : Functions)
  (paramName : string)
  (symtable : Map<string, Expr>)
  (expr : Expr)
  : Ply<Expr> =
  let identityPly x = uply { return x }
  RuntimeTypesAst.postTraversalAsync
    (fun expr ->

      let rec mapLetPattern
        (symtable : Map<string, Expr>)
        (currentExpr : Expr, letPattern : LetPattern)
        : Map<string, Expr> =
        match letPattern with
        | LPVariable(_id, name) -> Map.add name currentExpr symtable
        | LPUnit _ -> symtable
        | LPTuple(_id, first, second, theRest) ->
          match currentExpr with
          | ETuple(_, firstExpr, secondExpr, restExpr) ->
            let exprList = firstExpr :: secondExpr :: restExpr
            let patternList = first :: second :: theRest

            if List.length exprList <> List.length patternList then
              error "Tuple length mismatch"

            let zipped = List.zip exprList patternList

            List.fold mapLetPattern symtable zipped

          | _ -> error "Expected a tuple"

      match expr with
      | EApply(_, EFnName(_, (fnName)), [], args) ->
        uply {
          let arguments = args |> NEList.toList
          match fnName with
          | FQFnName.Package p ->
            match! fns.package p with
            | Some fn ->
              let parameters =
                fn.parameters |> NEList.toList |> List.map (fun p -> p.name, p.typ)
              let body = fn.body
              let paramToArgMap =
                List.zip parameters arguments
                |> List.map (fun ((name, _), arg) -> name, arg)
                |> Map.ofList
              return! inline' fns paramName paramToArgMap body
            | None -> return expr
          | _ -> return expr
        }
      | ELet(_, lpVariable, expr, body) ->
        let newMap = mapLetPattern symtable (expr, lpVariable)
        inline' fns paramName newMap body
      | EVariable(_, name) as expr when name <> paramName ->
        match Map.get name symtable with
        | Some found -> Ply found
        | None ->
          // the variable might be in the symtable, so put it back to fill in later
          Ply expr
      | ERecordFieldAccess(id, expr, fieldname) ->
        uply {
          let! newexpr = inline' fns paramName symtable expr
          return ERecordFieldAccess(id, newexpr, fieldname)
        }
      | expr -> Ply expr

    )
    identityPly
    identityPly
    identityPly
    identityPly
    identityPly
    identityPly
    expr

let (|Fn|_|) (fName : string) (v : int) (expr : Expr) =
  match expr with
  | EApply(_,
           EFnName(_, FQFnName.Builtin({ name = name; version = version })),
           [],
           args) when name = fName && version = v -> Some args
  | _ -> None

type CompiledSqlQuery =
  { sql : string; vars : List<string * SqlValue>; actualType : TypeReference }



/// Generate SQL from an Expr. This expects that all the hard stuff has been
/// removed by previous passes, and should only be called as the final pass.
/// Returns the sql snippet for this expression, the variables that need to be
/// bound to it, and the actual type of the expression.
let rec lambdaToSql
  (fns : Functions)
  (types : Types)
  (constants : Constants)
  (callStack : CallStack)
  (tst : TypeSymbolTable)
  (symtable : DvalMap)
  (paramName : string)
  (dbTypeRef : TypeReference)
  (expectedType : TypeReference)
  (expr : Expr)
  : Ply<CompiledSqlQuery> =
  let callStack =
    { callStack with lastCalled = (fst callStack.lastCalled, Some(Expr.toID expr)) }

  uply {
    let lts (expectedType : TypeReference) (expr : Expr) : Ply<CompiledSqlQuery> =
      lambdaToSql
        fns
        types
        constants
        callStack
        tst
        symtable
        paramName
        dbTypeRef
        expectedType
        expr

    let! (sql, vars, actualType) =
      uply {
        match expr with
        | EConstant(_, (constantName)) ->
          let nameStr =
            // CLEANUP remove - will print UUID if Package Constant
            FQConstantName.toString constantName

          let! constant =
            uply {
              match constantName with
              | FQConstantName.Builtin b ->
                match Map.get b constants.builtIn with
                | Some c -> return c.body
                | None -> return! error $"No built-in constant {nameStr} found"
              | FQConstantName.Package p ->
                match! constants.package p with
                | Some c ->
                  return LibExecution.Interpreter.evalConst callStack c.body
                | None -> return error $"No package constant {nameStr} found"
            }
          do! typecheckDval nameStr types constant expectedType
          let random = randomString 8
          let newname = $"const_{random}"
          let! (sqlValue, actualType) =
            dvalToSql callStack types expectedType constant
          return ($"(@{newname})", [ newname, sqlValue ], actualType)

        | EApply(_, EFnName(_, fnName), [], args) ->
          let nameStr = FQFnName.toString fnName

          let! (returnType, parameters, sqlSpec) =
            uply {
              match fnName with
              | FQFnName.Builtin b ->
                match Map.get b fns.builtIn with
                | Some fn ->
                  let parameters = fn.parameters |> List.map (fun p -> p.name, p.typ)
                  return fn.returnType, parameters, fn.sqlSpec
                | None -> return error $"Builtin function {nameStr} not found"
              | _ -> return error $"Function {nameStr} not found"
            }

          typecheck nameStr returnType expectedType

          let! (actualTypes, argSqls, sqlVars) =
            uply {
              let paramCount = List.length parameters
              let argCount = NEList.length args

              if argCount = paramCount then
                // While checking the arguments, record the actual types for any abstract
                // types so that we can compare them and give a good error message as well
                // as have the types for the correct Npgsql wrapper for lists and other
                // polymorphic values

                let zipped = List.zip (NEList.toList args) parameters

                return!
                  Ply.List.foldSequentially
                    (fun
                         (actualTypes, prevSqls, prevVars)
                         (argExpr, (paramName, paramType)) ->
                      uply {
                        let paramType =
                          match paramType with
                          | TVariable name ->
                            match Map.get name actualTypes with
                            | Some typ -> typ
                            | None -> paramType
                          | _ -> paramType
                        let! compiled = lts paramType argExpr
                        let newActuals =
                          match paramType with
                          | TVariable name ->
                            match Map.get name actualTypes with
                            // We've seen this type before, check it matches
                            | Some expected ->
                              typecheck paramName compiled.actualType expected
                              actualTypes
                            | None -> Map.add name compiled.actualType actualTypes
                          | _ -> actualTypes
                        return
                          newActuals,
                          prevSqls @ [ compiled.sql ],
                          prevVars @ compiled.vars
                      })
                    (tst, [], [])
                    zipped

              else
                return
                  error
                    $"{nameStr} has {paramCount} functions but we have {argCount} arguments"
            }

          // Check the unified return type (basic on the actual arguments) against the
          // expected type
          let returnType =
            match returnType with
            | TVariable name ->
              match Map.get name actualTypes with
              | Some typ -> typ
              | None -> error "Could not find return type"
            | TList(TVariable name) ->
              match Map.get name actualTypes with
              | Some typ -> TList typ
              | None -> error "Could not find return type"
            | typ -> typ

          typecheck nameStr returnType expectedType

          return
            match sqlSpec, argSqls with
            | SqlBinOp op, [ argL; argR ] ->
              $"({argL} {op} {argR})", sqlVars, returnType
            | SqlUnaryOp op, [ argSql ] -> $"({op} {argSql})", sqlVars, returnType
            | SqlFunction fnName, _ ->
              let argSql = String.concat ", " argSqls
              $"({fnName}({argSql}))", sqlVars, returnType
            | SqlFunctionWithPrefixArgs(fnName, fnArgs), _ ->
              let argSql = fnArgs @ argSqls |> String.concat ", "
              $"({fnName} ({argSql}))", sqlVars, returnType
            | SqlFunctionWithSuffixArgs(fnName, fnArgs), _ ->
              let argSql = argSqls @ fnArgs |> String.concat ", "
              $"({fnName} ({argSql}))", sqlVars, returnType
            | SqlCallback2 fn, [ arg1; arg2 ] ->
              $"({fn arg1 arg2})", sqlVars, returnType
            | _, _ -> error $"This function ({nameStr}) is not yet implemented"


        | EAnd(_, left, right) ->
          let! left = lts TBool left
          let! right = lts TBool right
          typecheck "left side of and" left.actualType TBool
          typecheck "right side of and" right.actualType TBool
          return $"({left.sql} AND {right.sql})", left.vars @ right.vars, TBool


        | EOr(_, left, right) ->
          let! left = lts TBool left
          let! right = lts TBool right
          typecheck "left side of or" left.actualType TBool
          typecheck "right side of or" right.actualType TBool
          return $"({left.sql} OR {right.sql})", left.vars @ right.vars, TBool


        // TYPESCLEANUP - this could be the paramName, now that we support more than
        // records here.
        | EVariable(_, varname) ->
          match Map.get varname symtable with
          | Some dval ->
            do! typecheckDval varname types dval expectedType
            let random = randomString 8
            let newname = $"{varname}_{random}"
            // Fetch the actualType here as well as we might be passing in an abstract
            // type.
            let! (sqlValue, actualType) = dvalToSql callStack types expectedType dval
            return $"(@{newname})", [ newname, sqlValue ], actualType
          | None -> return error $"This variable is not defined: {varname}"

        | EInt64(_, v) ->
          typecheck $"Int64 {v}" TInt64 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int64 v ], TInt64

        | EUInt64(_, v) ->
          typecheck $"UInt64 {v}" TUInt64 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, uint64 v ], TUInt64

        | EInt8(_, v) ->
          typecheck $"Int8 {v}" TInt8 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int16 (int16 v) ], TInt8

        | EUInt8(_, v) ->
          typecheck $"UInt8 {v}" TUInt8 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int16 (int16 v) ], TUInt8

        | EInt16(_, v) ->
          typecheck $"Int16 {v}" TInt16 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int16 v ], TInt16

        | EUInt16(_, v) ->
          typecheck $"UInt16 {v}" TUInt16 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int64 (int v) ], TUInt16

        | EInt32(_, v) ->
          typecheck $"Int32 {v}" TInt32 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int v ], TInt32

        | EUInt32(_, v) ->
          typecheck $"UInt32 {v}" TUInt32 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int64 (int v) ], TUInt32

        | EInt128(_, v) ->
          typecheck $"Int128 {v}" TInt128 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, int128 v ], TInt128

        | EUInt128(_, v) ->
          typecheck $"UInt128 {v}" TUInt128 expectedType
          let name = randomString 10
          return $"(@{name})", [ name, uint128 v ], TUInt128

        | EBool(_, v) ->
          typecheck $"Bool {v}" TBool expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.bool v ], TBool

        | EUnit _ ->
          typecheck "Unit" TUnit expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int64 0L ], TUnit

        | EFloat(_, v) ->
          typecheck $"Float {v}" TFloat expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.double v ], TFloat

        | EString(_, parts) ->
          let! strParts, vars =
            parts
            |> Ply.List.mapSequentially (fun part ->
              uply {
                match part with
                | StringText(s) ->
                  typecheck $"String \"{s}\"" TString expectedType
                  let name = randomString 10
                  return $"(@{name})", [ name, Sql.string s ]
                | StringInterpolation e ->
                  let! compiled = lts TString e
                  typecheck $"String interpolation" TString compiled.actualType
                  return compiled.sql, compiled.vars
              })
            |> Ply.map List.unzip

          let result = String.concat ", " strParts
          let strPart = $"concat({result})"
          let vars = List.concat vars

          return strPart, vars, TString

        | EChar(_, v) ->
          typecheck $"Char '{v}'" TChar expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.string v ], TChar

        | EList(_, items) ->
          match expectedType with
          | TVariable _ as expectedType
          | TList expectedType ->
            let! (sqls, vars, actualType) =
              Ply.List.foldSequentially
                (fun (prevSqls, prevVars, prevActualType) v ->
                  uply {
                    let! compiled = lts expectedType v
                    typecheck $"List item" compiled.actualType prevActualType

                    return
                      (prevSqls @ [ compiled.sql ],
                       prevVars @ compiled.vars,
                       compiled.actualType)
                  })
                ([], [], expectedType)
                items

            let sql =
              sqls
              |> String.concat ", "
              |> (fun s -> "((ARRAY[ " + s + " ] )::bigint[])")

            return (sql, vars, TList actualType)
          | _ -> return error "Expected a List"

        // TODO: handle cases where `fields` is non-empty
        | EEnum(_, sourceTypeName, sourceCaseName, []) ->
          let! dv =
            TypeChecker.DvalCreator.enum
              sourceTypeName
              sourceTypeName
              sourceCaseName
              []

          let typeArgs = [] // TODO - get from the dval above? maybe typeName too...
          let typ = TCustomType(Ok sourceTypeName, typeArgs)

          typecheck $"Enum '{dv}'" typ expectedType
          let! v = DvalReprInternalQueryable.toJsonStringV0 callStack types typ dv
          let name = randomString 10

          return $"(@{name})", [ name, Sql.jsonb v ], typ


        | ERecordFieldAccess(_, subExpr, fieldName) ->
          // This is the core part of the query compiler - being able to query fields
          // of the DB rows.
          //
          // Typically, there a DB row:
          //   { a : 5 }
          //
          // and we want to query it with
          //   (fun v -> v.a = 5)
          //
          // We're going to produce the SQL:
          //   ((data::jsonb->'a')::bigint = 5)
          //
          // This part of the code is responsible for producing the SQL:
          //   (data::jsonb->'a')::bigint
          //
          // Note that the type is needed so it can do a comparison. However, we are
          // allowed fallback to untyped (`::jsonb`) so long as we know that the rhs
          // of the expression will also be typed as jsonb. We do not have a
          // coordination mechanism though, so this is iffy at present.


          // returns (variable name * fieldPath)
          // i.e. `fun (a, b) -> b.x.y` would be `(Some b, [ "x"; "y" ])`
          let rec getPath
            (pathSoFar : NEList<string>)
            (subExpr : Expr)
            : (Option<string> * NEList<string>) =
            match subExpr with
            | EVariable(_, v) -> (Some v, pathSoFar)
            | ERecordFieldAccess(_, subExpr, childFieldName) ->
              getPath (NEList.push childFieldName pathSoFar) subExpr
            | _ -> error $"Invalid field access pattern: {subExpr}"

          let (varName, fieldAccessPath) =
            getPath (NEList.singleton fieldName) subExpr

          // I don't think it's really possible for the varName to end up None,
          // but just in case the parser did something wonky, we're prepared...
          match varName with
          | None ->
            error "Did not find a final variable name in field access expression"
          | Some v ->
            if v <> paramName then
              // CLEANUP
              // Realistically - if someone is accessing a variable that isn't the
              // variable bound in the lambda, we should just eval the expression,
              // right?
              return
                error
                  $"We do not yet support compiling lambdas with fields that are not the lambda paramName: {expr}"

          let rec dbFieldType
            (typeRef : TypeReference)
            (fieldName : string)
            : Ply<TypeReference> =
            uply {
              match typeRef with
              | TCustomType(Ok typeName, typeArgs) ->
                match! Types.find typeName types with
                | Some({ definition = TypeDeclaration.Alias aliasedType
                         typeParams = typeParams }) ->
                  let! fieldType = dbFieldType aliasedType fieldName
                  return Types.substitute typeParams typeArgs fieldType

                | Some({ definition = TypeDeclaration.Record fields
                         typeParams = typeParams }) ->
                  let field = fields |> NEList.find (fun f -> f.name = fieldName)
                  match field with
                  | Some v -> return Types.substitute typeParams typeArgs v.typ
                  | None ->
                    return
                      error2 "The datastore does not have a field named" fieldName

                | Some({ definition = TypeDeclaration.Enum _ }) ->
                  return
                    error2
                      "The datastore's type is not a record - it's an enum"
                      (FQTypeName.toString typeName)

                | None ->
                  return
                    error2
                      "The datastore does not have a type named"
                      (FQTypeName.toString typeName)
              | _ -> return error "The datastore is not a record"
            }

          let rec dbFieldTypeFromPath
            (typeRef : TypeReference)
            (path : List<string>)
            =
            uply {
              match path with
              | [] -> return typeRef
              | [ fieldName ] -> return! dbFieldType typeRef fieldName
              | fieldName :: rest ->
                let! fieldType = dbFieldType typeRef fieldName
                return! dbFieldTypeFromPath fieldType rest
            }

          let! dbFieldType =
            dbFieldTypeFromPath dbTypeRef (NEList.toList fieldAccessPath)

          typecheck fieldName dbFieldType expectedType

          let rec primitiveFieldType t =
            match t with
            | TString -> "text"
            | TInt64 -> "bigint"
            | TUInt64 -> "numeric(20,0)"
            | TInt8 -> "smallint"
            | TUInt8 -> "smallint"
            | TInt16 -> "smallint"
            | TUInt16 -> "integer"
            | TInt32 -> "integer"
            | TUInt32 -> "bigint"
            | TInt128 -> "numeric(39,0)"
            | TUInt128 -> "numeric(39,0)"
            | TFloat -> "double precision"
            | TBool -> "bool"
            | TDateTime -> "timestamp with time zone"
            | TChar -> "text"
            | TUuid -> "uuid"
            | TUnit -> "bigint" // CLEANUP why is this bigint?
            | TVariable varName ->
              match Map.get varName tst with
              | Some found -> primitiveFieldType found
              | None ->
                error $"Could not resolve type variable in lambdaToSql: {varName}"
            | _ ->
              error $"We do not support this type of primitive DB field yet: {t}"

          let fieldAccessPath = NEList.map escapeFieldname fieldAccessPath
          match dbFieldType with
          | TString
          | TInt64
          | TUInt64
          | TInt8
          | TUInt8
          | TInt16
          | TUInt16
          | TInt32
          | TUInt32
          | TInt128
          | TUInt128
          | TFloat
          | TBool
          | TDateTime
          | TChar
          | TUuid
          | TVariable _
          | TUnit ->
            let typename = primitiveFieldType dbFieldType

            // for varName.a.b.c, we need to do (data::jsonb->'a'->'b'->>'c')
            let jsonAccessPath =
              let reversed = NEList.reverse fieldAccessPath
              let lastPart = $"->>'{reversed.head}'"
              let previousParts =
                reversed.tail |> List.map (fun part -> $"->'{part}'")
              (lastPart :: previousParts) |> List.rev |> String.concat " "

            return $"((data::jsonb {jsonAccessPath})::{typename})", [], dbFieldType

          | TList t ->
            let typename = primitiveFieldType t

            let jsonAccessPath =
              fieldAccessPath
              |> NEList.toList
              |> List.map (fun fieldNamePart -> $"->'{fieldNamePart}'")
              |> String.concat " "

            let sql =
              $"(ARRAY(SELECT jsonb_array_elements_text(data::jsonb {jsonAccessPath})::{typename}))::{typename}[]"

            return (sql, [], dbFieldType)

          | TCustomType(Ok(_t), []) ->
            let jsonAccessPath =
              fieldAccessPath
              |> NEList.toList
              |> List.map (fun fieldNamePart -> $"->'{fieldNamePart}'")
              |> String.concat " "

            // No typename, we'll just compare the json value
            return ($"(data::jsonb {jsonAccessPath})", [], dbFieldType)

          | _ ->
            return
              error $"We do not support this type of DB field yet: {dbFieldType}"
        | _ -> return error $"We do not yet support compiling this code: {expr}"
      }

    assert_
      "typeReference is concrete"
      [ "actualType", actualType
        "expectedType", expectedType
        "dbTypeRef", dbTypeRef
        "expr", expr
        "sql", sql
        "vars", vars ]
      (actualType.isConcrete ())

    return { sql = sql; vars = vars; actualType = actualType }
  }


//  Trying to get rid of complex expressions, including values which can be
//  evaluated at compile-time, expressions that rely on external values. We do
//  this by evaluating them and moving their results into the symbol table.
//
//  The purpose of this step is as a convenience to the user. We could force
//  them to rewrite:
//
//   Db.query Person \value ->
//     value.age < Int64.sqrt (String.length (String.append a b))
//
//  into
//
//   let myAge = Int64.sqrt (String.length (String::append a b))
//   Db.query Person \value ->
//     value.age < myAge
//
//  This is simply a convenience function to do that. Since users don't have a
//  good understanding of where execution is happening, they expect to be able
//  to do this, and really they're not wrong.
//
//  This should work on all expressions which operate on known values at
//  run-time (since this compiler operates at run-time, we have all the values
//  except the ones in the DB).
//
//  Expects inlining to have finished first, so that it has all the values it
//  needs in the right place.
let partiallyEvaluate
  (state : ExecutionState)
  (paramName : string)
  (body : Expr)
  : Ply.Ply<DvalMap * Expr> =
  uply {
    // This isn't really a good implementation, but right now we only do
    // straight-line code here, so it should work
    let mutable symtable = state.symbolTable

    let exec (expr : Expr) : Ply.Ply<Expr> =
      uply {
        let state = { state with symbolTable = symtable }
        let newName = "dark_generated_" + randomString 8
        let! value = LibExecution.Interpreter.eval state expr
        symtable <- Map.add newName value symtable
        return (EVariable(gid (), newName))
      }

    let f (expr : Expr) : Ply.Ply<Expr> =
      uply {
        // We list any construction that we think is safe to evaluate in now in the
        // interpreter instead of in the DB. Anything immutable should be good,
        // including literals and variables with known values (so not `paramName`)
        match expr with
        | ERecordFieldAccess(_, EVariable(_, name), _) when name <> paramName ->
          return! exec expr
        | ERecordFieldAccess(_, ERecord _, _) ->
          // inlining can create these situations
          return! exec expr
        | EAnd(_, EBool _, EBool _)
        | EOr(_, EBool _, EBool _) -> return! exec expr
        | EAnd(_, EBool _, EVariable(_, name))
        | EAnd(_, EVariable(_, name), EBool _)
        | EOr(_, EBool _, EVariable(_, name))
        | EOr(_, EVariable(_, name), EBool _) when name <> paramName ->
          return! exec expr
        | EOr(_, EVariable(_, name1), EVariable(_, name2))
        | EAnd(_, EVariable(_, name1), EVariable(_, name2)) when
          name1 <> paramName && name2 <> paramName
          ->
          return! exec expr
        | EApply(_, _, typeArgs, args) ->
          let rec fullySpecified (expr : Expr) =
            match expr with
            | EInt64 _
            | EUInt64 _
            | EInt8 _
            | EUInt8 _
            | EInt16 _
            | EUInt16 _
            | EInt32 _
            | EUInt32 _
            | EInt128 _
            | EUInt128 _
            | EBool _
            | EUnit _
            | EFloat _
            | EString _
            | EChar _
            | EVariable _ -> true
            | ETuple(_, e1, e2, rest) -> List.all fullySpecified (e1 :: e2 :: rest)
            | EList(_, exprs) -> List.all fullySpecified exprs
            | _ -> false

          if NEList.forall fullySpecified args && typeArgs = [] then
            // TODO: should limit this further to pure functions.
            return! exec expr
          else
            return expr
        | EString _
        | EInt64 _
        | EUInt64 _
        | EInt8 _
        | EUInt8 _
        | EInt16 _
        | EUInt16 _
        | EInt32 _
        | EUInt32 _
        | EInt128 _
        | EUInt128 _
        | EFloat _
        | EBool _
        | EUnit _
        | EConstant _
        | EChar _
        | ELet _
        | EIf _
        | ELambda _
        | ERecordFieldAccess _
        | EVariable _
        | EList _
        | ETuple _
        | ERecord _
        | ERecordUpdate _
        | EDict _
        | EEnum _
        | EMatch _
        | EError _
        | EAnd _
        | EOr _
        | EFnName _ -> return expr
      }

    // This is a copy of Ast.postTraversal, made to  work with uplys
    let rec postTraversal (expr : Expr) : Ply.Ply<Expr> =
      let r = postTraversal
      uply {
        let! result =
          uply {
            match expr with
            | EInt64 _
            | EUInt64 _
            | EInt8 _
            | EUInt8 _
            | EInt16 _
            | EUInt16 _
            | EInt32 _
            | EUInt32 _
            | EInt128 _
            | EUInt128 _
            | EString _
            | EVariable _
            | EChar _
            | EBool _
            | EUnit _
            | EConstant _
            | EFloat _ -> return expr
            | ELet(id, pat, rhs, next) ->
              let! rhs = r rhs
              let! next = r next
              return ELet(id, pat, rhs, next)
            | EApply(id, fn, typeArgs, exprs) ->
              let! fn = r fn
              let! exprs = exprs |> Ply.NEList.mapSequentially r
              return EApply(id, fn, typeArgs, exprs)
            | EFnName _ -> return expr
            | EIf(id, cond, ifExpr, elseExpr) ->
              let! cond = r cond
              let! ifExpr = r ifExpr
              let! elseExpr =
                uply {
                  match elseExpr with
                  | Some value ->
                    let! newValue = r value
                    return Some newValue
                  | None -> return None
                }
              return EIf(id, cond, ifExpr, elseExpr)
            | ERecordFieldAccess(id, expr, fieldname) ->
              let! expr = r expr
              return ERecordFieldAccess(id, expr, fieldname)
            | ELambda(id, names, expr) ->
              let! expr = r expr
              return ELambda(id, names, expr)
            | EList(id, exprs) ->
              let! exprs = Ply.List.mapSequentially r exprs
              return EList(id, exprs)
            | ETuple(id, first, second, theRest) ->
              let! first = r first
              let! second = r second
              let! theRest = Ply.List.mapSequentially r theRest
              return ETuple(id, first, second, theRest)
            | EMatch(id, mexpr, cases) ->
              let! mexpr = r mexpr

              let! cases =
                Ply.NEList.mapSequentially
                  (fun case ->
                    uply {
                      let! expr = r case.rhs
                      let! whenCondition =
                        uply {
                          match case.whenCondition with
                          | Some whenCondition ->
                            let! whenCondition = r whenCondition
                            return Some whenCondition
                          | None -> return None
                        }
                      return
                        { pat = case.pat
                          whenCondition = whenCondition
                          rhs = expr }
                    })
                  cases

              return EMatch(id, mexpr, cases)
            | ERecord(id, typeName, fields) ->
              let! fields =
                Ply.NEList.mapSequentially
                  (fun (name, expr) ->
                    uply {
                      let! expr = r expr
                      return (name, expr)
                    })
                  fields

              return ERecord(id, typeName, fields)
            | ERecordUpdate(id, record, updates) ->
              let! updates =
                Ply.NEList.mapSequentially
                  (fun (name, expr) ->
                    uply {
                      let! expr = r expr
                      return (name, expr)
                    })
                  updates
              let! record = r record
              return ERecordUpdate(id, record, updates)
            | EDict(id, fields) ->
              let! fields =
                Ply.List.mapSequentially
                  (fun (key, expr) ->
                    uply {
                      let! expr = r expr
                      return (key, expr)
                    })
                  fields

              return EDict(id, fields)
            | EEnum(id, typeName, caseName, fields) ->
              let! fields = Ply.List.mapSequentially r fields
              return EEnum(id, typeName, caseName, fields)
            | EAnd(id, left, right) ->
              let! left = r left
              let! right = r right
              return EAnd(id, left, right)
            | EOr(id, left, right) ->
              let! left = r left
              let! right = r right
              return EOr(id, left, right)
            | EError _ -> return expr
          }

        return! f result
      }

    let! result = postTraversal body
    return (symtable, result)
  }


type CompileLambdaResult = { sql : string; vars : List<string * SqlValue> }

let compileLambda
  (state : ExecutionState)
  (paramName : string)
  (dbType : TypeReference)
  (body : Expr)
  : Ply<Result<CompileLambdaResult, RuntimeError>> =
  uply {
    try
      let fns = ExecutionState.fns state
      let types = ExecutionState.types state
      let constants = ExecutionState.availableConstants state

      let! symtable, body =
        body
        // Simplify for later passes
        |> canonicalize
        // Inline the RHS of any let within the lambda body. See comment for more
        // details.
        |> inline' fns paramName Map.empty
        // Replace expressions which can be calculated now with their result. See
        // comment for more details.
        |> Ply.bind (partiallyEvaluate state paramName)


      // Resolve typeArgs/aliases in the definition
      let! dbType = getTypeReferenceFromAlias types dbType
      match dbType with
      | Ok dbType ->
        let! compiled =
          lambdaToSql
            fns
            types
            constants
            state.tracing.callStack
            state.typeSymbolTable
            symtable
            paramName
            dbType
            TBool
            body

        return Ok { sql = compiled.sql; vars = compiled.vars }
      | Error err -> return Error err
    with
    | SqlCompilerRuntimeError internalError ->
      let err = RuntimeError.sqlCompilerRuntimeError internalError
      return Error err

    | SqlCompilerException errStr -> return Error(RuntimeError.oldError errStr)
  // return Error(RuntimeError.oldError (errStr + $"\n\nIn body: {body}"))
  }
