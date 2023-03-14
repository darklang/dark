/// Fns used to compile Exprs into SQL queries
module LibBackend.SqlCompiler

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes

module DvalReprDeveloper = LibExecution.DvalReprDeveloper

module RuntimeTypesAst = LibExecution.RuntimeTypesAst
module Errors = LibExecution.Errors

let errorTemplate =
  "You're using our new experimental Datastore query compiler. It compiles your lambdas into optimized (and partially indexed) Datastore queries, which should be reasonably fast.\n\nUnfortunately, we hit a snag while compiling your lambda. We only support a subset of Darklang's functionality, but will be expanding it in the future.\n\nSome Darklang code is not supported in DB::query lambdas for now, and some of it won't be supported because it's an odd thing to do in a datastore query. If you think your operation should be supported, let us know in #general in Discord.\n\n  Error: "

let error (str : string) : 'a = Exception.raiseCode (errorTemplate + str)

let error2 (msg : string) (v : string) : 'a = error $"{msg}: {v}"

let error3 (msg : string) (v1 : string) (v2 : string) : 'a =
  error $"{msg}: {v1}, {v2}"

type position =
  | First
  | Last


// This canonicalizes an expression, meaning it removes multiple ways of
// representing the same thing. Currently nothing needs to be canonicalized.
let rec canonicalize (expr : Expr) : Expr = expr

let rec dvalToSql (expectedType : DType) (dval : Dval) : SqlValue =
  match expectedType, dval with
  | _, DError _
  | _, DIncomplete _ -> Errors.foundFakeDval dval
  | _, DHttpResponse _
  | _, DFnVal _
  | _, DDB _
  | _, DObj _ // CLEANUP allow
  | _, DPassword _ // CLEANUP allow
  | _, DOption _ // CLEANUP allow
  | _, DResult _ // CLEANUP allow
  | _, DBytes _ // CLEANUP allow
  | _, DConstructor _ // TODO: revisit
  | _, DTuple _ ->
    error2 "This value is not yet supported" (DvalReprDeveloper.toRepr dval)
  | TDateTime, DDateTime date ->
    date |> DarkDateTime.toDateTimeUtc |> Sql.timestamptz
  | TInt, DInt i -> Sql.int64 i
  | TFloat, DFloat v -> Sql.double v
  | TBool, DBool b -> Sql.bool b
  | TStr, DStr s -> Sql.string s
  | TChar, DChar c -> Sql.string c
  | TUuid, DUuid id -> Sql.uuid id
  | TUnit, DUnit -> Sql.int64 0
  // CLEANUP: add test first
  // | TList typ, DList l ->
  //   let typeName = DvalReprDeveloper.typeName typ
  //   let typeToNpgSqlType (t : DType) : NpgsqlTypes.NpgsqlDbType =
  //     match t with
  //     | TStr -> NpgsqlTypes.NpgsqlDbType.Text
  //     | TInt -> NpgsqlTypes.NpgsqlDbType.Bigint
  //     | TFloat -> NpgsqlTypes.NpgsqlDbType.Double
  //     | TBool -> NpgsqlTypes.NpgsqlDbType.Boolean
  //     | TDateTime -> NpgsqlTypes.NpgsqlDbType.TimestampTz
  //     | TChar -> NpgsqlTypes.NpgsqlDbType.Text
  //     | TUuid -> NpgsqlTypes.NpgsqlDbType.Uuid
  //     | _ -> error $"We do not support this type of DB field yet: {t}"
  //   l
  //   |> List.map (fun v ->
  //     match v with
  //     | DStr s -> s : obj
  //     | DInt i -> i
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
  | _ ->
    error3
      "This value is not of the expected type"
      (DvalReprDeveloper.toRepr dval)
      (DvalReprDeveloper.typeName expectedType)


let rec typecheck
  (name : string)
  (actualType : DType)
  (expectedType : DType)
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

let escapeFieldname (str : string) : string =
  // Allow underscore, numbers, letters, only
  // CLEANUP: error on bad field name
  System.Text.RegularExpressions.Regex.Replace(str, "[^a-zA-Z0-9_]", "")


//  Inline `let` statements directly into where they are used. Replaces
//
//    let y = 10
//    DB::query Person \value ->
//      let x = 5
//      value.age < x
//
//   with
//    let y = 10
//    DB::query Person \value ->
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
//   DB::query Person \value ->
//     let x = launch_the_missiles ()
//     value.person < x + x
//
//  into
//
//   DB::query Person \value ->
//     value.person < launch_the_missiles () + launch_the_missiles ()
//
//  As a first attempt, this is fine, as it's hard to avoid without making a
//  more sophisticated algorithm, or iterating between inlining and partial
//  evaluation. We expect users to write pure code in here, and if they don't
//  we can solve that in the next version.
let rec inline'
  (paramName : string)
  (symtable : Map<string, Expr>)
  (expr : Expr)
  : Expr =
  RuntimeTypesAst.postTraversal
    (fun expr ->
      match expr with
      | ELet (_, pat, expr, body) ->
        let varName =
          match pat with
          | LPVariable (_id, name) -> name

        inline' paramName (Map.add varName expr symtable) body

      | EVariable (_, name) as expr when name <> paramName ->
        (match Map.get name symtable with
         | Some found -> found
         | None ->
           // the variable might be in the symtable, so put it back to fill in later
           expr)

      | expr -> expr)
    expr

let (|Fn|_|) (mName : string) (fName : string) (v : int) (expr : Expr) =
  match expr with
  | EApply (_, EFQFnValue (_, FQFnName.Stdlib std), args, _) when
    std.module_ = mName && std.function_ = fName && std.version = v
    ->
    Some args
  | _ -> None

/// Generate SQL from an Expr. This expects that all the hard stuff has been
/// removed by previous passes, and should only be called as the final pass.
/// Returns the sql snippet for this expression, the variables that need to be
/// bound to it, and the actual type of the expression.
let rec lambdaToSql
  (fns : Map<FQFnName.T, BuiltInFn>)
  (symtable : DvalMap)
  (paramName : string)
  (dbFields : Map<string, DType>)
  (expectedType : DType)
  (expr : Expr)
  : string * List<string * SqlValue> * DType =
  let lts (typ : DType) (e : Expr) =
    lambdaToSql fns symtable paramName dbFields typ e

  match expr with
  | EApply (_, EFQFnValue (_, name), args, _) ->

    match Map.get name fns with
    | Some fn ->
      // check the abstract type here. We will check the concrete type later
      typecheck (FQFnName.toString name) fn.returnType expectedType

      let actualTypes, argSqls, sqlVars =
        let paramCount = List.length fn.parameters
        let argCount = List.length args

        if argCount = paramCount then

          // While checking the arguments, record the actual types for any abstract
          // types so that we can compare them and give a good error message as well
          // as have the types for the correct Npgsql wrapper for lists and other
          // polymorphic values
          List.fold2
            (fun (actualTypes, prevSqls, prevVars) argExpr param ->
              let sql, vars, argActualType = lts param.typ argExpr
              let newActuals =
                match param.typ with
                | TVariable name ->
                  match Map.get name actualTypes with
                  // We've seen this type before, check it matches
                  | Some expected ->
                    typecheck param.name argActualType expected
                    actualTypes
                  | None -> Map.add name argActualType actualTypes
                | _ -> actualTypes

              newActuals, prevSqls @ [ sql ], prevVars @ vars)

            (Map.empty, [], [])
            args
            fn.parameters
        else
          error
            $"{FQFnName.StdlibFnName.toString fn.name} has {paramCount} functions but we have {argCount} arguments"

      // Check the unified return type (basic on the actual arguments) against the
      // expected type
      let returnType =
        match fn.returnType with
        | TVariable name ->
          match Map.get name actualTypes with
          | Some typ -> typ
          | None -> error "Could not find return type"
        | TList (TVariable name) ->
          match Map.get name actualTypes with
          | Some typ -> TList typ
          | None -> error "Could not find return type"
        | typ -> typ

      typecheck (FQFnName.toString name) returnType expectedType


      match fn, argSqls with
      | { sqlSpec = SqlBinOp op }, [ argL; argR ] ->
        $"({argL} {op} {argR})", sqlVars, returnType
      | { sqlSpec = SqlUnaryOp op }, [ argSql ] ->
        $"({op} {argSql})", sqlVars, returnType
      | { sqlSpec = SqlFunction fnname }, _ ->
        let argSql = String.concat ", " argSqls
        $"({fnname}({argSql}))", sqlVars, returnType
      | { sqlSpec = SqlFunctionWithPrefixArgs (fnName, fnArgs) }, _ ->
        let argSql = fnArgs @ argSqls |> String.concat ", "
        $"({fnName} ({argSql}))", sqlVars, returnType
      | { sqlSpec = SqlFunctionWithSuffixArgs (fnName, fnArgs) }, _ ->
        let argSql = argSqls @ fnArgs |> String.concat ", "
        $"({fnName} ({argSql}))", sqlVars, returnType
      | { sqlSpec = SqlCallback2 fn }, [ arg1; arg2 ] ->
        $"({fn arg1 arg2})", sqlVars, returnType
      | _, _ ->
        error $"This function ({FQFnName.toString name}) is not yet implemented"
    | None ->
      error
        $"Only builtin functions can be used in queries right now; {FQFnName.toString name} is not a builtin function"

  | EAnd (_, left, right) ->
    let leftSql, leftVars, leftActual = lts TBool left
    let rightSql, rightVars, rightActual = lts TBool right
    typecheck "left side of and" leftActual TBool
    typecheck "right side of and" rightActual TBool
    $"({leftSql} AND {rightSql})", leftVars @ rightVars, TBool


  | EOr (_, left, right) ->
    let leftSql, leftVars, leftActual = lts TBool left
    let rightSql, rightVars, rightActual = lts TBool right
    typecheck "left side of or" leftActual TBool
    typecheck "right side of or" rightActual TBool
    $"({leftSql} OR {rightSql})", leftVars @ rightVars, TBool


  | EVariable (_, varname) ->
    match Map.get varname symtable with
    | Some dval ->
      let actualType = Dval.toType dval
      typecheck $"variable {varname}" actualType expectedType
      let random = randomString 8
      let newname = $"{varname}_{random}"
      $"(@{newname})", [ newname, dvalToSql actualType dval ], actualType
    | None -> error $"This variable is not defined: {varname}"

  | EInteger (_, v) ->
    typecheck $"integer {v}" TInt expectedType
    let name = randomString 10
    $"(@{name})", [ name, Sql.int64 v ], TInt

  | EBool (_, v) ->
    typecheck $"bool {v}" TBool expectedType
    let name = randomString 10
    $"(@{name})", [ name, Sql.bool v ], TBool

  | EUnit _ ->
    typecheck "unit" TUnit expectedType
    let name = randomString 10
    $"(@{name})", [ name, Sql.int64 0L ], TUnit

  | EFloat (_, v) ->
    typecheck $"float {v}" TFloat expectedType
    let name = randomString 10
    $"(@{name})", [ name, Sql.double v ], TFloat

  | EString (_, parts) ->
    let strParts, vars =
      parts
      |> List.map (fun part ->
        match part with
        | StringText (s) ->
          typecheck $"string \"{s}\"" TStr expectedType
          let name = randomString 10
          $"(@{name})", [ name, Sql.string s ]
        | StringInterpolation e ->
          let strPart, vars, actualType = lts TStr e
          typecheck $"string interpolation" TStr actualType
          strPart, vars)
      |> List.unzip
    let result = String.concat ", " strParts
    let strPart = $"concat({result})"
    let vars = vars |> List.concat
    strPart, vars, TStr

  | ECharacter (_, v) ->
    typecheck $"char '{v}'" TChar expectedType
    let name = randomString 10
    $"(@{name})", [ name, Sql.string v ], TChar

  | EList (_, items) ->
    match expectedType with
    | TVariable _ as expectedType
    | TList expectedType ->
      let sqls, vars, actualType =
        List.fold
          ([], [], expectedType)
          (fun (prevSqls, prevVars, prevActualType) v ->
            let sql, vars, actualType = lts expectedType v
            typecheck $"list item" actualType prevActualType
            prevSqls @ [ sql ], prevVars @ vars, actualType)
          items
      let sql =
        sqls |> String.concat ", " |> (fun s -> "((ARRAY[ " + s + " ] )::bigint[])")
      (sql, vars, TList actualType)
    | _ -> error "Expected a list"


  | EFieldAccess (_, EVariable (_, v), fieldname) when v = paramName ->
    let dbFieldType =
      match Map.get fieldname dbFields with
      | Some v -> v
      | None -> error2 "The datastore does not have a field named" fieldname
    typecheck fieldname dbFieldType expectedType

    let primitiveFieldType t =
      match t with
      | TStr -> "text"
      | TInt -> "bigint"
      | TFloat -> "double precision"
      | TBool -> "bool"
      | TDateTime -> "timestamp with time zone"
      | TChar -> "text"
      | TUuid -> "uuid"
      | TUnit -> "bigint"
      | _ -> error $"We do not support this type of DB field yet: {t}"

    let fieldname = escapeFieldname fieldname
    match dbFieldType with
    | TStr
    | TInt
    | TFloat
    | TBool
    | TDateTime
    | TChar
    | TUuid
    | TUnit ->
      let typename = primitiveFieldType dbFieldType
      $"((data::jsonb->>'{fieldname}')::{typename})", [], dbFieldType
    | TList t ->
      let typename = primitiveFieldType t
      let sql =
        $"(ARRAY(SELECT jsonb_array_elements_text(data::jsonb->'{fieldname}')::{typename}))::{typename}[]"
      (sql, [], dbFieldType)
    | _ -> error $"We do not support this type of DB field yet: {dbFieldType}"
  | _ -> error $"We do not yet support compiling this code: {expr}"


//  Trying to get rid of complex expressions, including values which can be
//  evaluated at compile-time, expressions that rely on external values. We do
//  this by evaluating them and moving their results into the symbol table.
//
//  The purpose of this step is as a convenience to the user. We could force
//  them to rewrite:
//
//   Db::query Person \value ->
//     value.age < Int::sqrt (String::length (String::append a b))
//
//  into
//
//   let myAge = Int::sqrt (String::length (String::append a b))
//   Db::query Person \value ->
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
  (symtable : Symtable)
  (body : Expr)
  : Ply.Ply<DvalMap * Expr> =
  uply {

    // This isn't really a good implementation, but right now we only do
    // straight-line code here, so it should work
    let symtable = ref symtable

    let exec (expr : Expr) : Ply.Ply<Expr> =
      uply {
        let newName = "dark_generated_" + randomString 8
        let! value = LibExecution.Interpreter.eval state symtable.Value expr
        symtable.Value <- Map.add newName value symtable.Value
        return (EVariable(gid (), newName))
      }

    let f (expr : Expr) : Ply.Ply<Expr> =
      uply {
        // We list any construction that we think is safe to evaluate in now in the
        // interpreter instead of in the DB. Anything immutable should be good,
        // including literals and variables with known values (so not `paramName`)
        match expr with
        | EFieldAccess (_, EVariable (_, name), _) when name <> paramName ->
          return! exec expr
        | EFieldAccess (_, ERecord _, _) ->
          // inlining can create these situations
          return! exec expr
        | EAnd (_, EBool _, EBool _)
        | EOr (_, EBool _, EBool _) -> return! exec expr
        | EAnd (_, EBool _, EVariable (_, name))
        | EAnd (_, EVariable (_, name), EBool _)
        | EOr (_, EBool _, EVariable (_, name))
        | EOr (_, EVariable (_, name), EBool _) when name <> paramName ->
          return! exec expr
        | EOr (_, EVariable (_, name1), EVariable (_, name2))
        | EAnd (_, EVariable (_, name1), EVariable (_, name2)) when
          name1 <> paramName && name2 <> paramName
          ->
          return! exec expr
        | EApply (_, EFQFnValue _, args, _) ->
          let rec fullySpecified (expr : Expr) =
            match expr with
            | EInteger _
            | EBool _
            | EUnit _
            | EFloat _
            | EString _
            | ECharacter _
            | EVariable _ -> true
            | ETuple (_, e1, e2, rest) -> List.all fullySpecified (e1 :: e2 :: rest)
            | EList (_, exprs) -> List.all fullySpecified exprs
            | _ -> false

          if List.all fullySpecified args then
            // TODO: should limit this further to pure functions.
            return! exec expr
          else
            return expr
        | _ -> return expr
      }

    // This is a copy of Ast.postTraversal, made to  work with uplys
    let rec postTraversal (expr : Expr) : Ply.Ply<Expr> =
      let r = postTraversal

      uply {
        let! result =
          uply {
            match expr with
            | EInteger _
            | EString _
            | EVariable _
            | ECharacter _
            | EFQFnValue _
            | EBool _
            | EUnit _
            | EFloat _ -> return expr
            | ELet (id, pat, rhs, next) ->
              let! rhs = r rhs
              let! next = r next
              return ELet(id, pat, rhs, next)
            | EApply (id, name, exprs, inPipe) ->
              let! exprs = Ply.List.mapSequentially r exprs
              return EApply(id, name, exprs, inPipe)
            | EIf (id, cond, ifexpr, elseexpr) ->
              let! cond = r cond
              let! ifexpr = r ifexpr
              let! elseexpr = r elseexpr
              return EIf(id, cond, ifexpr, elseexpr)
            | EFieldAccess (id, expr, fieldname) ->
              let! expr = r expr
              return EFieldAccess(id, expr, fieldname)
            | ELambda (id, names, expr) ->
              let! expr = r expr
              return ELambda(id, names, expr)
            | EList (id, exprs) ->
              let! exprs = Ply.List.mapSequentially r exprs
              return EList(id, exprs)
            | ETuple (id, first, second, theRest) ->
              let! first = r first
              let! second = r second
              let! theRest = Ply.List.mapSequentially r theRest
              return ETuple(id, first, second, theRest)
            | EMatch (id, mexpr, pairs) ->
              let! mexpr = r mexpr

              let! pairs =
                Ply.List.mapSequentially
                  (fun (pat, expr) ->
                    uply {
                      let! expr = r expr
                      return (pat, expr)
                    })
                  pairs

              return EMatch(id, mexpr, pairs)
            | ERecord (id, typeName, fields) ->
              let! fields =
                Ply.List.mapSequentially
                  (fun (name, expr) ->
                    uply {
                      let! expr = r expr
                      return (name, expr)
                    })
                  fields

              return ERecord(id, typeName, fields)
            | EConstructor (id, typeName, caseName, fields) ->
              let! fields = Ply.List.mapSequentially r fields
              return EConstructor(id, typeName, caseName, fields)
            | EFeatureFlag (id, cond, casea, caseb) ->
              let! cond = r cond
              let! casea = r casea
              let! caseb = r caseb
              return EFeatureFlag(id, cond, casea, caseb)
            | EAnd (id, left, right) ->
              let! left = r left
              let! right = r right
              return EAnd(id, left, right)
            | EOr (id, left, right) ->
              let! left = r left
              let! right = r right
              return EOr(id, left, right)
          }

        return! f result
      }

    let! result = postTraversal body
    return (symtable.Value, result)
  }



let compileLambda
  (state : ExecutionState)
  (symtable : DvalMap)
  (paramName : string)
  (dbFields : Map<string, DType>)
  (body : Expr)
  : Task<string * List<string * SqlValue>> =
  task {
    let! symtable, body =
      body
      // Replace threads with nested function calls - simplifies all later passes
      |> canonicalize
      // Inline the rhs of any let within the lambda body. See comment for more
      // details.
      |> inline' paramName Map.empty
      // Replace expressions which can be calculated now with their result. See
      // comment for more details.
      |> partiallyEvaluate state paramName symtable
      |> Ply.TplPrimitives.runPlyAsTask

    let sql, vars, _expectedType =
      lambdaToSql state.libraries.stdlib symtable paramName dbFields TBool body

    return (sql, vars)
  }
