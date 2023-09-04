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
module TypeChecker = LibExecution.TypeChecker

module RuntimeTypesAst = LibExecution.RuntimeTypesAst
module Errors = LibExecution.Errors

let errorTemplate =
  "You're using our new experimental Datastore query compiler. It compiles your lambdas into optimized (and partially indexed) Datastore queries, which should be reasonably fast.\n\nUnfortunately, we hit a snag while compiling your lambda. We only support a subset of Darklang's functionality, but will be expanding it in the future.\n\nSome Darklang code is not supported in DB::query lambdas for now, and some of it won't be supported because it's an odd thing to do in a datastore query. If you think your operation should be supported, let us know in #general in Discord.\n\n  Error: "

exception SqlCompilerException of string
exception SqlCompilerRuntimeError of internalError : RuntimeError

let error (str : string) : 'a = raise (SqlCompilerException(errorTemplate + str))

let error2 (msg : string) (v : string) : 'a = error $"{msg}: {v}"

type position =
  | First
  | Last


// This canonicalizes an expression, meaning it removes multiple ways of
// representing the same thing. Currently nothing needs to be canonicalized.
let rec canonicalize (expr : Expr) : Expr = expr

// Returns a typeReference since we don't always know what type it should have (eg is
// a polymorphic function is being called)
let rec dvalToSql
  (expectedType : TypeReference)
  (dval : Dval)
  : (SqlValue * TypeReference) =
  match expectedType, dval with
  | _, DError _ -> Errors.foundFakeDval dval
  | _, DFnVal _
  | _, DDB _
  | _, DDict _ // CLEANUP allow
  | _, DPassword _ // CLEANUP allow
  | _, DBytes _ // CLEANUP allow
  | _, DEnum _ // TODO: revisit
  | _, DRecord _ // CLEANUP allow
  | _, DTuple _ -> // CLEANUP allow
    error2 "This value is not yet supported" (DvalReprDeveloper.toRepr dval)
  | TVariable _, DDateTime date
  | TDateTime, DDateTime date ->
    (date |> LibExecution.DarkDateTime.toDateTimeUtc |> Sql.timestamptz, TDateTime)
  | TVariable _, DInt i
  | TInt, DInt i -> Sql.int64 i, TInt
  | TVariable _, DFloat v
  | TFloat, DFloat v -> Sql.double v, TFloat
  | TVariable _, DBool b
  | TBool, DBool b -> Sql.bool b, TBool
  | TVariable _, DString s
  | TString, DString s -> Sql.string s, TString
  | TVariable _, DChar c
  | TChar, DChar c -> Sql.string c, TChar
  | TVariable _, DUuid id
  | TUuid, DUuid id -> Sql.uuid id, TUuid
  | TVariable _, DUnit
  | TUnit, DUnit -> Sql.int64 0, TUnit
  // CLEANUP: add test first
  // | TList typ, DList l ->
  //   let typeName = DvalReprDeveloper.typeName typ
  //   let typeToNpgSqlType (t : DType) : NpgsqlTypes.NpgsqlDbType =
  //     match t with
  //     | TString -> NpgsqlTypes.NpgsqlDbType.Text
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
  //     | DString s -> s : obj
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
  // exhaustiveness check
  | _, DInt _
  | _, DFloat _
  | _, DBool _
  | _, DString _
  | _, DChar _
  | _, DUuid _
  | _, DUnit
  | _, DList _
  | _, DDateTime _ ->
    error
      "This value is not of the expected type"
      (DvalReprDeveloper.toRepr dval)
      (DvalReprDeveloper.typeName expectedType)


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
    let context = TypeChecker.DBQueryVariable(name, expectedType, None)
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
  (paramName : string)
  (symtable : Map<string, Expr>)
  (expr : Expr)
  : Expr =
  RuntimeTypesAst.postTraversal
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
      | ELet(_, lpVariable, expr, body) ->
        let newMap = mapLetPattern symtable (expr, lpVariable)

        inline' paramName newMap body

      | EVariable(_, name) as expr when name <> paramName ->
        (match Map.get name symtable with
         | Some found -> found
         | None ->
           // the variable might be in the symtable, so put it back to fill in later
           expr)

      | expr -> expr)
    identity
    identity
    identity
    identity
    identity
    identity
    expr

let (|Fn|_|) (mName : string) (fName : string) (v : int) (expr : Expr) =
  match expr with
  | EApply(_,
           EFnName(_,
                   FQName.BuiltIn({ modules = modules
                                    name = FnName.FnName name
                                    version = version })),
           [],
           args) when modules = [ mName ] && name = fName && version = v -> Some args
  | _ -> None

type CompiledSqlQuery =
  { sql : string; vars : List<string * SqlValue>; actualType : TypeReference }


let sqlOpForPackageFunction (fnName : FnName.Package) : SqlSpec =
  match fnName.owner, fnName.modules, fnName.name with
  | "Darklang", [ "Stdlib"; "Bool" ], FnName.FnName "and" -> SqlBinOp "AND"
  | "Darklang", [ "Stdlib"; "Bool" ], FnName.FnName "or" -> SqlBinOp "OR"
  | "Darklang", [ "Stdlib"; "Bool" ], FnName.FnName "not" -> SqlFunction "NOT"
  | _ -> NotQueryable


/// Generate SQL from an Expr. This expects that all the hard stuff has been
/// removed by previous passes, and should only be called as the final pass.
/// Returns the sql snippet for this expression, the variables that need to be
/// bound to it, and the actual type of the expression.
let rec lambdaToSql
  (fns : Functions)
  (types : Types)
  (constants : Constants)
  (tat : TypeArgTable)
  (symtable : DvalMap)
  (paramName : string)
  (dbTypeRef : TypeReference)
  (expectedType : TypeReference)
  (expr : Expr)
  : Ply<CompiledSqlQuery> =
  uply {
    let lts (expectedType : TypeReference) (expr : Expr) : Ply<CompiledSqlQuery> =
      lambdaToSql
        fns
        types
        constants
        tat
        symtable
        paramName
        dbTypeRef
        expectedType
        expr

    let! (sql, vars, actualType) =
      uply {
        match expr with
        | EConstant(_, (constantName)) ->
          let nameStr = ConstantName.toString constantName

          let! constant =
            uply {
              match constantName with
              | FQName.BuiltIn b ->
                match Map.get b constants.builtIn with
                | None -> return! error $"No built-in constant {nameStr} found"
                | Some c ->
                  typecheck nameStr c.typ expectedType
                  return c.body
              | FQName.Package p ->
                match! constants.package p with
                | None -> return error $"No package constant {nameStr} found"
                | Some c ->
                  do! typecheckDval nameStr types c.body expectedType
                  return c.body
              | FQName.UserProgram _ ->
                return error $"User constants are not yet supported"
            }
          let random = randomString 8
          let newname = $"{nameStr}_{random}"
          let (sqlValue, actualType) = dvalToSql expectedType constant
          return ($"(@{newname})", [ newname, sqlValue ], actualType)

        | EApply(_, EFnName(_, (fnName)), [], args) ->
          let nameStr = FnName.toString fnName

          let! (returnType, parameters, sqlSpec) =
            uply {
              match fnName with
              | FQName.BuiltIn b ->
                match Map.get b fns.builtIn with
                | Some fn ->
                  let parameters = fn.parameters |> List.map (fun p -> p.name, p.typ)
                  return fn.returnType, parameters, fn.sqlSpec
                | None -> return error $"Builtin functions {nameStr} not found"
              | FQName.Package p ->
                match! fns.package p with
                | Some fn ->
                  let parameters =
                    fn.parameters
                    |> NEList.toList
                    |> List.map (fun p -> p.name, p.typ)
                  let sqlSpec = sqlOpForPackageFunction fn.name
                  return fn.returnType, parameters, sqlSpec
                | None -> return error $"Package functions {nameStr} not found"
              | FQName.UserProgram u -> return error $"Todo: user program {u}"
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
                    (Map.empty, [], [])
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
            | SqlFunction fnname, _ ->
              let argSql = String.concat ", " argSqls
              $"({fnname}({argSql}))", sqlVars, returnType
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
            let (sqlValue, actualType) = dvalToSql expectedType dval
            return $"(@{newname})", [ newname, sqlValue ], actualType
          | None -> return error $"This variable is not defined: {varname}"

        | EInt(_, v) ->
          typecheck $"Int {v}" TInt expectedType
          let name = randomString 10
          return $"(@{name})", [ name, Sql.int64 v ], TInt

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


        | EFieldAccess(_, subExpr, fieldname) ->
          // returns (variable name * fieldPath)
          // i.e. `fun (a, b) -> b.x.y` would be `(Some b, [ "x"; "y" ])`
          let rec getPath
            (pathSoFar : NEList<string>)
            (subExpr : Expr)
            : (Option<string> * NEList<string>) =
            match subExpr with
            | EFieldAccess(_, subExpr, childFieldName) ->
              getPath (NEList.push childFieldName pathSoFar) subExpr

            | EVariable(_, v) -> (Some v, pathSoFar)

            | _ -> error "Support for field access in DB queries may be incomplete"

          let (varName, fieldAccessPath) =
            getPath (NEList.singleton fieldname) subExpr

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
              return error $"We do not yet support compiling this code: {expr}"

          let rec dbFieldType
            (typeRef : TypeReference)
            (fieldName : string)
            : Ply<TypeReference> =
            uply {
              match typeRef with
              // TYPESCLEANUP use typeArgs
              | TCustomType(Ok typeName, _typeArgs) ->
                match! Types.find typeName types with
                | Some({ definition = TypeDeclaration.Alias aliasedType }) ->
                  return! dbFieldType aliasedType fieldName

                | Some({ definition = TypeDeclaration.Record fields }) ->
                  let field = fields |> NEList.find (fun f -> f.name = fieldName)
                  match field with
                  | Some v -> return v.typ
                  | None ->
                    return
                      error2 "The datastore does not have a field named" fieldName

                | Some({ definition = TypeDeclaration.Enum _ }) ->
                  return
                    error2
                      "The datastore's type is not a record - it's an enum"
                      (TypeName.toString typeName)

                | None ->
                  return
                    error2
                      "The datastore does not have a type named"
                      (TypeName.toString typeName)
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

          typecheck fieldname dbFieldType expectedType

          let rec primitiveFieldType t =
            match t with
            | TString -> "text"
            | TInt -> "bigint"
            | TFloat -> "double precision"
            | TBool -> "bool"
            | TDateTime -> "timestamp with time zone"
            | TChar -> "text"
            | TUuid -> "uuid"
            | TUnit -> "bigint" // CLEANUP why is this bigint?
            | TVariable varName ->
              match Map.get varName tat with
              | Some found -> primitiveFieldType found
              | None ->
                error $"Could not resolve type variable in lambdaToSql: {varName}"
            | _ -> error $"We do not support this type of DB field yet: {t}"

          let fieldAccessPath = NEList.map escapeFieldname fieldAccessPath
          match dbFieldType with
          | TString
          | TInt
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
//     value.age < Int.sqrt (String.length (String.append a b))
//
//  into
//
//   let myAge = Int.sqrt (String.length (String::append a b))
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
  (tat : TypeArgTable)
  (symtable : Symtable)
  (paramName : string)
  (body : Expr)
  : Ply.Ply<DvalMap * Expr> =
  uply {

    // This isn't really a good implementation, but right now we only do
    // straight-line code here, so it should work
    let symtable = ref symtable

    let exec (expr : Expr) : Ply.Ply<Expr> =
      uply {
        let newName = "dark_generated_" + randomString 8
        let! value = LibExecution.Interpreter.eval state tat symtable.Value expr
        symtable.Value <- Map.add newName value symtable.Value
        return (EVariable(gid (), newName))
      }

    let f (expr : Expr) : Ply.Ply<Expr> =
      uply {
        // We list any construction that we think is safe to evaluate in now in the
        // interpreter instead of in the DB. Anything immutable should be good,
        // including literals and variables with known values (so not `paramName`)
        match expr with
        | EFieldAccess(_, EVariable(_, name), _) when name <> paramName ->
          return! exec expr
        | EFieldAccess(_, ERecord _, _) ->
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
            | EInt _
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
        | EInt _
        | EFloat _
        | EBool _
        | EUnit _
        | EConstant _
        | EChar _
        | ELet _
        | EIf _
        | ELambda _
        | EFieldAccess _
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
            | EInt _
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
            | EFieldAccess(id, expr, fieldname) ->
              let! expr = r expr
              return EFieldAccess(id, expr, fieldname)
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
            | EMatch(id, mexpr, pairs) ->
              let! mexpr = r mexpr

              let! pairs =
                Ply.NEList.mapSequentially
                  (fun (pat, expr) ->
                    uply {
                      let! expr = r expr
                      return (pat, expr)
                    })
                  pairs

              return EMatch(id, mexpr, pairs)
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
    return (symtable.Value, result)
  }


type CompileLambdaResult = { sql : string; vars : List<string * SqlValue> }

let compileLambda
  (state : ExecutionState)
  (tat : TypeArgTable)
  (symtable : DvalMap)
  (paramName : string)
  (dbType : TypeReference)
  (body : Expr)
  : Ply<Result<CompileLambdaResult, RuntimeError>> =
  uply {
    try
      let! symtable, body =
        body
        // Simplify for later passes
        |> canonicalize
        // Inline the RHS of any let within the lambda body. See comment for more
        // details.
        |> inline' paramName Map.empty
        // Replace expressions which can be calculated now with their result. See
        // comment for more details.
        |> partiallyEvaluate state tat symtable paramName
      let types = ExecutionState.availableTypes state
      let fns = ExecutionState.availableFunctions state
      let constants = ExecutionState.availableConstants state

      let! compiled =
        lambdaToSql fns types constants tat symtable paramName dbType TBool body

      return Ok { sql = compiled.sql; vars = compiled.vars }

    with
    | SqlCompilerRuntimeError internalError ->
      let err = RuntimeError.sqlCompilerRuntimeError internalError
      return Error err

    | SqlCompilerException errStr -> return Error(RuntimeError.oldError errStr)
  }
