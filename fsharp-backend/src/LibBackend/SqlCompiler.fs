module LibBackend.SqlCompiler

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp.Tasks
open Npgsql
open Db

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr

module Ast = LibExecution.Ast

let error (str : string) : 'a =
  printfn $"DB Exception: {str}"
  raise (DBQueryException str)

let error2 (msg : string) (str : string) : 'a = error $"{msg}: {str}"

type position =
  | First
  | Last

// let compilerSupportedFns =
//   ; "Float::mod"
//   ; "Float::add"
//   ; "Float::subtract"
//   ; "Float::multiply"
//   ; "Float::power"
//   ; "Float::divide"
//   ; "Float::greaterThan"
//   ; "Float::greaterThanOrEqualTo"
//   ; "Float::lessThan"
//   ; "Float::lessThanOrEqualTo"
//   ; "Bool::not"
//   ; "String::toLowercase"
//   ; "String::toLowercase_v1"
//   ; "String::toUppercase"
//   ; "String::toUppercase_v1"
//   ; "String::length"
//   ; "String::reverse"
//   ; "String::trim"
//   ; "String::trimStart"
//   ; "String::trimEnd"
//   ; "Date::hour_v1"
//   ; "Date::day"
//   ; "Date::minute"
//   ; "Date::month"
//   ; "Date::second"
//   ; "Date::year"
//   ; "Date::atStartOfDay"
//   ; "String::isSubstring_v1"
//   ; "String::contains"
//   ; "String::replaceAll"
//
//
// match op with
// | "Float::mod" -> allFloats "%"
// | "Float::add" -> allFloats "+"
// | "Float::subtract" -> allFloats "-"
// | "Float::multiply" -> allFloats "*"
// | "Float::power" -> allFloats "^"
// | "Float::divide" -> allFloats "/"
// | "Float::greaterThan" -> boolOp TFloat ">"
// | "Float::greaterThanOrEqualTo" -> boolOp TFloat ">="
// | "Float::lessThan" -> boolOp TFloat "<"
// | "Float::lessThanOrEqualTo" -> boolOp TFloat "<="


let unaryOpToSql op : DType * DType * string * string list * position =
  // Returns a postgres function name, and arguments to the function. The
  // argument the user provides will be inserted as the First or Last argument. *)
  match op.ToString() with
  | "Bool::not" -> (TBool, TBool, "not", [], First)
  (* Not sure if any of the string functions are strictly correct for unicode *)
  | "String::toLowercase"
  | "String::toLowercase_v1" -> (TStr, TStr, "lower", [], First)
  | "String::toUppercase"
  | "String::toUppercase_v1" -> (TStr, TStr, "upper", [], First)
  | "String::length" ->
      (* There is a unicode version of length but it only works on bytea data *)
      (TStr, TInt, "length", [], First)
  | "String::reverse" -> (TStr, TStr, "reverse", [], First)
  | "String::trim" -> (TStr, TStr, "trim", [], First)
  | "String::trimStart" -> (TStr, TStr, "ltrim", [], First)
  | "String::trimEnd" -> (TStr, TStr, "rtrim", [], First)
  | "Date::hour_v1" -> (TDate, TInt, "date_part", [ "'hour'" ], Last)
  | "Date::day" -> (TDate, TInt, "date_part", [ "'day'" ], Last)
  | "Date::minute" -> (TDate, TInt, "date_part", [ "'minute'" ], Last)
  | "Date::month" -> (TDate, TInt, "date_part", [ "'month'" ], Last)
  | "Date::second" -> (TDate, TInt, "date_part", [ "'second'" ], Last)
  | "Date::year" -> (TDate, TInt, "date_part", [ "'year'" ], Last)
  | "Date::atStartOfDay" -> (TDate, TInt, "date_trunc", [ "'day'" ], Last)
  | _ -> error2 "This function is not yet implemented" op


let typeToSqlType (t : DType) : string =
  match t with
  | TStr -> "text"
  | TInt -> "integer"
  | TFloat -> "double precision"
  | TBool -> "bool"
  | TDate -> "timestamp with time zone"
  | _ -> error $"We do not support this type of DB field yet: {t}"


// This canonicalizes an expression, meaning it removes multiple ways of
// representing the same thing. Currently nothing needs to be canonicalized.
let rec canonicalize (expr : Expr) : Expr = expr

let dvalToSql (dval : Dval) : SqlValue =
  match dval with
  | DFakeVal _ -> raise (Db.FakeValFoundInQuery dval)
  | DObj _
  | DList _
  | DHttpResponse _
  | DFnVal _
  | DChar _
  | DDB _
  // | DPassword _
  | DOption _
  | DResult _
  | DBytes _ ->
      error2 "This value is not yet supported" (DvalRepr.toDeveloperReprV0 dval)
  | DDate date -> Sql.timestamp date
  | DInt i ->
      // FSTODO: gonna have to do better than this for infinite precision
      Sql.int64 (int64 i)
  | DFloat v -> Sql.double v
  | DBool b -> Sql.bool b
  | DNull -> Sql.dbnull
  | DStr s -> Sql.string s
  | DUuid id -> Sql.uuid id


let typecheck (name : string) (actualType : DType) (expectedType : DType) : unit =
  match expectedType with
  | TVariable _ -> ()
  | TAny -> ()
  | other when actualType = other -> ()
  | _ ->
      let actual = DvalRepr.typeToDeveloperReprV0 actualType
      let expected = DvalRepr.typeToDeveloperReprV0 expectedType
      error $"Incorrect type in {name}, expected {expected}, but got a {actual}"

// (* TODO: support character. And maybe lists and
//  * bytes. Probably something can be done with options and results. *)
let typecheckDval (name : string) (dval : Dval) (expectedType : DType) : unit =
  if Dval.isFake dval then raise (Db.FakeValFoundInQuery dval)
  typecheck name (Dval.toType dval) expectedType

let escapeFieldname (str : string) : string =
  // Allow underscore, numbers, letters, only
  // TODO: should allow hyphen?
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
//  expressions are handled by a later partial_evaluation pass (which relies on
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
  Ast.postTraversal
    (function
    | ELet (_, name, expr, body) ->
        inline' paramName (Map.add name expr symtable) body
    | EVariable (_, name) as expr when name <> paramName ->
        (match Map.get name symtable with
         | Some found -> found
         | None ->
             // the variable might be in the symtable, so put it back to fill in later
             expr)
    | expr -> expr)
    expr

let (|Fn|_|) (mName : string) (fName : string) (v : int) (pattern : Expr) =
  match pattern with
  | EApply (_,
            EFQFnValue (_,
                        { owner = "dark"
                          package = "stdlib"
                          module_ = module_
                          function_ = function_
                          version = version }),
            args,
            _,
            NoRail) when module_ = mName && function_ = fName && version = v ->
      Some args
  | _ -> None

// Generate SQL from an Expr. This expects that all the hard stuff has been
// removed by previous passes, and should only be called as the final pass.
let rec lambdaToSql
  (fns : Map<FQFnName.T, BuiltInFn>)
  (symtable : DvalMap)
  (paramName : string)
  (dbFields : Map<string, DType>)
  (expectedType : DType)
  (expr : Expr)
  : string * List<string * SqlValue> =
  let lts (typ : DType) (e : Expr) =
    lambdaToSql fns symtable paramName dbFields typ e in

  // We don't have good string escaping facilities here, plus it was always a bit dangerous to have string escaoing as we night miss one.
  let vars = ref Map.empty

  match expr with
  // The correct way to handle null in SQL is "is null" or "is not null"
  // rather than a comparison with null. *)
  | Fn "" "==" 0 [ ENull _; e ]
  | Fn "" "==" 0 [ e; ENull _ ] ->
      let sql, vars = lts TNull e
      $"({sql} is null)", vars
  | Fn "" "!=" 0 [ ENull _; e ]
  | Fn "" "!=" 0 [ e; ENull _ ] ->
      let sql, vars = lts TNull e
      $"({sql} is not null)", vars
  | Fn "String" "isSubString" 1 [ lookingIn; searchingFor ]
  | Fn "String" "contains" 0 [ lookingIn; searchingFor ] ->
      let lookingInSql, vars1 = lts TStr lookingIn
      let searchingForSql, vars2 = lts TStr searchingFor
      // strpos returns indexed from 1; 0 means missing
      $"(strpos({lookingInSql}, {searchingForSql}) > 0)", vars1 @ vars2
  | Fn "String" "replaceAll" 0 [ lookingIn; searchingFor; replaceWith ] ->
      let lookingInSql, vars1 = lts TStr lookingIn
      let searchingForSql, vars2 = lts TStr searchingFor
      let replaceWithSql, vars3 = lts TStr replaceWith
      let vars = vars1 @ vars2 @ vars3
      $"(replace({lookingInSql}, {searchingForSql}, {replaceWithSql}))", vars
  | EApply (_, EFQFnValue (_, name), args, _, NoRail) ->
      match Map.get name fns with
      | Some fn ->
          typecheck (toString name) fn.returnType expectedType

          let argSqls, sqlVars =
            let paramCount = List.length fn.parameters
            let argCount = List.length args

            if argCount = paramCount then
              List.map2 (fun arg param -> lts param.typ arg) args fn.parameters
              |> List.unzip
              |> (fun (sqls, vars) -> (sqls, List.concat vars))
            else
              error
                $"{fn.name} has {paramCount} functions but we have {argCount} arguments"

          match fn, argSqls with
          | { sqlSpec = SqlBinOp op }, [ argL; argR ] ->
              $"({argL} {op} {argR})", sqlVars
          | { sqlSpec = SqlUnaryOp op }, [ argSql ] -> $"({op} {argSql})", sqlVars
          | { sqlSpec = SqlFunction fnname }, _ ->
              let argSql = String.concat ", " argSqls
              $"({fnname}({argSql}))", sqlVars
          | { sqlSpec = SqlFunctionWithPrefixArgs (fnName, fnArgs) }, _ ->
              let argSql = fnArgs @ argSqls |> String.concat ", "
              $"({fnName} ({argSql}))", sqlVars
          | { sqlSpec = SqlFunctionWithSuffixArgs (fnName, fnArgs) }, _ ->
              let argSql = argSqls @ fnArgs |> String.concat ", "
              $"({fnName} ({argSql}))", sqlVars

          | fn, args -> error $"This function ({name}) is not yet implemented"
      | None ->
          error
            $"Only builtin functions can be used in queries right now; {name} is not a builtin function"
  | EVariable (_, varname) ->
      match Map.get varname symtable with
      | Some dval ->
          typecheckDval $"variable {varname}" dval expectedType
          let random = randomString 8
          let newname = $"{varname}_{random}"
          $"(@{newname})", [ newname, dvalToSql dval ]
      | None -> error $"This variable is not defined: {varname}"
  | EInteger (_, v) ->
      typecheck $"integer {v}" TInt expectedType
      let name = randomString 10
      $"(@{name})", [ name, v |> int64 |> Sql.int64 ]
  | EBool (_, v) ->
      typecheck $"bool {v}" TBool expectedType
      let name = randomString 10
      $"(@{name})", [ name, Sql.bool v ]
  | ENull _ ->
      typecheck "value null" TNull expectedType
      let name = randomString 10
      $"(@{name})", [ name, Sql.dbnull ]
  | EFloat (_, v) ->
      typecheck $"float {v}" TFloat expectedType
      let name = randomString 10
      $"(@{name})", [ name, Sql.double v ]
  | EString (_, v) ->
      typecheck $"string \"{v}\"" TStr expectedType
      let name = randomString 10
      $"(@{name})", [ name, Sql.string v ]
  | EFieldAccess (_, EVariable (_, v), fieldname) when v = paramName ->
      let typ =
        match Map.get fieldname dbFields with
        | Some v -> v
        | None -> error2 "The datastore does not have a field named" fieldname

      if expectedType <> TNull (* Fields are allowed be null *) then
        typecheck fieldname typ expectedType

      let fieldname = escapeFieldname fieldname
      let typename = typeToSqlType typ

      (match expectedType with
       | TDate ->
           // This match arm handles types that are serialized in
           // unsafe_dval_to_yojson using wrap_user_type or wrap_user_str, meaning
           // they are wrapped in {type:, value:}. Right now, of the types sql
           // compiler supports, that's just TDate.
           // Likely future candidates include DPassword and DUuid; at time of
           // writing, DCharacter and DBytes also serialize this way but are not
           // allowed as DB field types.
           ($"(CAST(data::jsonb->'{fieldname}'->>'value' as {typename}))", [])
       | _ -> ($"(CAST(data::jsonb->>'{fieldname}' as {typename}))", []))
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
  : TaskOrValue<DvalMap * Expr> =
  taskv {

    // This isn't really a good implementation, but right now we only do
    // straight-line code here, so it should work *)
    let symtable = ref symtable

    let exec (expr : Expr) : TaskOrValue<Expr> =
      taskv {
        let newName = "dark_generated_" + randomString 8
        let! value = LibExecution.Interpreter.eval state !symtable expr
        symtable := Map.add newName value !symtable
        return (EVariable(gid (), newName))
      }

    let f (expr : Expr) : TaskOrValue<Expr> =
      taskv {
        // We list any construction that we think is safe to evaluate
        match expr with
        | EFieldAccess (_, EVariable (_, name), _) when name <> paramName ->
            return! exec expr
        | EFieldAccess (_, ERecord _, _) ->
            // inlining can create these situations
            return! exec expr
        | EApply (_, EFQFnValue (_, name), args, _, _) when
          // functions that are fully specified
          List.all
            (function
            | EInteger _
            | EBool _
            | ENull _
            | EFloat _
            | EString _
            | EVariable _ -> true
            | _ -> false)
            args ->
            // TODO: should limit this further to pure functions.
            return! exec expr
        | _ -> return expr
      }

    // This is a copy of Ast.postTraversal, made to  work with taskvs
    let rec postTraversal (expr : Expr) : TaskOrValue<Expr> =
      let r = postTraversal in

      taskv {
        let! result =
          taskv {
            match expr with
            | EInteger _
            | EBlank _
            | EString _
            | EVariable _
            | ECharacter _
            | EFQFnValue _
            | EBool _
            | ENull _
            | EFloat _ -> return expr
            | ELet (id, name, rhs, next) ->
                let! rhs = r rhs
                let! next = r next
                return ELet(id, name, rhs, next)
            | EApply (id, name, exprs, inPipe, ster) ->
                let! exprs = map_s r exprs
                return EApply(id, name, exprs, inPipe, ster)
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
                let! exprs = map_s r exprs
                return EList(id, exprs)
            | EMatch (id, mexpr, pairs) ->
                let! mexpr = r mexpr

                let! pairs =
                  map_s
                    (fun (name, expr) ->
                      taskv {
                        let! expr = r expr
                        return (name, expr)
                      })
                    pairs

                return EMatch(id, mexpr, pairs)
            | ERecord (id, fields) ->
                let! fields =
                  map_s
                    (fun (name, expr) ->
                      taskv {
                        let! expr = r expr
                        return (name, expr)
                      })
                    fields

                return ERecord(id, fields)
            | EConstructor (id, name, exprs) ->
                let! exprs = map_s r exprs
                return EConstructor(id, name, exprs)
            | EPartial (id, oldExpr) ->
                let! oldExpr = r oldExpr
                return EPartial(id, oldExpr)
            | EFeatureFlag (id, cond, casea, caseb) ->
                let! cond = r cond
                let! casea = r casea
                let! caseb = r caseb
                return EFeatureFlag(id, cond, casea, caseb)
          }

        return! f result
      }

    let! result = postTraversal body
    return (!symtable, result)
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
      |> TaskOrValue.toTask

    // printfn $"AST being compiled: {body} with {dbFields} and {symtable}"

    return lambdaToSql state.functions symtable paramName dbFields TBool body
  }
