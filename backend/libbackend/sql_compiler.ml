open Core_kernel
open Libexecution
open Types
open Types.RuntimeT
open Types.RuntimeT.DbT
module RT = Runtime
open Db
module E = Libshared.FluidExpression

let error str = raise (DBQueryException str)

let error2 msg str = error (msg ^ ": " ^ str)

type position =
  | First
  | Last

let binop_to_sql (op : string) : tipe * tipe * tipe * string =
  let allInts str = (TInt, TInt, TInt, str) in
  let allFloats str = (TFloat, TFloat, TFloat, str) in
  let boolOp tipe str = (tipe, tipe, TBool, str) in
  let dateOp str = (TDate, TDate, TDate, str) in
  match op with
  | ">" | "<" | "<=" | ">=" ->
      boolOp TInt op
  | "+" | "-" | "*" | "/" | "%" | "^" ->
      allInts op
  | "Date::<" | "Date::lessThan" ->
      boolOp TDate "<"
  | "Date::>" | "Date::greaterThan" ->
      boolOp TDate ">"
  | "Date::<=" | "Date::lessThanOrEqualTo" ->
      boolOp TDate "<="
  | "Date::>=" | "Date::greaterThanOrEqualTo" ->
      boolOp TDate ">="
  | "Date::subtract" ->
      dateOp "-"
  | "Date::add" ->
      dateOp "+"
  | "Int::mod" ->
      allInts "%"
  | "Int::add" ->
      allInts "+"
  | "Int::subtract" ->
      allInts "-"
  | "Int::multiply" ->
      allInts "*"
  | "Int::power" ->
      allInts "^"
  | "Int::divide" ->
      allInts "/"
  | "Int::greaterThan" ->
      boolOp TInt ">"
  | "Int::greaterThanOrEqualTo" ->
      boolOp TInt ">="
  | "Int::lessThan" ->
      boolOp TInt "<"
  | "Int::lessThanOrEqualTo" ->
      boolOp TInt "<="
  | "Float::mod" ->
      allFloats "%"
  | "Float::add" ->
      allFloats "+"
  | "Float::subtract" ->
      allFloats "-"
  | "Float::multiply" ->
      allFloats "*"
  | "Float::power" ->
      allFloats "^"
  | "Float::divide" ->
      allFloats "/"
  | "Float::greaterThan" ->
      boolOp TFloat ">"
  | "Float::greaterThanOrEqualTo" ->
      boolOp TFloat ">="
  | "Float::lessThan" ->
      boolOp TFloat "<"
  | "Float::lessThanOrEqualTo" ->
      boolOp TFloat "<="
  | "==" | "equals" ->
      boolOp TAny "="
  | "!=" | "notEquals" ->
      boolOp TAny "<>"
  | "&&" ->
      boolOp TBool "AND"
  | "||" ->
      boolOp TBool "OR"
  | _ ->
      error2 "This function is not yet implemented" op


let unary_op_to_sql op : tipe * tipe * string * string list * position =
  (* Returns a postgres function name, and arguments to the function. The
   * argument the user provides will be inserted as the First or Last argument. *)
  match op with
  | "Bool::not" ->
      (TBool, TBool, "not", [], First)
  (* Not sure if any of the string functions are strictly correct for unicode *)
  | "String::toLowercase" | "String::toLowercase_v1" ->
      (TStr, TStr, "lower", [], First)
  | "String::toUppercase" | "String::toUppercase_v1" ->
      (TStr, TStr, "upper", [], First)
  | "String::length" ->
      (* There is a unicode version of length but it only works on bytea data *)
      (TStr, TInt, "length", [], First)
  | "String::reverse" ->
      (TStr, TStr, "reverse", [], First)
  | "String::trim" ->
      (TStr, TStr, "trim", [], First)
  | "String::trimStart" ->
      (TStr, TStr, "ltrim", [], First)
  | "String::trimEnd" ->
      (TStr, TStr, "rtrim", [], First)
  | "Date::hour_v1" ->
      (TDate, TInt, "date_part", ["'hour'"], Last)
  | _ ->
      error2 "This function is not yet implemented" op


let tipe_to_sql_tipe (t : tipe) : string =
  match t with
  | TStr ->
      "text"
  | TInt ->
      "integer"
  | TFloat ->
      "double precision"
  | TBool ->
      "bool"
  | TDate ->
      "timestamp with time zone"
  | _ ->
      error2 "We do not support this type of DB field yet" (show_tipe t)


(* This canonicalizes an expression, meaning it removes multiple ways of
 * representing the same thing. For now, it removes threads and replaces
 * them with nested function calls.
 *
 * Replaces
 *
 *  a
 *  |> function1 b
 *  |> function2 c d
 *
 * with
 *
 *  (function2 c d (function1 b a))
 *
 * *)
let rec canonicalize (expr : E.t) : E.t =
  let open E in
  postTraversal expr ~f:(function
      | EPipe (id, []) ->
          EBlank id
      | EPipe (id, head :: tail) ->
          Prelude.List.foldl tail ~init:head ~f:(fun expr arg ->
              match expr with
              | EFnCall (id, name, EPipeTarget _ :: args, NoRail) ->
                  EFnCall (id, name, arg :: args, NoRail)
              | EBinOp (id, name, EPipeTarget _, r, NoRail) ->
                  EBinOp (id, name, arg, r, NoRail)
              | _ ->
                  error2
                    "Currently, only function calls are supported in Pipes"
                    (show_fluid_expr expr))
      | e ->
          e)


let dval_to_sql (dval : dval) : string =
  match dval with
  | DObj _
  | DList _
  | DResp _
  | DBlock _
  | DError _
  | DCharacter _
  | DDB _
  | DIncomplete _
  | DPassword _
  | DOption _
  | DErrorRail _
  | DResult _
  | DBytes _ ->
      error2 "This value is not yet supported" (Dval.to_developer_repr_v0 dval)
  | DDate date ->
      Printf.sprintf
        (* Note: a previous iteration of this (during development, never
         * shipped) used Util.isostring_of_date and skipped the
         * ::jsonb->'value'; however, then we risk the serialization format
         * changing in one place and not the other. By doing it how we are now,
         * we make this consistent with the serialization used to store Dates in
         * the DB *)
        "CAST(%s::jsonb->>'value' as timestamp with time zone)"
        (Db.escape (String (DDate date |> Dval.to_internal_queryable_field_v1)))
  | DInt i ->
      (* types don't line up to use Db.Int *)
      Db.escape_string (Dint.to_string i)
  | DFloat v ->
      Db.escape (Float v)
  | DBool b ->
      Db.escape (Bool b)
  | DNull ->
      Db.escape Null
  | DStr s ->
      Db.escape (String (Unicode_string.to_string s))
  | DUuid id ->
      Db.escape (Uuid id)


(* TODO: support characters, floats, dates, and uuids. And maybe lists and
 * bytes. Probably something can be done with options and results. *)

let typecheckDval (name : string) (dval : dval) (expected_tipe : tipe) : unit =
  if Dval.tipe_of dval = expected_tipe || expected_tipe = TAny
  then ()
  else
    error
      ( "Incorrect type in `"
      ^ name
      ^ "`, expected "
      ^ Dval.tipe_to_string expected_tipe
      ^ " but got a "
      ^ Dval.pretty_tipename dval
      ^ " in "
      ^ Dval.to_developer_repr_v0 dval )


let typecheck (name : string) (actual_tipe : tipe) (expected_tipe : tipe) : unit
    =
  if actual_tipe = expected_tipe || expected_tipe = TAny
  then ()
  else
    error
      ( "Incorrect type in `"
      ^ name
      ^ "`, expected "
      ^ Dval.tipe_to_string expected_tipe
      ^ " but got a "
      ^ Dval.tipe_to_string actual_tipe )


(* Inline `let` statements directly into where they are used. Replaces
 *
 *   let y = 10
 *   DB::query Person \value ->
 *     let x = 5
 *     value.age < x
 *
 *  with
 *   let y = 10
 *   DB::query Person \value ->
 *     value.age < 5
 *
 * The main purpose of inlining is to get `value.fieldname` inlined. Other
 * expressions are handled by a later partial_evaluation pass (which relies on
 * this pass having run first).
 *
 * We need to inline value.fieldname because it can't be computed at
 * compile-time, (value here is the variable whose name symbolizes the DB rows
 * eg value.name, value.age, etc).
 *
 * It's possible that we over-inline here, and introduce duplicate code that
 * has slightly different behaviour. For example, converting
 *
 *  DB::query Person \value ->
 *    let x = launch_the_missiles ()
 *    value.person < x + x
 *
 * into
 *
 *  DB::query Person \value ->
 *    value.person < launch_the_missiles () + launch_the_missiles ()
 *
 * As a first attempt, this is fine, as it's hard to avoid without making a
 * more sophisticated algorithm, or iterating between inlining and partial
 * evaluation. We expect users to write pure code in here, and if they don't
 * we can solve that in the next version. *)
let rec inline
    (paramName : string) (symtable : E.t Prelude.StrDict.t) (expr : E.t) : E.t =
  E.postTraversal expr ~f:(function
      | ELet (_, name, expr, body) ->
          inline
            paramName
            (Prelude.StrDict.insert ~key:name ~value:expr symtable)
            body
      | EVariable (_, name) as expr when name <> paramName ->
        ( match Prelude.StrDict.get ~key:name symtable with
        | Some found ->
            found
        | None ->
            (* the variable might be in the symtable, so put it back to fill in
             * later *)
            expr )
      | expr ->
          expr)


(* Generate SQL from an Expr. This expects that all the hard stuff has been
 * removed by previous passes, and should only be called as the final pass. *)
let rec lambda_to_sql
    (symtable : dval_map)
    (paramName : string)
    (dbFields : tipe Prelude.StrDict.t)
    (expected_tipe : tipe)
    (expr : E.t) : string =
  let lts tipe e = lambda_to_sql symtable paramName dbFields tipe e in
  match expr with
  (* The correct way to handle null in SQL is "is null" or "is not null"
   * rather than a comparison with null. *)
  | EBinOp (_, "==", ENull _, e, NoRail) | EBinOp (_, "==", e, ENull _, NoRail)
    ->
      "(" ^ lts TNull e ^ " is null)"
  | EBinOp (_, "!=", ENull _, e, NoRail) | EBinOp (_, "!=", e, ENull _, NoRail)
    ->
      "(" ^ lts TNull e ^ " is not null)"
  | EFnCall (_, "String::isSubstring_v1", [lookingIn; searchingFor], NoRail)
  | EFnCall (_, "String::contains", [lookingIn; searchingFor], NoRail) ->
      (* strpos returns indexed from 1; 0 means missing *)
      "(strpos(" ^ lts TStr lookingIn ^ ", " ^ lts TStr searchingFor ^ ") > 0)"
  | EFnCall (_, op, [l; r], NoRail) | EBinOp (_, op, l, r, NoRail) ->
      let ltipe, rtipe, result_tipe, opname = binop_to_sql op in
      typecheck op result_tipe expected_tipe ;
      "(" ^ lts ltipe l ^ " " ^ opname ^ " " ^ lts rtipe r ^ ")"
  | EFnCall (_, op, [e], NoRail) ->
      let arg_tipe, result_tipe, opname, args, position = unary_op_to_sql op in
      typecheck op result_tipe expected_tipe ;
      let args =
        match position with
        | First ->
            Tc.String.join ~sep:", " (lts arg_tipe e :: args)
        | Last ->
            Tc.String.join ~sep:", " (List.append args [lts arg_tipe e])
      in
      "(" ^ opname ^ " (" ^ args ^ "))"
  | EVariable (_, name) ->
    ( match DvalMap.get ~key:name symtable with
    | Some dval ->
        typecheckDval name dval expected_tipe ;
        "(" ^ dval_to_sql dval ^ ")"
    | None ->
        error2 "This variable is not defined" name )
  | EInteger (_, str) ->
      let dval = DInt (Dint.of_string_exn str) in
      typecheckDval str dval expected_tipe ;
      "(" ^ dval_to_sql dval ^ ")"
  | EBool (id, bool) ->
      let dval = DBool bool in
      typecheckDval (if bool then "true" else "false") dval expected_tipe ;
      "(" ^ dval_to_sql dval ^ ")"
  | ENull _ ->
      typecheckDval "null" DNull expected_tipe ;
      "(" ^ dval_to_sql DNull ^ ")"
  | EFloat (_, whole, fraction) ->
      let str = whole ^ "." ^ fraction in
      let dval = Dval.parse_literal str |> Option.value_exn in
      typecheckDval str dval expected_tipe ;
      "(" ^ dval_to_sql dval ^ ")"
  | EString (_, str) ->
      let dval = Dval.dstr_of_string_exn str in
      typecheckDval ("\"" ^ str ^ "\"") dval expected_tipe ;
      "(" ^ dval_to_sql dval ^ ")"
  | EFieldAccess (_, EVariable (_, v), fieldname) when v = paramName ->
      let tipe =
        match Prelude.StrDict.get dbFields ~key:fieldname with
        | Some v ->
            v
        | None ->
            error2 "The datastore does not have a field named" fieldname
      in
      if expected_tipe <> TNull (* Fields are allowed be null *)
      then typecheck fieldname tipe expected_tipe ;
      ( match expected_tipe with
      | TDate ->
          (* This match arm handles types that are serialized in
           * unsafe_dval_to_yojson using wrap_user_type or wrap_user_str, maning
           * they are wrapped in {type:, value:}.  Right now, of the types sql
           * compiler supports, that's just TDate.
             Likely future candidates include DPassword and DUuid; at time of
             writing, DCharacter and DBytes also serialize this way but are not
             allowed as DB field types. *)
          Printf.sprintf
            "(CAST(data::jsonb->'%s'->>'value' as %s))"
            (Db.escape_string fieldname)
            (Db.escape_string (tipe_to_sql_tipe tipe))
      | _ ->
          Printf.sprintf
            "(CAST(data::jsonb->>'%s' as %s))"
            (Db.escape_string fieldname)
            (Db.escape_string (tipe_to_sql_tipe tipe)) )
  | _ ->
      error2 "We do not yet support compiling this code" (E.show expr)


(* Trying to get rid of complex expressions, including values which can be
 * evaluated at compile-time, expressions that rely on external values. We do
 * this by evaluating them and moving their results into the symbol table.
 *
 * The purpose of this step is as a convenience to the user. We could force
 * them to rewrite:
 *
 *  Db::query Person \value ->
 *    value.age < Int::sqrt (String::length (String::append a b))
 *
 * into
 *
 *  let myAge = Int::sqrt (String::length (String::append a b))
 *  Db::query Person \value ->
 *    value.age < myAge
 *
 * This is simply a convenience function to do that. Since users don't have a
 * good understanding of where execution is happening, they expect to be able
 * to do this, and really they're not wrong.
 *
 * This should work on all expressions which operate on known values at
 * run-time (since this compiler operates at run-time, we have all the values
 * except the ones in the DB).
 *
 * Expects inlining to have finished first, so that it has all the values it
 * needs in the right place. *)
let partially_evaluate
    (state : exec_state)
    (param_name : string)
    (symtable : dval_map)
    (body : E.t) : dval_map * E.t =
  (* This isn't really a good implementation, but right now we only do
   * straight-line code here, so it should work *)
  let open E in
  let symtable = ref symtable in
  let exec expr =
    let gid = Libshared.Shared.gid in
    let new_name = "dark_generated_" ^ Util.random_string 8 in
    let value = state.exec ~state !symtable expr in
    symtable := DvalMap.insert ~key:new_name ~value !symtable ;
    EVariable (gid (), new_name)
  in
  let f expr =
    (* We list any construction that we think is safe to evaluate *)
    match expr with
    | EFieldAccess (_, EVariable (_, name), field) when name <> param_name ->
        exec expr
    | EFieldAccess (_, ERecord _, _) ->
        (* inlining can create these situations *)
        exec expr
    | EFnCall (_, name, args, _)
      when Tc.List.all args ~f:(function
               | EInteger _
               | EBool _
               | ENull _
               | EFloat _
               | EString _
               | EVariable _ ->
                   true
               | _ ->
                   false) ->
        (* functions that are fully specified *)
        (* TODO: should limit this further to pure functions. *)
        exec expr
    | _ ->
        expr
  in
  let result = E.postTraversal ~f body in
  (!symtable, result)


let compile_lambda
    ~(state : exec_state)
    (symtable : dval_map)
    (param_name : string)
    (db_fields : tipe Prelude.StrDict.t)
    (body : fluid_expr) : string =
  let symtable, body =
    body
    (* Replace threads with nested function calls - simplifies all later passes *)
    |> canonicalize
    (* Inline the rhs of any let within the lambda body. See comment for more
     * details. *)
    |> inline param_name Prelude.StrDict.empty
    (* Replace expressions which can be calculated now with their result. See
     * comment for more details. *)
    |> partially_evaluate state param_name symtable
  in
  body |> lambda_to_sql symtable param_name db_fields TBool
