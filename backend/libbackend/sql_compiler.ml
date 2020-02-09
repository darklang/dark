open Core_kernel
open Libexecution
open Types
open Types.RuntimeT
open Types.RuntimeT.DbT
module RT = Runtime
open Db

let error str = raise (DBQueryException str)

let error2 msg str = error (msg ^ ": " ^ str)

let binop_to_sql (op : string) : tipe_ * tipe_ * tipe_ * string =
  let allInts str = (TInt, TInt, TInt, str) in
  let allFloats str = (TFloat, TFloat, TFloat, str) in
  let boolOp tipe str = (tipe, tipe, TBool, str) in
  match op with
  | ">" | "<" | "<=" | ">=" ->
      boolOp TInt op
  | "+" | "-" | "*" | "/" | "%" | "^" ->
      allInts op
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
      boolOp TInt "%"
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
      boolOp TFloat "%"
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


let unary_op_to_sql op : tipe_ * tipe_ * string * string list =
  (* Returns a postgres function name, and arguments to the function. The
   * argument the user provides will be inserted as the first argument. *)
  match op with
  | "Bool::not" ->
      (TBool, TBool, "not", [])
  (* Not sure if any of the string functions are strictly correct for unicode *)
  | "String::toLowercase" | "String::toLowercase_v1" ->
      (TStr, TStr, "lower", [])
  | "String::toUppercase" | "String::toUppercase_v1" ->
      (TStr, TStr, "upper", [])
  | "String::length" ->
      (* There is a unicode version of length but it only works on bytea data *)
      (TStr, TInt, "length", [])
  | "String::reverse" ->
      (TStr, TStr, "reverse", [])
  | _ ->
      error2 "This function is not yet implemented" op


let tipe_to_sql_tipe (t : tipe_) : string =
  match t with
  | TStr ->
      "text"
  | TInt ->
      "integer"
  | TFloat ->
      "double precision"
  | TBool ->
      "bool"
  | _ ->
      error2 "We do not support this type of DB field yet" (show_tipe_ t)


(* This canonicalizes an expression, meaning it removes multiple ways of
 * representing the same thing. For now, it removes threads and replaces
 * them with nested function calls. *)
let rec canonicalize expr =
  match expr with
  | Filled (id, Thread []) ->
      Blank id
  | Filled (id, Thread (head :: tail)) ->
      Prelude.List.foldl tail ~init:head ~f:(fun expr arg ->
          match expr with
          | Filled (id, FnCall (name, args)) ->
              Filled (id, FnCall (name, arg :: args))
          | _ ->
              error2
                "Currently, only function calls are supported in Pipes"
                (show_expr expr))
  | _ ->
      Ast.traverse ~f:canonicalize expr


let dval_to_sql (dval : dval) : string =
  match dval with
  | DObj _
  | DList _
  | DResp _
  | DDate _
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

let typecheckDval (name : string) (dval : dval) (expected_tipe : tipe_) : unit =
  if Dval.tipe_of dval = expected_tipe || expected_tipe = TAny
  then ()
  else
    error
      ( "Incorrect type in `"
      ^ name
      ^ "`, expected "
      ^ Dval.tipe_to_string expected_tipe
      ^ " but got a "
      ^ Dval.tipename dval
      ^ " in "
      ^ Dval.to_developer_repr_v0 dval )


let typecheck (name : string) (actual_tipe : tipe_) (expected_tipe : tipe_) :
    unit =
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


(* Inline `let` statements directly into where they are used. Since the code in
* the lambda is supposed to be pure, inlining it should work. It may be that
* using SQL variables for this is possible, but I couldn't find a way to do
* that. This is important because value.field can only be evaluated at query
* run-time, so we need to make sure they're propagated. *)

let rec inline
    (paramName : string) (symtable : expr Prelude.StrDict.t) (expr : expr) :
    expr =
  match expr with
  | Filled (_, Let (Filled (_, name), expr, body)) ->
      inline
        paramName
        (Prelude.StrDict.insert ~key:name ~value:expr symtable)
        body
  | Filled (_, Variable name) when name <> paramName ->
    ( match Prelude.StrDict.get ~key:name symtable with
    | Some found ->
        found
    | None ->
        (* the variable might be in the symtable, so put it back to fill in
         * later *)
        expr )
  | _ ->
      Ast.traverse ~f:(inline paramName symtable) expr


let rec lambda_to_sql
    (symtable : dval_map)
    (paramName : string)
    (dbFields : tipe_ Prelude.StrDict.t)
    (expected_tipe : tipe_)
    (expr : expr) : string =
  let lts tipe e = lambda_to_sql symtable paramName dbFields tipe e in
  match expr with
  (* The correct way to handle null in SQL is "is null" or "is not null"
   * rather than a comparison with null. *)
  | Filled (_, FnCall ("==", [Filled (_, Value "null"); e]))
  | Filled (_, FnCall ("==", [e; Filled (_, Value "null")])) ->
      "(" ^ lts TNull e ^ " is null)"
  | Filled (_, FnCall ("!=", [Filled (_, Value "null"); e]))
  | Filled (_, FnCall ("!=", [e; Filled (_, Value "null")])) ->
      "(" ^ lts TNull e ^ " is not null)"
  | Filled (_, FnCall (op, [l; r])) ->
      let ltipe, rtipe, result_tipe, opname = binop_to_sql op in
      typecheck op result_tipe expected_tipe ;
      "(" ^ lts ltipe l ^ " " ^ opname ^ " " ^ lts rtipe r ^ ")"
  | Filled (_, FnCall (op, [e])) ->
      let arg_tipe, result_tipe, opname, args = unary_op_to_sql op in
      typecheck op result_tipe expected_tipe ;
      let args = Tc.String.join ~sep:", " (lts arg_tipe e :: args) in
      "(" ^ opname ^ " (" ^ args ^ "))"
  | Filled (_, Variable name) ->
    ( match DvalMap.get ~key:name symtable with
    | Some dval ->
        typecheckDval name dval expected_tipe ;
        "(" ^ dval_to_sql dval ^ ")"
    | None ->
        error2 "This variable is not defined" name )
  | Filled (_, Value str) ->
      let dval = Dval.parse_literal str |> Option.value_exn in
      typecheckDval str dval expected_tipe ;
      "(" ^ dval_to_sql dval ^ ")"
  | Filled (_, FieldAccess (Filled (_, Variable v), Filled (_, fieldname)))
    when v = paramName ->
      let tipe =
        match Prelude.StrDict.get dbFields ~key:fieldname with
        | Some v ->
            v
        | None ->
            error2 "The datastore does not have a field named" fieldname
      in
      if expected_tipe <> TNull (* Fields are allowed be null *)
      then typecheck fieldname tipe expected_tipe ;
      "(CAST(data::jsonb->>'"
      ^ Db.escape_string fieldname
      ^ "' as "
      ^ Db.escape_string (tipe_to_sql_tipe tipe)
      ^ "))"
  | _ ->
      error2 "We do not yet support compiling this code" (show_expr expr)


(* Trying to get rid of complex expressions, including values which can be
 * evaluated at compile-time, expressions that rely on external values. We do
 * this by evaluating them and moving their results into the symbol table. *)
let partially_evaluate
    (state : exec_state)
    (param_name : string)
    (symtable : dval_map)
    (body : expr) : dval_map * expr =
  (* This isn't really a good implementation, but right now we only do
   * straight-line code here, so it should work *)
  let symtable = ref symtable in
  Libcommon.Log.inspecT "body before" ~f:show_expr body ;
  let exec expr =
    let gid = Libshared.Shared.gid in
    let new_name = "dark_generated_" ^ Util.random_string 8 in
    let value = state.exec ~state !symtable expr in
    symtable := DvalMap.insert ~key:new_name ~value !symtable ;
    Filled (gid (), Variable new_name)
  in
  let f expr =
    (* We list any construction that we think is safe to evaluate *)
    match expr with
    | Filled (_, FieldAccess (Filled (_, Variable name), Filled (_, field)))
      when name <> param_name ->
        exec expr
    | Filled (_, FieldAccess (Filled (_, ObjectLiteral _), Filled (_, _))) ->
        (* inlining can create these situations *)
        exec expr
    | Filled (_, FnCall (_, args))
      when Tc.List.all args ~f:(function
               | Filled (_, (Value _ | Variable _)) ->
                   true
               | _ ->
                   false) ->
        (* functions that are fully specified *)
        (* TODO: should limit this further to pure functions. *)
        exec expr
    | _ ->
        expr
  in
  let result = Ast.postTraverse ~f body in
  Libcommon.Log.inspecT "body after" ~f:show_expr result ;
  (!symtable, result)


let compile_lambda
    ~(state : exec_state)
    (symtable : dval_map)
    (param_name : string)
    (db_fields : tipe_ Prelude.StrDict.t)
    (body : expr) : string =
  let symtable, body =
    body
    (* remove threads *)
    |> canonicalize
    (* remove lets within the body *)
    |> inline param_name Prelude.StrDict.empty
    (* remove external or complex expressions *)
    |> partially_evaluate state param_name symtable
  in
  body
  |> lambda_to_sql symtable param_name db_fields TBool
  |> Libcommon.Log.inspect "final_sql"
