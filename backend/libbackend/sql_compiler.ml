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
  | "Int::substract" ->
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


let unary_op_to_sql op : tipe_ * tipe_ * string =
  match op with
  | "Bool::not" ->
      (TBool, TBool, "not")
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


(* Inline `let` statements directly into where they are used. Since the code
 * in the lambda is supposed to be pure, inlining it should work. It may be
 * that using SQL variables for this is possible, but I couldn't find a way
 * to do that. *)
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
  | DFloat _
  | DBytes _ ->
      error2 "This value is not yet supported" (Dval.to_developer_repr_v0 dval)
  | DInt i ->
      (* types don't line up to use Db.Int *)
      Db.escape_string (Dint.to_string i)
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
      ^ Dval.tipename dval )


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
      let arg_tipe, result_tipe, opname = unary_op_to_sql op in
      typecheck op result_tipe expected_tipe ;
      "(" ^ opname ^ " " ^ lts arg_tipe e ^ ")"
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


let compile_lambda
    (symtable : dval_map)
    (paramName : string)
    (dbFields : tipe_ Prelude.StrDict.t)
    (body : expr) : string =
  body
  |> canonicalize
  |> inline paramName Prelude.StrDict.empty
  |> lambda_to_sql symtable paramName dbFields TBool
