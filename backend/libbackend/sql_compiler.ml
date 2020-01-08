open Core_kernel
open Libexecution
open Types
open Types.RuntimeT
open Types.RuntimeT.DbT
module RT = Runtime
open Db

let error str = raise (DBFilterException str)

let error2 msg str = error (msg ^ ": " ^ str)

let binop_to_sql op : string =
  match op with
  | ">" | "<" | "<=" | ">=" | "+" | "-" | "*" | "/" | "%" | "^" ->
      op
  | "Int::mod" ->
      "%"
  | "Int::add" ->
      "+"
  | "Int::substract" ->
      "-"
  | "Int::multiply" ->
      "*"
  | "Int::power" ->
      "^"
  | "Int::divide" ->
      "/"
  | "Int::greaterThan" ->
      ">"
  | "Int::greaterThanOrEqualTo" ->
      ">="
  | "Int::lessThan" ->
      "<"
  | "Int::lessThanOrEqualTo" ->
      "%"
  | "==" | "equals" ->
      "="
  | "!=" | "notEquals" ->
      "<>"
  | "&&" ->
      "AND"
  | "||" ->
      "OR"
  | _ ->
      error2 "op not supported" op


let unary_op_to_sql op : string =
  match op with "Bool::not" -> "not" | _ -> error2 "op not supported" op


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
      error2 "unsupported DB field tipe" (show_tipe_ t)


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
              error2 "unsupport expression in pipe" (show_expr expr))
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
      error2 "unsupported value" (Dval.to_developer_repr_v0 dval)
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


let rec lambda_to_sql_inner
    (symtable : dval_map)
    (paramName : string)
    (dbFields : tipe_ Prelude.StrDict.t)
    (expr : expr) : string =
  let lts e = lambda_to_sql_inner symtable paramName dbFields e in
  match expr with
  | Filled (_, FnCall ("==", [Filled (_, Value "null"); e]))
  | Filled (_, FnCall ("==", [e; Filled (_, Value "null")])) ->
      "(" ^ lts e ^ " is null)"
  | Filled (_, FnCall ("!=", [Filled (_, Value "null"); e]))
  | Filled (_, FnCall ("!=", [e; Filled (_, Value "null")])) ->
      "(" ^ lts e ^ " is not null)"
  | Filled (_, FnCall (op, [l; r])) ->
      "(" ^ lts l ^ " " ^ binop_to_sql op ^ " " ^ lts r ^ ")"
  | Filled (_, FnCall (op, [e])) ->
      "(" ^ unary_op_to_sql op ^ " " ^ lts e ^ ")"
  | Filled (_, Variable name) ->
    ( match DvalMap.get ~key:name symtable with
    | Some dval ->
        dval_to_sql dval
    | None ->
        error2 "Variable is undefined" name )
  | Filled (_, Value str) ->
      let dval = Dval.parse_literal str |> Option.value_exn in
      "(" ^ dval_to_sql dval ^ ")"
  | Filled (_, FieldAccess (Filled (_, Variable v), Filled (_, fieldname)))
    when v = paramName ->
      let tipe =
        match Prelude.StrDict.get dbFields ~key:fieldname with
        | Some v ->
            v
        | None ->
            error2 "DB does not have field named" fieldname
      in
      "(CAST(data::jsonb->>'"
      ^ Db.escape_string fieldname
      ^ "' as "
      ^ Db.escape_string (tipe_to_sql_tipe tipe)
      ^ "))"
  | _ ->
      error2 "unsupported code in DB::filter query" (show_expr expr)


let compile_lambda
    (symtable : dval_map)
    (paramName : string)
    (dbFields : tipe_ Prelude.StrDict.t)
    (body : expr) : string =
  body
  |> canonicalize
  |> inline paramName Prelude.StrDict.empty
  |> lambda_to_sql_inner symtable paramName dbFields
