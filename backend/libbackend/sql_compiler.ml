open Core_kernel
open Libexecution
open Types
open Types.RuntimeT
open Types.RuntimeT.DbT
module RT = Runtime
open Db

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
  | "==" | "notEquals" ->
      "="
  | "!=" | "equals" ->
      "<>"
  | "&&" ->
      "AND"
  | "||" ->
      "OR"
  | _ ->
      Exception.internal ("op not supported: " ^ op)


let unary_op_to_sql op : string =
  match op with
  | "Bool::not" ->
      "not"
  | _ ->
      Exception.internal ("op not supported: " ^ op)


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
      Exception.internal ("unsupported DB field tipe" ^ show_tipe_ t)


let rec inline (symtable : expr Prelude.StrDict.t) (expr : expr) : expr =
  match expr with
  | Filled (_, Let (Filled (_, name), expr, body)) ->
      inline (Prelude.StrDict.insert ~key:name ~value:expr symtable) body
  | Filled (_, Variable name) when name <> "value" ->
    ( match Prelude.StrDict.get ~key:name symtable with
    | Some expr ->
        expr
    | None ->
        Exception.internal ("variable not defined: " ^ name) )
  | _ ->
      Ast.traverse ~f:(inline symtable) expr


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
              Exception.internal
                ("unsupport expression in pipe: " ^ show_expr expr))
  | _ ->
      Ast.traverse ~f:canonicalize expr


let dval_to_sql (dval : dval) : string = "null"

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
        Exception.internal ("Variable is undefined: " ^ name) )
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
            Exception.internal ("DB does not have field named: " ^ fieldname)
      in
      "CAST(data::jsonb->>'"
      ^ fieldname
      ^ "' as "
      ^ tipe_to_sql_tipe tipe
      ^ ") "
  | _ ->
      Exception.internal
        ("unsupported code in DB::filter query: " ^ show_expr expr)


let compile_lambda
    (symtable : dval_map)
    (paramName : string)
    (dbFields : tipe_ Prelude.StrDict.t)
    (body : expr) =
  body
  |> canonicalize
  |> inline Prelude.StrDict.empty
  |> lambda_to_sql_inner symtable paramName dbFields
