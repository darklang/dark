open Core_kernel

open Types
open Types.RuntimeT

module RT = Runtime
module FF = Feature_flag

let blank_to_id (bo : 'a or_blank) : id =
  match bo with
  | Filled (id, _) -> id
  | Blank (id) -> id
  | Flagged (id, _, _, _, _) -> id

let is_blank (bo: 'a or_blank) : bool =
  match bo with
  | Blank _ -> true
  | _ -> false

let rec blank_to_option (bo: 'a or_blank) : 'a option =
  match bo with
  | Blank _ -> None
  | Filled (_, a) -> Some a
  | Flagged (_, _, _, a, _)  -> blank_to_option a



let to_id (expr: expr) : id =
  blank_to_id expr


let rec traverse ~(f: expr -> expr) (expr:expr) : expr =
  match expr with
  | Blank _ -> expr
  | Flagged (id, msg, setting, l, r) ->
    Flagged (id, msg, setting, f l, f r)
  | Filled (id, nexpr) ->
    Filled (id,
            (match nexpr with
             | Value _ -> nexpr
             | Variable _ -> nexpr

             | Let (lhs, rhs, body) ->
               Let (lhs, f rhs, f body)

             | If (cond, ifbody, elsebody) ->
               If (f cond, f ifbody, f elsebody)

             | FnCall (name, exprs) ->
               FnCall (name, List.map ~f exprs)

             | Lambda (vars, lexpr) ->
               Lambda (vars, f lexpr)

             | Thread exprs ->
               Thread (List.map ~f exprs)

             | FieldAccess (obj, field) ->
               FieldAccess (f obj, field)

             | ListLiteral exprs ->
               ListLiteral (List.map ~f exprs)

             | ObjectLiteral pairs ->
               ObjectLiteral (List.map ~f:(fun (k, v) -> (k, f v)) pairs)

             | FeatureFlag (msg, cond, a, b) ->
               FeatureFlag (msg, f cond, f a, f b)
           ))

let rec set_expr ~(search: id) ~(replacement: expr) (expr: expr) : expr =
  let replace = set_expr ~search ~replacement in
  if search = to_id expr
  then replacement
  else
    traverse ~f:replace expr


