open Core_kernel

open Types
open Types.RuntimeT

module RT = Runtime

let blank_to_id (bo : 'a or_blank) : id =
  match bo with
  | Filled (id, _) -> id
  | Blank (id) -> id

let is_blank (bo: 'a or_blank) : bool =
  match bo with
  | Blank _ -> true
  | _ -> false

let rec blank_to_option (bo: 'a or_blank) : 'a option =
  match bo with
  | Blank _ -> None
  | Filled (_, a) -> Some a



let to_id (expr: expr) : id =
  blank_to_id expr

(* Co-recursive. See example below. *)
let rec traverse ~(f: expr -> expr) (expr:expr) : expr =
  match expr with
  | Blank _ -> expr
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

(* Example usage of traverse. See also AST.elm *)
let rec example_traversal expr =
  match expr with
  | Blank _ -> Filled (Util.create_id (), Value "\"example\"")
  | expr -> traverse ~f:example_traversal expr


let rec set_expr ~(search: id) ~(replacement: expr) (expr: expr) : expr =
  let replace = set_expr ~search ~replacement in
  if search = to_id expr
  then replacement
  else
    traverse ~f:replace expr


