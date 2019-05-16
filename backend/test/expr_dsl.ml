open Core_kernel
open Libcommon
open Libexecution
open Types
open Types.RuntimeT
open Ast

let b () = Blank (Util.create_id ())

let f a = Filled (Util.create_id (), a)

let is_fn (name : string) : bool = Libs.get_fn ~user_fns:[] name <> None

let b_or_f (name : string) : string or_blank =
  match name with "_" -> b () | name -> f name


let rail2fnname name : string = String.slice name 1 0

let is_send_to_rail (name : string) : bool =
  String.is_prefix ~prefix:"`" name && name |> rail2fnname |> is_fn


let rec ast_for_ (sexp : Sexp.t) : expr =
  match sexp with
  (* SendToRail: (`List::head []) *)
  | Sexp.List (Sexp.Atom fnname :: args) when is_send_to_rail fnname ->
      f (FnCallSendToRail (rail2fnname fnname, List.map args ~f:ast_for_))
  (* fncall: (List::add 1 2) *)
  | Sexp.List (Sexp.Atom fnname :: args) when is_fn fnname ->
      f (FnCall (fnname, List.map args ~f:ast_for_))
  (* blocks (\\x -> (List::head [])) *)
  | Sexp.List [Sexp.Atom var; Sexp.Atom "->"; body]
    when String.is_prefix ~prefix:"\\" var ->
      let var = String.lstrip ~drop:(( = ) '\\') var in
      f (Lambda ([f var], ast_for_ body))
  | Sexp.List [Sexp.Atom var1; Sexp.Atom var2; Sexp.Atom "->"; body]
    when String.is_prefix ~prefix:"\\" var1 ->
      let var1 = String.lstrip ~drop:(( = ) '\\') var1 in
      f (Lambda ([f var1; f var2], ast_for_ body))
  (* let: (let a 1 2) *)
  | Sexp.List [Sexp.Atom "let"; Sexp.Atom var; value; body] ->
      f (Let (b_or_f var, ast_for_ value, ast_for_ body))
  (* if: (if a 1 2) *)
  | Sexp.List [Sexp.Atom "if"; cond; ifbody; elsebody] ->
      f (If (ast_for_ cond, ast_for_ ifbody, ast_for_ elsebody))
  (* feature-flag: (flag a 1 2) *)
  | Sexp.List [Sexp.Atom "flag"; Sexp.Atom name; cond; oldcode; newcode] ->
      f
        (FeatureFlag
           (b_or_f name, ast_for_ cond, ast_for_ oldcode, ast_for_ newcode))
  (* thread: (| 5 (+ 4) (+ 3))  *)
  | Sexp.List (Sexp.Atom "|" :: exprs) ->
      f (Thread (List.map exprs ~f:ast_for_))
  (* objects: (obj (a 4) (b 6)) *)
  | Sexp.List (Sexp.Atom "obj" :: rest) ->
      let to_pair pair =
        match pair with
        | Sexp.List [Sexp.Atom key; value] ->
            (b_or_f key, ast_for_ value)
        | x ->
            Log.infO "pair" ~data:(Log.dump pair) ;
            failwith "invalid pair when creating obj"
      in
      let args = List.map ~f:to_pair rest in
      f (ObjectLiteral args)
  (* field access: (. (obj (a 5)) a) *)
  | Sexp.List [Sexp.Atom "."; obj; Sexp.Atom field] ->
      f (FieldAccess (ast_for_ obj, b_or_f field))
  (* lists: (5 6) *)
  | Sexp.List args ->
      f (ListLiteral (List.map ~f:ast_for_ args))
  (* blanks: (let _ _ _) *)
  | Sexp.Atom "_" ->
      b ()
  | Sexp.Atom "[]" ->
      f (ListLiteral [])
  | Sexp.Atom "{}" ->
      f (ObjectLiteral [])
  | Sexp.Atom "nothing" ->
      f (Constructor (f "Nothing", []))
  | Sexp.Atom "ok" ->
      f (Constructor (f "Ok", []))
  (* literals / variables *)
  | Sexp.Atom value ->
    ( match Dval.parse_literal value with
    | Some v ->
        f (Value value)
    | None ->
        f (Variable value) )


let ast_for (ast : string) : expr =
  let quotes = Re2.create_exn "'(.*?)'" in
  ast
  |> (fun s ->
       (* dunno whether it's a bug or just annoying, but we need to specify
     * quotes as \"\\\", so let's "'" instead. (The ocaml parser demands
     * we insert a " here smdh) *)
       Re2.replace_exn quotes s ~f:(fun m ->
           "\"\\\"" ^ Re2.Match.get_exn ~sub:(`Index 1) m ^ "\\\"\"" ) )
  |> Sexp.of_string
  (* |> (fun s -> *)
  (*       let b = Buffer.create 16000 in *)
  (*       Sexp.to_buffer_hum b s; *)
  (*       Log.inspecT "buf" (Buffer.contents b); *)
  (*       s *)
  (* ) *)
  |> ast_for_
  |> Log.inspect ~f:show_expr "expr"
