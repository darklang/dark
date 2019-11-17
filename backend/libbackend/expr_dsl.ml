open Core_kernel
open Libcommon
open Libexecution
open Types
open Types.RuntimeT
open Ast

let b () = Blank (Util.create_id ())

let f a = Filled (Util.create_id (), a)

let is_fn (name : string) : bool =
  Libs.get_fn ~user_fns:[] name <> None
  || name = "fake_test_fn"
  || name = "test_fn"


let b_or_f (name : string) : string or_blank =
  match name with "_" -> b () | name -> f name


let string_of_orblank (sob : string or_blank) : string =
  match sob with
  | Blank _ ->
      "blank"
  | Filled (_, str) ->
      str
  | Partial _ ->
      "PARTIAL NOT IMPLEMENTED"


let rail2fnname name : string = String.slice name 1 0

let is_send_to_rail (name : string) : bool =
  String.is_prefix ~prefix:"`" name && name |> rail2fnname |> is_fn


let rec sexp_for_ (e : expr) : Sexp.t =
  match e with
  | Blank _ ->
      Sexp.Atom "_"
  | Partial _ ->
      Sexp.Atom "PARTIAL NOT IMPLEMENTED"
  | Filled (_, nexp) ->
    ( match nexp with
    | If (cond, ifbody, elsebody) ->
        Sexp.List
          [Sexp.Atom "if"; sexp_for_ cond; sexp_for_ ifbody; sexp_for_ elsebody]
    | Thread exprs ->
        Sexp.List (Sexp.Atom "|" :: List.map ~f:sexp_for_ exprs)
    | FnCall (fnname, args) ->
        Sexp.List (Sexp.Atom fnname :: List.map ~f:sexp_for_ args)
    | Variable varname ->
        Sexp.Atom varname
    | Let (var, value, body) ->
        (* TODO is this redundant? *)
        (* let var =
          match var with
          | Filled (_, var) ->
              var
          | Blank _ ->
              "_"
          | Partial _ ->
              "PARTIAL NOT IMPLEMENTED"
        in *)
        Sexp.List
          [ Sexp.Atom "let"
          ; Sexp.Atom (string_of_orblank var)
          ; sexp_for_ value
          ; sexp_for_ body ]
    | Lambda (varbinds, body) ->
        (* TODO check this works for arity > 1, and that ast_for_ does likewise
         *)
        Sexp.List
          [ Sexp.Atom
              ( varbinds
              |> List.map ~f:(fun v -> "\\" ^ string_of_orblank v)
              |> String.concat ~sep:" " )
          ; Sexp.Atom "->"
          ; sexp_for_ body ]
    | Value v ->
        (* TODO confirm? *)
        Sexp.Atom ("\"" ^ v ^ "\"")
    | FieldAccess (obj, fld) ->
        Sexp.List
          [Sexp.Atom "."; sexp_for_ obj; Sexp.Atom (string_of_orblank fld)]
    | ObjectLiteral alist ->
        Sexp.List
          ( Sexp.Atom "obj"
          :: ( alist
             |> List.map ~f:(fun (k, v) ->
                    Sexp.List [Sexp.Atom (string_of_orblank k); sexp_for_ v] )
             ) )
    | ListLiteral exprs ->
        Sexp.List (List.map ~f:sexp_for_ exprs)
    | FeatureFlag (name, cond, oldcode, newcode) ->
        Sexp.List
          [ Sexp.Atom "flag"
          ; Sexp.Atom (string_of_orblank name)
          ; sexp_for_ cond
          ; sexp_for_ oldcode
          ; sexp_for_ newcode ]
    | FnCallSendToRail (fnname, args) ->
        (* TODO confirm - rail2fnname? *)
        Sexp.List (Sexp.Atom ("`" ^ fnname) :: List.map ~f:sexp_for_ args)
    | Constructor (ctor, exprs) ->
        let ctor = ctor |> string_of_orblank in
        Sexp.List (Sexp.Atom ctor :: List.map ~f:sexp_for_ exprs)
    | Match (exp, pairs) ->
        Sexp.Atom "MATCH NOT IMPLEMENTED"
    | FluidPartial (str, exp) ->
        Sexp.Atom "PARTIAL NOT IMPLEMENTED"
    | FluidRightPartial (str, exp) ->
        Sexp.Atom "PARTIAL NOT IMPLEMENTED" )


let rec ast_for_ (sexp : Sexp.t) : expr =
  match sexp with
  (* SendToRail: (`List::head []) *)
  | Sexp.List (Sexp.Atom fnname :: args) when is_send_to_rail fnname ->
      f (FnCallSendToRail (rail2fnname fnname, List.map args ~f:ast_for_))
  (* fncall: (List::add 1 2) *)
  | Sexp.List (Sexp.Atom fnname :: args) when is_fn fnname ->
      f (FnCall (fnname, List.map args ~f:ast_for_))
  (* Constructors: (Just 2) *)
  | Sexp.List [Sexp.Atom "Just"; arg] ->
      f (Constructor (f "Just", [ast_for_ arg]))
  | Sexp.List [Sexp.Atom "Nothing"] ->
      f (Constructor (f "Nothing", []))
  | Sexp.List [Sexp.Atom "Ok"; arg] ->
      f (Constructor (f "Ok", [ast_for_ arg]))
  | Sexp.List [Sexp.Atom "Error"; arg] ->
      f (Constructor (f "Error", [ast_for_ arg]))
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
           "\"\\\"" ^ Re2.Match.get_exn ~sub:(`Index 1) m ^ "\\\"\""))
  |> Sexp.of_string
  (* |> (fun s -> *)
  (*       let b = Buffer.create 16000 in *)
  (*       Sexp.to_buffer_hum b s; *)
  (*       Log.inspecT "buf" (Buffer.contents b); *)
  (*       s *)
  (* ) *)
  |> ast_for_
  |> Log.inspect ~f:show_expr "expr"
