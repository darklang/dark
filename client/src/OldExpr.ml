open Tc
open Types
open Prelude

(* ----------------- *)
(* Types *)
(* ----------------- *)

type varBind = string Types.blankOr

type field = string Types.blankOr

type key = string Types.blankOr

type lambdaParameter = string Types.blankOr

type nPattern =
  | PVariable of string
  | PLiteral of string
  | PConstructor of string * pattern list

and pattern = nPattern Types.blankOr

type expr = nExpr Types.blankOr

and nExpr =
  | If of expr * expr * expr
  | FnCall of Types.fnName Types.blankOr * expr list * Types.sendToRail
  | Variable of string
  | Let of varBind * expr * expr
  | Lambda of lambdaParameter list * expr
  | Value of string
  | ObjectLiteral of (key * expr) list
  | ListLiteral of expr list
  | Thread of expr list
  | FieldAccess of expr * field
  | FeatureFlag of string Types.blankOr * expr * expr * expr
  | Match of expr * (pattern * expr) list
  | Constructor of string Types.blankOr * expr list
  | FluidPartial of string * expr
  | FluidRightPartial of string * expr

(* ----------------- *)
(* Expressions *)
(* ----------------- *)
let rec toFluidExpr' ?(inPipe = false) (expr : expr) : FluidExpression.t =
  let fns =
    assertFn
      ~f:(( <> ) [])
      "empty functions passed to toFluidExpr'"
      !FluidExpression.functions
  in
  let f = toFluidExpr' ~inPipe:false in
  let varToName var = match var with Blank _ -> "" | F (_, name) -> name in
  match expr with
  | Blank id ->
      EBlank id
  | F (id, nExpr) ->
    ( match nExpr with
    | Let (name, rhs, body) ->
        ELet (id, Blank.toID name, varToName name, f rhs, f body)
    | Variable varname ->
        EVariable (id, varname)
    | If (cond, thenExpr, elseExpr) ->
        EIf (id, f cond, f thenExpr, f elseExpr)
    | ListLiteral exprs ->
        EList (id, List.map ~f exprs)
    | ObjectLiteral pairs ->
        ERecord
          ( id
          , List.map pairs ~f:(fun (k, v) -> (Blank.toID k, varToName k, f v))
          )
    | FieldAccess (expr, field) ->
        EFieldAccess (id, f expr, Blank.toID field, varToName field)
    | FnCall (name, args, ster) ->
        let args = List.map ~f args in
        (* add a pipetarget in the front *)
        let args = if inPipe then EPipeTarget (gid ()) :: args else args in
        let fnCall = EFnCall (id, varToName name, args, ster) in
        let fn = List.find fns ~f:(fun fn -> fn.fnName = varToName name) in
        ( match fn with
        | Some fn when fn.fnInfix ->
          ( match args with
          | [a; b] ->
              EBinOp (id, varToName name, a, b, ster)
          | _ ->
              fnCall )
        | _ ->
            fnCall )
    | Thread exprs ->
      ( match exprs with
      | head :: tail ->
          EPipe (id, f head :: List.map ~f:(toFluidExpr' ~inPipe:true) tail)
      | _ ->
          recover "empty pipe" ~debug:expr (EBlank (gid ())) )
    | Lambda (varnames, exprs) ->
        ELambda
          ( id
          , List.map varnames ~f:(fun var -> (Blank.toID var, varToName var))
          , f exprs )
    | Value str ->
      ( match FluidUtil.parseString str with
      | `Bool b ->
          EBool (id, b)
      | `Int i ->
          EInteger (id, i)
      | `String s ->
          EString (id, s)
      | `Null ->
          ENull id
      | `Float (whole, fraction) ->
          EFloat (id, whole, fraction)
      | `Unknown ->
          EBlank id )
    | Constructor (name, exprs) ->
        EConstructor (id, Blank.toID name, varToName name, List.map ~f exprs)
    | Match (mexpr, pairs) ->
        let mid = id in
        let pairs =
          List.map pairs ~f:(fun (p, e) -> (toFluidPattern mid p, f e))
        in
        EMatch (id, f mexpr, pairs)
    | FeatureFlag (msg, cond, casea, caseb) ->
        EFeatureFlag
          ( id
          , varToName msg
          , Blank.toID msg
          , f cond
          , toFluidExpr' ~inPipe casea
          , toFluidExpr' ~inPipe caseb )
    | FluidPartial (str, oldExpr) ->
        EPartial (id, str, toFluidExpr' ~inPipe oldExpr)
    | FluidRightPartial (str, oldExpr) ->
        ERightPartial (id, str, toFluidExpr' ~inPipe oldExpr) )


and toFluidExpr (expr : expr) : FluidExpression.t = toFluidExpr' expr

and fromFluidExpr (expr : FluidExpression.t) : expr =
  let open Types in
  let rec fromFluidExpr ?(inPipe = false) expr =
    (* inPipe is whether it's the immediate child of a pipe. *)
    let r = fromFluidExpr ~inPipe:false in
    match expr with
    | EInteger (id, num) ->
        F (id, Value (FluidUtil.literalToString (`Int num)))
    | EString (id, str) ->
        F (id, Value (FluidUtil.literalToString (`String str)))
    | EFloat (id, whole, fraction) ->
        F (id, Value (FluidUtil.literalToString (`Float (whole, fraction))))
    | EBool (id, b) ->
        F (id, Value (FluidUtil.literalToString (`Bool b)))
    | ENull id ->
        F (id, Value (FluidUtil.literalToString `Null))
    | EVariable (id, var) ->
        F (id, Variable var)
    | EFieldAccess (id, obj, fieldID, "") ->
        F (id, FieldAccess (fromFluidExpr obj, Blank fieldID))
    | EFieldAccess (id, obj, fieldID, fieldname) ->
        F (id, FieldAccess (fromFluidExpr obj, F (fieldID, fieldname)))
    | EFnCall (id, name, args, ster) ->
      ( match args with
      | EPipeTarget _ :: _ when not inPipe ->
          recover "fn has a pipe target but no pipe" ~debug:expr (Blank.new_ ())
      | EPipeTarget _ :: args when inPipe ->
          F
            ( id
            , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
            )
      | _nonPipeTarget :: _ when inPipe ->
          recover "fn has a pipe but no pipe target" ~debug:expr (Blank.new_ ())
      | args ->
          F
            ( id
            , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
            ) )
    | EBinOp (id, name, arg1, arg2, ster) ->
      ( match arg1 with
      | EPipeTarget _ when not inPipe ->
          recover
            "binop has a pipe target but no pipe"
            ~debug:expr
            (Blank.new_ ())
      | EPipeTarget _ when inPipe ->
          F
            ( id
            , FnCall
                (F (ID (deID id ^ "_name"), name), [fromFluidExpr arg2], ster)
            )
      | _nonPipeTarget when inPipe ->
          recover
            "binop has a pipe but no pipe target"
            ~debug:expr
            (Blank.new_ ())
      | _ ->
          F
            ( id
            , FnCall
                ( F (ID (deID id ^ "_name"), name)
                , [fromFluidExpr arg1; fromFluidExpr arg2]
                , ster ) ) )
    | ELambda (id, vars, body) ->
        F
          ( id
          , Lambda
              ( List.map vars ~f:(fun (vid, var) -> Types.F (vid, var))
              , fromFluidExpr body ) )
    | EBlank id ->
        Blank id
    | ELet (id, lhsID, lhs, rhs, body) ->
        F (id, Let (F (lhsID, lhs), fromFluidExpr rhs, fromFluidExpr body))
    | EIf (id, cond, thenExpr, elseExpr) ->
        F
          ( id
          , If
              ( fromFluidExpr cond
              , fromFluidExpr thenExpr
              , fromFluidExpr elseExpr ) )
    | EPartial (id, str, oldVal) ->
        F (id, FluidPartial (str, fromFluidExpr ~inPipe oldVal))
    | ERightPartial (id, str, oldVal) ->
        F (id, FluidRightPartial (str, fromFluidExpr ~inPipe oldVal))
    | EList (id, exprs) ->
        F (id, ListLiteral (List.map ~f:r exprs))
    | ERecord (id, pairs) ->
        F
          ( id
          , ObjectLiteral
              (List.map pairs ~f:(fun (id, k, v) ->
                   (Types.F (id, k), fromFluidExpr v))) )
    | EPipe (id, exprs) ->
      ( match exprs with
      | head :: tail ->
          F
            ( id
            , Thread (r head :: List.map ~f:(fromFluidExpr ~inPipe:true) tail)
            )
      | [] ->
          Blank id )
    | EConstructor (id, nameID, name, exprs) ->
        F (id, Constructor (F (nameID, name), List.map ~f:r exprs))
    | EMatch (id, mexpr, pairs) ->
        let pairs =
          List.map pairs ~f:(fun (p, e) ->
              (fromFluidPattern p, fromFluidExpr e))
        in
        F (id, Match (fromFluidExpr mexpr, pairs))
    | EPipeTarget _ ->
        recover
          "Cant convert pipetargets back to exprs"
          ~debug:expr
          (Blank.new_ ())
    | EFeatureFlag (id, name, nameID, cond, caseA, caseB) ->
        F
          ( id
          , FeatureFlag
              ( F (nameID, name)
              , fromFluidExpr cond
              , fromFluidExpr ~inPipe caseA
              , fromFluidExpr ~inPipe caseB ) )
  in
  fromFluidExpr expr


(* ----------------- *)
(* Patterns *)
(* ----------------- *)
and fromFluidPattern (p : fluidPattern) : pattern =
  match p with
  | FPVariable (_, id, var) ->
      F (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      F (id, PConstructor (name, List.map ~f:fromFluidPattern patterns))
  | FPInteger (_, id, i) ->
      F (id, PLiteral (FluidUtil.literalToString (`Int i)))
  | FPBool (_, id, b) ->
      F (id, PLiteral (FluidUtil.literalToString (`Bool b)))
  | FPString (_, id, str) ->
      F (id, PLiteral (FluidUtil.literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      F (id, PLiteral (FluidUtil.literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      F (id, PLiteral (FluidUtil.literalToString `Null))
  | FPBlank (_, id) ->
      Blank id


and toFluidPattern (mid : id) (p : pattern) : fluidPattern =
  match p with
  | Blank id ->
      FPBlank (mid, id)
  | F (id, np) ->
    ( match np with
    | PVariable name ->
        FPVariable (mid, id, name)
    | PConstructor (name, patterns) ->
        FPConstructor (mid, id, name, List.map ~f:(toFluidPattern mid) patterns)
    | PLiteral str ->
      ( match FluidUtil.parseString str with
      | `Bool b ->
          FPBool (mid, id, b)
      | `Int i ->
          FPInteger (mid, id, i)
      | `String s ->
          FPString (mid, id, s)
      | `Null ->
          FPNull (mid, id)
      | `Float (whole, fraction) ->
          FPFloat (mid, id, whole, fraction)
      | `Unknown ->
          FPBlank (mid, id) ) )
