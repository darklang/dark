open Core_kernel

let gid = Libshared.Shared.gid

(* match id, then the pattern id. We have a pattern id cause they can be
 * nested. *)
let literalToString
    (v : [> `Bool of bool | `Int of string | `Null | `Float of string * string])
    : string =
  match v with
  | `Int i ->
      i
  | `String str ->
      "\"" ^ str ^ "\""
  | `Bool b ->
      if b then "true" else "false"
  | `Null ->
      "null"
  | `Float (whole, fraction) ->
      whole ^ "." ^ fraction


let parseString str :
    [> `Bool of bool
    | `Int of string
    | `Null
    | `Float of string * string
    | `Unknown ] =
  (* Only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
  let is63BitInt (s : string) : bool =
    try
      let i = Int63.of_string s in
      Int63.of_int (-4611686018427387903) <= i
      && i <= Int63.of_int 4611686018427387903
    with _ -> false
  in
  let asBool =
    if str = "true"
    then Some (`Bool true)
    else if str = "false"
    then Some (`Bool false)
    else if str = "null"
    then Some `Null
    else None
  in
  let asInt = if is63BitInt str then Some (`Int str) else None in
  let asFloat =
    try
      (* for the exception *)
      ignore (float_of_string str) ;
      match String.split ~on:'.' str with
      | [whole; fraction] ->
          Some (`Float (whole, fraction))
      | _ ->
          None
    with _ -> None
  in
  let asString =
    if Tc.String.startsWith ~prefix:"\"" str
       && Tc.String.endsWith ~suffix:"\"" str
    then
      Some
        (`String
          (str |> Tc.String.dropLeft ~count:1 |> Tc.String.dropRight ~count:1))
    else None
  in
  asInt
  |> Tc.Option.orElse asString
  |> Tc.Option.orElse asBool
  |> Tc.Option.orElse asFloat
  |> Tc.Option.withDefault ~default:`Unknown


let rec fromFluidPattern (p : Libshared.FluidPattern.t) : Types.RuntimeT.pattern
    =
  match p with
  | FPVariable (_, id, var) ->
      Filled (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      Filled (id, PConstructor (name, List.map ~f:fromFluidPattern patterns))
  | FPInteger (_, id, i) ->
      Filled (id, PLiteral (literalToString (`Int i)))
  | FPBool (_, id, b) ->
      Filled (id, PLiteral (literalToString (`Bool b)))
  | FPString {patternID = id; str; matchID = _} ->
      Filled (id, PLiteral (literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      Filled (id, PLiteral (literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      Filled (id, PLiteral (literalToString `Null))
  | FPBlank (_, id) ->
      Blank id


and fromFluidExpr (expr : Libshared.FluidExpression.t) : Types.RuntimeT.expr =
  let open Types in
  let open Types.RuntimeT in
  let open Libshared.FluidExpression in
  let newB () = Types.Blank (gid ()) in
  let rec fromFluidExpr ?(inPipe = false) expr =
    (* inPipe is whether it's the immediate child of a pipe. *)
    let r = fromFluidExpr ~inPipe:false in
    match expr with
    | EInteger (id, num) ->
        Filled (id, Value (literalToString (`Int num)))
    | EString (id, str) ->
        Filled (id, Value (literalToString (`String str)))
    | EFloat (id, whole, fraction) ->
        Filled (id, Value (literalToString (`Float (whole, fraction))))
    | EBool (id, b) ->
        Filled (id, Value (literalToString (`Bool b)))
    | ENull id ->
        Filled (id, Value (literalToString `Null))
    | EVariable (id, var) ->
        Filled (id, Variable var)
    | EFieldAccess (id, obj, "") ->
        Filled (id, FieldAccess (fromFluidExpr obj, Blank (gid ())))
    | EFieldAccess (id, obj, fieldname) ->
        Filled (id, FieldAccess (fromFluidExpr obj, Filled (gid (), fieldname)))
    | EFnCall (id, name, args, ster) ->
      ( match args with
      | EPipeTarget _ :: _ when not inPipe ->
          newB ()
      | EPipeTarget _ :: args when inPipe ->
          if ster = Rail
          then Filled (id, FnCallSendToRail (name, List.map ~f:r args))
          else Filled (id, FnCall (name, List.map ~f:r args))
      | _nonPipeTarget :: _ when inPipe ->
          newB ()
      | args ->
          if ster = Rail
          then Filled (id, FnCallSendToRail (name, List.map ~f:r args))
          else Filled (id, FnCall (name, List.map ~f:r args)) )
    | EBinOp (id, name, arg1, arg2, ster) ->
      ( match arg1 with
      | EPipeTarget _ when not inPipe ->
          newB ()
      | EPipeTarget _ when inPipe ->
          if ster = Rail
          then Filled (id, FnCallSendToRail (name, [fromFluidExpr arg2]))
          else Filled (id, FnCall (name, [fromFluidExpr arg2]))
      | _nonPipeTarget when inPipe ->
          newB ()
      | _ ->
          if ster = Rail
          then
            Filled
              ( id
              , FnCallSendToRail (name, [fromFluidExpr arg1; fromFluidExpr arg2])
              )
          else
            Filled (id, FnCall (name, [fromFluidExpr arg1; fromFluidExpr arg2]))
      )
    | ELambda (id, vars, body) ->
        Filled
          ( id
          , Lambda
              ( List.map vars ~f:(fun (vid, var) -> Types.Filled (vid, var))
              , fromFluidExpr body ) )
    | EBlank id ->
        Blank id
    | ELet (id, lhs, rhs, body) ->
        Filled
          (id, Let (Filled (gid (), lhs), fromFluidExpr rhs, fromFluidExpr body))
    | EIf (id, cond, thenExpr, elseExpr) ->
        Filled
          ( id
          , If
              ( fromFluidExpr cond
              , fromFluidExpr thenExpr
              , fromFluidExpr elseExpr ) )
    | EPartial (id, str, oldVal) ->
        Filled (id, FluidPartial (str, fromFluidExpr ~inPipe oldVal))
    | ERightPartial (id, str, oldVal) ->
        Filled (id, FluidRightPartial (str, fromFluidExpr ~inPipe oldVal))
    | EList (id, exprs) ->
        Filled (id, ListLiteral (List.map ~f:r exprs))
    | ERecord (id, pairs) ->
        Filled
          ( id
          , ObjectLiteral
              (List.map pairs ~f:(fun (k, v) ->
                   (Types.Filled (gid (), k), fromFluidExpr v))) )
    | EPipe (id, exprs) ->
      ( match exprs with
      | head :: tail ->
          Filled
            ( id
            , Thread (r head :: List.map ~f:(fromFluidExpr ~inPipe:true) tail)
            )
      | [] ->
          Blank id )
    | EConstructor (id, name, exprs) ->
        Filled (id, Constructor (Filled (gid (), name), List.map ~f:r exprs))
    | EMatch (id, mexpr, pairs) ->
        let pairs =
          List.map pairs ~f:(fun (p, e) ->
              (fromFluidPattern p, fromFluidExpr e))
        in
        Filled (id, Match (fromFluidExpr mexpr, pairs))
    | EPipeTarget _ ->
        newB ()
    | EFeatureFlag (id, name, cond, caseA, caseB) ->
        Filled
          ( id
          , FeatureFlag
              ( Filled (gid (), name)
              , fromFluidExpr cond
              , fromFluidExpr ~inPipe caseA
              , fromFluidExpr ~inPipe caseB ) )
  in
  fromFluidExpr expr


let rec toFluidPattern (mid : Types.id) (p : Types.RuntimeT.pattern) :
    Libshared.FluidPattern.t =
  match p with
  | Partial (id, _) | Blank id ->
      FPBlank (mid, id)
  | Filled (id, np) ->
    ( match np with
    | PVariable name ->
        FPVariable (mid, id, name)
    | PConstructor (name, patterns) ->
        FPConstructor (mid, id, name, List.map ~f:(toFluidPattern mid) patterns)
    | PLiteral str ->
      ( match parseString str with
      | `Bool b ->
          FPBool (mid, id, b)
      | `Int i ->
          FPInteger (mid, id, i)
      | `String s ->
          FPString {matchID = mid; patternID = id; str = s}
      | `Null ->
          FPNull (mid, id)
      | `Float (whole, fraction) ->
          FPFloat (mid, id, whole, fraction)
      | `Unknown ->
          FPBlank (mid, id) ) )


let rec toFluidExpr ?(inPipe = false) (expr : Types.RuntimeT.expr) :
    Libshared.FluidExpression.t =
  let open Types in
  let open RuntimeT in
  let open Libshared.FluidExpression in
  let f = toFluidExpr ~inPipe:false in
  let varToName var =
    match var with Blank _ -> "" | Filled (_, name) -> name | Partial _ -> ""
  in
  match expr with
  | Blank id ->
      EBlank id
  | Partial (id, _) ->
      EBlank id
  | Filled (id, nExpr) ->
    ( match nExpr with
    | Let (name, rhs, body) ->
        ELet (id, varToName name, f rhs, f body)
    | Variable varname ->
        EVariable (id, varname)
    | If (cond, thenExpr, elseExpr) ->
        EIf (id, f cond, f thenExpr, f elseExpr)
    | ListLiteral exprs ->
        EList (id, List.map ~f exprs)
    | ObjectLiteral pairs ->
        ERecord (id, List.map pairs ~f:(fun (k, v) -> (varToName k, f v)))
    | FieldAccess (expr, field) ->
        EFieldAccess (id, f expr, varToName field)
    | FnCallSendToRail (name, args) ->
        let args = List.map ~f args in
        (* add a pipetarget in the front *)
        let args = if inPipe then EPipeTarget (gid ()) :: args else args in
        let fnCall = EFnCall (id, name, args, Rail) in
        ( match Libs.get_fn ~user_fns:[] name with
        | Some fn when Tc.List.member ~value:name fn.infix_names ->
          ( match args with
          | [a; b] ->
              EBinOp (id, name, a, b, Rail)
          | _ ->
              fnCall )
        | _ ->
            fnCall )
    | FnCall (name, args) ->
        let args = List.map ~f args in
        (* add a pipetarget in the front *)
        let args = if inPipe then EPipeTarget (gid ()) :: args else args in
        let fnCall = EFnCall (id, name, args, NoRail) in
        ( match Libs.get_fn ~user_fns:[] name with
        | Some fn when Tc.List.member ~value:name fn.infix_names ->
          ( match args with
          | [a; b] ->
              EBinOp (id, name, a, b, NoRail)
          | _ ->
              fnCall )
        | _ ->
            fnCall )
    | Thread exprs ->
      ( match exprs with
      | head :: tail ->
          EPipe (id, f head :: List.map ~f:(toFluidExpr ~inPipe:true) tail)
      | _ ->
          EBlank (gid ()) )
    | Lambda (varnames, exprs) ->
        ELambda
          ( id
          , List.map varnames ~f:(fun var ->
                (Ast.blank_to_id var, varToName var))
          , f exprs )
    | Value str ->
      ( match parseString str with
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
        EConstructor (id, varToName name, List.map ~f exprs)
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
          , f cond
          , toFluidExpr ~inPipe casea
          , toFluidExpr ~inPipe caseb )
    | FluidPartial (str, oldExpr) ->
        EPartial (id, str, toFluidExpr ~inPipe oldExpr)
    | FluidRightPartial (str, oldExpr) ->
        ERightPartial (id, str, toFluidExpr ~inPipe oldExpr) )
