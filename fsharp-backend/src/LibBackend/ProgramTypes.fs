module LibBackend.ProgramTypes

// These are the types that are serialized for the program.

// The purpose of having this format is that it's easier to work with than the
// serialized formats (which have to handle legacy). So we transform any
// serialized formats into this one for running. We remove any "syntactic
// sugar" (editor/display only features).

// This format should never be serialized/deserialized, that defeats the purpose (convert it into a
// different format if you must, or track the other code along-side this and
// use the ID to find it).

open FSharpPlus

open Prelude
open Tablecloth

// Used for conversion functions
module RT = LibExecution.RuntimeTypes

// Expressions - the main part of the language.

module FQFnName =
  type T = RT.FQFnName.T

  let packageName = RT.FQFnName.packageName
  let userFnName = RT.FQFnName.userFnName
  let stdlibName = RT.FQFnName.stdlibName

  let parse (fnName : string) : T =
    let owner, package, module_, function_, version =
      match fnName with
      | Regex "^([a-z][a-z0-9_]*)/([a-z][a-z0-9A-Z]*)/([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
              [ owner; package; module_; name; version ] ->
          (owner, package, module_, name, int version)
      | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
              [ module_; name; version ] ->
          ("dark", "stdlib", module_, name, int version)
      | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)$" [ module_; name ] ->
          ("dark", "stdlib", module_, name, 0)
      | Regex "^([a-z][a-z0-9A-Z_]*)_v(\d+)$" [ name; version ] ->
          ("dark", "stdlib", "", name, int version)
      | Regex "^Date::([-+><&|!=^%/*]{1,2})$" [ name ] ->
          ("dark", "stdlib", "Date", name, 0)
      | Regex "^([-+><&|!=^%/*]{1,2})$" [ name ] -> ("dark", "stdlib", "", name, 0)
      | Regex "^([-+><&|!=^%/*]{1,2})_v(\d+)$" [ name; version ] ->
          ("dark", "stdlib", "", name, int version)
      | Regex "^([a-z][a-z0-9A-Z_]*)$" [ name ] -> ("dark", "stdlib", "", name, 0)
      | _ -> failwith $"Bad format in function name: \"{fnName}\""

    RT.FQFnName.packageName owner package module_ function_ version



type Expr =
  | EInteger of id * bigint
  | EBool of id * bool
  | EString of id * string
  | ECharacter of id * string
  // allow the user to have arbitrarily big numbers, even if they don't make sense as floats
  | EFloat of id * Sign * bigint * bigint
  | ENull of id
  | EBlank of id
  | ELet of id * string * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EBinOp of id * FQFnName.T * Expr * Expr * SendToRail
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * List<Expr> * SendToRail
  | EPartial of id * string * Expr
  | ERightPartial of id * string * Expr
  | ELeftPartial of id * string * Expr
  | EList of id * List<Expr>
  | ERecord of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  | EConstructor of id * string * List<Expr>
  | EMatch of id * Expr * List<Pattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr

  member this.testEqualIgnoringIDs(other : Expr) : bool =
    (* helpers for recursive calls *)
    let eq (e : Expr) (e' : Expr) = e.testEqualIgnoringIDs (e')

    let eqList l1 l2 = List.length l1 = List.length l2 && List.forall2 eq l1 l2

    match this, other with
    (* expressions with no values *)
    | ENull _, ENull _
    | EBlank _, EBlank _
    | EPipeTarget _, EPipeTarget _ -> true
    (* expressions with single string values *)
    | EString (_, v), EString (_, v')
    | ECharacter (_, v), ECharacter (_, v')
    | EVariable (_, v), EVariable (_, v') -> v = v'
    | EInteger (_, v), EInteger (_, v') -> v = v'
    | EFloat (_, s, w, f), EFloat (_, s', w', f') -> s = s' && w = w' && f = f'
    | EBool (_, v), EBool (_, v') -> v = v'
    | ELet (_, lhs, rhs, body), ELet (_, lhs', rhs', body') ->
        lhs = lhs' && eq rhs rhs' && eq body body'
    | EIf (_, con, thn, els), EIf (_, con', thn', els') ->
        eq con con' && eq thn thn' && eq els els'
    | EList (_, l), EList (_, l') -> eqList l l'
    | EFnCall (_, name, args, toRail), EFnCall (_, name', args', toRail') ->
        name = name' && eqList args args' && toRail = toRail'
    | EBinOp (_, name, lhs, rhs, toRail), EBinOp (_, name', lhs', rhs', toRail') ->
        name = name' && eq lhs lhs' && eq rhs rhs' && toRail = toRail'
    | ERecord (_, pairs), ERecord (_, pairs') ->
        let sort = List.sortBy (fun (k, _) -> k)

        List.forall2
          (fun (k, v) (k', v') -> k = k' && eq v v')
          (sort pairs)
          (sort pairs')
    | EFieldAccess (_, e, f), EFieldAccess (_, e', f') -> eq e e' && f = f'
    | EPipe (_, e1, e2, l), EPipe (_, e1', e2', l') ->
        eqList l l' && eq e1 e1' && eq e2 e2'
    | EFeatureFlag (_, _, cond, old, knew), EFeatureFlag (_, _, cond', old', knew') ->
        eq cond cond' && eq old old' && eq knew knew'
    | EConstructor (_, s, ts), EConstructor (_, s', ts') -> s = s' && eqList ts ts'
    | ERightPartial (_, str, e), ERightPartial (_, str', e')
    | ELeftPartial (_, str, e), ELeftPartial (_, str', e')
    | EPartial (_, str, e), EPartial (_, str', e') -> str = str' && eq e e'
    | ELambda (_, vars, e), ELambda (_, vars', e') ->
        eq e e' && List.forall2 (fun (_, v) (_, v') -> v = v') vars vars'
    | EMatch (_, e, branches), EMatch (_, e', branches') ->
        eq e e'
        && List.forall2
             (fun ((p, v) : Pattern * Expr) (p', v') ->
               p.testEqualIgnoringIDs (p') && eq v v')
             branches
             branches'
    | ENull _, _
    | EBlank _, _
    | EPipeTarget _, _
    | EInteger _, _
    | EString _, _
    | ECharacter _, _
    | EVariable _, _
    | EBool _, _
    | EFloat _, _
    | ELet _, _
    | EIf _, _
    | EList _, _
    | EFnCall _, _
    | EBinOp _, _
    | ERecord _, _
    | EFieldAccess _, _
    | EPipe _, _
    | EFeatureFlag _, _
    | EConstructor _, _
    | ELeftPartial _, _
    | ERightPartial _, _
    | EPartial _, _
    | ELambda _, _
    | EMatch _, _ ->
        (* exhaustiveness check *)
        false



  member this.toRuntimeType() : RT.Expr =
    let r(v : Expr) = v.toRuntimeType ()

    match this with
    | EBlank id -> RT.EBlank id
    | ECharacter (id, char) -> RT.ECharacter(id, char)
    | EInteger (id, num) -> RT.EInteger(id, num)
    | EString (id, str) -> RT.EString(id, str)
    | EFloat (id, sign, whole, fraction) ->
        RT.EFloat(id, makeFloat (sign = Positive) whole fraction)
    | EBool (id, b) -> RT.EBool(id, b)
    | ENull id -> RT.ENull id
    | EVariable (id, var) -> RT.EVariable(id, var)
    | EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | EFnCall (id, name, args, ster) ->
        RT.EApply(
          id,
          RT.EFQFnValue(gid (), name),
          List.map r args,
          RT.NotInPipe,
          ster.toRuntimeType ()
        )
    | EBinOp (id, name, arg1, arg2, ster) ->
        r (EFnCall(id, name, [ arg1; arg2 ], ster))
    | ELambda (id, vars, body) -> RT.ELambda(id, vars, r body)
    | ELet (id, lhs, rhs, body) -> RT.ELet(id, lhs, r rhs, r body)
    | EIf (id, cond, thenExpr, elseExpr) ->
        RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | EPartial (id, _, oldExpr)
    | ERightPartial (id, _, oldExpr)
    | ELeftPartial (id, _, oldExpr) -> RT.EPartial(id, r oldExpr)
    | EList (id, exprs) -> RT.EList(id, List.map r exprs)
    | ERecord (id, pairs) -> RT.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
    | EPipe (id, expr1, expr2, rest) ->
        // Convert v |> fn1 a |> fn2 |> fn3 b c
        // into fn3 (fn2 (fn1 v a)) b c
        // This conversion should correspond to ast.ml:inject_param_and_execute
        // from the OCaml interpreter
        let inner = r expr1

        List.fold
          inner
          (fun prev next ->
            match next with
            // TODO: support currying
            | EFnCall (id, name, EPipeTarget ptID :: exprs, rail) ->
                RT.EApply(
                  id,
                  RT.EFQFnValue(ptID, name),
                  prev :: List.map r exprs,
                  RT.InPipe,
                  rail.toRuntimeType ()
                )
            // TODO: support currying
            | EBinOp (id, name, EPipeTarget ptID, expr2, rail) ->
                RT.EApply(
                  id,
                  RT.EFQFnValue(ptID, name),
                  [ prev; r expr2 ],
                  RT.InPipe,
                  rail.toRuntimeType ()
                )
            // If there's a hole, run the computation right through it as if it wasn't there
            | EBlank _ -> prev
            // Here, the expression evaluates to an FnValue. This is for eg variables containing values
            | other ->
                RT.EApply(id, r other, [ prev ], RT.InPipe, NoRail.toRuntimeType ()))
          (expr2 :: rest)

    | EConstructor (id, name, exprs) -> RT.EConstructor(id, name, List.map r exprs)
    | EMatch (id, mexpr, pairs) ->
        RT.EMatch(
          id,
          r mexpr,
          List.map
            ((Tuple2.mapItem1 (fun (p : Pattern) -> p.toRuntimeType ()))
             << (Tuple2.mapItem2 r))
            pairs
        )
    | EPipeTarget id -> failwith "No EPipeTargets should remain"
    | EFeatureFlag (id, name, cond, caseA, caseB) ->
        RT.EFeatureFlag(id, r cond, r caseA, r caseB)


and SendToRail =
  | Rail
  | NoRail

  member this.toRuntimeType() : RT.SendToRail =
    match this with
    | Rail -> RT.Rail
    | NoRail -> RT.NoRail

and Pattern =
  | PVariable of id * string
  | PConstructor of id * string * List<Pattern>
  | PInteger of id * bigint
  | PBool of id * bool
  | PCharacter of id * string
  | PString of id * string
  | PFloat of id * Sign * bigint * bigint
  | PNull of id
  | PBlank of id

  member this.toRuntimeType() : RT.Pattern =
    let r(v : Pattern) = v.toRuntimeType ()

    match this with
    | PVariable (id, str) -> RT.PVariable(id, str)
    | PConstructor (id, name, pats) -> RT.PConstructor(id, name, List.map r pats)
    | PInteger (id, i) -> RT.PInteger(id, i)
    | PBool (id, b) -> RT.PBool(id, b)
    | PCharacter (id, c) -> RT.PCharacter(id, c)
    | PString (id, s) -> RT.PString(id, s)
    | PFloat (id, s, w, f) -> RT.PFloat(id, makeFloat (s = Positive) w f)
    | PNull id -> RT.PNull id
    | PBlank id -> RT.PBlank id

  member this.testEqualIgnoringIDs(other : Pattern) : bool =
    let eq (a : Pattern) (b : Pattern) = a.testEqualIgnoringIDs (b)

    let eqList l1 l2 = List.length l1 = List.length l2 && List.forall2 eq l1 l2

    match (this, other) with
    | PVariable (_, name), PVariable (_, name') -> name = name'
    | (PConstructor (_, name, patterns), PConstructor (_, name', patterns')) ->
        name = name' && eqList patterns patterns'
    | PString (_, str), PString (_, str') -> str = str'
    | PInteger (_, l), PInteger (_, l') -> l = l'
    | PFloat (_, s, w, f), PFloat (_, s', w', f') -> (s, w, f) = (s', w', f')
    | PBool (_, l), PBool (_, l') -> l = l'
    | PCharacter (_, c), PCharacter (_, c') -> c = c'
    | PNull (_), PNull (_) -> true
    | PBlank (_), PBlank (_) -> true
    | PVariable _, _
    | PConstructor _, _
    | PString _, _
    | PInteger _, _
    | PFloat _, _
    | PBool _, _
    | PCharacter _, _
    | PNull _, _
    | PBlank _, _ -> false




module Shortcuts =

  let rec toStringRepr (e : Expr) : string =
    let r (v : Expr) = $"{toStringRepr v}"
    let pr (v : Expr) = $"({toStringRepr v})" // parenthesized repr
    let q (v : string) = $"\"{v}\""

    match e with
    | EBlank id -> "eBlank ()"
    | ECharacter (_, char) -> $"eChar '{char}'"
    | EInteger (_, num) -> $"eInt {num}"
    | EString (_, str) -> $"eStr {q str}"
    | EFloat (_, sign, whole, fraction) ->
        $"eFloat {sign = Positive} {whole} {fraction}"
    | EBool (_, b) -> $"eBool {b}"
    | ENull _ -> $"eNull ()"
    | EVariable (_, var) -> $"eVar {q var}"
    | EFieldAccess (_, obj, fieldname) -> $"eFieldAccess {pr obj} {q fieldname}"
    // | EApply (_, EFQFnValue (_, name), args, NotInPipe, ster) ->
    //     let fn, package =
    //       if name.owner = "dark" && name.package = "stdlib" then
    //         "eStdFn", ""
    //       else
    //         "eFn", " {q name.owner} {q name.package} "
    //
    //     let args = List.map r args |> String.concat "; "
    //     $"{fn} {package} {q name.module_} {q name.function_} {name.version} [{args}]"
    // | EApply (_, expr, [ arg ], InPipe, ster) -> $"ePipe {pr arg} {pr expr} []"
    // | EApply (_, _, args, pipe, ster) -> $"TODO other EAPPlY {e}"
    // | ELambda (_, vars, body) ->
    //     let vars = List.map (fun (_, y) -> q y) vars |> String.concat "; "
    //     $"eLambda {vars} {pr body}"
    // | ELet (_, lhs, rhs, body) -> $"eLet {q lhs} {pr rhs} {pr body}"
    | _ -> $"Bored now: {e}"
  // | EIf (_, cond, thenExpr, elseExpr) -> R.EIf(id, r cond, r thenExpr, r elseExpr)
  // | EPartial (_, _, oldExpr)
  // | ERightPartial (_, _, oldExpr)
  // | ELeftPartial (_, _, oldExpr) -> R.EPartial(id, r oldExpr)
  // | EList (_, exprs) -> R.EList(id, List.map r exprs)
  // | ERecord (_, pairs) -> R.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
  // | EPipe (_, expr1, expr2, rest) ->
  //     // Convert v |> fn1 a |> fn2 |> fn3 b c
  //     // into fn3 (fn2 (fn1 v a)) b c
  //     // This conversion should correspond to ast.ml:inject_param_and_execute
  //     // from the OCaml interpreter
  //     let inner = r expr1
  //     List.fold (fun prev next ->
  //       match next with
  //       // TODO: support currying
  //       | EFnCall (id, name, EPipeTarget ptID :: exprs, rail) ->
  //           R.EApply
  //             (id,
  //              R.EFQFnValue(ptID, name.toRuntimeType ()),
  //              prev :: List.map r exprs,
  //              R.InPipe,
  //              rail.toRuntimeType ())
  //       // TODO: support currying
  //       | EBinOp (id, name, EPipeTarget ptID, expr2, rail) ->
  //           R.EApply
  //             (id,
  //              R.EFQFnValue(ptID, name.toRuntimeType ()),
  //              [ prev; r expr2 ],
  //              R.InPipe,
  //              rail.toRuntimeType ())
  //       // If there's a hole, run the computation right through it as if it wasn't there
  //       | EBlank _ -> prev
  //       // Here, the expression evaluates to an FnValue. This is for eg variables containing values
  //       | other ->
  //           R.EApply(id, r other, [ prev ], R.InPipe, NoRail.toRuntimeType ()))
  //
  //       inner (expr2 :: rest)
  //
  // | EConstructor (_, name, exprs) -> R.EConstructor(id, name, List.map r exprs)
  // | EMatch (_, mexpr, pairs) ->
  //     R.EMatch
  //       (id,
  //        r mexpr,
  //        List.map
  //          ((Tuple2.mapItem1 (fun (p : Pattern) -> p.toRuntimeType ()))
  //           << (Tuple2.mapItem2 r))
  //          pairs)
  // | EPipeTarget _ -> failwith "No EPipeTargets should remain"
  // | EFeatureFlag (_, name, cond, caseA, caseB) ->
  //     R.EFeatureFlag(id, r cond, r caseA, r caseB)
  //
  let eFn'
    (module_ : string)
    (function_ : string)
    (version : int)
    (args : List<Expr>)
    (ster : SendToRail)
    : Expr =
    EFnCall(
      gid (),
      { owner = "dark"
        package = "stdlib"
        module_ = module_
        function_ = function_
        version = version },
      args,
      ster
    )


  let eFn
    (module_ : string)
    (function_ : string)
    (version : int)
    (args : List<Expr>)
    : Expr =
    eFn' module_ function_ version args NoRail

  let eRailFn
    (module_ : string)
    (function_ : string)
    (version : int)
    (args : List<Expr>)
    : Expr =
    eFn' module_ function_ version args Rail

  let eBinOp'
    (module_ : string)
    (function_ : string)
    (version : int)
    (arg1 : Expr)
    (arg2 : Expr)
    (ster : SendToRail)
    : Expr =
    EBinOp(
      gid (),
      { owner = "dark"
        package = "stdlib"
        module_ = module_
        function_ = function_
        version = version },
      arg1,
      arg2,
      ster
    )

  let eBinOp
    (module_ : string)
    (function_ : string)
    (version : int)
    (arg1 : Expr)
    (arg2 : Expr)
    : Expr =
    eBinOp' module_ function_ version arg1 arg2 NoRail

  // An ebinOp that's on the rail
  let eRailBinOp
    (module_ : string)
    (function_ : string)
    (version : int)
    (arg1 : Expr)
    (arg2 : Expr)
    : Expr =
    eBinOp' module_ function_ version arg1 arg2 Rail

  let eStr (str : string) : Expr = EString(gid (), str)
  let eInt (i : int) : Expr = EInteger(gid (), bigint i)

  let eIntStr (i : string) : Expr = EInteger(gid (), parseBigint i)

  let eChar (c : char) : Expr = ECharacter(gid (), string c)
  let eCharStr (c : string) : Expr = ECharacter(gid (), c)
  let eBlank () : Expr = EBlank(gid ())
  let eBool (b : bool) : Expr = EBool(gid (), b)

  let eFloat (sign : Sign) (whole : bigint) (fraction : bigint) : Expr =
    EFloat(gid (), sign, whole, fraction)

  let eFloatStr (sign : Sign) (whole : string) (fraction : string) : Expr =
    EFloat(gid (), sign, parseBigint whole, parseBigint fraction)

  let eNull () : Expr = ENull(gid ())

  let eRecord (rows : (string * Expr) list) : Expr = ERecord(gid (), rows)

  let eList (elems : Expr list) : Expr = EList(gid (), elems)
  let ePipeTarget () = EPipeTarget(gid ())

  let ePartial (str : string) (e : Expr) : Expr = EPartial(gid (), str, e)

  let eRightPartial (str : string) (e : Expr) : Expr = ERightPartial(gid (), str, e)

  let eLeftPartial (str : string) (e : Expr) : Expr = ELeftPartial(gid (), str, e)

  let eVar (name : string) : Expr = EVariable(gid (), name)

  let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
    EFieldAccess(gid (), expr, fieldName)

  let eIf (cond : Expr) (then' : Expr) (else' : Expr) : Expr =
    EIf(gid (), cond, then', else')


  let eLet (varName : string) (rhs : Expr) (body : Expr) : Expr =
    ELet(gid (), varName, rhs, body)


  let eLambda (varNames : string list) (body : Expr) : Expr =
    ELambda(gid (), List.map (fun name -> (gid (), name)) varNames, body)


  let ePipe (first : Expr) (second : Expr) (rest : Expr list) : Expr =
    EPipe(gid (), first, second, rest)

  let eConstructor (name : string) (args : Expr list) : Expr =
    EConstructor(gid (), name, args)


  let eJust (arg : Expr) : Expr = EConstructor(gid (), "Just", [ arg ])

  let eNothing () : Expr = EConstructor(gid (), "Nothing", [])

  let eError (arg : Expr) : Expr = EConstructor(gid (), "Error", [ arg ])

  let eOk (arg : Expr) : Expr = EConstructor(gid (), "Ok", [ arg ])

  let eMatch (cond : Expr) (matches : List<Pattern * Expr>) : Expr =
    EMatch(gid (), cond, matches)


  let pInt (int : int) : Pattern = PInteger(gid (), bigint int)


  let pIntStr (int : string) : Pattern = PInteger(gid (), parseBigint int)

  let pVar (name : string) : Pattern = PVariable(gid (), name)

  let pConstructor (name : string) (patterns : Pattern list) : Pattern =
    PConstructor(gid (), name, patterns)

  let pJust (arg : Pattern) : Pattern = PConstructor(gid (), "Just", [ arg ])

  let pNothing () : Pattern = PConstructor(gid (), "Nothing", [])

  let pError (arg : Pattern) : Pattern = PConstructor(gid (), "Error", [ arg ])

  let pOk (arg : Pattern) : Pattern = PConstructor(gid (), "Ok", [ arg ])

  let pBool (b : bool) : Pattern = PBool(gid (), b)

  let pChar (c : char) : Pattern = PCharacter(gid (), string c)
  let pCharStr (c : string) : Pattern = PCharacter(gid (), c)

  let pString (str : string) : Pattern = PString(gid (), str)

  let pFloatStr (sign : Sign) (whole : string) (fraction : string) : Pattern =
    PFloat(gid (), sign, parseBigint whole, parseBigint fraction)

  let pFloat (sign : Sign) (whole : bigint) (fraction : bigint) : Pattern =
    PFloat(gid (), sign, whole, fraction)

  let pNull () : Pattern = PNull(gid ())

  let pBlank () : Pattern = PBlank(gid ())

  let eflag name cond oldCode newCode =
    EFeatureFlag(gid (), name, cond, oldCode, newCode)


type DType =
  | TAny
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList of DType
  | TDict of DType
  | TIncomplete
  | TError
  | TLambda
  | THttpResponse of DType
  | TDB of DType
  | TDate
  | TChar
  | TPassword
  | TUuid
  | TOption of DType
  | TErrorRail
  | TUserType of string * int
  | TBytes
  | TResult of DType * DType
  // A named variable, eg `a` in `List<a>`
  | TVariable of string
  | TFn of List<DType> * DType
  | TRecord of List<string * DType>
  | TDbList of DType // TODO: cleanup and remove
  // This allows you to build up a record to eventually be the right shape.
  // | TRecordWithFields of List<string * DType>
  // | TRecordPlusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType
  // | TRecordMinusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType

  member this.toRuntimeType() : RT.DType =
    match this with
    | TAny -> RT.TAny
    | TInt -> RT.TInt
    | TFloat -> RT.TFloat
    | TBool -> RT.TBool
    | TNull -> RT.TNull
    | TStr -> RT.TStr
    | TList typ -> RT.TList(typ.toRuntimeType ())
    | TDict typ -> RT.TDict(typ.toRuntimeType ())
    | TIncomplete -> RT.TIncomplete
    | TError -> RT.TError
    | TLambda -> RT.TLambda
    | THttpResponse typ -> RT.THttpResponse(typ.toRuntimeType ())
    | TDB typ -> RT.TDB(typ.toRuntimeType ())
    | TDate -> RT.TDate
    | TChar -> RT.TChar
    | TPassword -> RT.TPassword
    | TUuid -> RT.TUuid
    | TOption typ -> RT.TOption(typ.toRuntimeType ())
    | TErrorRail -> RT.TErrorRail
    | TUserType (name, version) -> RT.TUserType(name, version)
    | TBytes -> RT.TBytes
    | TResult (okType, errType) ->
        RT.TResult(okType.toRuntimeType (), errType.toRuntimeType ())
    | TVariable (name) -> RT.TVariable(name)
    | TFn (paramTypes, returnType) ->
        RT.TFn(
          List.map (fun (pt : DType) -> pt.toRuntimeType ()) paramTypes,
          returnType.toRuntimeType ()
        )
    | TRecord (rows) ->
        RT.TRecord(List.map (fun (f, v : DType) -> f, v.toRuntimeType ()) rows)
    | TDbList typ -> RT.TList(typ.toRuntimeType ())

  static member parse(str : string) : DType =
    match String.toLower str with
    | "any" -> TAny
    | "int" -> TInt
    | "integer" -> TInt
    | "float" -> TFloat
    | "bool" -> TBool
    | "boolean" -> TBool
    | "nothing" -> TNull
    | "character"
    | "char" -> TChar
    | "str" -> TStr
    | "string" -> TStr
    | "list" -> TList TAny
    | "obj" -> TDict TAny
    | "block" -> TFn([ TAny ], TAny)
    | "incomplete" -> TIncomplete
    | "error" -> TError
    | "response" -> THttpResponse TAny
    | "datastore" -> TDB TAny
    | "date" -> TDate
    | "password" -> TPassword
    | "uuid" -> TUuid
    | "option" -> TOption TAny
    | "errorrail" -> TErrorRail
    | "result" -> TResult(TAny, TAny)
    | "dict" -> TDict TAny
    | _ ->
        let parseListTyp(listTyp : string) : DType =
          match String.toLower listTyp with
          | "str" -> TDbList TStr
          | "string" -> TDbList TStr
          | "int" -> TDbList TInt
          | "integer" -> TDbList TInt
          | "float" -> TDbList TFloat
          | "bool" -> TDbList TBool
          | "boolean" -> TDbList TBool
          | "password" -> TDbList TPassword
          | "uuid" -> TDbList TUuid
          | "dict" -> TDbList(TDict TAny)
          | "date" -> TDbList TDate
          | "title" -> TDbList TStr
          | "url" -> TDbList TStr
          | _ -> failwith $"Unhandled parseListTyp: {listTyp}"

        if String.startsWith "[" str && String.endsWith "]" str then
          str |> String.dropLeft 1 |> String.dropRight 1 |> parseListTyp
        else
          failwith $"Unhandled DType.parse: {str}"


module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

    member this.toRuntimeType() : RT.Handler.CronInterval =
      match this with
      | EveryDay -> RT.Handler.EveryDay
      | EveryWeek -> RT.Handler.EveryWeek
      | EveryFortnight -> RT.Handler.EveryFortnight
      | EveryHour -> RT.Handler.EveryHour
      | Every12Hours -> RT.Handler.Every12Hours
      | EveryMinute -> RT.Handler.EveryMinute


  // We need to keep the IDs around until we get rid of them on the client
  type ids = { moduleID : id; nameID : id; modifierID : id }

  type Spec =
    | HTTP of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    // Deprecated but still supported form
    | OldWorker of modulename : string * name : string * ids : ids
    | Cron of name : string * interval : string * ids : ids
    | REPL of name : string * ids : ids

    member this.toRuntimeType() : RT.Handler.Spec =
      match this with
      | HTTP (route, method, _ids) -> RT.Handler.HTTP(route, method)
      | Worker (name, _ids) -> RT.Handler.Worker(name)
      | OldWorker (modulename, name, _ids) -> RT.Handler.OldWorker(modulename, name)
      | Cron (name, interval, _ids) -> RT.Handler.Cron(name, interval)
      | REPL (name, _ids) -> RT.Handler.REPL(name)

    member this.name() =
      match this with
      | HTTP (route, method, _ids) -> route
      | Worker (name, _ids) -> name
      | OldWorker (modulename, name, _ids) -> name
      | Cron (name, interval, _ids) -> name
      | REPL (name, _ids) -> name

    member this.modifier() =
      match this with
      | HTTP (route, method, _ids) -> method
      | Worker (name, _ids) -> "_"
      | OldWorker (modulename, name, _ids) -> "_"
      | Cron (name, interval, _ids) -> interval
      | REPL (name, _ids) -> "_"

    member this.module'() =
      match this with
      | HTTP (route, method, _ids) -> "HTTP"
      | Worker (name, _ids) -> "Worker"
      | OldWorker (modulename, name, _ids) -> modulename
      | Cron (name, interval, _ids) -> "Cron"
      | REPL (name, _ids) -> "REPL"

    member this.complete() : bool =
      match this with
      | HTTP ("", _, _) -> false
      | HTTP (_, "", _) -> false
      | Worker ("", _) -> false
      | OldWorker ("", _, _) -> false
      | OldWorker (_, "", _) -> false
      | Cron ("", _, _) -> false
      | Cron (_, "", _) -> false
      | REPL ("", _) -> false
      | _ -> true

    // Same as a TraceInput.EventDesc
    member this.toDesc() : Option<string * string * string> =
      if this.complete () then
        Some(this.name (), this.name (), this.modifier ())
      else
        None

  type T =
    { tlid : tlid
      pos : pos
      ast : Expr
      spec : Spec }

    member this.toRuntimeType() : RT.Handler.T =
      { tlid = this.tlid
        ast = this.ast.toRuntimeType ()
        spec = this.spec.toRuntimeType () }

module DB =
  type Col = { name : string; typ : Option<DType>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
      pos : pos
      nameID : id
      name : string
      version : int
      cols : List<Col> }

    member this.toRuntimeType() : RT.DB.T =
      { tlid = this.tlid
        name = this.name
        version = this.version
        cols =
          List.filterMap
            (fun c ->
              match c.typ with
              | Some t -> Some(c.name, t.toRuntimeType ())
              | None -> None)
            this.cols }

module UserType =
  type RecordField = { name : string; typ : Option<DType>; nameID : id; typeID : id }

  type Definition =
    | Record of List<RecordField>

    member this.toRuntimeType() : RT.UserType.Definition =
      match this with
      | Record fields ->
          RT.UserType.UTRecord(
            List.filterMap
              (fun (rf : RecordField) ->
                match rf.typ with
                | Some t -> Some({ name = rf.name; typ = t.toRuntimeType () })
                | None -> None)
              fields
          )


  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      definition : Definition }

    member this.toRuntimeType() : RT.UserType.T =
      { tlid = this.tlid
        name = this.name
        version = this.version
        definition = this.definition.toRuntimeType () }

module UserFunction =
  type Parameter =
    { name : string
      nameID : id
      typ : Option<DType>
      typeID : id
      description : string }

    member this.toRuntimeType() : RT.UserFunction.Parameter =
      { name = this.name
        typ = (Option.unwrap TAny this.typ).toRuntimeType()
        description = this.description }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      parameters : List<Parameter>
      returnType : DType
      returnTypeID : id
      description : string
      infix : bool
      body : Expr }

    member this.toRuntimeType() : RT.UserFunction.T =
      { tlid = this.tlid
        name = this.name
        parameters =
          List.map (fun (p : Parameter) -> p.toRuntimeType ()) this.parameters
        returnType = this.returnType.toRuntimeType ()
        description = this.description
        infix = this.infix
        body = this.body.toRuntimeType () }

type Toplevel =
  | TLHandler of Handler.T
  | TLDB of DB.T
  | TLFunction of UserFunction.T
  | TLType of UserType.T

  member this.toTLID() : tlid =
    match this with
    | TLHandler h -> h.tlid
    | TLDB db -> db.tlid
    | TLFunction f -> f.tlid
    | TLType t -> t.tlid

  member this.toRuntimeType() : RT.Toplevel =
    match this with
    | TLHandler h -> RT.TLHandler(h.toRuntimeType ())
    | TLDB db -> RT.TLDB(db.toRuntimeType ())
    | TLFunction f -> RT.TLFunction(f.toRuntimeType ())
    | TLType t -> RT.TLType(t.toRuntimeType ())

module Secret =
  type T =
    { secretName : string
      secretValue : string }

    member this.toRuntimeType() : RT.Secret.T =
      { secretName = this.secretName; secretValue = this.secretValue }


type DeprecatedMigrationKind = | DeprecatedMigrationKind

type Op =
  | SetHandler of tlid * pos * Handler.T
  | CreateDB of tlid * pos * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid
  | MoveTL of tlid * pos
  | SetFunction of UserFunction.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid
  | CreateDBMigration of tlid * id * id * (string * id * string * id) list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * string
  | SetDBColTypeInDBMigration of tlid * id * string
  | AbandonDBMigration of tlid
  | DeleteColInDBMigration of tlid * id
  | DeleteDBCol of tlid * id
  | DeprecatedInitDBm of tlid * id * id * id * DeprecatedMigrationKind
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * pos * id * string
  | DeleteTLForever of tlid
  | DeleteFunctionForever of tlid
  | SetType of UserType.T
  | DeleteType of tlid
  | DeleteTypeForever of tlid

type Oplist = List<Op>
type TLIDOplists = List<tlid * Oplist>

module PackageManager =
  type Parameter = { name : string; typ : DType; description : string }

  type Fn =
    { name : FQFnName.T
      body : Expr
      parameters : List<Parameter>
      returnType : DType
      description : string
      author : string
      deprecated : bool
      tlid : tlid }


let rec preTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = preTraversal f in
  let expr = f expr in

  match expr with
  | EInteger _
  | EBlank _
  | EString _
  | EVariable _
  | EBool _
  | ENull _
  | ECharacter _
  | EPipeTarget _
  | EFloat _ -> expr
  | ELet (id, name, rhs, next) -> ELet(id, name, r rhs, r next)
  | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, r cond, r ifexpr, r elseexpr)
  | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, r expr, fieldname)
  | ELambda (id, names, expr) -> ELambda(id, names, r expr)
  | EList (id, exprs) -> EList(id, List.map r exprs)
  | EBinOp (id, op, arg1, arg2, ster) -> EBinOp(id, op, r arg1, r arg2, ster)
  | EFnCall (id, fn, args, ster) -> EFnCall(id, fn, List.map r args, ster)
  | EPipe (id, arg0, arg1, args) -> EPipe(id, r arg0, r arg1, List.map r args)
  | EMatch (id, mexpr, pairs) ->
      EMatch(id, r mexpr, List.map (fun (name, expr) -> (name, r expr)) pairs)
  | ERecord (id, fields) ->
      ERecord(id, List.map (fun (name, expr) -> (name, r expr)) fields)
  | EConstructor (id, name, exprs) -> EConstructor(id, name, List.map r exprs)
  | EPartial (id, str, oldExpr) -> EPartial(id, str, r oldExpr)
  | ELeftPartial (id, str, oldExpr) -> ELeftPartial(id, str, r oldExpr)
  | ERightPartial (id, str, oldExpr) -> ERightPartial(id, str, r oldExpr)
  | EFeatureFlag (id, msg, cond, casea, caseb) ->
      EFeatureFlag(id, msg, r cond, r casea, r caseb)


let rec postTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = postTraversal f in

  let result =
    match expr with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | ECharacter _
    | EPipeTarget _
    | EBool
    | ENull _
    | EFloat _ -> expr
    | ELet (id, name, rhs, next) -> ELet(id, name, r rhs, r next)
    | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, r cond, r ifexpr, r elseexpr)
    | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, r expr, fieldname)
    | ELambda (id, names, expr) -> ELambda(id, names, r expr)
    | EList (id, exprs) -> EList(id, List.map r exprs)
    | EMatch (id, mexpr, pairs) ->
        EMatch(id, r mexpr, List.map (fun (name, expr) -> (name, r expr)) pairs)
    | EBinOp (id, op, arg1, arg2, ster) -> EBinOp(id, op, r arg1, r arg2, ster)
    | EFnCall (id, fn, args, ster) -> EFnCall(id, fn, List.map r args, ster)
    | EPipe (id, arg0, arg1, args) -> EPipe(id, r arg0, r arg1, List.map r args)
    | ERecord (id, fields) ->
        ERecord(id, List.map (fun (name, expr) -> (name, r expr)) fields)
    | EConstructor (id, name, exprs) -> EConstructor(id, name, List.map r exprs)
    | EPartial (id, str, oldExpr) -> EPartial(id, str, r oldExpr)
    | ELeftPartial (id, str, oldExpr) -> ELeftPartial(id, str, r oldExpr)
    | ERightPartial (id, str, oldExpr) -> ERightPartial(id, str, r oldExpr)
    | EFeatureFlag (id, str, cond, casea, caseb) ->
        EFeatureFlag(id, str, r cond, r casea, r caseb)

  f result
