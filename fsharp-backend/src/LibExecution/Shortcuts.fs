module LibExecution.Shortcuts

open Prelude
open VendoredTablecloth
open RuntimeTypes

// Returns a string representation of an expr using shortcuts. This makes it
// useful for creating test cases and similar.
let rec toStringRepr (e : Expr) : string =
  let r (v : Expr) = $"{toStringRepr v}"
  let pr (v : Expr) = $"({toStringRepr v})" // parenthesized repr
  let q (v : string) = $"\"{v}\""

  match e with
  | EBlank id -> "eBlank ()"
  | ECharacter (_, char) -> $"eChar '{char}'"
  | EInteger (_, num) -> $"eInt {num}"
  | EString (_, str) -> $"eStr {q str}"
  | EFloat (_, number) ->
    let sign = if System.Double.IsNegative number then Negative else Positive
    let stringified = number |> abs |> string

    match String.split "." stringified with
    | [ whole; fraction ] -> $"eFloat {sign} {whole}I {fraction}I"
    | [ whole ] -> $"eFloat {sign} {whole}I 0I"
    | _ ->
      Exception.raiseInternal
        $"can't print float"
        [ "number", number; "stringified", stringified ]
  | EBool (_, b) -> $"eBool {b |> string |> String.toLowercase}"
  | ENull _ -> $"eNull ()"
  | EVariable (_, var) -> $"eVar {q var}"
  | EFieldAccess (_, obj, fieldname) -> $"eFieldAccess {pr obj} {q fieldname}"
  | EApply (_, EFQFnValue (_, FQFnName.Stdlib name), args, NotInPipe, ster) ->
    let fn, suffix =
      match ster with
      | NoRail -> "eFn", ""
      | Rail -> "eFnRail", ""

    let args = List.map r args |> String.concat "; "
    $"{fn} {q name.module_} {q name.function_} {name.version} [{args}] {suffix}"

  | EApply (_, expr, args, pipe, ster) ->
    let fn, suffix =
      match pipe, ster with
      | InPipe _, NoRail -> "ePipeApply", ""
      | NotInPipe, Rail -> "eRailApply", ""
      | InPipe _, Rail -> "ePipeAndRailApply", ""
      | _ -> "eApply'", "{rail} {ster}"

    let args = List.map r args |> String.concat "; "
    $"{fn} {pr expr} [{args}] {suffix}"
  | EFQFnValue (_, FQFnName.Stdlib std) ->
    $"eStdFnVal {q std.module_} {q std.function_} {std.version}"
  | EFQFnValue (_, FQFnName.Package pkg) ->
    $"ePackageFnVal {q pkg.owner} {q pkg.package}"
    + $" {q pkg.module_} {q pkg.function_} {pkg.version}"
  | EFQFnValue (_, FQFnName.User name) -> $"eUserFnVal {q name}"
  | ELambda (_, vars, body) ->
    let vars = List.map (fun (_, y) -> q y) vars |> String.concat "; "
    $"eLambda [{vars}] {pr body}"
  | ELet (_, lhs, rhs, body) -> $"eLet {q lhs} {pr rhs} {pr body}"
  | EList (_, exprs) ->
    let exprs = List.map r exprs |> String.concat "; "
    $"eList [{exprs}]"
  | _ -> $"Bored now: {e}"
// | EIf (_, cond, thenExpr, elseExpr) -> R.EIf(id, r cond, r thenExpr, r elseExpr)
// | EPartial (_, _, oldExpr)
// | ERightPartial (_, _, oldExpr)
// | ELeftPartial (_, _, oldExpr) -> R.EPartial(id, r oldExpr)
// | ERecord (_, pairs) -> R.ERecord(id, List.map (Tuple2.mapSecond r) pairs)
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
//          ((Tuple2.mapFirst (fun (p : Pattern) -> p.toRuntimeType ()))
//           << (Tuple2.mapSecond r))
//          pairs)
// | EPipeTarget _ -> Exception.raiseInternal "No EPipeTargets should remain"
// | EFeatureFlag (_, name, cond, caseA, caseB) ->
//     R.EFeatureFlag(id, r cond, r caseA, r caseB)
//
//

let eFnVal
  (owner : string)
  (package : string)
  (module_ : string)
  (function_ : string)
  (version : int)
  : Expr =
  EFQFnValue(gid (), FQFnName.packageFqName owner package module_ function_ version)

let ePackageFnVal
  (owner : string)
  (package : string)
  (module_ : string)
  (function_ : string)
  (version : int)
  : Expr =
  EFQFnValue(gid (), FQFnName.packageFqName owner package module_ function_ version)

let eStdFnVal (module_ : string) (function_ : string) (version : int) : Expr =
  EFQFnValue(gid (), FQFnName.stdlibFqName module_ function_ version)

let eUserFnVal (function_ : string) : Expr =
  EFQFnValue(gid (), FQFnName.userFqName function_)

let eBinopFnVal (op : string) : Expr = EFQFnValue(gid (), FQFnName.binopFqName op)

let eFn'
  (module_ : string)
  (function_ : string)
  (version : int)
  (args : List<Expr>)
  (ster : SendToRail)
  : Expr =
  EApply(gid (), (eStdFnVal module_ function_ version), args, NotInPipe, ster)

let eFn
  (module_ : string)
  (function_ : string)
  (version : int)
  (args : List<Expr>)
  : Expr =
  eFn' module_ function_ version args NoRail

let eFnRail
  (module_ : string)
  (function_ : string)
  (version : int)
  (args : List<Expr>)
  : Expr =
  eFn' module_ function_ version args Rail

let eApply'
  (fnVal : Expr)
  (args : List<Expr>)
  (isInPipe : IsInPipe)
  (ster : SendToRail)
  : Expr =
  EApply(gid (), fnVal, args, isInPipe, ster)

let eApply (fnVal : Expr) (args : List<Expr>) : Expr =
  eApply' fnVal args NotInPipe NoRail

let ePipeApply (fnVal : Expr) (args : List<Expr>) : Expr =
  eApply' fnVal args (InPipe(gid ())) NoRail

let eRailApply (fnVal : Expr) (args : List<Expr>) : Expr =
  eApply' fnVal args NotInPipe Rail

let ePipeAndRailApply (fnVal : Expr) (args : List<Expr>) : Expr =
  eApply' fnVal args (InPipe(gid ())) Rail

let eStr (str : string) : Expr = EString(gid (), str)

let eInt (i : int) : Expr = EInteger(gid (), int64 i)

let eIntStr (i : string) : Expr = EInteger(gid (), parseInt64 i)

let eChar (c : char) : Expr = ECharacter(gid (), string c)
let eCharStr (c : string) : Expr = ECharacter(gid (), c)
let eBlank () : Expr = EBlank(gid ())

let eBool (b : bool) : Expr = EBool(gid (), b)

let eFloat (sign : Sign) (whole : string) (fraction : string) : Expr =
  EFloat(gid (), makeFloat sign whole fraction)

let eNull () : Expr = ENull(gid ())

let eRecord (rows : (string * Expr) list) : Expr = ERecord(gid (), rows)

let eList (elems : Expr list) : Expr = EList(gid (), elems)


let ePartial (e : Expr) : Expr = EPartial(gid (), e)

let eVar (name : string) : Expr = EVariable(gid (), name)

let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
  EFieldAccess(gid (), expr, fieldName)

let eIf (cond : Expr) (then' : Expr) (else' : Expr) : Expr =
  EIf(gid (), cond, then', else')

let eLet (varName : string) (rhs : Expr) (body : Expr) : Expr =
  ELet(gid (), varName, rhs, body)


let eLambda (varNames : string list) (body : Expr) : Expr =
  ELambda(gid (), List.map (fun name -> (gid (), name)) varNames, body)

let eConstructor (name : string) (args : Expr list) : Expr =
  EConstructor(gid (), name, args)

let eJust (arg : Expr) : Expr = EConstructor(gid (), "Just", [ arg ])

let eNothing () : Expr = EConstructor(gid (), "Nothing", [])

let eError (arg : Expr) : Expr = EConstructor(gid (), "Error", [ arg ])

let eOk (arg : Expr) : Expr = EConstructor(gid (), "Ok", [ arg ])

let eMatch (cond : Expr) (matches : List<Pattern * Expr>) : Expr =
  EMatch(gid (), cond, matches)

let pInt (int : int) : Pattern = PInteger(gid (), int64 int)

let pIntStr (int : string) : Pattern = PInteger(gid (), parseInt64 int)

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

let pFloat (whole : string) (fraction : string) : Pattern =
  PFloat(gid (), float $"{whole}{fraction}")

let pNull () : Pattern = PNull(gid ())

let pBlank () : Pattern = PBlank(gid ())

let eflag cond oldCode newCode = EFeatureFlag(gid (), cond, oldCode, newCode)



let httpRouteHandler (route : string) (method : string) (ast : Expr) : Handler.T =
  { tlid = gid (); ast = ast; spec = Handler.HTTP(route, method) }


let dailyCron (name : string) (ast : Expr) : Handler.T =
  { tlid = gid (); ast = ast; spec = Handler.Cron(name, Some Handler.EveryDay) }

let worker (name : string) (ast : Expr) : Handler.T =
  { tlid = gid (); ast = ast; spec = Handler.Worker(name) }


let userFn
  (name : string)
  (description : string)
  (returnType : DType)
  (parameters : UserFunction.Parameter list)
  (body : Expr)
  : UserFunction.T =
  { tlid = gid ()
    body = body
    name = name
    parameters = parameters
    returnType = returnType
    description = description
    infix = false }
