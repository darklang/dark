module LibBackend.ProgramSerialization.ProgramTypes

// These are the types that are serialized for the program.

// The purpose of having this format is that it's easier to work with than the
// serialized formats (which have to handle legacy). So we transform any
// serialized formats into this one for running. We remove any "syntactic
// sugar" (editor/display only features).

// This format should never be serialized/deserialized, that defeats the purpose (convert it into a
// different format if you must, or track the other code along-side this and
// use the ID to find it).

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp
open Npgsql
open FSharp.Data
open System.Text.RegularExpressions

open Prelude
open LibExecution.SharedTypes

// Used for conversion functions
module R = LibExecution.RuntimeTypes

// Expressions - the main part of the language.

module FQFnName =
  type T =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

    member this.ToString : string =
      let module_ = if this.module_ = "" then "" else $"{this.module_}::"
      let fn = $"{this.module_}{this.function_}_v{this.version}"

      if this.owner = "dark" && module_ = "stdlib" then
        fn
      else
        $"{this.owner}/{this.package}::{fn}"

    member this.toRuntimeType() : R.FQFnName.T =
      { owner = this.owner
        package = this.package
        module_ = this.module_
        function_ = this.function_
        version = this.version }


  let name (owner : string)
           (package : string)
           (module_ : string)
           (function_ : string)
           (version : int)
           : T =
    { owner = owner
      package = package
      module_ = module_
      function_ = function_
      version = version }


  let stdlibName (module_ : string) (function_ : string) (version : int) : T =
    name "dark" "stdlib" module_ function_ version


type R = LibExecution.RuntimeTypes.Expr

type Expr =
  | EInteger of id * string
  | EBool of id * bool
  | EString of id * string
  | ECharacter of id * string
  | EFloat of id * string * string // first string might have a sign in it
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
    | EInteger (_, v), EInteger (_, v')
    | EString (_, v), EString (_, v')
    | ECharacter (_, v), ECharacter (_, v')
    | EVariable (_, v), EVariable (_, v') -> v = v'
    | EBool (_, v), EBool (_, v') -> v = v'
    | EFloat (_, whole, frac), EFloat (_, whole', frac') ->
        whole = whole' && frac = frac'
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
        List.forall2 (fun (k, v) (k', v') -> k = k' && eq v v') (sort pairs)
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
        && List.forall2 (fun ((p, v) : Pattern * Expr) (p', v') ->
             p.testEqualIgnoringIDs (p') && eq v v') branches branches'
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



  member this.toRuntimeType() : LibExecution.RuntimeTypes.Expr =
    let r (v : Expr) = v.toRuntimeType ()
    match this with
    | EBlank id -> R.EBlank id
    | ECharacter (id, char) -> R.ECharacter(id, char)
    | EInteger (id, num) -> R.EInteger(id, num)
    | EString (id, str) -> R.EString(id, str)
    | EFloat (id, whole, fraction) -> R.EFloat(id, whole, fraction)
    | EBool (id, b) -> R.EBool(id, b)
    | ENull id -> R.ENull id
    | EVariable (id, var) -> R.EVariable(id, var)
    | EFieldAccess (id, obj, fieldname) -> R.EFieldAccess(id, r obj, fieldname)
    | EFnCall (id, name, args, ster) ->
        R.EFnCall
          (id,
           R.EFQFnValue(gid (), name.toRuntimeType ()),
           List.map r args,
           ster.toRuntimeType ())
    | EBinOp (id, name, arg1, arg2, ster) ->
        r (EFnCall(id, name, [ arg1; arg2 ], ster))
    | ELambda (id, vars, body) -> R.ELambda(id, vars, r body)
    | ELet (id, lhs, rhs, body) -> R.ELet(id, lhs, r rhs, r body)
    | EIf (id, cond, thenExpr, elseExpr) -> R.EIf(id, r cond, r thenExpr, r elseExpr)
    | EPartial (id, _, oldExpr)
    | ERightPartial (id, _, oldExpr)
    | ELeftPartial (id, _, oldExpr) -> R.EPartial(id, r oldExpr)
    | EList (id, exprs) -> R.EList(id, List.map r exprs)
    | ERecord (id, pairs) -> R.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
    | EPipe (id, expr1, expr2, rest) ->
        // Convert v |> fn1 a |> fn2 |> fn3 b c
        // into fn3 (fn2 (fn1 v a)) b c
        let inner = r expr1
        List.fold (fun prev next ->
          match next with
          | EFnCall (id, fnVal, EPipeTarget ptID :: exprs, rail) ->
              R.EFnCall
                (id,
                 R.EFQFnValue(ptID, fnVal.toRuntimeType ()),
                 prev :: List.map r exprs,
                 rail.toRuntimeType ())
          // Here, the expression evaluates to an FnValue with no args. This is for eg variables containing values
          | other -> R.EFnCall(id, r other, [ prev ], NoRail.toRuntimeType ()))

          inner (expr2 :: rest)

    | EConstructor (id, name, exprs) -> R.EConstructor(id, name, List.map r exprs)
    | EMatch (id, mexpr, pairs) ->
        R.EMatch
          (id,
           r mexpr,
           List.map
             ((Tuple2.mapItem1 (fun (p : Pattern) -> p.toRuntimeType ()))
              << (Tuple2.mapItem2 r))
             pairs)
    | EPipeTarget id -> failwith "No EPipeTargets should remain"
    | EFeatureFlag (id, name, cond, caseA, caseB) ->
        R.EFeatureFlag(id, r cond, r caseA, r caseB)


and SendToRail =
  | Rail
  | NoRail

  member this.toRuntimeType() : LibExecution.RuntimeTypes.SendToRail =
    match this with
    | Rail -> R.Rail
    | NoRail -> R.NoRail

and Pattern =
  | PVariable of id * string
  | PConstructor of id * string * List<Pattern>
  | PInteger of id * string
  | PBool of id * bool
  | PCharacter of id * string
  | PString of id * string
  | PFloat of id * string * string
  | PNull of id
  | PBlank of id

  member this.toRuntimeType() : LibExecution.RuntimeTypes.Pattern =
    let r (v : Pattern) = v.toRuntimeType ()
    match this with
    | PVariable (id, str) -> R.PVariable(id, str)
    | PConstructor (id, name, pats) -> R.PConstructor(id, name, List.map r pats)
    | PInteger (id, i) -> R.PInteger(id, i)
    | PBool (id, b) -> R.PBool(id, b)
    | PCharacter (id, c) -> R.PCharacter(id, c)
    | PString (id, s) -> R.PString(id, s)
    | PFloat (id, w, f) -> R.PFloat(id, w, f)
    | PNull id -> R.PNull id
    | PBlank id -> R.PBlank id

  member this.testEqualIgnoringIDs(other : Pattern) : bool =
    let eq (a : Pattern) (b : Pattern) = a.testEqualIgnoringIDs (other)

    let eqList l1 l2 = List.length l1 = List.length l2 && List.forall2 eq l1 l2

    match (this, other) with
    | PVariable (_, name), PVariable (_, name') -> name = name'
    | (PConstructor (_, name, patterns), PConstructor (_, name', patterns')) ->
        name = name' && eqList patterns patterns'
    | PString (_, str), PString (_, str') -> str = str'
    | PInteger (_, l), PInteger (_, l') -> l = l'
    | PFloat (_, w, f), PFloat (_, w', f') -> (w, f) = (w', f')
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
  let eFn' (module_ : string)
           (function_ : string)
           (version : int)
           (args : List<Expr>)
           (ster : SendToRail)
           : Expr =
    EFnCall
      (gid (),
       { owner = "dark"
         package = "stdlib"
         module_ = module_
         function_ = function_
         version = version },
       args,
       ster)


  let eFn (module_ : string)
          (function_ : string)
          (version : int)
          (args : List<Expr>)
          : Expr =
    eFn' module_ function_ version args NoRail

  let eRailFn (module_ : string)
              (function_ : string)
              (version : int)
              (args : List<Expr>)
              : Expr =
    eFn' module_ function_ version args Rail

  let eBinOp' (module_ : string)
              (function_ : string)
              (version : int)
              (arg1 : Expr)
              (arg2 : Expr)
              (ster : SendToRail)
              : Expr =
    EBinOp
      (gid (),
       { owner = "dark"
         package = "stdlib"
         module_ = module_
         function_ = function_
         version = version },
       arg1,
       arg2,
       ster)

  let eBinOp (module_ : string)
             (function_ : string)
             (version : int)
             (arg1 : Expr)
             (arg2 : Expr)
             : Expr =
    eBinOp' module_ function_ version arg1 arg2 NoRail

  // An ebinOp that's on the rail
  let eRailBinOp (module_ : string)
                 (function_ : string)
                 (version : int)
                 (arg1 : Expr)
                 (arg2 : Expr)
                 : Expr =
    eBinOp' module_ function_ version arg1 arg2 Rail

  let eStr (str : string) : Expr = EString(gid (), str)
  let eInt (i : int) : Expr = EInteger(gid (), i.ToString())

  let eIntStr (i : string) : Expr =
    assert ((new Regex(@"-?\d+")).IsMatch(i))
    EInteger(gid (), i)

  let eChar (c : char) : Expr = ECharacter(gid (), string c)
  let eCharStr (c : string) : Expr = ECharacter(gid (), c)
  let eBlank () : Expr = EBlank(gid ())
  let eBool (b : bool) : Expr = EBool(gid (), b)

  let eFloat (whole : int) (fraction : int) : Expr =
    EFloat(gid (), whole.ToString(), fraction.ToString())

  let eFloatStr (whole : string) (fraction : string) : Expr =
    // FSTODO: don't actually assert, report to rollbar
    assert ((new Regex(@"-?\d+")).IsMatch(whole))
    assert ((new Regex(@"\d+")).IsMatch(fraction))
    EFloat(gid (), whole, fraction)

  let eNull () : Expr = ENull(gid ())

  let eRecord (rows : (string * Expr) list) : Expr = ERecord(gid (), rows)

  let eList (elems : Expr list) : Expr = EList(gid (), elems)
  let ePipeTarget () = EPipeTarget(gid ())


  let ePartial (str : string) (e : Expr) : Expr = EPartial(gid (), str, e)

  let eRightPartial (str : string) (e : Expr) : Expr = ERightPartial(gid (), str, e)


  let eLeftPartial (str : string) (e : Expr) : Expr = ELeftPartial(gid (), str, e)


  let eVar (name : string) : Expr = EVariable(gid (), name)

  (* let fieldAccess (expr : Expr) (fieldName : string) : Expr = *)
  (*   EFieldAccess (gid () ,expr, fieldName) *)

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


  let pInt (int : int) : Pattern = PInteger(gid (), int.ToString())


  let pIntStr (int : string) : Pattern = PInteger(gid (), int)

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

  let pFloatStr (whole : string) (fraction : string) : Pattern =
    PFloat(gid (), whole, fraction)

  let pFloat (whole : int) (fraction : int) : Pattern =
    PFloat(gid (), whole.ToString(), fraction.ToString())

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
  | THTTPResponse
  | TDB
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
  | TRecord of List<string * DType> // has exactly these fields
  // This allows you to build up a record to eventually be the right shape.
  | TRecordWithFields of List<string * DType>
  | TRecordPlusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType
  | TRecordMinusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType



module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  // We need to keep the IDs around until we get rid of them on the client
  type ids = { moduleID : id; nameID : id; modifierID : id }

  type Spec =
    | HTTP of path : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    // Deprecated but still supported form
    | OldWorker of modulename : string * name : string * ids : ids
    | Cron of name : string * interval : string * ids : ids
    | REPL of name : string * ids : ids


  type T = { tlid : tlid; ast : Expr; spec : Spec }

module DB =
  type Col = string * DType
  type T = { tlid : tlid; name : string; cols : List<Col> }

module UserType =
  type RecordField = { name : string; typ : DType }
  type Definition = UTRecord of List<RecordField>

  type T = { tlid : tlid; name : string; version : int; definition : Definition }

module UserFunction =
  type Parameter = { name : string; typ : DType; description : string }

  type T =
    { tlid : tlid
      name : string
      parameters : List<Parameter>
      returnType : DType
      description : string
      infix : bool
      ast : Expr }

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
