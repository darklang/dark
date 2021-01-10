module LibExecution.RuntimeTypes

// The core types and functions used by the Dark language's runtime. These are
// not idential to the serialized types or the types used in the Editor, as
// those have unique constraints (typically, backward compatibility or
// continuous delivery).
//
// The design of these types is intended to accomodate the unique design of
// Dark, that it's being run sometimes in an editor and sometimes in
// production, etc.
//
// This typically represents our most accurate representation of the language
// as it is today, however, slight variations of these types are expected to
// exist in other places representing different constraints, such as how we've
// put something in some kind of storage, sending it to some API, etc. Those
// types will always be converted to these types for execution.

// The reason these are distinct formats from the serialized types is that
// those types are very difficult to change, while we want this to be
// straightforward to change.  So we transform any serialized formats into this
// one for running. We remove any "syntactic sugar" (editor/display only
// features).

// These formats should never be serialized/deserialized, that defeats the
// purpose. If you need to save data of this format, create a set of new types
// and convert this type into them. (even if they are identical).

// This format is lossy, relative to the serialized types. Use IDs to refer
// back.


open Prelude

module J = Prelude.Json


// A function description: a fully-qualified function name, including package,
// module, and version information.
module FQFnName =
  type T =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

    override this.ToString() : string =
      let module_ = if this.module_ = "" then "" else $"{this.module_}::"
      let fn = $"{module_}{this.function_}_v{this.version}"

      if this.owner = "dark" && this.package = "stdlib" then
        fn
      else
        $"{this.owner}/{this.package}::{fn}"

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

// This Expr is the AST, expressing what the user sees in their editor.
type Expr =
  | EInteger of id * bigint
  | EBool of id * bool
  | EString of id * string
  | ECharacter of id * string
  | EFloat of id * double // first string might have a sign in it
  | ENull of id
  | EBlank of id
  | ELet of id * string * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  // This is a function call, the first expression is the value of the function.
  | EApply of id * Expr * List<Expr> * IsInPipe * SendToRail
  | EPartial of id * Expr
  // Since functions aren't real values in the symbol table, we look them up directly
  | EFQFnValue of id * FQFnName.T
  | EList of id * List<Expr>
  | ERecord of id * List<string * Expr>
  | EConstructor of id * string * List<Expr>
  | EMatch of id * Expr * List<Pattern * Expr>
  | EFeatureFlag of id * Expr * Expr * Expr

  member this.isBlank : bool =
    match this with
    | EBlank _ -> true
    | _ -> false

and SendToRail =
  | Rail
  | NoRail

// EApply has slightly different semantics when it is in a pipe. When piping
// into Incomplete values, we ignore the Incomplete and return the piped-in
// argument (which is the first parameter). This is to allow editing live code
// by creating a new pipe entry and then filling it in.
and IsInPipe =
  | InPipe
  | NotInPipe

and Pattern =
  | PVariable of id * string
  | PConstructor of id * string * List<Pattern>
  | PInteger of id * bigint
  | PBool of id * bool
  | PCharacter of id * string
  | PString of id * string
  | PFloat of id * double
  | PNull of id
  | PBlank of id

// Runtime values
type DvalMap = Map<string, Dval>

and LambdaImpl = { parameters : List<id * string>; symtable : Symtable; body : Expr }

and FnValImpl =
  | Lambda of LambdaImpl
  | FQFnName of FQFnName.T

and Dval =
  | DInt of bigint
  | DFloat of double
  | DBool of bool
  | DNull
  | DStr of string
  | DChar of string // TextElements (extended grapheme clusters) are provided as strings
  (* compound types *)
  | DList of List<Dval>
  | DObj of DvalMap
  (* special types - see notes above *)
  | DFnVal of FnValImpl
  | DFakeVal of FakeDval
  (* user types: awaiting a better type system *)
  | DHttpResponse of
    statusCode : int *
    headers : (string * string) list *
    body : Dval
  | DDB of string
  // FSTODO
  (* | DDate of time *)
  // FSTODO
  (* | DPassword of PasswordBytes.t *)
  | DUuid of System.Guid
  | DOption of Option<Dval>
  | DResult of Result<Dval, Dval>
  | DBytes of byte array

  member this.isFake : bool =
    match this with
    | DFakeVal _ -> true
    | _ -> false

  member this.isIncomplete : bool =
    match this with
    | DFakeVal (DIncomplete _) -> true
    | _ -> false

  member this.isErrorRail : bool =
    match this with
    | DFakeVal (DErrorRail _) -> true
    | _ -> false

  member this.isDError : bool =
    match this with
    | DFakeVal (DError _) -> true
    | _ -> false

  member this.unwrapFromErrorRail : Dval =
    match this with
    | DFakeVal (DErrorRail dv) -> dv
    | other -> other

  static member int(i : int) = DInt(bigint i)
  static member int(i : bigint) = DInt i
  static member int(i : string) = DInt(parseBigint i)

  static member float(value : double) : Dval = DFloat value

  static member float(sign : Sign, whole : bigint, fraction : bigint) : Dval =
    // FSTODO - add sourceID to errors
    try
      DFloat(makeFloat (sign = Positive) whole fraction)
    with _ ->
      DFakeVal(
        DError(InvalidFloatExpression(sign, whole.ToString(), fraction.ToString()))
      )

  static member float(sign : Sign, whole : string, fraction : string) : Dval =
    // FSTODO - add sourceID to errors
    try
      DFloat(parseFloat whole fraction)
    with _ -> DFakeVal(DError((InvalidFloatExpression(sign, whole, fraction))))




  // Dvals should never be constructed that contain fakevals - the fakeval
  // should always propagate (though, there are specific cases in the
  // interpreter where they are discarded instead of propagated; still they are
  // never put into other dvals). These static members check before creating the values

  static member list(list : List<Dval>) : Dval =
    List.tryFind (fun (dv : Dval) -> dv.isFake) list
    |> Option.defaultValue (DList list)

  static member obj(fields : List<string * Dval>) : Dval =
    List.tryFind (fun (k, dv : Dval) -> dv.isFake) fields
    |> Option.map snd
    |> Option.defaultValue (DObj(Map fields))

  static member resultOk(dv : Dval) : Dval = if dv.isFake then dv else DResult(Ok dv)

  static member resultError(dv : Dval) : Dval =
    if dv.isFake then dv else DResult(Error dv)

  static member optionJust(dv : Dval) : Dval =
    if dv.isFake then dv else DOption(Some dv)


and DvalTask = Prelude.TaskOrValue<Dval>

and Symtable = Map<string, Dval>

and Param =
  { name : string
    typ : DType
    doc : string }

  static member make (name : string) (typ : DType) (doc : string) =
    { name = name; typ = typ; doc = doc }

// A Fake Dval is some control-flow that's modelled in the interpreter as a
// Dval. This is sort of like an Exception. Anytime we see a FakeDval we return
// it instead of operating on it, including when they're put in a list, in a
// value, in a record, as a parameter to a function, etc.
and FakeDval =
  // a DError represents something that shouldn't have happened in the engine,
  // that should have been reported elsewhere. It's usually a type error of
  // some kind, but occasionally we'll paint ourselves into a corner and need
  // to represent a runtime error using this.
  | DError of RuntimeError
  // A DIncomplete represents incomplete computation, whose source is
  // always a Blank. When the code runs into a blank, it must return
  // incomplete because the code is not finished. An incomplete value
  // results in a 500 because it is a developer error.
  //
  // Propagating DIncompletes is straightforward: any computation
  // relying on an incomplete must itself be incomplete.
  //
  // Some examples:
  // - calling a function with an incomplete as a parameter is an
  //   incomplete function call.
  // - an if statement with an incomplete in the cond must be incomplete.
  //
  // But computation that doesn't rely on the incomplete value can
  // ignore it:
  //
  // - an if statement which with a blank in the ifbody and a
  //   complete expression in the elsebody will execute just fine if
  //   cond is false. It has not hit any part of the program that is
  //   being worked on.
  //
  // - a list with blanks in it can just ignore the blanks.
  // - an incomplete in a list should be filtered out, because the
  //   program has not been completed, and so that list entry just
  //   doesn't "exist" yet.
  // - incompletes in keys or values of objects cause the entire row
  //   to be ignored.
  | DIncomplete of DvalSource
  // DErrorRail represents a value which has been sent over to the
  // errorrail. Because the computation is happening on the errorrail,
  // no other computation occurs.
  //
  // In all cases, we can consider it equivalent to goto
  // end_of_function.
  //
  // - an if with an derrorrail in an subexpression is a derrorrail
  // -  a list containing a derrorrail is a derrorail
  | DErrorRail of Dval

and DType =
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
  | TRecord of List<string * DType>

// Runtime errors can be things that happen relatively commonly (such as calling
// a function with an incorrect type), or things that aren't supposed to happen
// but technically can (such as accessing a variable which doesn't exist) *)
and RuntimeError =
  | NotAFunction of FQFnName.T
  | FunctionRemoved of FQFnName.T
  | CondWithNonBool of Dval
  | FnCalledWithWrongTypes of FQFnName.T * List<Dval> * List<Param>
  | FnCalledWhenNotSync of FQFnName.T * List<Dval> * List<Param>
  | LambdaCalledWithWrongCount of List<Dval> * List<string>
  | LambdaCalledWithWrongType of List<Dval> * List<string>
  | LambdaResultHasWrongType of Dval * DType
  | InvalidFloatExpression of Sign * string * string
  | UndefinedVariable of string
  | UndefinedConstructor of string
  // We want to remove this and make it just a string. So let's start here. And
  // include a DvalSource while we're at it
  | JustAString of DvalSource * string

// Within a function call, we don't have the data available to make good
// RuntimeErrors, so instead return a signal and let the calling code fill in the
// blanks.
//
// Note: Functions shouldn't have runtime errors - those kinds of functions
// should use Results instead. But, we can't really change what a function
// does, so sometimes we discover we made a mistake and need to paper around
// it. In those cases, we return DErrors.
and FnCallError =
  | FnFunctionRemoved
  | FnWrongTypes

and HttpCallError = RequestRouteMismatch of route : string * requestPath : string

// Record the source of an incomplete or error. Would be useful to add more
// information later, such as the iteration count that let to this, or
// something like a stack trace
and DvalSource =
  | SourceNone
  | SourceID of tlid * id


// ------------
// Exceptions
// ------------

// This creates an error which can be wrapped in a DError. All errors that
// occur at runtime should be represented here
exception RuntimeException of RuntimeError

// Error made when calling a function. This allows us to call them when we don't have the information to
// make a RuntimeException, and they are converted to runtimeExceptions at the call site.
exception FnCallException of FnCallError

// When we encounter a fakeDval, this exception allows us to jump out of the
// computation immediately, and the caller can return the dval. This is useful
// for jumping out of folds and other complicated constructs.
exception FakeDvalException of Dval




let err (e : RuntimeError) : Dval = (DFakeVal(DError(e)))
let errStr (s : string) : Dval = (DFakeVal(DError(JustAString(SourceNone, s))))

let errSStr (source : DvalSource) (s : string) : Dval =
  (DFakeVal(DError(JustAString(source, s))))

module Symtable =
  type T = Symtable
  let empty : T = Map []

  let get (name : string) (st : T) : Dval =
    st.TryFind(name) |> Option.defaultValue (err (UndefinedVariable name))

  let add (name : string) (dv : Dval) (st : T) = st.Add(name, dv)


// The runtime needs to know whether to save a function's results when it
// runs. Pure functions that can be run on the client do not need to have
// their results saved.
// In addition, some functions can be run without side-effects; to give
// the user a good experience, we can run them as soon as they are added.
// this includes Date::now and Int::random, as well as
type Previewable =
  // Do not need to be saved, can be recalculated in JS
  | Pure
  // Save their results. We can preview these safely
  | ImpurePreviewable
  // Save their results, cannot be safely previewed
  | Impure

type Deprecation =
  | NotDeprecated
  // This has been deprecated and has a replacement we can suggest
  | ReplacedBy of FQFnName.T
  // This has been deprecated and not replaced, provide a message for the user
  | DeprecatedBecause of string

type SqlSpec =
  // This can be implemented by we haven't yet
  | NotYetImplementedTODO
  // This is not a function which can be queried
  | NotQueryable
  // This can be implemented by a builtin postgres 9.6 function.
  | SqlFunction of string
  // This is a query function (it can't be called inside a query, but it's argument can be a query)
  | QueryFunction


type BuiltInFn =
  { name : FQFnName.T
    parameters : List<Param>
    returnType : DType
    description : string
    previewable : Previewable
    deprecated : Deprecation
    sqlSpec : SqlSpec
    // Functions can be run in JS if they have an implementation in this
    // LibExecution. Functions who's implementation is in LibBackend can only be
    // implemented on the server.
    // May throw a
    fn : BuiltInFnSig }

and BuiltInFnSig = (ExecutionState * List<Dval>) -> DvalTask

and ExecutionState = { functions : Map<FQFnName.T, BuiltInFn>; tlid : tlid }
//    tlid : tlid
// ; canvas_id : Uuidm.t
// ; account_id : Uuidm.t
// ; user_fns : user_fn list
// ; user_tipes : user_tipe list
// ; package_fns : fn list
// ; dbs : DbT.db list
// ; secrets : secret list
// ; trace : on_execution_path:bool -> id -> dval -> unit
// ; trace_tlid : tlid -> unit
// ; callstack :
//     (* Used for recursion detection in the editor. In the editor, we call all
//      * paths to show live values, but with recursion that causes infinite
//      * recursion. *)
//     Tc.StrSet.t
// ; context : context
// ; execution_id : id
// ; on_execution_path :
//     (* Whether the currently executing code is really being executed (as
//      * opposed to being executed for traces) *)
//     bool

// Some parts of the execution need to call AST.exec, but cannot call
// AST.exec without a cyclic dependency. This function enables that, and it
// is safe to do so because all of the state is in the exec_state
// structure.
// exec: ExecutionState -> Symtable -> Expr -> DvalTask
// ; load_fn_result : load_fn_result_type
// ; store_fn_result : store_fn_result_type
// ; load_fn_arguments : load_fn_arguments_type
// ; store_fn_arguments : store_fn_arguments_type
// ; executing_fnname : string
// ; fail_fn : fail_fn_type


let incorrectArgs () = raise (FnCallException FnWrongTypes)

let removedFunction : BuiltInFnSig =
  fun _ -> raise (FnCallException FnFunctionRemoved)

module Shortcuts =

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
    | EFloat (_, number) -> $"eFloat {number}"
    | EBool (_, b) -> $"eBool {b}"
    | ENull _ -> $"eNull ()"
    | EVariable (_, var) -> $"eVar {q var}"
    | EFieldAccess (_, obj, fieldname) -> $"eFieldAccess {pr obj} {q fieldname}"
    | EApply (_, EFQFnValue (_, name), args, NotInPipe, ster) when name.owner = "dark"
                                                                   && name.package = "stdlib" ->
        let fn, suffix =
          match ster with
          | NoRail -> "eFn", ""
          | Rail -> "eFnRail", ""

        let args = List.map r args |> String.concat "; "
        $"{fn} {q name.module_} {q name.function_} {name.version} [{args}] {suffix}"

    | EApply (_, expr, args, pipe, ster) ->
        let fn, suffix =
          match pipe, ster with
          | InPipe, NoRail -> "ePipeApply", ""
          | NotInPipe, Rail -> "eRailApply", ""
          | InPipe, Rail -> "ePipeAndRailApply", ""
          | _ -> "eApply'", "{rail} {ster}"

        let args = List.map r args |> String.concat "; "
        $"{fn} {pr expr} [{args}] {suffix}"
    | EFQFnValue (_, name) ->
        let fn, package =
          if name.owner = "dark" && name.package = "stdlib" then
            "eStdFnVal", ""
          else
            "eFnVal", " {q name.owner} {q name.package} "

        $"{fn} {package} {q name.module_} {q name.function_} {name.version}"
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
  //

  let eFnVal (owner : string)
             (package : string)
             (module_ : string)
             (function_ : string)
             (version : int)
             : Expr =
    EFQFnValue(gid (), FQFnName.name owner package module_ function_ version)

  let eStdFnVal (module_ : string) (function_ : string) (version : int) : Expr =
    eFnVal "dark" "stdlib" module_ function_ version

  let eFn' (module_ : string)
           (function_ : string)
           (version : int)
           (args : List<Expr>)
           (ster : SendToRail)
           : Expr =
    EApply(gid (), (eStdFnVal module_ function_ version), args, NotInPipe, ster)

  let eFn (module_ : string)
          (function_ : string)
          (version : int)
          (args : List<Expr>)
          : Expr =
    eFn' module_ function_ version args NoRail

  let eFnRail (module_ : string)
              (function_ : string)
              (version : int)
              (args : List<Expr>)
              : Expr =
    eFn' module_ function_ version args Rail

  let eApply' (fnVal : Expr)
              (args : List<Expr>)
              (isInPipe : IsInPipe)
              (ster : SendToRail)
              : Expr =
    EApply(gid (), fnVal, args, isInPipe, ster)

  let eApply (fnVal : Expr) (args : List<Expr>) : Expr =
    eApply' fnVal args NotInPipe NoRail

  let ePipeApply (fnVal : Expr) (args : List<Expr>) : Expr =
    eApply' fnVal args InPipe NoRail

  let eRailApply (fnVal : Expr) (args : List<Expr>) : Expr =
    eApply' fnVal args NotInPipe Rail

  let ePipeAndRailApply (fnVal : Expr) (args : List<Expr>) : Expr =
    eApply' fnVal args InPipe Rail

  let eStr (str : string) : Expr = EString(gid (), str)

  let eInt (i : int) : Expr = EInteger(gid (), bigint i)

  let eIntStr (i : string) : Expr = EInteger(gid (), parseBigint i)

  let eChar (c : char) : Expr = ECharacter(gid (), string c)
  let eCharStr (c : string) : Expr = ECharacter(gid (), c)
  let eBlank () : Expr = EBlank(gid ())

  let eBool (b : bool) : Expr = EBool(gid (), b)

  let eFloat (sign : Sign) (whole : bigint) (fraction : bigint) : Expr =
    EFloat(gid (), makeFloat (sign = Positive) whole fraction)

  let eFloatStr (whole : string) (fraction : string) : Expr =
    EFloat(gid (), parseFloat whole fraction)

  let eNull () : Expr = ENull(gid ())

  let eRecord (rows : (string * Expr) list) : Expr = ERecord(gid (), rows)

  let eList (elems : Expr list) : Expr = EList(gid (), elems)


  let ePartial (e : Expr) : Expr = EPartial(gid (), e)

  let eVar (name : string) : Expr = EVariable(gid (), name)

  let fieldAccess (expr : Expr) (fieldName : string) : Expr =
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

  let pFloatStr (whole : string) (fraction : string) : Pattern =
    PFloat(gid (), float $"{whole}{fraction}")

  let pFloat (whole : int) (fraction : int) : Pattern =
    PFloat(gid (), float $"{whole}{fraction}")

  let pNull () : Pattern = PNull(gid ())

  let pBlank () : Pattern = PBlank(gid ())

  let eflag cond oldCode newCode = EFeatureFlag(gid (), cond, oldCode, newCode)

module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  type Spec =
    | HTTP of path : string * method : string
    | Worker of name : string
    // Deprecated but still supported form
    | OldWorker of modulename : string * name : string
    | Cron of name : string * interval : string
    | REPL of name : string


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
