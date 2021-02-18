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
open Tablecloth

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

      if this.owner = "dark" && this.package = "stdlib" then fn
      else if this.owner = "" && this.package = "" then fn
      else $"{this.owner}/{this.package}/{fn}"

  let namePat = @"^[a-z][a-z0-9_]*$"
  let modNamePat = @"^[A-Z][a-z0-9A-Z_]*$"
  let fnnamePat = @"^([a-z][a-z0-9A-Z_]*|[-+><&|!=^%/*]{1,2})$"

  let packageName
    (owner : string)
    (package : string)
    (module_ : string)
    (function_ : string)
    (version : int)
    : T =
    assertRe "owner must match" namePat owner
    assertRe "package must match" namePat package
    if module_ <> "" then assertRe "modName name must match" modNamePat module_
    assertRe "function name must match" fnnamePat function_
    assert_ "version can't be negative" (version >= 0)

    { owner = owner
      package = package
      module_ = module_
      function_ = function_
      version = version }

  let userFnName (fnName : string) : T =
    assertRe "function name must match" fnnamePat fnName
    { owner = ""; package = ""; module_ = ""; function_ = fnName; version = 0 }

  let stdlibName (module_ : string) (function_ : string) (version : int) : T =
    packageName "dark" "stdlib" module_ function_ version

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
  | FnName of FQFnName.T

and DHTTP =
  | Redirect of string
  | Response of int * List<string * string>

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
  | DHttpResponse of DHTTP * Dval
  | DDB of string
  | DDate of System.DateTime
  // FSTODO
  (* | DPassword of PasswordBytes.t *)
  | DUuid of System.Guid
  | DOption of Option<Dval>
  | DResult of Result<Dval, Dval>
  | DBytes of byte array

and DvalTask = Prelude.TaskOrValue<Dval>

and Symtable = Map<string, Dval>

// A Fake Dval is some control-flow that's modelled in the interpreter as a
// Dval. This is sort of like an Exception. Anytime we see a FakeDval we return
// it instead of operating on it, including when they're put in a list, in a
// value, in a record, as a parameter to a function, etc.
and FakeDval =
  // a DError represents something that shouldn't have happened in the engine,
  // that should have been reported elsewhere. It's usually a type error of
  // some kind, but occasionally we'll paint ourselves into a corner and need
  // to represent a runtime error using this.
  | DError of DvalSource * string
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

// Record the source of an incomplete or error. Would be useful to add more
// information later, such as the iteration count that let to this, or
// something like a stack trace
and DvalSource =
  | SourceNone
  | SourceID of tlid * id

and Param =
  { name : string
    typ : DType
    blockArgs : List<string>
    description : string }

  static member make (name : string) (typ : DType) (description : string) : Param =
    { name = name; typ = typ; description = description; blockArgs = [] }

module Expr =
  let toID (expr : Expr) : id =
    match expr with
    | EInteger (id, _)
    | EString (id, _)
    | ECharacter (id, _)
    | EBool (id, _)
    | ENull id
    | EFloat (id, _)
    | EVariable (id, _)
    | EFieldAccess (id, _, _)
    | ELambda (id, _, _)
    | EBlank id
    | ELet (id, _, _, _)
    | EIf (id, _, _, _)
    | EPartial (id, _)
    | EApply (id, _, _, _, _)
    | EList (id, _)
    | ERecord (id, _)
    | EFQFnValue (id, _)
    | EConstructor (id, _, _)
    | EFeatureFlag (id, _, _, _)
    | EMatch (id, _, _) -> id



module Dval =
  let isFake (dv : Dval) : bool =
    match dv with
    | DFakeVal _ -> true
    | _ -> false

  let isIncomplete (dv : Dval) : bool =
    match dv with
    | DFakeVal (DIncomplete _) -> true
    | _ -> false

  let isErrorRail (dv : Dval) : bool =
    match dv with
    | DFakeVal (DErrorRail _) -> true
    | _ -> false

  let isDError (dv : Dval) : bool =
    match dv with
    | DFakeVal (DError _) -> true
    | _ -> false

  let unwrapFromErrorRail (dv : Dval) : Dval =
    match dv with
    | DFakeVal (DErrorRail dv) -> dv
    | other -> other

  let toPairs (dv : Dval) : (string * Dval) list =
    match dv with
    | DObj obj -> Map.toList obj
    | _ -> failwith "expecting str"

  let rec toType (dv : Dval) : DType =
    match dv with
    | DInt _ -> TInt
    | DFloat _ -> TFloat
    | DBool _ -> TBool
    | DNull -> TNull
    | DChar _ -> TChar
    | DStr _ -> TStr
    | DList (head :: _) -> TList(toType head)
    | DList [] -> TList TAny
    | DObj map ->
        map |> Map.toList |> List.map (fun (k, v) -> (k, toType v)) |> TRecord
    | DFnVal _ -> TLambda
    | DFakeVal (DError _) -> TError
    | DFakeVal (DIncomplete _) -> TIncomplete
    | DFakeVal (DErrorRail _) -> TErrorRail
    | DHttpResponse (_, dv) -> THttpResponse(toType dv)
    | DDB _ -> TDB TAny
    | DDate _ -> TDate
    // | DPassword _ -> TPassword
    | DUuid _ -> TUuid
    | DOption None -> TOption TAny
    | DOption (Some v) -> TOption(toType v)
    | DResult (Ok v) -> TResult(toType v, TAny)
    | DResult (Error v) -> TResult(TAny, toType v)
    | DBytes _ -> TBytes

  let int (i : int) = DInt(bigint i)
  let bigint (i : bigint) = DInt i
  let parseInt (i : string) = DInt(parseBigint i)

  let float (value : double) : Dval = DFloat value

  let floatParts (sign : Sign, whole : bigint, fraction : bigint) : Dval =
    // FSTODO - add sourceID to errors
    try
      DFloat(makeFloat (sign = Positive) whole fraction)
    with _ ->
      DFakeVal(DError(SourceNone, $"Invalid float: {sign}{whole}.{fraction}"))

  let floatStringParts (sign : Sign, whole : string, fraction : string) : Dval =
    // FSTODO - add sourceID to errors
    try
      DFloat(parseFloat whole fraction)
    with _ ->
      DFakeVal(DError(SourceNone, $"Invalid float: {sign}{whole}.{fraction}"))


  // Dvals should never be constructed that contain fakevals - the fakeval
  // should always propagate (though, there are specific cases in the
  // interpreter where they are discarded instead of propagated; still they are
  // never put into other dvals). These static members check before creating the values

  let list (list : List<Dval>) : Dval =
    List.find (fun (dv : Dval) -> isFake dv) list
    |> Option.defaultValue (DList list)

  let obj (fields : List<string * Dval>) : Dval =
    // Give a warning for duplicate keys
    List.fold
      (DObj Map.empty)
      (fun m (k, v) ->
        match m, k, v with
        // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
        | m, k, v when isFake m -> m
        // Skip empty rows
        | _, "", _ -> m
        | _, _, DFakeVal (DIncomplete _) -> m
        // Errors and Errorrail should propagate (but only if we're not already propagating an error)
        | DObj _, _, v when isFake v -> v
        // Error if the key appears twice
        | DObj m, k, v when Map.containsKey k m ->
            DFakeVal(DError(SourceNone, $"Duplicate key: {k}"))
        // Otherwise add it
        | DObj m, k, v -> DObj(Map.add k v m)
        // If we haven't got a DObj we're propagating an error so let it go
        | m, _, _ -> m)
      fields

  let resultOk (dv : Dval) : Dval = if isFake dv then dv else DResult(Ok dv)

  let resultError (dv : Dval) : Dval = if isFake dv then dv else DResult(Error dv)

  let optionJust (dv : Dval) : Dval = if isFake dv then dv else DOption(Some dv)

  let option (dv : Option<Dval>) : Dval =
    match dv with
    | Some dv -> optionJust dv // checks isFake
    | None -> DOption None

  let errStr (s : string) : Dval = DFakeVal(DError(SourceNone, s))

  let errSStr (source : DvalSource) (s : string) : Dval = DFakeVal(DError(source, s))

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
  type T = { tlid : tlid; name : string; cols : List<Col>; version : int }

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
      body : Expr }

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

module Secret =
  type T = { secretName : string; secretValue : string }


// ------------
// Functions
// ------------

module Package =
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
  // This is a query function (it can't be called inside a query, but it's argument can be a query)
  | QueryFunction
  // This can be implemented by a builtin postgres 9.6 operator with 1 arg (eg `@ x`)
  | SqlUnaryOp of string
  // This can be implemented by a builtin postgres 9.6 operator with 2 args (eg `x + y`)
  | SqlBinOp of string
  // This can be implemented by a builtin postgres 9.6 function
  | SqlFunction of string
  // This can be implemented by a builtin postgres 9.6 function with extra arguments that go first
  | SqlFunctionWithPrefixArgs of string * List<string>
  // This can be implemented by a builtin postgres 9.6 function with extra arguments that go last
  | SqlFunctionWithSuffixArgs of string * List<string>
  // This can be implemented by this callback that receives 1 SQLified-string argument
// | SqlCallback of (string -> string)
  // This can be implemented by this callback that receives 2 SQLified-string argument
  | SqlCallback2 of (string -> string -> string)

  member this.isQueryable() : bool =
    match this with
    | NotYetImplementedTODO
    | NotQueryable
    | QueryFunction -> false
    | SqlUnaryOp _
    | SqlBinOp _
    | SqlFunction _
    | SqlFunctionWithPrefixArgs _
    | SqlFunctionWithSuffixArgs _
    | SqlCallback2 _ -> true

type BuiltInFn =
  { name : FQFnName.T
    parameters : List<Param>
    returnType : DType
    description : string
    previewable : Previewable
    deprecated : Deprecation
    sqlSpec : SqlSpec
    // Functions can be run in JS if they have an implementation in this
    // LibExecution. Functions whose implementation is in LibBackend can only be
    // implemented on the server.
    // May throw an exception, though we're trying to get them to never throw exceptions.
    fn : BuiltInFnSig }

and Fn =
  { name : FQFnName.T
    parameters : List<Param>
    returnType : DType
    description : string
    previewable : Previewable
    deprecated : Deprecation
    sqlSpec : SqlSpec
    // Functions can be run in JS if they have an implementation in this
    // LibExecution. Functions whose implementation is in LibBackend can only be
    // implemented on the server.
    // May throw an exception, though we're trying to get them to never throw exceptions.
    fn : FnImpl }

and BuiltInFnSig = (ExecutionState * List<Dval>) -> DvalTask

and FnImpl =
  | StdLib of BuiltInFnSig
  | UserFunction of tlid * Expr
  | PackageFunction of tlid * Expr

and Context =
  | Real
  | Preview

and ExecutionState =
  { functions : Map<FQFnName.T, BuiltInFn>
    tlid : tlid
    canvasID : CanvasID
    accountID : UserID
    dbs : Map<string, DB.T>
    userFns : Map<string, UserFunction.T>
    userTypes : Map<string, UserType.T>
    packageFns : Map<FQFnName.T, Package.Fn>
    secrets : List<Secret.T>
    trace : bool -> id -> Dval -> unit
    traceTLID : tlid -> unit
    executingFnName : string
    // Used for recursion detection in the editor. In the editor, we call all
    // paths to show live values, but with recursion that causes infinite
    // recursion.
    callstack : Set<string>
    context : Context
    // Whether the currently executing code is really being executed (as
    // opposed to being executed for traces)
    onExecutionPath : bool }

let builtInFnToFn (fn : BuiltInFn) : Fn =
  { name = fn.name
    parameters = fn.parameters
    returnType = fn.returnType
    description = ""
    previewable = Impure
    deprecated = NotDeprecated
    sqlSpec = NotQueryable
    fn = StdLib fn.fn }

let userFnToFn (fn : UserFunction.T) : Fn =
  let toParam (p : UserFunction.Parameter) : Param =
    { name = p.name; typ = p.typ; description = p.description; blockArgs = [] }

  { name = FQFnName.userFnName fn.name
    parameters = fn.parameters |> List.map toParam
    returnType = fn.returnType
    description = ""
    previewable = Impure
    deprecated = NotDeprecated
    sqlSpec = NotQueryable
    fn = UserFunction(fn.tlid, fn.body) }

let packageFnToFn (fn : Package.Fn) : Fn =
  let toParam (p : Package.Parameter) : Param =
    { name = p.name; typ = p.typ; description = p.description; blockArgs = [] }

  { name = fn.name
    parameters = fn.parameters |> List.map toParam

    returnType = fn.returnType
    description = ""
    previewable = Impure
    deprecated = NotDeprecated
    sqlSpec = NotQueryable
    fn = PackageFunction(fn.tlid, fn.body) }


// let toFn (uf : T) : Option<BuiltInFn> =
//   let parameters = List.filterMap paramToBuiltinParam uf.parameters in
//   let paramsAllFilled = List.length parameters = List.length uf.parameters
//
//   if uf.name = "" || (not paramsAllFilled) then
//     None
//   else
//     Some
//       { name = FQFnName.name "" "" "" uf.name 0
//         parameters = parameters
//         returnType = uf.returnType
//         description = uf.description
//         previewable = Impure
//         deprecated = NotDeprecated
//         sqlSpec = NotQueryable
//         fn = UserCreated(uf.tlid, uf.ast) }

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
