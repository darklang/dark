module LibExecution.Runtime

// The core types and functions used by the Dark language.
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


open Thoth.Json.Net
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

// fsharplint:disable FL0039

exception InternalException of string

type pos = { x : int; y : int }

type tlid = int64
type id = int64

// A function description: a fully-qualified function name, including package,
// module, and version information.
module FnDesc =
  type T =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

  let fnDesc (owner : string)
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


  let stdFnDesc (module_ : string) (function_ : string) (version : int) : T =
    fnDesc "dark" "stdlib" module_ function_ version

// Expressions - the main part of the language
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
  | EBinOp of id * FnDesc.T * Expr * Expr * SendToRail
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FnDesc.T * List<Expr> * SendToRail
  | EPartial of id * string * Expr
  | ERightPartial of id * string * Expr
  | ELeftPartial of id * string * Expr
  | EList of id * List<Expr>
  | ERecord of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  | EConstructor of id * string * List<Expr>
  | EMatch of id //FSTODO pattern
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr

and SendToRail =
  | Rail
  | NoRail

and LambdaBlock =
  { parameters : List<id * string>
    symtable : Symtable
    body : Expr }

// Runtime values
and Dval =
  | DInt of bigint
  | DFloat of double
  | DBool of bool
  | DNull
  | DStr of string
  | DChar of string // TextElements (extended grapheme clusters) are provided as strings
  (* compound types *)
  | DList of List<Dval>
  | DObj of Map<string, Dval>
  (* special types - see notes above *)
  | DLambda of LambdaBlock
  | DFakeVal of FakeDval
  (* user types: awaiting a better type system *)
  // FSTODO
  (* | DResp of (dhttp * dval) *)
  | DDB of string
  // FSTODO
  (* | DDate of time *)
  // FSTODO
  (* | DPassword of PasswordBytes.t *)
  // FSTODO
  (* | DUuid of uuid *)
  | DOption of Option<Dval>
  | DResult of Result<Dval, Dval>
  // FSTODO
  (* | DBytes of RawBytes.t *)



  static member int(i : int) = DInt(bigint i)

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

  // FSTODO: what kind of JSON is this?
  // Split into multiple files, each for the different kinds of serializers
  member this.toJSON() : JsonValue =
    let rec encodeDval (dv : Dval) : JsonValue =
      let encodeWithType name value = Encode.object [ name, Encode.string value ]
      match dv with
      | DInt i -> Encode.bigint i
      | DChar c -> Encode.string c
      | DFloat d -> Encode.float d
      | DStr str -> Encode.string str
      | DNull -> Encode.unit ()
      | DList l -> l |> List.map encodeDval |> Encode.list
      | DBool b -> Encode.bool b
      | DLambda _ -> Encode.nil
      | DFakeVal (DError (e)) ->
          Encode.object [ "error", Encode.string (e.ToString()) ]
      | DFakeVal (DIncomplete (_)) -> Encode.object [ "incomplete", Encode.unit () ]
      | DFakeVal (DErrorRail (value)) ->
          Encode.object [ "errorrail", encodeDval value ]
      | DObj obj ->
          Encode.object
            (obj |> Map.toList |> List.map (fun (k, v) -> k, encodeDval v))
      | DDB name -> encodeWithType "db" name
      | DOption (Some dv) -> encodeDval dv
      | DOption (None) -> Encode.unit ()
      | DResult (Ok dv) -> encodeDval dv
      | DResult (Error dv) -> Encode.object [ "error", encodeDval dv ]
    // FSTODO
    (* | _ -> Encode.unit () *)

    encodeDval this

  static member toDList(list : List<Dval>) : Dval =
    List.tryFind (fun (dv : Dval) -> dv.isFake) list
    |> Option.defaultValue (DList list)

// We want Dark to by asynchronous (eg, while some code is doing IO, we want
// other code to run instead). F#/.NET uses Tasks for that. However, it's
// expensive to use create tasks for simple Dvals like DInts. Instead, we wrap
// the return value in DvalTask, and simple DInts don't have to create an
// entire Task. (I previously tried making DTask part of the Dval, but the
// types were hard to get right and ensure execution happened as expected)
and DvalTask =
  | Plain of Dval
  | Task of Task<Dval>

  member dt.toTask() : Task<Dval> =
    match dt with
    | Task t -> t
    | Plain dv -> task { return dv }

  member dt.bind(f : Dval -> DvalTask) : DvalTask =
    match dt with
    | Task t ->
        Task
          (task {
            let! resolved = t
            // If `f` returns a task, don't wrap it
            return! (f resolved).toTask()
           })
    | Plain dv -> (f dv)

  member dt.map(f : Dval -> Dval) : DvalTask =
    match dt with
    | Task t ->
        Task
          (task {
            let! dv = t
            return (f dv)
           })
    | Plain dv -> Plain dv

  member dt1.bind2 (dt2 : DvalTask) (f : Dval -> Dval -> DvalTask) : DvalTask =
    match dt1, dt2 with
    | _, Task _
    | Task _, _ ->
        Task
          (task {
            let! t1 = dt1.toTask ()
            let! t2 = dt2.toTask ()
            // If `f` returns a task, don't wrap it
            return! (f t1 t2).toTask()
           })
    | Plain dv1, Plain dv2 -> f dv1 dv2


and Symtable = Map<string, Dval>

and Param =
  { name : string
    typ : DType
    doc : string }

  static member make (name : string) (typ : DType) (doc : string) =
    { name = name; typ = typ; doc = doc }


// Runtime errors can be things that happen relatively commonly (such as calling
// a function with an incorrect type), or things that aren't supposed to happen
// but technically can (such as accessing a variable which doesn't exist) *)
// FSTODO: this will be better for everyone if it's just a string. We can turn
// these into functions to create the string if we want. The type safety here
// isn't worth the storage/serialization challenges of changing these later.
and RuntimeError =
  | NotAFunction of FnDesc.T
  | FunctionRemoved of FnDesc.T
  | CondWithNonBool of Dval
  | FnCalledWithWrongTypes of FnDesc.T * List<Dval> * List<Param>
  | FnCalledWhenNotSync of FnDesc.T * List<Dval> * List<Param>
  | LambdaCalledWithWrongCount of List<Dval> * List<string>
  | LambdaCalledWithWrongType of List<Dval> * List<string>
  | LambdaResultHasWrongType of Dval * DType
  | InvalidFloatExpression of string * string
  | UndefinedVariable of string
  | UndefinedConstructor of string

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

// Record the source of an incomplete or error. Would be useful to add more
// information later, such as the iteration count that let to this, or
// something like a stack trace
and DvalSource =
  | SourceNone
  | SourceID of tlid * id

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
  | TDict of DType * DType
  | TIncomplete
  | TError
  | TLambda
  | TResp
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

// StdLib functions can go wrong for various reasons. We previous tied
// ourselves in knots, trying to do elabortate folds to get them working. Much
// easier to throw an exception (perhaps a lesson for Dark?)
exception RuntimeException of RuntimeError // when we know the runtime error to raise
exception FnCallException of FnCallError // when we need callFn to fill in
exception FakeDvalException of Dval // when we encounter a fakeDval, jump right own

// .NET's System.Random is a PRNG, and on .NET Core, this is seeded from an
// OS-generated truly-random number.
// https://github.com/dotnet/runtime/issues/23198#issuecomment-668263511 We
// also use a single global value for the VM, so that users cannot be
// guaranteed to get multiple consequetive values (as other requests may intervene)
let random : System.Random = System.Random()

let err (e : RuntimeError) : Dval = (DFakeVal(DError(e)))

module Symtable =
  type T = Symtable
  let empty : T = Map []

  let get (st : T) (name : string) : Dval =
    st.TryFind(name) |> Option.defaultValue (err (UndefinedVariable name))



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
  | ReplacedBy of FnDesc.T
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
  { name : FnDesc.T
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

and ExecutionState = { functions : Map<FnDesc.T, BuiltInFn>; tlid : tlid }
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


// Processes each item of the list in order, waiting for the previous one to
// finish. This ensures each request in the list is processed to completion
// before the next one is done, making sure that, for example, a HttpClient
// call will finish before the next one starts. Will allow other requests to
// run which waiting.
let map_s (f : 'a -> DvalTask) (list : List<'a>) : Task<List<Dval>> =
  task {
    let! result =
      match list with
      | [] -> task { return [] }
      | head :: tail ->
          task {
            let firstComp =
              task {
                let! result = (f head).toTask()
                return ([], result)
              }

            let! ((accum, lastcomp) : (List<Dval> * Dval)) =
              List.fold (fun (prevcomp : Task<List<Dval> * Dval>) (arg : 'a) ->
                task {
                  // Ensure the previous computation is done first
                  let! ((accum, prev) : (List<Dval> * Dval)) = prevcomp
                  let accum = prev :: accum

                  let! result = (f arg).toTask()

                  return (accum, result)
                }) firstComp tail

            return List.rev (lastcomp :: accum)
          }

    return (result |> Seq.toList)
  }

let incorrectArgs () = raise (FnCallException FnWrongTypes)

let removedFunction : BuiltInFnSig =
  fun _ -> raise (FnCallException FnFunctionRemoved)

module Shortcuts =
  let gid () : int64 =
    // get enough bytes for an int64, trim it to an int63 for now to match ocaml.
    let bytes = Array.init 8 (fun _ -> (byte) 0)
    random.NextBytes(bytes)
    let rand64 : int64 = System.BitConverter.ToInt64(bytes, 0)
    let mask : int64 = 9223372036854775807L
    rand64 &&& mask

  let efn' (module_ : string)
           (function_ : string)
           (version : int)
           (args : List<Expr>)
           (ster : SendToRail)
           : Expr =
    EFnCall
      (gid (), FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args, ster)

  let efn (module_ : string)
          (function_ : string)
          (version : int)
          (args : List<Expr>)
          : Expr =
    efn' module_ function_ version args NoRail

  let erailFn (module_ : string)
              (function_ : string)
              (version : int)
              (args : List<Expr>)
              : Expr =
    efn' module_ function_ version args Rail

  let ebinOp' (module_ : string)
              (function_ : string)
              (version : int)
              (arg1 : Expr)
              (arg2 : Expr)
              (ster : SendToRail)
              : Expr =
    EBinOp
      (gid (),
       FnDesc.fnDesc "dark" "stdlib" module_ function_ version,
       arg1,
       arg2,
       ster)

  let ebinOp (module_ : string)
             (function_ : string)
             (version : int)
             (arg1 : Expr)
             (arg2 : Expr)
             : Expr =
    ebinOp' module_ function_ version arg1 arg2 NoRail

  // An ebinOp that's on the rail
  let erailBinOp (module_ : string)
                 (function_ : string)
                 (version : int)
                 (arg1 : Expr)
                 (arg2 : Expr)
                 : Expr =
    ebinOp' module_ function_ version arg1 arg2 Rail

  let estr (str : string) : Expr = EString(gid (), str)
  let eint (i : int) : Expr = EInteger(gid (), i.ToString())

  let eintStr (i : string) : Expr =
    assert ((new Regex(@"-?\d+")).IsMatch(i))
    EInteger(gid (), i)

  let echar (c : char) : Expr = ECharacter(gid (), string c)
  let echarStr (c : string) : Expr = ECharacter(gid (), c)
  let eblank () : Expr = EBlank(gid ())
  let ebool (b : bool) : Expr = EBool(gid (), b)

  let efloat (whole : int) (fraction : int) : Expr =
    EFloat(gid (), whole.ToString(), fraction.ToString())

  let efloatStr (whole : string) (fraction : string) : Expr =
    // FSTODO: don't actually assert, report to rollbar
    assert ((new Regex(@"-?\d+")).IsMatch(whole))
    assert ((new Regex(@"\d+")).IsMatch(fraction))
    EFloat(gid (), whole, fraction)

  let enull () : Expr = ENull(gid ())

  let erecord (rows : (string * Expr) list) : Expr = ERecord(gid (), rows)

  let elist (elems : Expr list) : Expr = EList(gid (), elems)
  let epipeTarget () = EPipeTarget(gid ())


  let epartial (str : string) (e : Expr) : Expr = EPartial(gid (), str, e)

  let erightPartial (str : string) (e : Expr) : Expr = ERightPartial(gid (), str, e)


  let eleftPartial (str : string) (e : Expr) : Expr = ELeftPartial(gid (), str, e)


  let evar (name : string) : Expr = EVariable(gid (), name)

  (* let fieldAccess (expr : Expr) (fieldName : string) : Expr = *)
  (*   EFieldAccess (gid () ,expr, fieldName) *)

  let eif (cond : Expr) (then' : Expr) (else' : Expr) : Expr =
    EIf(gid (), cond, then', else')


  let elet (varName : string) (rhs : Expr) (body : Expr) : Expr =
    ELet(gid (), varName, rhs, body)


  let elambda (varNames : string list) (body : Expr) : Expr =
    ELambda(gid (), List.map (fun name -> (gid (), name)) varNames, body)


  (* let pipe (first : Expr) (rest : Expr list) : Expr = *)
  (*   EPipe (gid () ,first :: rest) *)

  let econstructor (name : string) (args : Expr list) : Expr =
    EConstructor(gid (), name, args)


  let ejust (arg : Expr) : Expr = EConstructor(gid (), "Just", [ arg ])

  let enothing () : Expr = EConstructor(gid (), "Nothing", [])

  let eerror (arg : Expr) : Expr = EConstructor(gid (), "Error", [ arg ])

  let eok (arg : Expr) : Expr = EConstructor(gid (), "Ok", [ arg ])

(* let match' (cond : Expr) (matches : (FluidPattern.t * t) list) : Expr = *)
(*   EMatch (gid () ,cond, matches) *)
(*  *)
(*  *)
(* let pInt (int : int) : FluidPattern.t = *)
(*   FPInteger (mid, id, string_of_int int) *)
(*  *)
(*  *)
(* let pIntStr (int : string) : FluidPattern.t = *)
(*   FPInteger (mid, id, int) *)
(*  *)
(*  *)
(* let pVar (name : string) : FluidPattern.t = *)
(*   FPVariable (mid, id, name) *)
(*  *)
(*  *)
(* let pConstructor *)
(*     ?(mid = gid ()) *)
(*     ?(id = gid ()) *)
(*     (name : string) *)
(*     (patterns : FluidPattern.t list) : FluidPattern.t = *)
(*   FPConstructor (mid, id, name, patterns) *)
(*  *)
(*  *)
(* let pJust (arg : FluidPattern.t) : FluidPattern.t *)
(*     = *)
(*   FPConstructor (mid, id, "Just", [arg]) *)
(*  *)
(*  *)
(* let pNothing () : FluidPattern.t = *)
(*   FPConstructor (mid, id, "Nothing", []) *)
(*  *)
(*  *)
(* let pError (arg : FluidPattern.t) : *)
(*     FluidPattern.t = *)
(*   FPConstructor (mid, id, "Error", [arg]) *)
(*  *)
(*  *)
(* let pOk (arg : FluidPattern.t) : FluidPattern.t = *)
(*   FPConstructor (mid, id, "Ok", [arg]) *)
(*  *)
(*  *)
(* let pBool (b : bool) : FluidPattern.t = *)
(*   FPBool (mid, id, b) *)
(*  *)
(*  *)
(* let pString (str : string) : FluidPattern.t = *)
(*   FPString {matchID = mid; patternID = id; str} *)
(*  *)
(*  *)
(* let pFloatStr *)
(*     (whole : string) (fraction : string) : *)
(*     FluidPattern.t = *)
(*   FPFloat (mid, id, whole, fraction) *)
(*  *)
(*  *)
(* let pFloat (whole : int) (fraction : int) : *)
(*     FluidPattern.t = *)
(*   FPFloat (mid, id, string_of_int whole, string_of_int fraction) *)
(*  *)
(*  *)
(* let pNull () : FluidPattern.t = FPNull (mid, id) *)
(*  *)
(* let pBlank () : FluidPattern.t = FPBlank (mid, id) *)
(*  *)
(* let flag ?(name = "flag-1") cond oldCode newCode = *)
(*   EFeatureFlag (gid () ,name, cond, oldCode, newCode) *)
(*  *)
(*  *)
