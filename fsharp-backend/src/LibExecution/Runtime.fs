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


#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open System.Threading.Tasks
open FSharp.Control.Tasks

// fsharplint:disable FL0039

exception InternalException of string

// A function description: a fully-qualified function name, including package,
// module, and version information.

module FnDesc =
  type T =
    { owner: string
      package: string
      module_: string
      function_: string
      version: int }

  let fnDesc (owner: string) (package: string) (module_: string) (function_: string) (version: int): T =
    { owner = owner
      package = package
      module_ = module_
      function_ = function_
      version = version }


  let stdFnDesc (module_: string) (function_: string) (version: int): T =
    fnDesc "dark" "stdlib" module_ function_ version

// Expressions - the main part of the language
type Expr =
  | EInt of bigint
  | EString of string
  | ELet of string * Expr * Expr
  | EVariable of string
  | EFnCall of FnDesc.T * List<Expr> // FSTODO: Error rail
  | EBinOp of Expr * FnDesc.T * Expr
  | ELambda of List<string> * Expr
  | EIf of Expr * Expr * Expr


// Runtime values
and Dval =
  | DInt of bigint
  | DStr of string
  | DFakeVal of FakeDval
  // | DChar of TextElement
  | DList of List<Dval>
  | DBool of bool
  | DLambda of Symtable * List<string> * Expr
  static member int(i: int) = DInt(bigint i)

  member this.isFake: bool =
    match this with
    | DFakeVal _ -> true
    | _ -> false

  member this.toJSON(): JsonValue =
    let rec encodeDval (dv: Dval): JsonValue =
      match dv with
      | DInt i -> Encode.bigint i
      | DStr str -> Encode.string str
      | DList l -> l |> List.map encodeDval |> Encode.list
      | DBool b -> Encode.bool b
      | DLambda _ -> Encode.nil
      | DFakeVal (DError (e)) -> Encode.object [ "error", Encode.string (e.ToString()) ]

    encodeDval this

  static member toDList(list: List<Dval>): Dval =
    List.tryFind (fun (dv: Dval) -> dv.isFake) list
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

  member dt.toTask(): Task<Dval> =
    match dt with
    | Task t -> t
    | Plain dv -> task { return dv }

  member dt.bind(f: Dval -> DvalTask): DvalTask =
    match dt with
    | Task t ->
        Task
          (task {
            let! resolved = t
            // If `f` returns a task, don't wrap it
            return! (f resolved).toTask()
           })
    | Plain dv -> (f dv)

  member dt.map(f: Dval -> Dval): DvalTask =
    match dt with
    | Task t ->
        Task
          (task {
            let! dv = t
            return (f dv)
           })
    | Plain dv -> Plain dv

  member dt1.bind2 (dt2: DvalTask) (f: Dval -> Dval -> DvalTask): DvalTask =
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
  { name: string
    typ: DType
    doc: string }

  static member make (name: string) (typ: DType) (doc: string) = { name = name; typ = typ; doc = doc }


// Runtime errors can be things that happen relatively commonly (such as calling
// a function with an incorrect type), or things that aren't supposed to happen
// but technically can (such as accessing a variable which doesn't exist) *)
and RuntimeError =
  | NotAFunction of FnDesc.T
  | FunctionRemoved of FnDesc.T
  | CondWithNonBool of Dval
  | FnCalledWithWrongTypes of FnDesc.T * List<Dval> * List<Param>
  | FnCalledWhenNotSync of FnDesc.T * List<Dval> * List<Param>
  | UndefinedVariable of string

// Within a function call, we don't have the data available to make good
// RuntimeErrors, so instead return a signal and let the calling code fill in the
// blanks.
//
// Note: Functions shouldn't have runtime errors - those kinds of functions
// should use Results instead. But, we can't really change what a function
// does, so sometimes we discover we made a mistake and need to paper around
// it. In those cases, we return DErrors.
and FnError =
  | FnFunctionRemoved
  | FnWrongTypes

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

and DType =
  | TStr
  | TInt
  | TBool
  | TChar
  | TList of DType
  // A named variable, eg `a` in `List<a>`
  | TVariable of string
  | TFn of List<DType> * DType


let err (e: RuntimeError): Dval = (DFakeVal(DError(e)))

module Symtable =
  type T = Symtable
  let empty: T = Map []

  let get (st: T) (name: string): Dval =
    st.TryFind(name)
    |> Option.defaultValue (err (UndefinedVariable name))


module Environment =

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

  type BuiltInFn =
    { name: FnDesc.T
      parameters: List<Param>
      returnType: DType
      description: string
      previewable: Previewable
      deprecated: Deprecation
      sqlSpec: SqlSpec
      (* Functions can be run in JS if they have an implementation in this
       * LibExecution. Functions who's implementation is in LibBackend can only be
       * implemented on the server. *)
      fn: (T * List<Dval>) -> Result<DvalTask, FnError> }

  and T = { functions: Map<FnDesc.T, BuiltInFn> }

  let envWith (functions: Map<FnDesc.T, BuiltInFn>): T = { functions = functions }


let map_s (list: List<'a>) (f: 'a -> DvalTask): Task<List<Dval>> =
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

            let! ((accum, lastcomp): (List<Dval> * Dval)) =
              List.fold (fun (prevcomp: Task<List<Dval> * Dval>) (arg: 'a) ->
                task {
                  // Ensure the previous computation is done first
                  let! ((accum, prev): (List<Dval> * Dval)) = prevcomp
                  let accum = prev :: accum

                  let! result = (f arg).toTask()

                  return (accum, result)
                }) firstComp tail

            return List.rev (lastcomp :: accum)
          }

    return (result |> Seq.toList)
  }

module Shortcuts =
  let fn (module_: string) (function_: string) (version: int) (args: List<Expr>): Expr =
    EFnCall(FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args)

  let binOp (arg1: Expr) (module_: string) (function_: string) (version: int) (arg2: Expr): Expr =
    EBinOp(arg1, FnDesc.fnDesc "dark" "stdlib" module_ function_ version, arg2)

  let str (str: string) = EString(str)
