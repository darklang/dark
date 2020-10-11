module LibExecution.Interpreter

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open System.Threading.Tasks
open FSharp.Control.Tasks

// fsharplint:disable FL0039

exception InternalException of string

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


type Expr =
  | EInt of bigint
  | EString of string
  | ELet of string * Expr * Expr
  | EVariable of string
  | EFnCall of FnDesc.T * List<Expr>
  | EBinOp of Expr * FnDesc.T * Expr
  | ELambda of List<string> * Expr
  | EIf of Expr * Expr * Expr


type Dval =
  | DInt of bigint
  | DString of string
  | DSpecial of Special
  | DList of List<Dval>
  | DBool of bool
  | DLambda of Symtable * List<string> * Expr

  member this.isSpecial: bool =
    match this with
    | DSpecial _ -> true
    | _ -> false

  member this.toJSON(): JsonValue =
    let rec encodeDval (dv: Dval): JsonValue =
      match dv with
      | DInt i -> Encode.bigint i

      | DString str -> Encode.string str

      | DList l -> l |> List.map encodeDval |> Encode.list

      | DBool b -> Encode.bool b
      | DLambda _ -> Encode.nil
      | DSpecial (DError (e)) -> Encode.object [ "error", Encode.string (e.ToString()) ]

    encodeDval this

  static member toDList(list: List<Dval>): Dval =
    List.tryFind (fun (dv: Dval) -> dv.isSpecial) list
    |> Option.defaultValue (DList list)

// eval needs to return a Task<Dval>. This means it creates a Task for even
// simple Dvals like DInts. Instead, we wrap the return value in DvalTask, and
// simple DInts don't have to create an entire Task. (I previously tried making
// DTask part of the Dval, but the types were hard to get right and ensure
// execution happened as expected)
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
    tipe: DType
    doc: string }

(* Runtime errors can be things that happen relatively commonly (such as calling
   a function with an incorrect type), or things that aren't supposed to happen
   but technically can (such as accessing a variable which doesn't exist)
*)
and RuntimeError =
  | NotAFunction of FnDesc.T
  | CondWithNonBool of Dval
  | FnCalledWithWrongTypes of FnDesc.T * List<Dval> * List<Param>
  | FnCalledWhenNotSync of FnDesc.T * List<Dval> * List<Param>
  | UndefinedVariable of string


and Special = DError of RuntimeError

and DType =
  | TString
  | TInt
  | TBool
  | TList of DType
  (* A named variable, eg `a` in `List<a>` *)
  | TVariable of string
  | TFn of List<DType> * DType

let err (e: RuntimeError): Dval = (DSpecial(DError(e)))


module Symtable =
  type T = Symtable
  let empty: T = Map []

  let get (st: T) (name: string): Dval =
    st.TryFind(name)
    |> Option.defaultValue (err (UndefinedVariable name))


module Environment =
  type RetVal = { tipe: DType; doc: string }

  type BuiltInFn =
    { name: FnDesc.T
      parameters: List<Param>
      returnVal: RetVal
      fn: (T * List<Dval>) -> Result<DvalTask, unit> }

  and T = { functions: Map<FnDesc.T, BuiltInFn> }

  let envWith (functions: Map<FnDesc.T, BuiltInFn>): T = { functions = functions }

let param (name: string) (tipe: DType) (doc: string): Param = { name = name; tipe = tipe; doc = doc }
let retVal (tipe: DType) (doc: string): Environment.RetVal = { tipe = tipe; doc = doc }


let sfn (module_: string) (function_: string) (version: int) (args: List<Expr>): Expr =
  EFnCall(FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args)

let binOp (arg1: Expr) (module_: string) (function_: string) (version: int) (arg2: Expr): Expr =
  EBinOp(arg1, FnDesc.fnDesc "dark" "stdlib" module_ function_ version, arg2)

let fizzbuzz: Expr =
  ELet
    ("range",
     (sfn "Int" "range" 0 [ EInt(bigint 1); EInt(bigint 100) ]),
     (sfn
       "List"
        "map"
        0
        [ EVariable "range"
          (ELambda
            ([ "i" ],
             EIf
               ((binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 15))) "Int" "==" 0 (EInt(bigint 0))),
                EString "fizzbuzz",
                EIf
                  (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 5))) "Int" "==" 0 (EInt(bigint 0)),
                   EString "buzz",
                   EIf
                     (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3))) "Int" "==" 0 (EInt(bigint 0)),
                      EString "fizz",
                      sfn "Int" "toString" 0 [ EVariable "i" ]))))) ]))

let fizzboom: Expr =
  ELet
    ("range",
     (sfn "Int" "range" 0 [ EInt(bigint 1); EInt(bigint 100) ]),
     (sfn
       "List"
        "map"
        0
        [ EVariable "range"
          (ELambda
            ([ "i" ],
             EIf
               ((binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 15))) "Int" "==" 0 (EInt(bigint 0))),
                (sfn "HttpClient" "get" 0 [ EString "http://localhost:1025/delay/1" ]),
                EIf
                  (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 5))) "Int" "==" 0 (EInt(bigint 0)),
                   EString "buzz",
                   EIf
                     (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3))) "Int" "==" 0 (EInt(bigint 0)),
                      EString "fizz",
                      sfn "Int" "toString" 0 [ EVariable "i" ]))))) ]))

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

let rec eval (env: Environment.T) (st: Symtable.T) (e: Expr): DvalTask =
  let tryFindFn desc = env.functions.TryFind(desc)
  match e with
  | EInt i -> Plain(DInt i)
  | EString s -> Plain(DString s)
  | ELet (lhs, rhs, body) ->
      let rhs = eval env st rhs
      rhs.bind (fun rhs ->
        let st = st.Add(lhs, rhs)
        eval env st body)
  | EFnCall (desc, exprs) ->
      (match tryFindFn desc with
       | Some fn ->
           Task
             (task {
               let! args = map_s exprs (eval env st)
               return! (callFn env fn (Seq.toList args)).toTask()
              })
       | None -> Plain(err (NotAFunction desc)))
  | EBinOp (arg1, desc, arg2) ->
      (match tryFindFn desc with
       | Some fn ->
           let t1 = eval env st arg1
           let t2 = eval env st arg2
           t1.bind2 t2 (fun arg1 arg2 -> callFn env fn [ arg1; arg2 ])
       | None -> Plain(err (NotAFunction desc)))
  | ELambda (vars, expr) -> Plain(DLambda(st, vars, expr))
  | EVariable (name) -> Plain(Symtable.get st name)
  | EIf (cond, thenbody, elsebody) ->
      let cond = eval env st cond
      cond.bind (function
        | DBool (true) -> eval env st thenbody
        | DBool (false) -> eval env st elsebody
        | cond -> Task(task { return (err (CondWithNonBool cond)) }))


and callFn (env: Environment.T) (fn: Environment.BuiltInFn) (args: List<Dval>): DvalTask =
  match List.tryFind (fun (dv: Dval) -> dv.isSpecial) args with
  | Some special -> Plain special
  | None ->
      match fn.fn (env, args) with
      | Ok result -> result
      | Error () -> Plain(err (FnCalledWithWrongTypes(fn.name, args, fn.parameters)))

module StdLib =
  let functions (): Map<FnDesc.T, Environment.BuiltInFn> =
    let fns: List<Environment.BuiltInFn> =
      [ { name = (FnDesc.stdFnDesc "Int" "range" 0)
          parameters =
            [ param "list" (TList(TVariable("a"))) "The list to be operated on"
              param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
          returnVal = retVal (TList(TInt)) "List of ints between lowerBound and upperBound"
          fn =
            (function
            | _, [ DInt lower; DInt upper ] ->
                List.map DInt [ lower .. upper ]
                |> DList
                |> Plain
                |> Ok
            | _ -> Error()) }
        { name = (FnDesc.stdFnDesc "List" "map" 0)
          parameters =
            [ param "list" (TList(TVariable("a"))) "The list to be operated on"
              param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
          returnVal =
            (retVal
              (TList(TVariable("b")))
               "A list created by the elements of `list` with `fn` called on each of them in order")
          fn =
            (function
            | env, [ DList l; DLambda (st, [ var ], body) ] ->
                Ok
                  (Task
                    (task {
                      let! result =
                        map_s l (fun dv ->
                          let st = st.Add(var, dv)
                          eval env st body)

                      return (result |> Dval.toDList)
                     }))
            | _ -> Error()) }
        { name = (FnDesc.stdFnDesc "Int" "%" 0)
          parameters =
            [ param "a" TInt "Numerator"
              param "b" TInt "Denominator" ]
          returnVal = (retVal TInt "Returns the modulus of a / b")
          fn =
            (function
            | env, [ DInt a; DInt b ] ->
                try
                  Ok(Plain(DInt(a % b)))
                with _ -> Ok(Plain(DInt(bigint 0)))
            | _ -> Error()) }
        { name = (FnDesc.stdFnDesc "Int" "==" 0)
          parameters =
            [ param "a" TInt "a"
              param "b" TInt "b" ]
          returnVal =
            (retVal
              TBool
               "True if structurally equal (they do not have to be the same piece of memory, two dicts or lists or strings with the same value will be equal), false otherwise")
          fn =
            (function
            | env, [ DInt a; DInt b ] -> Ok(Plain(DBool(a = b)))
            | _ -> Error()) }
        { name = (FnDesc.stdFnDesc "Int" "toString" 0)
          parameters = [ param "a" TInt "value" ]
          returnVal = (retVal TString "Stringified version of a")
          fn =
            (function
            | env, [ DInt a ] -> Ok(Plain(DString(a.ToString())))

            | _ -> Error()) }
        { name = (FnDesc.stdFnDesc "HttpClient" "get" 0)
          parameters = [ param "url" TString "URL to fetch" ]
          returnVal = (retVal TString "Body of response")
          fn =
            (function
            | env, [ DString url ] ->
                try
                  Ok
                    (Task
                      (task {
                        let! response = FSharp.Data.Http.AsyncRequestString(url)
                        return DString(response)
                       }))
                with e ->
                  printfn "error in HttpClient::get: %s" (e.ToString())
                  Error()
            | _ -> Error()) } ]

    fns |> List.map (fun fn -> (fn.name, fn)) |> Map

let env =
  Environment.envWith (StdLib.functions ())

let run (e: Expr): Task<Dval> = (eval env Symtable.empty e).toTask()


let runString (e: Expr): Task<string> =
  task {
    let! result = run e
    return result.ToString()
  }


let runJSON (e: Expr): Task<string> =
  task {
    let! result = run e
    return result.toJSON().ToString()
  }
