module LibExecution.Interpreter

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

// fsharplint:disable FL0039

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
            match this with
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

let err (e: RuntimeError) = DSpecial(DError(e))


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
          fn: (T * List<Dval>) -> Result<Dval, unit> }

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
                                   (binOp
                                       (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3)))
                                        "Int"
                                        "=="
                                        0
                                        (EInt(bigint 0)),
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
                                   (binOp
                                       (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3)))
                                        "Int"
                                        "=="
                                        0
                                        (EInt(bigint 0)),
                                    EString "fizz",
                                    sfn "Int" "toString" 0 [ EVariable "i" ]))))) ]))




let rec eval (env: Environment.T) (st: Symtable.T) (e: Expr): Dval =
    match e with
    | EInt i -> DInt i
    | EString s -> DString s
    | ELet (lhs, rhs, body) ->
        let rhs = eval env st rhs
        let st = st.Add(lhs, rhs)
        let result = (eval env st body)
        result
    | EFnCall (desc, args) ->
        (match env.functions.TryFind desc with
         | Some fn ->
             let args = (List.map (eval env st) args)
             callFn env fn (Seq.toList args)
         | None -> (err (NotAFunction desc)))
    | EBinOp (arg1, desc, arg2) ->
        (match env.functions.TryFind desc with
         | Some fn ->
             let arg1 = eval env st arg1
             let arg2 = eval env st arg2
             callFn env fn [ arg1; arg2 ]
         | None -> (err (NotAFunction desc)))
    | ELambda (vars, expr) -> DLambda(st, vars, expr)
    | EVariable (name) -> Symtable.get st name
    | EIf (cond, thenbody, elsebody) ->
        let cond = eval env st cond
        match cond with
        | DBool (true) -> eval env st thenbody
        | DBool (false) -> eval env st elsebody
        | _ -> err (CondWithNonBool cond)


and callFn (env: Environment.T) (fn: Environment.BuiltInFn) (args: List<Dval>): Dval =
    match List.tryFind (fun (dv: Dval) -> dv.isSpecial) args with
    | Some special -> special
    | None ->
        let result = fn.fn (env, args)
        match result with
        | Ok result -> result
        | Error () -> err (FnCalledWithWrongTypes(fn.name, args, fn.parameters))

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
                    | _, [ DInt lower; DInt upper ] -> List.map DInt [ lower .. upper ] |> DList |> Ok
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
                        let result =
                            (List.map (fun dv -> let st = st.Add(var, dv) in eval env st body) l)

                        result |> Seq.toList |> Dval.toDList |> Ok

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
                            Ok(DInt(a % b))
                        with _ -> Ok(DInt(bigint 0))
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
                    | env, [ DInt a; DInt b ] -> Ok(DBool(a = b))
                    | _ -> Error()) }
              { name = (FnDesc.stdFnDesc "Int" "toString" 0)
                parameters = [ param "a" TInt "value" ]
                returnVal = (retVal TString "Stringified version of a")
                fn =
                    (function
                    | env, [ DInt a ] -> Ok(DString(a.ToString()))

                    | _ -> Error()) } ]

        fns |> List.map (fun fn -> (fn.name, fn)) |> Map

(*             { name = (FnDesc.stdFnDesc "HttpClient" "get" 0)
                parameters = [ param "url" TString "URL to fetch" ]
                returnVal = (retVal TString "Body of response")
                fn =
                    (function
                    | env, [ DString url ] ->
                        try
                            let response = FSharp.Data.Http.RequestString(url)
                            let info = JsonValue.Parse(response)
                            Ok(DString(info?data.AsString()))
                        with e ->
                            printfn "error in HttpClient::get: %s" (e.ToString())
                            Error()

                    | _ -> Error()) } *)


let run (e: Expr): Dval =
    let env =
        Environment.envWith (StdLib.functions ())

    eval env Symtable.empty e



let runString (e: Expr): string =

    (run e).ToString()



let runJSON (e: Expr): string = ((run e).toJSON()).ToString()
