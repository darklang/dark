module LibExecution.StdLib

open FSharp.Control.Tasks
open Runtime
open Interpreter

let param (name: string) (tipe: DType) (doc: string): Param = { name = name; tipe = tipe; doc = doc }
let retVal (tipe: DType) (doc: string): Environment.RetVal = { tipe = tipe; doc = doc }

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
