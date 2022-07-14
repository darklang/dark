/// Types used in many APIs
module ApiServer.ClientTypes

module RT = LibExecution.RuntimeTypes

module DType =
  type T =
    | TInt
    | TFloat
    | TBool
    | TNull
    | TStr
    | TList of T
    | TTuple of T * T * List<T>
    | TDict of T
    | TIncomplete
    | TError
    | THttpResponse of T
    | TDB of T
    | TDate
    | TChar
    | TPassword
    | TUuid
    | TOption of T
    | TErrorRail
    | TUserType of string * int
    | TBytes
    | TResult of T * T
    | TVariable of string
    | TFn of List<T> * T
    | TRecord of List<string * T>

  let rec fromRT (t : RT.DType) =
    let r = fromRT
    let rl = List.map fromRT
    match t with
    | RT.TInt -> TInt
    | RT.TFloat -> TFloat
    | RT.TBool -> TBool
    | RT.TNull -> TNull
    | RT.TStr -> TStr
    | RT.TList t -> TList(r t)
    | RT.TTuple (t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.TIncomplete -> TIncomplete
    | RT.TError -> TError
    | RT.THttpResponse t -> THttpResponse(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDate -> TDate
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TErrorRail -> TErrorRail
    | RT.TUserType (str, version) -> TUserType(str, version)
    | RT.TBytes -> TBytes
    | RT.TResult (ok, error) -> TResult(r ok, r error)
    | RT.TVariable (name) -> TVariable(name)
    | RT.TFn (ts, returnType) -> TFn(rl ts, r returnType)
    | RT.TRecord (pairs) -> TRecord(List.map (fun (k, t) -> (k, r t)) pairs)



// CLEANUP: before merging, use the shares types from PR 4277.
type Dval = DV of LibExecution.RuntimeTypes.Dval

let toRT (DV dv : Dval) : LibExecution.RuntimeTypes.Dval = dv
let fromRT (dv : LibExecution.RuntimeTypes.Dval) : Dval = DV dv
