@ppx.deriving(show({with_path: false}))
type rec t =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList
  | TTuple(t, t, list<t>)
  | TObj
  | TIncomplete
  | TError
  | TResp
  | TDB
  | TDate
  | TCharacter
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TUserType(string, int)
  | TBytes
  | TResult
  | TAny
  | TBlock
  | TDbList(t)

let rec decode = (j): t => {
  open Json_decode_extended
  let d = decode
  let dv0 = variant0
  let dv1 = variant1
  let dv2 = variant2
  let dv3 = variant3
  variants(
    list{
      ("TInt", dv0(TInt)),
      ("TAny", dv0(TAny)),
      ("TFloat", dv0(TFloat)),
      ("TBool", dv0(TBool)),
      ("TNull", dv0(TNull)),
      ("TStr", dv0(TStr)),
      ("TList", dv1(_ => TList, d)),
      ("TTuple", dv3((first, second, theRest) => TTuple(first, second, theRest), d, d, list(d))),
      ("TDict", dv1(_ => TObj, d)),
      ("TIncomplete", dv0(TIncomplete)),
      ("TError", dv0(TError)),
      ("THttpResponse", dv1(_ => TResp, d)),
      ("TDB", dv1(_ => TDB, d)),
      ("TDate", dv0(TDate)),
      ("TChar", dv0(TCharacter)),
      ("TPassword", dv0(TPassword)),
      ("TUuid", dv0(TUuid)),
      ("TOption", dv1(_ => TOption, d)),
      ("TErrorRail", dv0(TErrorRail)),
      ("TBytes", dv0(TBytes)),
      ("TResult", dv2((_t1, _t2) => TResult, d, d)),
      ("TVariable", dv1(_ => TAny, string)),
      ("TFn", dv2((_, _) => TBlock, list(d), d)),
      ("TRecord", dv1(_ => TAny, list(pair(string, d)))),
    },
    j,
  )
}

let rec encode = (t: t): Js.Json.t => {
  open Json_encode_extended
  let ev = variant
  switch t {
  | TInt => ev("TInt", list{})
  | TStr => ev("TStr", list{})
  | TCharacter => ev("TCharacter", list{})
  | TBool => ev("TBool", list{})
  | TFloat => ev("TFloat", list{})
  | TObj => ev("TObj", list{})
  | TList => ev("TList", list{})
  | TTuple(first, second, theRest) =>
    ev("TTuple", list{encode(first), encode(second), list(encode, theRest)})
  | TAny => ev("TAny", list{})
  | TNull => ev("TNull", list{})
  | TBlock => ev("TBlock", list{})
  | TIncomplete => ev("TIncomplete", list{})
  | TError => ev("TError", list{})
  | TResp => ev("TResp", list{})
  | TDB => ev("TDB", list{})
  | TDate => ev("TDate", list{})
  | TDbList(a) => ev("TDbList", list{encode(a)})
  | TPassword => ev("TPassword", list{})
  | TUuid => ev("TUuid", list{})
  | TOption => ev("TOption", list{})
  | TErrorRail => ev("TErrorRail", list{})
  | TResult => ev("TResult", list{})
  | TUserType(name, version) => ev("TUserType", list{string(name), int(version)})
  | TBytes => ev("TBytes", list{})
  }
}
