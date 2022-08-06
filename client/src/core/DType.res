@ppx.deriving(show({with_path: false}))
type rec t =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TChar
  | TList(t)
  | TTuple(t, t, list<t>)
  | TDict(t)
  | TIncomplete
  | TError
  | THttpResponse(t)
  | TDB(t)
  | TDate
  | TPassword
  | TUuid
  | TOption(t)
  | TErrorRail
  | TUserType(string, int)
  | TBytes
  | TResult(t, t)
  | TVariable(string)
  | TFn(list<t>, t)
  | TDbList(t)
  | TRecord(list<(string, t)>)

let any = TVariable("any")

let rec tipe2str = (t: t): string =>
  switch t {
  | TVariable(_) => "Any"
  | TInt => "Int"
  | TFloat => "Float"
  | TBool => "Bool"
  | TChar => "Char"
  | TNull => "Null"
  | TStr => "String"
  | TList(_) => "List"
  | TTuple(_, _, _) => "Tuple"
  | TDict(_) => "Dict"
  | TFn(_) => "Block"
  | TIncomplete => "Incomplete"
  | TError => "Error"
  | THttpResponse(_) => "Response"
  | TDB(_) => "Datastore"
  | TDate => "Date"
  | TOption(_) => "Option"
  | TPassword => "Password"
  | TUuid => "UUID"
  | TErrorRail => "ErrorRail"
  | TResult(_) => "Result"
  | TDbList(a) => "[" ++ (tipe2str(a) ++ "]")
  | TUserType(name, _) => name
  | TBytes => "Bytes"
  | TRecord(_) => "Record"
  }

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
      ("TFloat", dv0(TFloat)),
      ("TBool", dv0(TBool)),
      ("TNull", dv0(TNull)),
      ("TStr", dv0(TStr)),
      ("TList", dv1(t1 => TList(t1), d)),
      ("TDbList", dv1(t1 => TDbList(t1), d)),
      ("TTuple", dv3((first, second, theRest) => TTuple(first, second, theRest), d, d, list(d))),
      ("TDict", dv1(t1 => TDict(t1), d)),
      ("TIncomplete", dv0(TIncomplete)),
      ("TError", dv0(TError)),
      ("THttpResponse", dv1(t1 => THttpResponse(t1), d)),
      ("TDB", dv1(t1 => TDB(t1), d)),
      ("TDate", dv0(TDate)),
      ("TChar", dv0(TChar)),
      ("TPassword", dv0(TPassword)),
      ("TUuid", dv0(TUuid)),
      ("TOption", dv1(t1 => TOption(t1), d)),
      ("TErrorRail", dv0(TErrorRail)),
      ("TBytes", dv0(TBytes)),
      ("TResult", dv2((t1, t2) => TResult(t1, t2), d, d)),
      ("TUserType", dv2((n, v) => TUserType(n, v), string, int)),
      ("TVariable", dv1(name => TVariable(name), string)),
      ("TFn", dv2((args, rt) => TFn(args, rt), list(d), d)),
      ("TRecord", dv1(rows => TRecord(rows), list(pair(string, d)))),
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
  | TChar => ev("TChar", list{})
  | TBool => ev("TBool", list{})
  | TFloat => ev("TFloat", list{})
  | TDict(t1) => ev("TDict", list{encode(t1)})
  | TList(t1) => ev("TList", list{encode(t1)})
  | TTuple(first, second, theRest) =>
    ev("TTuple", list{encode(first), encode(second), list(encode, theRest)})
  | TVariable(name) => ev("TVariable", list{string(name)})
  | TNull => ev("TNull", list{})
  | TFn(args, rt) => ev("TFn", list{list(encode, args), encode(rt)})
  | TIncomplete => ev("TIncomplete", list{})
  | TError => ev("TError", list{})
  | THttpResponse(t1) => ev("THttpResponse", list{encode(t1)})
  | TDB(t1) => ev("TDB", list{encode(t1)})
  | TDate => ev("TDate", list{})
  | TDbList(a) => ev("TDbList", list{encode(a)})
  | TPassword => ev("TPassword", list{})
  | TUuid => ev("TUuid", list{})
  | TOption(t1) => ev("TOption", list{encode(t1)})
  | TErrorRail => ev("TErrorRail", list{})
  | TResult(t1, t2) => ev("TResult", list{encode(t1), encode(t2)})
  | TUserType(name, version) => ev("TUserType", list{string(name), int(version)})
  | TBytes => ev("TBytes", list{})
  | TRecord(rows) => ev("TRecord", list{list(pair(string, encode), rows)})
  }
}
