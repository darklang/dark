// The types that are used for execution. For all type definitions, see RuntimeTypes.fs

open Belt_extended

@ppx.deriving(show({with_path: false})) type rec id = ID.t
@ppx.deriving(show({with_path: false})) type rec tlid = TLID.t

module MatchPattern = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | MPVariable(id, string)
    | MPConstructor(id, string, list<t>)
    | MPInteger(id, int64)
    | MPBool(id, bool)
    | MPString(id, string)
    | MPCharacter(id, string)
    | MPFloat(id, float)
    | MPNull(id)
    | MPBlank(id)
    | MPTuple(id, t, t, list<t>)

  let rec encode = (matchPattern: t): Js.Json.t => {
    open Json_encode_extended
    let ep = encode
    let ev = variant
    switch matchPattern {
    | MPVariable(id', name) => ev("PVariable", list{ID.encode(id'), string(name)})
    | MPConstructor(id', name, args) =>
      ev("PConstructor", list{ID.encode(id'), string(name), list(ep, args)})
    | MPInteger(id', v) => ev("PInteger", list{ID.encode(id'), int64(v)})
    | MPBool(id', v) => ev("PBool", list{ID.encode(id'), bool(v)})
    | MPFloat(id', v) => ev("PFloat", list{ID.encode(id'), Json_encode_extended.float'(v)})
    | MPString(id', v) => ev("PString", list{ID.encode(id'), string(v)})
    | MPCharacter(id', v) => ev("PCharacter", list{ID.encode(id'), string(v)})
    | MPNull(id') => ev("PNull", list{ID.encode(id')})
    | MPBlank(id') => ev("PBlank", list{ID.encode(id')})
    | MPTuple(id', first, second, theRest) =>
      ev("PTuple", list{ID.encode(id'), ep(first), ep(second), list(ep, theRest)})
    }
  }

  let rec decode = (j): t => {
    open Json_decode_extended
    let dv4 = variant4
    let dv3 = variant3
    let dv2 = variant2
    let dv1 = variant1
    variants(
      list{
        ("PVariable", dv2((a, b) => MPVariable(a, b), ID.decode, string)),
        ("PConstructor", dv3((a, b, c) => MPConstructor(a, b, c), ID.decode, string, list(decode))),
        ("PInteger", dv2((a, b) => MPInteger(a, b), ID.decode, int64)),
        ("PBool", dv2((a, b) => MPBool(a, b), ID.decode, bool)),
        ("PString", dv2((a, b) => MPString(a, b), ID.decode, string)),
        ("PFloat", dv2((a, b) => MPFloat(a, b), ID.decode, Json_decode_extended.float')),
        ("PNull", dv1(a => MPNull(a), ID.decode)),
        ("PBlank", dv1(a => MPBlank(a), ID.decode)),
        (
          "PTuple",
          dv4(
            (a, first, second, theRest) => MPTuple(a, first, second, theRest),
            ID.decode,
            decode,
            decode,
            list(decode),
          ),
        ),
        // CLEANUP: remove the above list in favor of the below
        ("MPVariable", dv2((a, b) => MPVariable(a, b), ID.decode, string)),
        (
          "MPConstructor",
          dv3((a, b, c) => MPConstructor(a, b, c), ID.decode, string, list(decode)),
        ),
        ("MPInteger", dv2((a, b) => MPInteger(a, b), ID.decode, int64)),
        ("MPBool", dv2((a, b) => MPBool(a, b), ID.decode, bool)),
        ("MPString", dv2((a, b) => MPString(a, b), ID.decode, string)),
        ("MPFloat", dv2((a, b) => MPFloat(a, b), ID.decode, Json_decode_extended.float')),
        ("MPNull", dv1(a => MPNull(a), ID.decode)),
        ("MPBlank", dv1(a => MPBlank(a), ID.decode)),
        (
          "MPTuple",
          dv4(
            (a, first, second, theRest) => MPTuple(a, first, second, theRest),
            ID.decode,
            decode,
            decode,
            list(decode),
          ),
        ),
      },
      j,
    )
  }
}

module Expr = {
  module SendToRail = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | Rail
      | NoRail

    let encode = (str: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch str {
      | Rail => ev("Rail", list{})
      | NoRail => ev("NoRail", list{})
      }
    }
    let decode = j => {
      open Json_decode_extended
      let dv0 = variant0
      variants(list{("Rail", dv0(Rail)), ("NoRail", dv0(NoRail))}, j)
    }
  }

  module IsInPipe = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | InPipe(ID.t)
      | NotInPipe

    let encode = (str: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch str {
      | InPipe(id) => ev("InPipe", list{ID.encode(id)})
      | NotInPipe => ev("NotInPipe", list{})
      }
    }
    let decode = j => {
      open Json_decode_extended
      let dv0 = variant0
      let dv1 = variant1
      variants(list{("InPipe", dv1(id => InPipe(id), ID.decode)), ("NotInPipe", dv0(NotInPipe))}, j)
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec isInPipe =
    | InPipe(id)
    | NotInPipe

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | EInteger(id, int64)
    | EBool(id, bool)
    | EString(id, string)
    | ECharacter(id, string)
    | EFloat(id, float)
    | ENull(id)
    | EBlank(id)
    | ELet(id, string, t, t)
    | EIf(id, t, t, t)
    | ELambda(id, list<(id, string)>, t)
    | EFieldAccess(id, t, string)
    | EVariable(id, string)
    | EApply(id, t, list<t>, IsInPipe.t, SendToRail.t)
    | EFQFnValue(id, FQFnName.t)
    | EList(id, list<t>)
    | ETuple(id, t, t, list<t>)
    | ERecord(id, list<(string, t)>)
    | EConstructor(id, string, list<t>)
    | EMatch(id, t, list<(MatchPattern.t, t)>)
    | EFeatureFlag(id, t, t, t)

  let rec decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    let de = decode
    let dv5 = variant5
    let dv4 = variant4
    let dv3 = variant3
    let dv2 = variant2
    let dv1 = variant1
    variants(
      list{
        ("EInteger", dv2((x, y) => EInteger(x, y), ID.decode, int64)),
        ("EBool", dv2((x, y) => EBool(x, y), ID.decode, bool)),
        ("EString", dv2((x, y) => EString(x, y), ID.decode, string)),
        ("ECharacter", dv2((x, y) => ECharacter(x, y), ID.decode, string)),
        ("EFloat", dv2((a, b) => EFloat(a, b), ID.decode, Json_decode_extended.float')),
        ("ENull", dv1(x => ENull(x), ID.decode)),
        ("EBlank", dv1(x => EBlank(x), ID.decode)),
        ("ELet", dv4((a, b, c, d) => ELet(a, b, c, d), ID.decode, string, de, de)),
        ("EIf", dv4((a, b, c, d) => EIf(a, b, c, d), ID.decode, de, de, de)),
        (
          "ELambda",
          dv3((a, b, c) => ELambda(a, b, c), ID.decode, list(pair(ID.decode, string)), de),
        ),
        ("EFieldAccess", dv3((a, b, c) => EFieldAccess(a, b, c), ID.decode, de, string)),
        ("EVariable", dv2((x, y) => EVariable(x, y), ID.decode, string)),
        (
          "EApply",
          dv5(
            (a, b, c, d, e) => EApply(a, b, c, d, e),
            ID.decode,
            de,
            list(de),
            IsInPipe.decode,
            SendToRail.decode,
          ),
        ),
        ("EList", dv2((x, y) => EList(x, y), ID.decode, list(de))),
        (
          "ETuple",
          dv4((x, y1, y2, yRest) => ETuple(x, y1, y2, yRest), ID.decode, de, de, list(de)),
        ),
        ("ERecord", dv2((x, y) => ERecord(x, y), ID.decode, list(pair(string, de)))),
        ("EConstructor", dv3((a, b, c) => EConstructor(a, b, c), ID.decode, string, list(de))),
        (
          "EMatch",
          dv3((a, b, c) => EMatch(a, b, c), ID.decode, de, list(pair(MatchPattern.decode, de))),
        ),
        ("EFeatureFlag", dv4((a, b, c, d) => EFeatureFlag(a, b, c, d), ID.decode, de, de, de)),
        ("EFQFnValue", dv2((a, b) => EFQFnValue(a, b), ID.decode, FQFnName.decode)),
      },
      j,
    )
  }

  let rec encode = (expr: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch expr {
    | ELet(id, lhs, rhs, body) =>
      ev("ELet", list{ID.encode(id), string(lhs), encode(rhs), encode(body)})
    | EIf(id', cond, ifbody, elsebody) =>
      ev("EIf", list{ID.encode(id'), encode(cond), encode(ifbody), encode(elsebody)})
    | ELambda(id, vars, body) =>
      ev("ELambda", list{ID.encode(id), list(pair(ID.encode, string), vars), encode(body)})
    | EFieldAccess(id, obj, field) =>
      ev("EFieldAccess", list{ID.encode(id), encode(obj), string(field)})
    | EString(id, v) => ev("EString", list{ID.encode(id), string(v)})
    | ECharacter(id, v) => ev("ECharacter", list{ID.encode(id), string(v)})
    | EInteger(id, v) => ev("EInteger", list{ID.encode(id), int64(v)})
    | EBool(id, v) => ev("EBool", list{ID.encode(id), bool(v)})
    | EFloat(id, v) => ev("EFloat", list{ID.encode(id), Json_encode_extended.float'(v)})
    | ENull(id) => ev("ENull", list{ID.encode(id)})
    | EBlank(id) => ev("EBlank", list{ID.encode(id)})
    | EVariable(id, name) => ev("EVariable", list{ID.encode(id), string(name)})
    | EList(id, exprs) => ev("EList", list{ID.encode(id), list(encode, exprs)})
    | EApply(id, expr, exprs, inPipe, sendToRail) =>
      ev(
        "EApply",
        list{
          ID.encode(id),
          encode(expr),
          list(encode, exprs),
          IsInPipe.encode(inPipe),
          SendToRail.encode(sendToRail),
        },
      )
    | ETuple(id, first, second, theRest) =>
      ev("ETuple", list{ID.encode(id), encode(first), encode(second), list(encode, theRest)})
    | ERecord(id, pairs) => ev("ERecord", list{ID.encode(id), list(pair(string, encode), pairs)})
    | EFeatureFlag(id, cond, a, b) =>
      ev("EFeatureFlag", list{ID.encode(id), encode(cond), encode(a), encode(b)})
    | EMatch(id, matchExpr, cases) =>
      ev(
        "EMatch",
        list{ID.encode(id), encode(matchExpr), list(pair(MatchPattern.encode, encode), cases)},
      )
    | EFQFnValue(id, name) => ev("EFQFnValue", list{ID.encode(id), FQFnName.encode(name)})
    | EConstructor(id, name, args) =>
      ev("EConstructor", list{ID.encode(id), string(name), list(encode, args)})
    }
  }
}

module Dval = {
  module DvalSource = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | SourceNone
      | SourceID(TLID.t, ID.t)

    let decode = {
      open Json_decode_extended
      variants(list{
        ("SourceNone", variant0(SourceNone)),
        ("SourceID", variant2((x, y) => SourceID(x, y), TLID.decode, ID.decode)),
      })
    }

    let encode = (s: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch s {
      | SourceNone => ev("SourceNone", list{})
      | SourceID(t, i) => ev("SourceID", list{TLID.encode(t), ID.encode(i)})
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec lambdaImpl = {
    parameters: list<(ID.t, string)>,
    symtable: Belt.Map.String.t<t>,
    body: Expr.t,
  }

  and dHttp =
    | Redirect(string)
    | Response(int64, list<(string, string)>, t)

  and fnValImpl =
    | Lambda(lambdaImpl)
    | FnName(FQFnName.t)

  and t =
    | DInt(int64)
    | DFloat(float)
    | DBool(bool)
    | DNull
    | DStr(string)
    | DChar(string)
    | DList(list<t>)
    | DTuple(t, t, list<t>)
    // We use Belt.Map.String as Map.String.t has a comparator that doesn't work
    // with the cloning algorithm of web workers
    | DObj(Belt.Map.String.t<t>)
    | DFnVal(fnValImpl)
    | DError((DvalSource.t, string))
    | DIncomplete(DvalSource.t)
    | DErrorRail(t)
    | DHttpResponse(dHttp)
    | DDB(string)
    | DDate(string)
    | DPassword(string)
    | DUuid(string)
    | DOption(option<t>)
    | DResult(Belt.Result.t<t, t>)
    | DBytes(bytes)

  /// Gets the Dark runtime type from a runtime value
  let rec toType = (dv: t): DType.t => {
    // CLEANUP this function is derived from the backend version, but avoid blowing
    // everything up I've kept it on the current types.
    let any = DType.any

    switch dv {
    | DInt(_) => TInt
    | DFloat(_) => TFloat
    | DBool(_) => TBool
    | DNull => TNull
    | DChar(_) => TChar
    | DStr(_) => TStr
    | DList(_) => TList(any)
    // | DList(list{head, ..._}) => TList(toType(head))
    // | DList(list{}) => TList(any)
    | DTuple(first, second, theRest) =>
      TTuple(toType(first), toType(second), List.map(toType, theRest))
    // | DObj(map) =>
    //   switch map |> Belt.Map.String.toList |> Tc.List.head {
    //   | Some(_, v1) => TDict(toType(v1))
    //   | None => TDict(TAny)
    //   }
    | DObj(_) => TDict(any)
    | DFnVal(_) => TFn(list{}, any)
    | DError(_) => TError
    | DIncomplete(_) => TIncomplete
    | DErrorRail(_) => TErrorRail
    | DHttpResponse(_) => THttpResponse(any)
    // | DHttpResponse(Response(_, _, _dv)) => THttpResponse(toType(dv))
    // | DHttpResponse(Redirect(_)) => THttpResponse(TNull)
    | DDB(_) => TDB(any)
    | DDate(_) => TDate
    | DPassword(_) => TPassword
    | DUuid(_) => TUuid
    | DOption(_) => TOption(any)
    // | DOption(None) => TOption(any)
    // | DOption(Some(v)) => TOption(toType(v))
    | DResult(_) => TResult(any, any)
    // | DResult(Ok(_v)) => TResult //(toType(v), any)
    // | DResult(Error(_v)) => TResult //(any, toType(v))
    | DBytes(_) => TBytes
    }
  }

  let rec decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    let dv0 = variant0
    let dv1 = variant1
    let dv3 = variant3
    let dv2 = variant2
    let dd = decode

    let dhttp = variants(list{
      ("Redirect", dv1(x => Redirect(x), string)),
      (
        "Response",
        dv3((a, b, c) => Response(a, b, c), int64, list(tuple2(string, string)), decode),
      ),
    })

    let lambdaImpl = j => {
      {
        parameters: field("parameters", list(pair(ID.decode, string)), j),
        body: field("body", Expr.decode, j),
        symtable: field("symtable", beltStrDict(decode), j),
      }
    }

    let fnValImpl = variants(list{
      ("Lambda", dv1(x => Lambda(x), lambdaImpl)),
      ("FnName", dv1(x => FnName(x), FQFnName.decode)),
    })

    variants(
      list{
        ("DInt", dv1(x => DInt(x), int64)),
        ("DFloat", dv1(x => DFloat(x), Json_decode_extended.float')),
        ("DBool", dv1(x => DBool(x), bool)),
        ("DChar", dv1(x => DChar(x), string)),
        ("DNull", dv0(DNull)),
        ("DStr", dv1(x => DStr(x), string)),
        ("DList", dv1(x => DList(x), list(dd))),
        (
          "DTuple",
          dv3((first, second, theRest) => DTuple(first, second, theRest), dd, dd, list(dd)),
        ),
        ("DObj", dv1(x => DObj(x), beltStrDict(dd))),
        ("DIncomplete", dv1(x => DIncomplete(x), DvalSource.decode)),
        ("DError", dv2((i, msg) => DError(i, msg), DvalSource.decode, string)),
        ("DFnVal", dv1(x => DFnVal(x), fnValImpl)),
        ("DErrorRail", dv1(x => DErrorRail(x), dd)),
        ("DHttpResponse", dv1(a => DHttpResponse(a), dhttp)),
        ("DDB", dv1(x => DDB(x), string)),
        ("DDate", dv1(x => DDate(x), string)),
        ("DPassword", dv1(x => DPassword(x), string)),
        ("DUuid", dv1(x => DUuid(x), string)),
        ("DOption", dv1(x => DOption(x), optional(dd))),
        ("DResult", dv1(x => DResult(x), result(dd, dd))),
        ("DBytes", dv1(x => DBytes(x), base64EncodedBytes)),
      },
      j,
    )
  }

  let rec encode = (dv: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    let dhttp = h =>
      switch h {
      | Redirect(s) => ev("Redirect", list{string(s)})
      | Response(code, headers, dv) =>
        ev("Response", list{int64(code), list(tuple2(string, string), headers), encode(dv)})
      }

    switch dv {
    | DInt(i) => ev("DInt", list{int64(i)})
    | DFloat(f) => ev("DFloat", list{Json_encode_extended.float'(f)})
    | DBool(b) => ev("DBool", list{bool(b)})
    | DChar(b) => ev("DChar", list{string(b)})
    | DNull => ev("DNull", list{})
    | DStr(s) => ev("DStr", list{string(s)})
    | DList(l) => ev("DList", list{list(encode, l)})
    | DTuple(first, second, theRest) =>
      ev("DTuple", list{encode(first), encode(second), list(encode, theRest)})
    | DObj(o) =>
      o->Belt.Map.String.map(encode)->Belt.Map.String.toList
      |> Js.Dict.fromList
      |> jsonDict
      |> (x => list{x} |> ev("DObj"))
    | DFnVal(Lambda({body, parameters, symtable})) =>
      let dblock_args = object_(list{
        ("parameters", list(pair(ID.encode, string), parameters)),
        ("symtable", beltStrDict(encode, symtable)),
        ("body", Expr.encode(body)),
      })
      ev("DFnVal", list{ev("Lambda", list{dblock_args})})
    | DFnVal(FnName(name)) => ev("DFnVal", list{ev("FnName", list{FQFnName.encode(name)})})
    | DIncomplete(ds) => ev("DIncomplete", list{DvalSource.encode(ds)})
    // user-ish types
    | DError(ds, msg) => ev("DError", list{DvalSource.encode(ds), string(msg)})
    | DHttpResponse(http) => ev("DHttpResponse", list{dhttp(http)})
    | DDB(name) => ev("DDB", list{string(name)})
    | DDate(date) => ev("DDate", list{string(date)})
    | DPassword(hashed) => ev("DPassword", list{string(hashed)})
    | DUuid(uuid) => ev("DUuid", list{string(uuid)})
    | DOption(opt) => ev("DOption", list{nullable(encode, opt)})
    | DErrorRail(dv) => ev("DErrorRail", list{encode(dv)})
    | DResult(res) => ev("DResult", list{result(encode, encode, res)})
    | DBytes(bin) => ev("DBytes", list{base64EncodedBytes(bin)})
    }
  }
  let obj = (l: list<(string, t)>): t =>
    l |> Belt.List.toArray |> Belt.Map.String.fromArray |> (m => DObj(m))
}

module BuiltInFn = {
  module Previewable = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | Pure
      | ImpurePreviewable
      | Impure

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      variants(
        list{
          ("Pure", variant0(Pure)),
          ("ImpurePreviewable", variant0(ImpurePreviewable)),
          ("Impure", variant0(Impure)),
        },
        j,
      )
    }
    let encode = (s: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch s {
      | Pure => ev("Pure", list{})
      | Impure => ev("Impure", list{})
      | ImpurePreviewable => ev("ImpurePreviewable", list{})
      }
    }
  }

  module Deprecation = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | NotDeprecated
      | RenamedTo(FQFnName.StdlibFnName.t)
      | ReplacedBy(FQFnName.StdlibFnName.t)
      | DeprecatedBecause(string)

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      variants(
        list{
          ("NotDeprecated", variant0(NotDeprecated)),
          ("RenamedTo", variant1(name => RenamedTo(name), FQFnName.StdlibFnName.decode)),
          ("ReplacedBy", variant1(name => ReplacedBy(name), FQFnName.StdlibFnName.decode)),
          ("DeprecatedBecause", variant1(reason => DeprecatedBecause(reason), string)),
        },
        j,
      )
    }

    let encode = (s: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch s {
      | NotDeprecated => ev("NotDeprecated", list{})
      | RenamedTo(name) => ev("RenamedTo", list{FQFnName.StdlibFnName.encode(name)})
      | ReplacedBy(name) => ev("ReplacedBy", list{FQFnName.StdlibFnName.encode(name)})
      | DeprecatedBecause(str) => ev("DeprecatedBecause", list{string(str)})
      }
    }
  }

  module SqlSpec = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | Unknown
      | NotQueryable
      | QueryFunction
      | SqlUnaryOp(string)
      | SqlBinOp(string)
      | SqlFunction(string)
      | SqlFunctionWithPrefixArgs(string, list<string>)
      | SqlFunctionWithSuffixArgs(string, list<string>)
      | SqlCallback2

    let isQueryable = (s: t): bool =>
      switch s {
      | Unknown
      | NotQueryable
      | QueryFunction => false
      | SqlUnaryOp(_)
      | SqlBinOp(_)
      | SqlFunction(_)
      | SqlFunctionWithPrefixArgs(_)
      | SqlFunctionWithSuffixArgs(_)
      | SqlCallback2 => true
      }
    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      variants(
        list{
          ("Unknown", variant0(Unknown)),
          ("NotQueryable", variant0(NotQueryable)),
          ("QueryFunction", variant0(QueryFunction)),
          ("SqlUnaryOp", variant1(name => SqlUnaryOp(name), string)),
          ("SqlBinOp", variant1(name => SqlBinOp(name), string)),
          ("SqlFunction", variant1(name => SqlFunction(name), string)),
          (
            "SqlFunctionWithPrefixArgs",
            variant2((name, args) => SqlFunctionWithPrefixArgs(name, args), string, list(string)),
          ),
          (
            "SqlFunctionWithSuffixArgs",
            variant2((name, args) => SqlFunctionWithSuffixArgs(name, args), string, list(string)),
          ),
          ("SqlCallback2", variant0(SqlCallback2)),
        },
        j,
      )
    }

    let encode = (s: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch s {
      | Unknown => ev("Unknown", list{})
      | NotQueryable => ev("NotQueryable", list{})
      | QueryFunction => ev("QueryFunction", list{})
      | SqlUnaryOp(str) => ev("SqlUnaryOp", list{string(str)})
      | SqlBinOp(str) => ev("SqlBinOp", list{string(str)})
      | SqlFunction(str) => ev("SqlFunction", list{string(str)})
      | SqlFunctionWithPrefixArgs(name, args) =>
        ev("SqlFunctionWithPrefixArgs", list{string(name), list(string, args)})
      | SqlFunctionWithSuffixArgs(name, args) =>
        ev("SqlFunctionWithSuffixArgs", list{string(name), list(string, args)})
      | SqlCallback2 => ev("SqlCallback2", list{})
      }
    }
  }

  module Param = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: string,
      typ: DType.t,
      args: list<string>,
      description: string,
    }
    let decode = (j): t => {
      open Json.Decode
      {
        name: field("name", string, j),
        typ: field("type", DType.decode, j), // Note: "type", not "typ"
        args: field("args", list(string), j),
        description: field("description", string, j),
      }
    }

    let encode = (p: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("name", string(p.name)),
        ("type", DType.encode(p.typ)),
        ("args", list(string, p.args)),
        ("description", string(p.description)),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    name: FQFnName.StdlibFnName.t,
    parameters: list<Param.t>,
    returnType: DType.t,
    description: string,
    previewable: Previewable.t,
    deprecated: Deprecation.t,
    sqlSpec: SqlSpec.t,
    isInfix: bool,
  }

  let decode = (j): t => {
    open Json.Decode
    {
      name: field("name", FQFnName.StdlibFnName.decode, j),
      parameters: field("parameters", list(Param.decode), j),
      description: field("description", string, j),
      returnType: field("returnType", DType.decode, j),
      previewable: field("previewable", Previewable.decode, j),
      deprecated: field("deprecated", Deprecation.decode, j),
      isInfix: field("isInfix", bool, j),
      sqlSpec: field("sqlSpec", SqlSpec.decode, j),
    }
  }

  let encode = (f: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("name", FQFnName.StdlibFnName.encode(f.name)),
      ("parameters", list(Param.encode, f.parameters)),
      ("returnType", DType.encode(f.returnType)),
      ("description", string(f.description)),
      ("previewable", Previewable.encode(f.previewable)),
      ("deprecated", Deprecation.encode(f.deprecated)),
      ("isInfix", bool(f.isInfix)),
      ("sqlSpec", SqlSpec.encode(f.sqlSpec)),
    })
  }
}
