open Prelude

module HandlerProperty = AppTypes.HandlerProperty

let isCompatible = (t1: DType.t, t2: DType.t): bool =>
  switch (t1, t2) {
  | (TVariable(_), _) => true
  | (_, TVariable(_)) => true
  | _ => t1 == t2 // TODO recurse
  }

let isErrorRailType = (typ: DType.t): bool =>
  switch typ {
  | TOption(_)
  | TResult(_) => true
  | _ => false
  }

// Drop initial/final '"'
let stripQuotes = (s: string): string => {
  let s = if String.starts_with(~prefix="\"", s) {
    s |> String.dropLeft(~count=1)
  } else {
    s
  }

  let s = if String.ends_with(~suffix="\"", s) {
    s |> String.dropRight(~count=1)
  } else {
    s
  }

  s
}

// Copied from Dval.to_repr in backend code, but that's terrible and it should
// be recopied from to_developer_repr_v0
let rec toRepr_ = (oldIndent: int, dv: RT.Dval.t): string => {
  let wrap = value => "<" ++ ((dv |> RT.Dval.toType |> DType.tipe2str) ++ (": " ++ (value ++ ">")))
  let asType = "<" ++ ((dv |> RT.Dval.toType |> DType.tipe2str) ++ ">")
  let nl = "\n" ++ String.repeat(~count=oldIndent, " ")
  let inl = "\n" ++ String.repeat(~count=oldIndent + 2, " ")
  let indent = oldIndent + 2
  let objToString = l =>
    l
    |> List.map(~f=((k, v)) => k ++ (": " ++ toRepr_(indent, v)))
    |> String.join(~sep="," ++ inl)
    |> (s => "{" ++ (inl ++ (s ++ (nl ++ "}"))))

  switch dv {
  | DInt(i) => Int64.to_string(i)
  | DFloat(f) => Js.Float.toString(f)
  | DStr(s) => "\"" ++ (s ++ "\"")
  | DBool(true) => "true"
  | DBool(false) => "false"
  | DChar(c) => "'" ++ (c ++ "'")
  | DNull => "null"
  | DDate(s) => wrap(s)
  | DDB(s) => wrap(s)
  | DUuid(s) => wrap(s)
  | DError(_, s) =>
    open Json.Decode
    let decoder = (j): exception_ => {
      short: field("short", string, j),
      long: field("long", optional(string), j),
      exceptionTipe: field("tipe", string, j),
      actual: field("actual", optional(string), j),
      actualType: field("actual_tipe", optional(string), j),
      expected: field("expected", optional(string), j),
      result: field("result", optional(string), j),
      resultType: field("result_tipe", optional(string), j),
      info: field("info", strDict(string), j),
      workarounds: field("workarounds", list(string), j),
    }

    let maybe = (name, m) =>
      switch m {
      | Some("") | None => ""
      | Some(s) => "\n  " ++ (name ++ (": " ++ s))
      }

    try s
    |> decodeString(decoder)
    |> Result.toOption
    |> Option.map(~f=e =>
      "An error occurred: \n  " ++
      (e.short ++
      (maybe("message", e.long) ++
      (maybe("actual value", e.actual) ++
      (maybe("actual type", e.actualType) ++
      (maybe("expected", e.expected) ++
      (maybe("result", e.result) ++
      (maybe("result type", e.resultType) ++
      (if e.info == Map.String.empty {
        ""
      } else {
        ", info: " ++ Map.toString(e.info)
      } ++
      (if e.workarounds == list{} {
        ""
      } else {
        ", workarounds: [" ++ (String.join(~sep="", e.workarounds) ++ "]")
      } ++
      if e.exceptionTipe == "code" {
        ""
      } else {
        "\n  error type: " ++ e.exceptionTipe
      })))))))))
    )
    |> Option.unwrap(~default=wrap(s)) catch {
    | _ => wrap(s)
    }
  | DPassword(s) => wrap(s)
  | DFnVal(Lambda({parameters: _, body: _, _})) => // TODO: show relevant symtable entries
    "TODO"
  | DFnVal(FnName(_)) => "TODO"
  // FluidPrinter.eToHumanString(ELambda(gid(), parameters, body))
  | DIncomplete(_) => asType
  | DHttpResponse(Redirect(url)) => "302 " ++ url
  | DHttpResponse(Response(code, hs, dv_)) =>
    let headers = objToString(List.map(~f=Tuple2.mapSecond(~f=s => RT.Dval.DStr(s)), hs))
    Int64.to_string(code) ++ " " ++ headers ++ nl ++ toRepr(dv_)
  | DOption(None) => "Nothing"
  | DOption(Some(dv_)) => "Just " ++ toRepr(dv_)
  | DResult(Ok(dv_)) => "Ok " ++ toRepr(dv_)
  | DResult(Error(dv_)) => "Error " ++ toRepr(dv_)
  | DErrorRail(dv_) => wrap(toRepr(dv_))
  // TODO: newlines and indentation
  | DList(l) =>
    switch l {
    | list{} => "[]"
    | list{DObj(_), ..._} =>
      "[" ++
      (inl ++
      (String.join(~sep=inl ++ ", ", List.map(~f=toRepr_(indent), l)) ++ (nl ++ "]")))
    | l => "[ " ++ (String.join(~sep=", ", List.map(~f=toRepr_(indent), l)) ++ " ]")
    }
  | DTuple(first, second, theRest) =>
    let exprs = list{first, second, ...theRest}
    "(" ++ (String.join(~sep=", ", List.map(~f=toRepr_(indent), exprs)) ++ ")")
  | DObj(o) => objToString(Belt.Map.String.toList(o))
  | DBytes(s) => "<Bytes: length=" ++ (Bytes.length(s) |> string_of_int) ++ ">"
  }
}

and toRepr = (dv: RT.Dval.t): string => toRepr_(0, dv)

// TODO: copied from Libexecution/http.ml
let route_variables = (route: string): list<string> => {
  let split_uri_path = (path: string): list<string> => {
    let subs = String.split(~on="/", path)
    List.filter(~f=x => String.length(x) > 0, subs)
  }

  route
  |> split_uri_path
  |> List.filter(~f=String.startsWith(~prefix=":"))
  |> List.map(~f=String.dropLeft(~count=/* ":" */ 1))
}

let inputVariables = (tl: toplevel): list<string> =>
  switch tl {
  | TLHandler(h) =>
    switch h.spec {
    | HTTP(name, _, _)
    | HTTPBasic(name, _, _) =>
      let fromRoute = name |> route_variables
      list{"request", ...fromRoute}
    | Cron(_)
    | REPL(_) => list{}
    | OldWorker(_)
    | Worker(_) => list{"event"}
    | UnknownHandler(_) => list{"request", "event"}
    }
  | TLFunc(f) => f.parameters |> List.filterMap(~f=(p: PT.UserFunction.Parameter.t) => Some(p.name))
  | TLTipe(_) | TLDB(_) | TLPmFunc(_) => list{}
  }

let sampleInputValue = (tl: toplevel): AnalysisTypes.InputValueDict.t =>
  tl
  |> inputVariables
  |> List.toArray
  |> Array.map(~f=v => (v, RT.Dval.DIncomplete(SourceNone)))
  |> Belt.Map.String.fromArray

let inputValueAsString = (tl: toplevel, iv: AnalysisTypes.InputValueDict.t): string => {
  // Merge sample + trace, preferring trace.
  // This ensures newly added parameters show as incomplete.

  let dval = Belt.Map.String.merge(sampleInputValue(tl), iv, (_key, sampleVal, traceVal) =>
    switch (sampleVal, traceVal) {
    | (None, None) => None
    | (Some(v), None) => Some(v)
    | (None, Some(v)) => Some(v)
    | (Some(_sample), Some(trace)) => Some(trace)
    }
  ) |> (dict => RT.Dval.DObj(dict))

  dval
  |> toRepr
  |> String.split(~on="\n")
  |> List.drop(~count=1)
  |> List.initial
  |> Option.unwrap(~default=list{})
  |> List.map(~f=String.dropLeft(~count=2))
  |> String.join(~sep="\n")
}

let pathFromInputVars = (iv: AnalysisTypes.InputValueDict.t): option<string> =>
  Belt.Map.String.get(iv, "request")
  |> Option.andThen(~f=obj =>
    switch obj {
    | RT.Dval.DObj(r) => Belt.Map.String.get(r, "url")
    | _ => None
    }
  )
  |> Option.andThen(~f=dv =>
    switch dv {
    | RT.Dval.DStr(s) => Some(s)
    | _ => None
    }
  )
  |> Option.andThen(~f=url =>
    try {Some(Webapi.Url.make(url))} catch {
    | _ => None
    }
  )
  |> Option.map(~f=url => Webapi.Url.pathname(url) ++ Webapi.Url.search(url))

let setHandlerExeState = (
  tlid: TLID.t,
  state: HandlerProperty.ExecutionState.t,
  hp: TLID.Dict.t<HandlerProperty.t>,
): TLID.Dict.t<HandlerProperty.t> =>
  hp |> Map.update(~key=tlid, ~f=old => {
    let p = old |> Option.unwrap(~default=HandlerProperty.default)
    Some({...p, execution: state})
  })
