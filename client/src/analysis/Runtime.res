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

// This should be kept sync with backend DvalReprDeveloper.toRepr
let toRepr = (dval: RT.Dval.t): string => {
  let rec toRepr_ = (indent: int, dv: RT.Dval.t): string => {
    let makeSpaces = len => String.repeat(~count=len, " ")
    let nl = "\n" ++ makeSpaces(indent)
    let inl = "\n" ++ makeSpaces(indent + 2)
    let indent = indent + 2
    let typename = dv->RT.Dval.toType->DType.tipe2str
    let wrap = str => `<${typename}: ${str}>`
    let justType = `<${typename}>`

    switch dv {
    | DPassword(_) => "<password>"
    | DStr(s) => `"${s}"`
    | DChar(c) => `'${c}'`
    | DInt(i) => Int64.to_string(i)
    | DBool(true) => "true"
    | DBool(false) => "false"
    | DFloat(f) => Js.Float.toString(f)
    | DNull => "null"
    | DFnVal(Lambda({parameters, body, _})) =>
      // TODO: show relevant symtable entries
      RuntimeTokenizer.eToHumanString(parameters, body)
    | DFnVal(FnName(_)) => "TODO"
    | DIncomplete(_) => justType
    | DError(_, msg) => `<error: ${msg}>`
    | DDate(s) => wrap(s)
    | DDB(s) => wrap(s)
    | DUuid(s) => wrap(s)
    | DHttpResponse(Redirect(url)) => "302 " ++ url
    | DHttpResponse(Response(code, headers, hdv)) =>
      let headerString =
        headers
        |> List.map(~f=((k, v)) => k ++ ": " ++ v)
        |> String.join(~sep=", ")
        |> (s => "{ " ++ s ++ " }")
      Int64.to_string(code) ++ " " ++ headerString ++ nl ++ toRepr_(indent, hdv)
    | DList(l) =>
      if l == list{} {
        "[]"
      } else {
        let elems = Js.Array.joinWith(", ", l->List.map(~f=toRepr_(indent))->List.toArray)
        `[${inl}${elems}${nl}]`
      }
    | DTuple(first, second, theRest) =>
      let exprs = list{first, second, ...theRest}
      "(" ++ (String.join(~sep=", ", List.map(~f=toRepr_(indent), exprs)) ++ ")")
    | DObj(o) =>
      if Belt.Map.String.isEmpty(o) {
        "{}"
      } else {
        let strs =
          o
          |> Belt.Map.String.toList
          |> List.map(~f=((key, value)) => key ++ ": " ++ toRepr_(indent, value))

        let elems = String.join(~sep=`, ${inl}`, strs)
        `{${inl}${elems}${nl}}`
      }

    | DOption(None) => "Nothing"
    | DOption(Some(dv_)) => "Just " ++ toRepr_(indent, dv_)
    | DResult(Ok(dv_)) => "Ok " ++ toRepr_(indent, dv_)
    | DResult(Error(dv_)) => "Error " ++ toRepr_(indent, dv_)
    | DErrorRail(dv_) => wrap(toRepr_(indent, dv_))
    | DBytes(s) => "<Bytes: length=" ++ (Bytes.length(s) |> string_of_int) ++ ">"
    }
  }
  toRepr_(0, dval)
}

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
