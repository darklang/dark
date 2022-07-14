open Prelude

let isCompatible = (t1: DType.t, t2: DType.t): bool => t1 == TAny || (t2 == TAny || t1 == t2)

let errorRailTypes: list<DType.t> = list{TOption, TResult}

let tipe2str = Prelude.tipe2str

let str2tipe = (t: string): DType.t => {
  let parseListTipe = lt =>
    switch lt {
    | "str" => DType.TStr
    | "string" => TStr
    | "int" => TInt
    | "integer" => TInt
    | "float" => TFloat
    | "bool" => TBool
    | "boolean" => TBool
    | "password" => TPassword
    | "uuid" => TUuid
    | "option" => TOption
    | "null" => TNull
    | "any" => TAny
    | "list" => TList
    | "tuple" => TTuple(TAny, TAny, list{})
    | "obj" => TObj
    | "block" => TBlock
    | "incomplete" => TIncomplete
    | "response" => TResp
    | "datastore" => TDB
    | "date" => TDate
    | "error" => TError
    | "nothing" => TNull
    | "dict" => TObj
    | other => recover(~debug=other, "invalid type in str2tipe", DType.TAny)
    }

  switch String.toLowercase(t) {
  | "any" => TAny
  | "int" => TInt
  | "float" => TFloat
  | "bool" => TBool
  | "boolean" => TBool
  | "null" => TNull
  | "character" | "char" => TCharacter
  | "str" => TStr
  | "string" => TStr
  | "list" => TList
  | "tuple" => TTuple(TAny, TAny, list{})
  | "obj" => TObj
  | "block" => TBlock
  | "incomplete" => TIncomplete
  | "response" => TResp
  | "datastore" => TDB
  | "date" => TDate
  | "error" => TError
  | "option" => TOption
  | "result" => TResult
  | "bytes" => TBytes
  | "password" => TPassword
  | "uuid" => TUuid
  | "nothing" => TNull
  | "dict" => TObj
  | other =>
    if String.startsWith(~prefix="[", other) && String.endsWith(~suffix="]", other) {
      other |> String.dropLeft(~count=1) |> String.dropRight(~count=1) |> parseListTipe
    } else {
      recover("invalid list type in str2tipe", ~debug=other, DType.TAny)
    }
  }
}

let rec typeOf = (dv: dval): DType.t =>
  switch dv {
  | DInt(_) => TInt
  | DFloat(_) => TFloat
  | DBool(_) => TBool
  | DNull => TNull
  | DCharacter(_) => TCharacter
  | DStr(_) => TStr
  | DList(_) => TList
  | DTuple(first, second, theRest) =>
    TTuple(typeOf(first), typeOf(second), List.map(~f=t => typeOf(t), theRest))
  | DObj(_) => TObj
  | DBlock(_) => TBlock
  | DIncomplete(_) => TIncomplete
  | DError(_) => TError
  | DResp(_, _) => TResp
  | DDB(_) => TDB
  | DDate(_) => TDate
  | DOption(_) => TOption
  | DErrorRail(_) => TErrorRail
  | DPassword(_) => TPassword
  | DUuid(_) => TUuid
  | DResult(_) => TResult
  | DBytes(_) => TBytes
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

let isComplete = (dv: dval): bool =>
  switch dv {
  | DError(_) | DIncomplete(_) => false
  | _ => true
  }

// Copied from Dval.to_repr in backend code, but that's terrible and it should
// be recopied from to_developer_repr_v0
let rec toRepr_ = (oldIndent: int, dv: dval): string => {
  let wrap = value => "<" ++ ((dv |> typeOf |> tipe2str) ++ (": " ++ (value ++ ">")))
  let asType = "<" ++ ((dv |> typeOf |> tipe2str) ++ ">")
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
  | DCharacter(c) => "'" ++ (c ++ "'")
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
  | DBlock({params, body, _}) =>
    // TODO: show relevant symtable entries
    FluidPrinter.eToHumanString(ELambda(gid(), params, body))
  | DIncomplete(_) => asType
  | DResp(Redirect(url), dv_) => "302 " ++ (url ++ (nl ++ toRepr_(indent, dv_)))
  | DResp(Response(code, hs), dv_) =>
    let headers = objToString(List.map(~f=Tuple2.mapSecond(~f=s => DStr(s)), hs))

    string_of_int(code) ++ (" " ++ (headers ++ (nl ++ toRepr(dv_))))
  | DOption(OptNothing) => "Nothing"
  | DOption(OptJust(dv_)) => "Just " ++ toRepr(dv_)
  | DResult(ResOk(dv_)) => "Ok " ++ toRepr(dv_)
  | DResult(ResError(dv_)) => "Error " ++ toRepr(dv_)
  | DErrorRail(dv_) => wrap(toRepr(dv_))
  // TODO: newlines and indentation
  | DList(l) =>
    switch l |> Array.to_list {
    | list{} => "[]"
    | list{DObj(_), ..._} =>
      "[" ++
      (inl ++
      (String.join(~sep=inl ++ ", ", List.map(~f=toRepr_(indent), l |> Array.to_list)) ++
      (nl ++
      "]")))
    | l => "[ " ++ (String.join(~sep=", ", List.map(~f=toRepr_(indent), l)) ++ " ]")
    }
  | DTuple(first, second, theRest) =>
    let exprs = list{first, second, ...theRest}
    "(" ++ (String.join(~sep=", ", List.map(~f=toRepr_(indent), exprs)) ++ ")")
  | DObj(o) => objToString(Belt.Map.String.toList(o))
  | DBytes(s) =>
    "<" ++
    ((dv |> typeOf |> tipe2str) ++
    (": length=" ++ ((Bytes.length(s) |> string_of_int) ++ ">")))
  }
}

and toRepr = (dv: dval): string => toRepr_(0, dv)

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
    switch h.spec.space {
    | F(_, m) if String.toLowercase(m) == "http" =>
      let fromRoute =
        h.spec.name
        |> BlankOr.toOption
        |> Option.map(~f=route_variables)
        |> Option.unwrap(~default=list{})
      list{"request", ...fromRoute}
    | F(_, m) if String.toLowercase(m) == "cron" => list{}
    | F(_, m) if String.toLowercase(m) == "repl" => list{}
    | F(_, m) if String.toLowercase(m) == "worker" => list{"event"}
    | F(_, _) => // workers, including old names
      list{"event"}
    | Blank(_) => // we used to be allowed unknown
      list{"request", "event"}
    }
  | TLFunc(f) => f.ufMetadata.ufmParameters |> List.filterMap(~f=p => BlankOr.toOption(p.ufpName))
  | TLTipe(_) | TLDB(_) | TLPmFunc(_) => list{}
  }

let sampleInputValue = (tl: toplevel): inputValueDict =>
  tl
  |> inputVariables
  |> List.toArray
  |> Array.map(~f=v => (v, DIncomplete(SourceNone)))
  |> Belt.Map.String.fromArray

let inputValueAsString = (tl: toplevel, iv: inputValueDict): string => {
  let dval = /* Merge sample + trace, preferring trace.
   *
   * This ensures newly added parameters show as incomplete.
   * */
  Belt.Map.String.merge(sampleInputValue(tl), iv, (_key, sampleVal, traceVal) =>
    switch (sampleVal, traceVal) {
    | (None, None) => None
    | (Some(v), None) => Some(v)
    | (None, Some(v)) => Some(v)
    | (Some(_sample), Some(trace)) => Some(trace)
    }
  ) |> (dict => DObj(dict))

  dval
  |> toRepr
  |> String.split(~on="\n")
  |> List.drop(~count=1)
  |> List.initial
  |> Option.unwrap(~default=list{})
  |> List.map(~f=String.dropLeft(~count=2))
  |> String.join(~sep="\n")
}

let pathFromInputVars = (iv: inputValueDict): option<string> =>
  Belt.Map.String.get(iv, "request")
  |> Option.andThen(~f=obj =>
    switch obj {
    | DObj(r) => Belt.Map.String.get(r, "url")
    | _ => None
    }
  )
  |> Option.andThen(~f=dv =>
    switch dv {
    | DStr(s) => Some(s)
    | _ => None
    }
  )
  |> Option.andThen(~f=Native.Url.make)
  |> Option.map(~f=url => url["pathname"] ++ url["search"])

let setHandlerExeState = (tlid: TLID.t, state: exeState, hp: TLID.Dict.t<handlerProp>): TLID.Dict.t<
  handlerProp,
> =>
  hp |> Map.update(~key=tlid, ~f=old => {
    let p = old |> Option.unwrap(~default=Defaults.defaultHandlerProp)
    Some({...p, execution: state})
  })
