open Prelude

// Dark
module B = BlankOr
module RT = RuntimeTypes
module TL = Toplevel

let rec to_url_string = (dv: RT.Dval.t): option<string> =>
  switch dv {
  // things that can't be parsed out of a HTTP request
  | DFnVal(_)
  | DIncomplete(_)
  | DPassword(_)
  | DObj(_)
  | DOption(None)
  | DTuple(_)
  | DHttpResponse(_)
  | DResult(Error(_)) =>
    None

  // some of these things also can't be parsed out of a HTTP request,
  // but someone has added code for those cases anyway
  // CLEANUP we can probably remove types that cannot be created as part of
  // a HTTP request
  | DInt(i) => Some(Int64.to_string(i))
  | DBool(true) => Some("true")
  | DBool(false) => Some("false")
  | DStr(s) => Some(s)
  | DFloat(f) => Some(Tc.Float.toString(f))
  | DChar(c) => Some(c)
  | DNull => Some("null")
  | DDate(d) => Some(d)
  | DDB(dbname) => Some(dbname)
  | DErrorRail(d) => to_url_string(d)
  | DError(_, msg) => Some("error=" ++ msg)
  | DUuid(uuid) => Some(uuid)
  | DList(l) => Some("[ " ++ String.join(~sep=", ", List.filterMap(~f=to_url_string, l)) ++ " ]")
  | DOption(Some(v)) => to_url_string(v)
  | DResult(Ok(v)) => to_url_string(v)
  | DBytes(bytes) => Some(bytes |> Json_encode_extended.base64ToString)
  }

let strAsBodyCurl = (dv: RT.Dval.t): option<string> =>
  switch dv {
  | DStr(s) =>
    let body = s |> Regex.replace(~re=Regex.regex("\n"), ~repl="")
    Some("-d '" ++ (body ++ "'"))
  | _ => None
  }

let objAsHeaderCurl = (dv: RT.Dval.t): option<string> =>
  switch dv {
  | DObj(o) =>
    Belt.Map.String.toList(o)
    // curl will add content-length automatically, and having it specified
    // explicitly causes weird errors if the user, say, changes the body of
    // the request without changing the value of this header
    |> List.filter(~f=((k, _)) => k !== "content-length")
    |> List.map(~f=((k, v)) =>
      "-H '" ++ (k ++ (":" ++ ((Runtime.toRepr(v) |> Runtime.stripQuotes) ++ "'")))
    )
    |> String.join(~sep=" ")
    |> (s => Some(s))
  | _ => None
  }

let curlFromSpec = (m: AppTypes.model, tlid: TLID.t): option<string> =>
  TL.get(m, tlid)
  |> Option.andThen(~f=TL.asHandler)
  |> Option.andThen(~f=(h: PT.Handler.t) => {
    let s = h.spec
    switch (s.space, s.name, s.modifier) {
    | (F(_, "HTTP"), F(_, path), F(_, meth)) =>
      let proto = if m.environment == "production" {
        "https"
      } else {
        "http"
      }

      let route = proto ++ ("://" ++ (m.canvasName ++ ("." ++ (m.userContentHost ++ path))))

      switch meth {
      | "GET" => Some("curl " ++ route)
      | _ => Some("curl -X " ++ (meth ++ (" -H 'Content-Type: application/json' " ++ route)))
      }
    | _ => None
    }
  })

/* Constructs curl command from analysis dict.
  headers (which includes cookies),
  fullBody (for both formBody and jsonBody),
  url (which includes queryParams)
*/
let curlFromCurrentTrace = (m: AppTypes.model, tlid: TLID.t): option<string> => {
  let wrapInList = o => o |> Option.andThen(~f=v => Some(list{v})) |> Option.unwrap(~default=list{})

  let trace = Analysis.getSelectedTraceID(m, tlid) |> Option.andThen(~f=Analysis.getTrace(m, tlid))

  switch trace {
  | Some(_, Ok(td)) =>
    Belt.Map.String.get(td.input, "request")
    |> Option.andThen(~f=obj =>
      switch obj {
      | RT.Dval.DObj(r) => Some(r)
      | _ => None
      }
    )
    |> Option.andThen(~f=r => {
      let get = name => Belt.Map.String.get(r, name)
      switch get("url") {
      | Some(RT.Dval.DStr(url)) =>
        let headers = get("headers") |> Option.andThen(~f=objAsHeaderCurl) |> wrapInList

        let body = get("fullBody") |> Option.andThen(~f=strAsBodyCurl) |> wrapInList

        let meth =
          TL.get(m, tlid)
          |> Option.andThen(~f=TL.asHandler)
          |> Option.andThen(~f=(h: PT.Handler.t) => B.toOption(h.spec.modifier))
          |> Option.andThen(~f=s => Some("-X " ++ s))
          |> wrapInList

        Belt.List.concatMany([list{"curl", ...headers}, body, meth, list{"'" ++ (url ++ "'")}])
        |> String.join(~sep=" ")
        |> Option.some
      | _ => None
      }
    })
  | _ => None
  }
}

let curlFromHttpClientCall = (m: AppTypes.model, tlid: TLID.t, id: id, name: PT.FQFnName.t) => {
  let traces =
    Map.get(~key=tlid, m.traces) |> recoverOption(
      ~debug=TLID.toString(tlid),
      "TLID not found in m.traces in curlFromHttpClientCall",
    )

  let traceId =
    traces
    |> Option.andThen(~f=traces => Analysis.selectedTraceID(m.tlTraceIDs, traces, tlid))
    |> (
      // We don't recover here b/c it's very possible we don't have an analysis yet
      tid => {
        switch tid {
        | Some(_) => ()
        | None => Js.log("No selectedTrace present for tlid")
        }
        tid
      }
    )

  let tl =
    TL.get(m, tlid) |> recoverOption(
      ~debug=TLID.toString(tlid),
      "TLID not found in model in curlFromHttpClientCall",
    )

  let args = Option.andThen2(tl, traceId, ~f=(tl, traceId) =>
    Analysis.getArguments(m, tl, id, traceId)
  )
  // TODO this is what fails if we haven't clicked the ast yet; we should fix
  // that, or make it toast instructions?
  |> recoverOption(
    "Args not found in model in curlFromHttpClientCall",
    ~debug=(TLID.toString(tlid), Option.map(~f=show_traceID, traceId)),
  )

  let data = args |> Option.map(~f=args => {
    let (url, body, query, headers) = switch args {
    | list{url, body, query, headers} => (url, Some(body), query, headers)
    | list{url, query, headers} => (url, None, query, headers)
    | _ =>
      recover(
        ~debug="arg count" ++ string_of_int(List.length(args)),
        "args in curlFromHttpClientCall espected 3 or 4, failed",
        (RT.Dval.DNull, None, RT.Dval.DNull, RT.Dval.DNull),
      )
    }

    let headers = objAsHeaderCurl(headers) |> Option.unwrap(~default="")
    let body =
      strAsBodyCurl(body |> Option.unwrap(~default=RT.Dval.DNull)) |> Option.unwrap(~default="")

    let meth = switch name {
    | Stdlib({module_: "HttpClient", function, version: _}) => "-X " ++ function
    | Stdlib(_)
    | User(_)
    | Package(_) =>
      recoverOpt(~debug=name, ~default="", "Expected a HttpClient fn", None)
    }

    let base_url = switch url {
    | DStr(s) => s
    | _ => recover(~debug=RT.Dval.show(url), "Expected url arg to be a DStr", url) |> RT.Dval.show
    }

    let qps = switch query {
    | DObj(map) =>
      map
      |> Belt.Map.String.toList
      |> List.filterMap(~f=((k, v)) => to_url_string(v) |> Option.map(~f=v => k ++ ("=" ++ v)))
      |> String.join(~sep="&")
    | _ =>
      ignore(query |> recover(~debug=RT.Dval.show(query), "Expected query arg to be a dobj"))
      ""
    } |> (
      s =>
        if s === "" {
          ""
        } else {
          "?" ++ s
        }
    )

    let url = "'" ++ (base_url ++ (qps ++ "'"))
    list{"curl", headers, body, meth, url} |> List.filter(~f=s => s !== "") |> String.join(~sep=" ")
  })

  data
}

let makeCommand = (m: AppTypes.model, tlid: TLID.t): option<string> =>
  curlFromCurrentTrace(m, tlid) |> Option.orElse(curlFromSpec(m, tlid))

let copyCurlMod = (m: AppTypes.model, tlid: TLID.t, pos: AppTypes.VPos.t): AppTypes.modification =>
  switch makeCommand(m, tlid) {
  | Some(data) =>
    Native.Clipboard.copyToClipboard(data)
    ReplaceAllModificationsWithThisOne(
      m => {
        let m = TLMenu.update(m, tlid, CloseMenu)
        ({...m, toast: {toastMessage: Some("Copied!"), toastPos: Some(pos)}}, Tea.Cmd.none)
      },
    )
  | None =>
    ReplaceAllModificationsWithThisOne(m => (TLMenu.update(m, tlid, CloseMenu), Tea.Cmd.none))
  }
