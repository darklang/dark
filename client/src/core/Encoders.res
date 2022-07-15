open Prelude
open Json.Encode

// Dark

// XXX(JULIAN): All of this should be cleaned up and moved somewhere nice!
@deriving(abstract) type jsArrayBuffer = {byteLength: int}

@deriving(abstract) type jsUint8Array

@new external createUint8Array: int => jsUint8Array = "Uint8Array"

@set_index external setUint8ArrayIdx: (jsUint8Array, int, int) => unit = ""

let dark_arrayBuffer_to_b64url = %raw(`
  function (arraybuffer) {
    // TODO(JULIAN): Actually import https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js as a lib and use encode here
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    // Use a lookup table to find the index.
    var lookup = new Uint8Array(256);
    for (var i = 0; i < chars.length; i++) {
      lookup[chars.charCodeAt(i)] = i;
    }

      var bytes = new Uint8Array(arraybuffer),
      i, len = bytes.length, base64 = "";

      for (i = 0; i < len; i+=3) {
        base64 += chars[bytes[i] >> 2];
        base64 += chars[((bytes[i] & 3) << 4) | (bytes[i + 1] >> 4)];
        base64 += chars[((bytes[i + 1] & 15) << 2) | (bytes[i + 2] >> 6)];
        base64 += chars[bytes[i + 2] & 63];
      }

      if ((len % 3) === 2) {
        base64 = base64.substring(0, base64.length - 1) + "=";
      } else if (len % 3 === 1) {
        base64 = base64.substring(0, base64.length - 2) + "==";
      }

      return base64;
  }
`)

let _bytes_to_uint8Array = (input: Bytes.t): jsUint8Array => {
  let len = Bytes.length(input)
  let buf = createUint8Array(len)
  for i in 0 to len - 1 {
    setUint8ArrayIdx(buf, i, int_of_char(Bytes.get(input, i)))
  }
  buf
}

let base64url_bytes = (input: Bytes.t): string =>
  input |> _bytes_to_uint8Array |> dark_arrayBuffer_to_b64url

let id = ID.encode
let tlid = TLID.encode

let pos = (p: Types.pos) => object_(list{("x", int(p.x)), ("y", int(p.y))})

let vPos = (vp: vPos) => object_(list{("vx", int(vp.vx)), ("vy", int(vp.vy))})

let blankOr = BaseTypes.encodeBlankOr

let dval_source = (s: Types.dval_source): Js.Json.t => {
  let ev = variant
  switch s {
  | SourceNone => ev("SourceNone", list{})
  | SourceId(t, i) => ev("SourceId", list{tlid(t), id(i)})
  }
}

let rec ocamlDval = (dv: Types.dval): Js.Json.t => {
  let ev = variant
  let dhttp = h =>
    switch h {
    | Redirect(s) => ev("Redirect", list{string(s)})
    | Response(code, headers) =>
      ev("Response", list{int(code), list(tuple2(string, string), headers)})
    }

  switch dv {
  | DInt(i) => ev("DInt", list{int64(i)})
  | DFloat(f) => ev("DFloat", list{Json.Encode.float(f)})
  | DBool(b) => ev("DBool", list{bool(b)})
  | DNull => ev("DNull", list{})
  | DStr(s) => ev("DStr", list{string(s)})
  | DList(l) => ev("DList", list{array(ocamlDval, l)})
  | DTuple(first, second, theRest) =>
    ev("DTuple", list{ocamlDval(first), ocamlDval(second), array(ocamlDval, List.toArray(theRest))})
  | DObj(o) =>
    o->Belt.Map.String.map(ocamlDval)->Belt.Map.String.toList
    |> Js.Dict.fromList
    |> jsonDict
    |> (x => list{x} |> ev("DObj"))
  | DBlock({body, params, symtable}) =>
    let dblock_args = object_(list{
      ("symtable", beltStrDict(ocamlDval, symtable)),
      ("params", list(pair(id, string), params)),
      ("body", fluidExpr(body)),
    })

    ev("DBlock", list{dblock_args})
  | DIncomplete(ds) => ev("DIncomplete", list{dval_source(ds)})
  // user-ish types
  | DCharacter(c) => ev("DCharacter", list{string(c)})
  | DError(ds, msg) => ev("DError", list{pair(dval_source, string, (ds, msg))})
  | DResp(h, hdv) => ev("DResp", list{tuple2(dhttp, ocamlDval, (h, hdv))})
  | DDB(name) => ev("DDB", list{string(name)})
  | DDate(date) => ev("DDate", list{string(date)})
  | DPassword(hashed) => ev("DPassword", list{string(hashed)})
  | DUuid(uuid) => ev("DUuid", list{string(uuid)})
  | DOption(opt) =>
    ev(
      "DOption",
      list{
        switch opt {
        | OptNothing => ev("OptNothing", list{})
        | OptJust(dv) => ev("OptJust", list{ocamlDval(dv)})
        },
      },
    )
  | DErrorRail(dv) => ev("DErrorRail", list{ocamlDval(dv)})
  | DResult(res) =>
    ev(
      "DResult",
      list{
        switch res {
        | ResOk(dv) => ev("ResOk", list{ocamlDval(dv)})
        | ResError(dv) => ev("ResError", list{ocamlDval(dv)})
        },
      },
    )
  | DBytes(bin) => ev("DBytes", list{string(bin |> base64url_bytes)})
  }
}

and blankOrData = (pd: Types.blankOrData): Js.Json.t => {
  let ev = variant
  switch pd {
  | PEventName(name) => ev("PEventName", list{blankOr(string, name)})
  | PEventModifier(modifier) => ev("PEventModifier", list{blankOr(string, modifier)})
  | PEventSpace(space) => ev("PEventSpace", list{blankOr(string, space)})
  | PDBName(name) => ev("PDBName", list{blankOr(string, name)})
  | PDBColName(colname) => ev("PDBColName", list{blankOr(string, colname)})
  | PDBColType(coltype) => ev("PDBColType", list{blankOr(string, coltype)})
  | PFnName(msg) => ev("PFnName", list{blankOr(string, msg)})
  | PFnReturnTipe(msg) => ev("PFnReturnTipe", list{blankOr(DType.encode, msg)})
  | PParamName(msg) => ev("PParamName", list{blankOr(string, msg)})
  | PParamTipe(msg) => ev("PParamTipe", list{blankOr(DType.encode, msg)})
  | PTypeName(n) => ev("PTypeName", list{blankOr(string, n)})
  | PTypeFieldName(n) => ev("PTypeFieldName", list{blankOr(string, n)})
  | PTypeFieldTipe(t) => ev("PTypeFieldTipe", list{blankOr(DType.encode, t)})
  }
}

and tlidOf = (op: Types.op): TLID.t =>
  switch op {
  | SetHandler(tlid, _, _) => tlid
  | CreateDB(tlid, _, _) => tlid
  | AddDBCol(tlid, _, _) => tlid
  | SetDBColName(tlid, _, _) => tlid
  | ChangeDBColName(tlid, _, _) => tlid
  | SetDBColType(tlid, _, _) => tlid
  | ChangeDBColType(tlid, _, _) => tlid
  | TLSavepoint(tlid) => tlid
  | UndoTL(tlid) => tlid
  | RedoTL(tlid) => tlid
  | DeleteTL(tlid) => tlid
  | MoveTL(tlid, _) => tlid
  | SetFunction(f) => f.ufTLID
  | DeleteFunction(tlid) => tlid
  | SetExpr(tlid, _, _) => tlid
  | DeleteDBCol(tlid, _) => tlid
  | RenameDBname(tlid, _) => tlid
  | CreateDBWithBlankOr(tlid, _, _, _) => tlid
  | SetType(ut) => ut.utTLID
  | DeleteType(tlid) => tlid
  }

and ops = (ops: list<Types.op>): Js.Json.t =>
  list(
    op,
    switch ops {
    | list{UndoTL(_)} => ops
    | list{RedoTL(_)} => ops
    | list{} => ops
    | _ =>
      let savepoints = List.map(~f=op => Types.TLSavepoint(tlidOf(op)), ops)

      Belt.List.concat(savepoints, ops)
    },
  )

and spec = (spec: Types.handlerSpec): Js.Json.t =>
  object_(list{
    ("name", blankOr(string, spec.name)),
    ("module", blankOr(string, spec.space)),
    ("modifier", blankOr(string, spec.modifier)),
    (
      "types",
      object_(list{
        ("input", blankOr(int, BlankOr.new_())),
        ("output", blankOr(int, BlankOr.new_())),
      }),
    ),
  })

and handler = (h: Types.handler): Js.Json.t =>
  object_(list{("tlid", tlid(h.hTLID)), ("spec", spec(h.spec)), ("ast", fluidAST(h.ast))})

and op = (call: Types.op): Js.Json.t => {
  let ev = variant
  switch call {
  | SetHandler(t, p, h) => ev("SetHandler", list{tlid(t), pos(p), handler(h)})
  | CreateDB(t, p, name) => ev("CreateDB", list{tlid(t), pos(p), string(name)})
  | AddDBCol(t, cn, ct) => ev("AddDBCol", list{tlid(t), id(cn), id(ct)})
  | SetDBColName(t, i, name) => ev("SetDBColName", list{tlid(t), id(i), string(name)})
  | ChangeDBColName(t, i, name) => ev("ChangeDBColName", list{tlid(t), id(i), string(name)})
  | SetDBColType(t, i, tipe) => ev("SetDBColType", list{tlid(t), id(i), string(tipe)})
  | ChangeDBColType(t, i, name) => ev("ChangeDBColType", list{tlid(t), id(i), string(name)})
  | DeleteDBCol(t, i) => ev("DeleteDBCol", list{tlid(t), id(i)})
  | TLSavepoint(t) => ev("TLSavepoint", list{tlid(t)})
  | UndoTL(t) => ev("UndoTL", list{tlid(t)})
  | RedoTL(t) => ev("RedoTL", list{tlid(t)})
  | DeleteTL(t) => ev("DeleteTL", list{tlid(t)})
  | MoveTL(t, p) => ev("MoveTL", list{tlid(t), pos(p)})
  | SetFunction(uf) => ev("SetFunction", list{userFunction(uf)})
  | DeleteFunction(t) => ev("DeleteFunction", list{tlid(t)})
  | SetExpr(t, i, e) => ev("SetExpr", list{tlid(t), id(i), e |> fluidExpr})
  | RenameDBname(t, name) => ev("RenameDBname", list{tlid(t), string(name)})
  | CreateDBWithBlankOr(t, p, i, name) =>
    ev("CreateDBWithBlankOr", list{tlid(t), pos(p), id(i), string(name)})
  | SetType(t) => ev("SetType", list{userTipe(t)})
  | DeleteType(t) => ev("DeleteType", list{tlid(t)})
  }
}

and addOpAPIParams = (params: Types.addOpAPIParams): Js.Json.t =>
  object_(list{
    ("ops", ops(params.ops)),
    ("opCtr", int(params.opCtr)),
    ("clientOpCtrId", string(params.clientOpCtrId)),
  })

and executeFunctionAPIParams = (params: Types.executeFunctionAPIParams): Js.Json.t =>
  object_(list{
    ("tlid", tlid(params.efpTLID)),
    ("trace_id", string(params.efpTraceID)),
    ("caller_id", id(params.efpCallerID)),
    ("args", list(ocamlDval, params.efpArgs)),
    ("fnname", string(params.efpFnName)),
  })

and deleteToplevelForeverAPIParams = (params: Types.deleteToplevelForeverAPIParams): Js.Json.t =>
  object_(list{("tlid", tlid(params.dtfTLID))})

and packageFnParameter = (pfp: Types.packageFnParameter): Js.Json.t =>
  object_(list{
    ("name", string(pfp.name)),
    ("tipe", DType.encode(pfp.tipe)),
    ("description", string(pfp.description)),
  })

and packageFn = (pf: Types.packageFn): Js.Json.t =>
  object_(list{
    ("user", string(pf.user)),
    ("package", string(pf.package)),
    ("module", string(pf.module_)),
    ("fnname", string(pf.fnname)),
    ("version", int(pf.version)),
    ("body", fluidExpr(pf.body)),
    ("parameters", list(packageFnParameter, pf.parameters)),
    ("return_type", DType.encode(pf.return_type)),
    ("description", string(pf.description)),
    ("author", string(pf.author)),
    ("deprecated", bool(pf.deprecated)),
    ("tlid", tlid(pf.pfTLID)),
  })

and uploadFnAPIParams = (params: Types.uploadFnAPIParams): Js.Json.t =>
  object_(list{("fn", userFunction(params.uplFn))})

and triggerHandlerAPIParams = (params: Types.triggerHandlerAPIParams): Js.Json.t =>
  object_(list{
    ("tlid", tlid(params.thTLID)),
    ("trace_id", string(params.thTraceID)),
    ("input", list(tuple2(string, ocamlDval), Belt.Map.String.toList(params.thInput))),
  })

and sendPresenceParams = (params: Types.sendPresenceParams): Js.Json.t =>
  object_(list{
    ("canvasName", string(params.canvasName)),
    ("browserId", string(params.browserId)),
    ("tlid", nullable(tlid, params.tlid)),
    ("timestamp", Json.Encode.float(params.timestamp)),
  })

and sendInviteParams = (params: Types.sendInviteParams): Js.Json.t =>
  object_(list{
    ("email", string(params.email)),
    ("inviterUsername", string(params.inviterUsername)),
    ("inviterName", string(params.inviterName)),
  })

and getTraceDataAPIParams = (params: Types.getTraceDataAPIParams): Js.Json.t =>
  object_(list{("tlid", tlid(params.gtdrpTlid)), ("trace_id", traceID(params.gtdrpTraceID))})

and dbStatsAPIParams = (params: Types.dbStatsAPIParams): Js.Json.t =>
  object_(list{("tlids", list(tlid, params.dbStatsTlids))})

and workerStatsAPIParams = (params: Types.workerStatsAPIParams): Js.Json.t =>
  object_(list{("tlid", tlid(params.workerStatsTlid))})

and updateWorkerScheduleAPIParams = (params: Types.updateWorkerScheduleAPIParams): Js.Json.t =>
  object_(list{("name", string(params.workerName)), ("schedule", string(params.schedule))})

and secret = (s: SecretTypes.t): Js.Json.t =>
  object_(list{("secret_name", string(s.secretName)), ("secret_value", string(s.secretValue))})

and performHandlerAnalysisParams = (params: Types.performHandlerAnalysisParams): Js.Json.t =>
  object_(list{
    ("handler", handler(params.handler)),
    ("trace_id", traceID(params.traceID)),
    ("trace_data", traceData(params.traceData)),
    ("dbs", list(PT.DB.encode, params.dbs)),
    ("user_fns", list(userFunction, params.userFns)),
    ("user_tipes", list(userTipe, params.userTipes)),
    ("secrets", list(secret, params.secrets)),
  })

and performFunctionAnalysisParams = (params: Types.performFunctionAnalysisParams): Js.Json.t =>
  object_(list{
    ("func", userFunction(params.func)),
    ("trace_id", traceID(params.traceID)),
    ("trace_data", traceData(params.traceData)),
    ("dbs", list(PT.DB.encode, params.dbs)),
    ("user_fns", list(userFunction, params.userFns)),
    ("user_tipes", list(userTipe, params.userTipes)),
    ("secrets", list(secret, params.secrets)),
  })

and performAnalysisParams = (params: Types.performAnalysisParams): Js.Json.t => {
  let ev = variant
  switch params {
  | AnalyzeHandler(h) => ev("AnalyzeHandler", list{performHandlerAnalysisParams(h)})
  | AnalyzeFunction(h) => ev("AnalyzeFunction", list{performFunctionAnalysisParams(h)})
  }
}

and userFunction = (uf: Types.userFunction): Js.Json.t =>
  object_(list{
    ("tlid", tlid(uf.ufTLID)),
    ("metadata", userFunctionMetadata(uf.ufMetadata)),
    ("ast", fluidAST(uf.ufAST)),
  })

and userFunctionMetadata = (f: Types.userFunctionMetadata): Js.Json.t =>
  object_(list{
    ("name", blankOr(string, f.ufmName)),
    ("parameters", list(userFunctionParameter, f.ufmParameters)),
    ("description", string(f.ufmDescription)),
    ("return_type", blankOr(DType.encode, f.ufmReturnTipe)),
    ("infix", bool(f.ufmInfix)),
  })

and userTipe = (ut: Types.userTipe): Js.Json.t =>
  object_(list{
    ("tlid", tlid(ut.utTLID)),
    ("name", blankOr(string, ut.utName)),
    ("version", int(ut.utVersion)),
    ("definition", userTipeDefinition(ut.utDefinition)),
  })

and userTipeDefinition = (utd: Types.userTipeDefinition): Js.Json.t => {
  let ev = variant
  switch utd {
  | UTRecord(fields) => ev("UTRecord", list{list(userRecordField)(fields)})
  }
}

and userRecordField = (urf: Types.userRecordField): Js.Json.t =>
  object_(list{
    ("name", blankOr(string, urf.urfName)),
    ("tipe", blankOr(DType.encode, urf.urfTipe)),
  })

and userFunctionParameter = (p: Types.userFunctionParameter): Js.Json.t =>
  object_(list{
    ("name", blankOr(string, p.ufpName)),
    ("tipe", blankOr(DType.encode, p.ufpTipe)),
    ("block_args", list(string, p.ufpBlock_args)),
    ("optional", bool(p.ufpOptional)),
    ("description", string(p.ufpDescription)),
  })

and sendToRail = (sendToRail: ProgramTypes.Expr.sendToRail): Js.Json.t => {
  let ev = variant
  switch sendToRail {
  | Rail => ev("Rail", list{})
  | NoRail => ev("NoRail", list{})
  }
}

and fluidPattern = (mid: id, pattern: FluidPattern.t): Js.Json.t => {
  let fp = fluidPattern(mid)
  let ev = variant
  switch pattern {
  /* Warning: A bunch of stuff here seems to expect that the
    second element of the tuples are match id but they are actually
    pattern ids. */
  | PVariable(id', name) => ev("FPVariable", list{id(mid), id(id'), string(name)})
  | PConstructor(id', name, patterns) =>
    ev("FPConstructor", list{id(mid), id(id'), string(name), list(fp, patterns)})
  | PInteger(id', v) => ev("FPInteger", list{id(mid), id(id'), string(Int64.to_string(v))})
  | PBool(id', v) => ev("FPBool", list{id(mid), id(id'), bool(v)})
  | PFloat(id', sign, whole, fraction) =>
    ev(
      "FPFloat",
      list{id(mid), id(id'), string(ProgramTypes.Sign.combine(sign, whole)), string(fraction)},
    )
  | PString(id', v) =>
    ev(
      "FPString",
      list{object_(list{("matchID", id(mid)), ("patternID", id(id')), ("str", string(v))})},
    )
  | PNull(id') => ev("FPNull", list{id(mid), id(id')})
  | PBlank(id') => ev("FPBlank", list{id(mid), id(id')})
  }
}

and fluidAST = (ast: FluidAST.t): Js.Json.t => ast |> FluidAST.toExpr |> fluidExpr

and fluidExpr = (expr: FluidExpression.t): Js.Json.t => {
  let fe = fluidExpr
  let ev = variant
  switch expr {
  | ELet(id', lhs, rhs, body) => ev("ELet", list{id(id'), string(lhs), fe(rhs), fe(body)})
  | EIf(id', cond, ifbody, elsebody) => ev("EIf", list{id(id'), fe(cond), fe(ifbody), fe(elsebody)})
  | EFnCall(id', name, exprs, r) =>
    ev("EFnCall", list{id(id'), string(name), list(fe, exprs), sendToRail(r)})
  | EBinOp(id', name, left, right, r) =>
    ev("EBinOp", list{id(id'), string(name), fe(left), fe(right), sendToRail(r)})
  | ELambda(id', vars, body) => ev("ELambda", list{id(id'), list(pair(id, string), vars), fe(body)})
  | EPipe(id', e1, e2, rest) => ev("EPipe", list{id(id'), list(fe, list{e1, e2, ...rest})})
  | EFieldAccess(id', obj, field) => ev("EFieldAccess", list{id(id'), fe(obj), string(field)})
  | EString(id', v) => ev("EString", list{id(id'), string(v)})
  | EInteger(id', v) => ev("EInteger", list{id(id'), string(Int64.to_string(v))})
  | EBool(id', v) => ev("EBool", list{id(id'), bool(v)})
  | EFloat(id', sign, whole, fraction) =>
    ev("EFloat", list{id(id'), string(ProgramTypes.Sign.combine(sign, whole)), string(fraction)})
  | ENull(id') => ev("ENull", list{id(id')})
  | EBlank(id') => ev("EBlank", list{id(id')})
  | EVariable(id', name) => ev("EVariable", list{id(id'), string(name)})
  | EList(id', exprs) => ev("EList", list{id(id'), list(fe, exprs)})
  | ETuple(id', first, second, theRest) =>
    ev("ETuple", list{id(id'), fe(first), fe(second), list(fe, theRest)})
  | ERecord(id', pairs) => ev("ERecord", list{id(id'), list(pair(string, fe), pairs)})
  | EFeatureFlag(id', name, cond, a, b) =>
    ev("EFeatureFlag", list{id(id'), string(name), fe(cond), fe(a), fe(b)})
  | EMatch(id', matchExpr, cases) =>
    ev("EMatch", list{id(id'), fe(matchExpr), list(pair(fluidPattern(id'), fe), cases)})
  | EConstructor(id', name, args) => ev("EConstructor", list{id(id'), string(name), list(fe, args)})
  | EPartial(id', str, oldExpr) => ev("EPartial", list{id(id'), string(str), fe(oldExpr)})
  | ERightPartial(id', str, oldExpr) => ev("ERightPartial", list{id(id'), string(str), fe(oldExpr)})
  | ELeftPartial(id', str, oldExpr) => ev("ELeftPartial", list{id(id'), string(str), fe(oldExpr)})
  | EPipeTarget(id') => ev("EPipeTarget", list{id(id')})
  }
}

and userTutorial = (ut: Types.tutorialStep): Js.Json.t => {
  let ev = variant
  switch ut {
  | Welcome => ev("Welcome", list{})
  | VerbChange => ev("VerbChange", list{})
  | ReturnValue => ev("ReturnValue", list{})
  | OpenTab => ev("OpenTab", list{})
  | GettingStarted => ev("GettingStarted", list{})
  }
}

and cursorState = (cs: Types.cursorState): Js.Json.t => {
  let ev = variant
  switch cs {
  | Selecting(tlid_, mId) => ev("Selecting", list{tlid(tlid_), nullable(id, mId)})
  | Omnibox(maybePos) => ev("OmniBox", list{nullable(pos, maybePos)})
  | Entering(tlid_, id_) => ev("Entering", list{tlid(tlid_), id(id_)})
  | DraggingTL(tlid_, vpos_, hasMoved, cursor) =>
    ev("DraggingTL", list{tlid(tlid_), vPos(vpos_), bool(hasMoved), cursorState(cursor)})
  | PanningCanvas({viewportStart, viewportCurr, prevCursorState}) =>
    /* TODO: There's a danger of mismatching the decoder order here because we're using an inline record.
     * An order-independent encoding would alleviate this. */
    ev("PanningCanvas", list{vPos(viewportStart), vPos(viewportCurr), cursorState(prevCursorState)})
  | Deselected => ev("Deselected", list{})
  | FluidEntering(tlid_) => ev("FluidEntering", list{tlid(tlid_)})
  }
}

and functionResult = (fr: Types.functionResult): Js.Json.t =>
  list(
    identity,
    list{
      string(fr.fnName),
      id(fr.callerID),
      string(fr.argHash),
      int(fr.argHashVersion),
      ocamlDval(fr.value),
    },
  )

and traceID = string

and traceData = (t: Types.traceData): Js.Json.t =>
  object_(list{
    ("input", list(tuple2(string, ocamlDval), Belt.Map.String.toList(t.input))),
    ("timestamp", string(t.timestamp)),
    ("function_results", list(functionResult, t.functionResults)),
  })

and trace = (t: Types.trace): Js.Json.t => {
  let data = v => Result.map(~f=traceData, v) |> Result.toOption |> Option.unwrap(~default=null)

  pair(traceID, data, t)
}

let handlerProp = (_p: Types.handlerProp): Js.Json.t => object_(list{})

let sidebarMode = (s: Types.sidebarMode): Js.Json.t =>
  switch s {
  | DetailedMode => variant("DetailedMode", list{})
  | AbridgedMode => variant("AbridgedMode", list{})
  }

let sidebarState = (s: Types.sidebarState): Js.Json.t =>
  object_(list{("mode", sidebarMode(s.mode)), ("openedCategories", tcStrSet(s.openedCategories))})

let editorSettings = (es: Types.editorSettings): Js.Json.t =>
  object_(list{
    ("runTimers", bool(es.runTimers)),
    ("showHandlerASTs", bool(es.showHandlerASTs)),
    ("showFluidDebugger", bool(es.showFluidDebugger)),
  })

let savedUserSettings = (se: Types.savedUserSettings): Js.Json.t =>
  object_(list{
    ("showUserWelcomeModal", bool(se.firstVisitToDark)),
    ("firstVisitToDark", bool(se.firstVisitToDark)),
    ("recordConsent", Option.map(~f=bool, se.recordConsent) |> Option.unwrap(~default=null)),
  })

let tlidDict = (valueEncoder: 'value => Js.Json.t, t: TLID.Dict.t<'value>): Js.Json.t =>
  t |> Map.toList |> List.map(~f=((k, v)) => (TLID.toString(k), valueEncoder(v))) |> object_

let savedSettings = (se: Types.savedSettings): Js.Json.t =>
  object_(list{
    ("editorSettings", editorSettings(se.editorSettings)),
    ("cursorState", cursorState(se.cursorState)),
    ("tlTraceIDs", tlidDict(traceID, se.tlTraceIDs)),
    ("featureFlags", tcStrDict(bool, se.featureFlags)),
    ("handlerProps", tlidDict(handlerProp, se.handlerProps)),
    ("canvasPos", pos(se.canvasPos)),
    ("lastReload", nullable(string, Option.map(~f=Js.Date.toString, se.lastReload))),
    ("sidebarState", sidebarState(se.sidebarState)),
    ("showTopbar1", bool(se.showTopbar)),
    ("firstVisitToThisCanvas", bool(se.firstVisitToThisCanvas)),
    ("userTutorial", Option.map(~f=userTutorial, se.userTutorial) |> Option.unwrap(~default=null)),
    ("userTutorialTLID", nullable(tlid, se.userTutorialTLID)),
  })

let fof = (fof: Types.fourOhFour): Js.Json.t =>
  object_(list{
    ("space", string(fof.space)),
    ("path", string(fof.path)),
    ("modifier", string(fof.modifier)),
  })

let httpError = (e: Tea.Http.error<string>): Js.Json.t => {
  module Http = Tea.Http
  let responseBody = (r: Http.responseBody) =>
    switch r {
    | NoResponse => object_(list{("noResponse", null)})
    | StringResponse(str) => string(str)
    | ArrayBufferResponse() => object_(list{("arrayBufferResponse", null)})
    | BlobResponse() => object_(list{("blobResponse", null)})
    | DocumentResponse(_) => object_(list{("documentResponse", string("<documentResponse>"))})
    | JsonResponse(json) => json
    | TextResponse(text) => string(text)
    | RawResponse(str, ()) => object_(list{("rawResponse", string(str))})
    }

  let response = (r: Http.response) => {
    module StringMap = Caml.Map.Make(Tc.Caml.String)
    object_(list{
      ("url", string(r.url)),
      (
        "status",
        object_(list{("code", int(r.status.code)), ("message", string(r.status.message))}),
      ),
      (
        "headers",
        r.headers |> StringMap.bindings |> List.map(~f=((k, v)) => (k, string(v))) |> object_,
      ),
      ("body", responseBody(r.body)),
    })
  }

  switch e {
  | Http.BadUrl(url) => object_(list{("type", string("BadUrl")), ("url", string(url))})
  | Http.Timeout => object_(list{("type", string("Timeout"))})
  | Http.NetworkError => object_(list{("type", string("NetworkError"))})
  | Http.BadStatus(r) => object_(list{("type", string("BadStatus")), ("response", response(r))})
  | Http.BadPayload(msg, r) =>
    object_(list{
      ("type", string("BadPayload")),
      ("message", string(msg)),
      ("response", response(r)),
    })
  | Http.Aborted => object_(list{("type", string("Aborted"))})
  }
}
