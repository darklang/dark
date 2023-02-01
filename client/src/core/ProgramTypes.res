// The types that the user sees. For all type definitions, see ProgramTypes.fs
module InfixStdlibFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {module_: option<string>, function: string}
  let encode = (n: t): Js.Json.t => {
    open Json.Encode
    object_(list{("module_", nullable(string, n.module_)), ("function_", string(n.function))})
  }
  let decode = j => {
    open Json.Decode
    {
      module_: field("module_", optional(string), j),
      function: field("function_", string, j), // Note underscore
    }
  }
  let toString = (n: t): string => {
    switch n.module_ {
    | None => n.function
    | Some(m) => `${m}::${n.function}`
    }
  }
  let toStdlib = (n: t): FQFnName.StdlibFnName.t => {
    function: n.function,
    version: 0,
    module_: switch n.module_ {
    | None => ""
    | Some(m) => m
    },
  }
}

module Sign = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | Positive
    | Negative

  let toString = (sign: t): string =>
    switch sign {
    | Positive => ""
    | Negative => "-"
    }
  // Split the string into a sign and a string (removes the sign if present and )
  let split = (whole: string): (t, string) => {
    if Tc.String.startsWith(~prefix="-", whole) {
      (Negative, Tc.String.dropLeft(~count=1, whole))
    } else if Tc.String.startsWith(~prefix="+", whole) {
      (Positive, Tc.String.dropLeft(~count=1, whole))
    } else {
      (Positive, whole)
    }
  }
  let combine = (sign: t, whole: string): string => {
    toString(sign) ++ whole
  }
  let encode = (n: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch n {
    | Negative => ev("Negative", list{})
    | Positive => ev("Positive", list{})
    }
  }
  let decode = (j): t => {
    open Json_decode_extended
    variants(list{("Negative", variant0(Negative)), ("Positive", variant0(Positive))}, j)
  }
}

module LetPattern = {
  @ppx.deriving(show({with_path: false}))
  type t = LPVariable(ID.t, string)

  let encode = (letPattern: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch letPattern {
    | LPVariable(id', name) => ev("LPVariable", list{ID.encode(id'), string(name)})
    }
  }

  let decode = (j): t => {
    open Json_decode_extended
    let dv2 = variant2
    variants(list{("LPVariable", dv2((a, b) => LPVariable(a, b), ID.decode, string))}, j)
  }
}

module MatchPattern = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    // match id, then pattern id
    | MPVariable(ID.t, string)
    | MPConstructor(ID.t, string, list<t>)
    | MPInteger(ID.t, int64)
    | MPBool(ID.t, bool)
    | MPString(ID.t, string)
    | MPCharacter(ID.t, string)
    | MPFloat(ID.t, Sign.t, string, string)
    | MPNull(ID.t)
    | MPBlank(ID.t)
    | MPTuple(ID.t, t, t, list<t>)

  let rec encode = (matchPattern: t): Js.Json.t => {
    open Json_encode_extended
    let ep = encode
    let ev = variant
    switch matchPattern {
    | MPVariable(id', name) => ev("MPVariable", list{ID.encode(id'), string(name)})
    | MPConstructor(id', name, args) =>
      ev("MPConstructor", list{ID.encode(id'), string(name), list(ep, args)})
    | MPInteger(id', v) => ev("MPInteger", list{ID.encode(id'), int64(v)})
    | MPBool(id', v) => ev("MPBool", list{ID.encode(id'), bool(v)})
    | MPFloat(id', sign, whole, fraction) =>
      ev("MPFloat", list{ID.encode(id'), Sign.encode(sign), string(whole), string(fraction)})
    | MPString(id', v) => ev("MPString", list{ID.encode(id'), string(v)})
    | MPCharacter(id', v) => ev("MPCharacter", list{ID.encode(id'), string(v)})
    | MPNull(id') => ev("MPNull", list{ID.encode(id')})
    | MPBlank(id') => ev("MPBlank", list{ID.encode(id')})
    | MPTuple(id', first, second, theRest) =>
      ev("MPTuple", list{ID.encode(id'), ep(first), ep(second), list(ep, theRest)})
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
        ("MPVariable", dv2((a, b) => MPVariable(a, b), ID.decode, string)),
        (
          "MPConstructor",
          dv3((a, b, c) => MPConstructor(a, b, c), ID.decode, string, list(decode)),
        ),
        ("MPInteger", dv2((a, b) => MPInteger(a, b), ID.decode, int64)),
        ("MPBool", dv2((a, b) => MPBool(a, b), ID.decode, bool)),
        ("MPString", dv2((a, b) => MPString(a, b), ID.decode, string)),
        ("MPCharacter", dv2((a, b) => MPCharacter(a, b), ID.decode, string)),
        (
          "MPFloat",
          dv4((a, b, c, d) => MPFloat(a, b, c, d), ID.decode, Sign.decode, string, string),
        ),
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

  module BinaryOperation = {
    type t =
      | BinOpAnd
      | BinOpOr

    let encode = (str: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch str {
      | BinOpAnd => ev("BinOpAnd", list{})
      | BinOpOr => ev("BinOpOr", list{})
      }
    }

    let decode = j => {
      open Json_decode_extended
      let dv0 = variant0
      variants(list{("BinOpAnd", dv0(BinOpAnd)), ("BinOpOr", dv0(BinOpOr))}, j)
    }

    let toString = (op: t): string => {
      switch op {
      | BinOpAnd => "&&"
      | BinOpOr => "||"
      }
    }
  }

  module Infix = {
    type t =
      | InfixFnCall(InfixStdlibFnName.t, SendToRail.t)
      | BinOp(BinaryOperation.t)

    let decode = j => {
      open Json_decode_extended
      let dv2 = variant2
      let dv1 = variant1
      variants(
        list{
          (
            "InfixFnCall",
            dv2((a, b) => InfixFnCall(a, b), InfixStdlibFnName.decode, SendToRail.decode),
          ),
          ("BinOp", dv1(a => BinOp(a), BinaryOperation.decode)),
        },
        j,
      )
    }

    let encode = (str: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch str {
      | InfixFnCall(a, b) =>
        ev("InfixFnCall", list{InfixStdlibFnName.encode(a), SendToRail.encode(b)})
      | BinOp(a) => ev("BinOp", list{BinaryOperation.encode(a)})
      }
    }

    let toString = (op: t): string => {
      switch op {
      | InfixFnCall(fnName, _) => InfixStdlibFnName.toString(fnName)
      | BinOp(op) => BinaryOperation.toString(op)
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | EInteger(ID.t, int64)
    | EBool(ID.t, bool)
    | EString(ID.t, string)
    | ECharacter(ID.t, string)
    | EFloat(ID.t, Sign.t, string, string)
    | ENull(ID.t)
    | EBlank(ID.t)
    | ELet(ID.t, LetPattern.t, t, t)
    | EIf(ID.t, t, t, t)
    | ELambda(ID.t, list<(ID.t, string)>, t)
    | EFieldAccess(ID.t, t, string)
    | EVariable(ID.t, string)
    | EFnCall(ID.t, FQFnName.t, list<t>, SendToRail.t)
    | EPartial(ID.t, string, t)
    | ERightPartial(ID.t, string, t)
    | ELeftPartial(ID.t, string, t)
    | EList(ID.t, list<t>)
    | ETuple(ID.t, t, t, list<t>)
    | ERecord(ID.t, list<(string, t)>)
    | EPipe(ID.t, t, t, list<t>)
    | EConstructor(ID.t, string, list<t>)
    | EMatch(ID.t, t, list<(MatchPattern.t, t)>)
    | EPipeTarget(ID.t)
    | EFeatureFlag(ID.t, string, t, t, t)
    | EInfix(ID.t, Infix.t, t, t)

  let rec encode = (expr: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch expr {
    | ELet(id, pat, rhs, body) =>
      let varName = switch pat {
      | LetPattern.LPVariable(_id, name) => name
      }
      ev("ELet", list{ID.encode(id), string(varName), encode(rhs), encode(body)})
    | EIf(id', cond, ifbody, elsebody) =>
      ev("EIf", list{ID.encode(id'), encode(cond), encode(ifbody), encode(elsebody)})
    | EFnCall(id', name, exprs, r) =>
      ev(
        "EFnCall",
        list{ID.encode(id'), FQFnName.encode(name), list(encode, exprs), SendToRail.encode(r)},
      )
    | EInfix(id, infix, left, right) =>
      ev("EInfix", list{ID.encode(id), Infix.encode(infix), encode(left), encode(right)})
    | ELambda(id, vars, body) =>
      ev("ELambda", list{ID.encode(id), list(pair(ID.encode, string), vars), encode(body)})
    | EPipe(id, e1, e2, rest) =>
      ev("EPipe", list{ID.encode(id), encode(e1), encode(e2), list(encode, rest)})
    | EFieldAccess(id, obj, field) =>
      ev("EFieldAccess", list{ID.encode(id), encode(obj), string(field)})
    | EString(id, v) => ev("EString", list{ID.encode(id), string(v)})
    | ECharacter(id, v) => ev("ECharacter", list{ID.encode(id), string(v)})
    | EInteger(id, v) => ev("EInteger", list{ID.encode(id), int64(v)})
    | EBool(id, v) => ev("EBool", list{ID.encode(id), bool(v)})
    | EFloat(id, sign, whole, fraction) =>
      ev("EFloat", list{ID.encode(id), Sign.encode(sign), string(whole), string(fraction)})
    | ENull(id) => ev("ENull", list{ID.encode(id)})
    | EBlank(id) => ev("EBlank", list{ID.encode(id)})
    | EVariable(id, name) => ev("EVariable", list{ID.encode(id), string(name)})
    | EList(id, exprs) => ev("EList", list{ID.encode(id), list(encode, exprs)})
    | ETuple(id, first, second, theRest) =>
      ev("ETuple", list{ID.encode(id), encode(first), encode(second), list(encode, theRest)})
    | ERecord(id, pairs) => ev("ERecord", list{ID.encode(id), list(pair(string, encode), pairs)})
    | EFeatureFlag(id, name, cond, a, b) =>
      ev("EFeatureFlag", list{ID.encode(id), string(name), encode(cond), encode(a), encode(b)})
    | EMatch(id, matchExpr, cases) =>
      ev(
        "EMatch",
        list{ID.encode(id), encode(matchExpr), list(pair(MatchPattern.encode, encode), cases)},
      )
    | EConstructor(id, name, args) =>
      ev("EConstructor", list{ID.encode(id), string(name), list(encode, args)})
    | EPartial(id, str, oldExpr) =>
      ev("EPartial", list{ID.encode(id), string(str), encode(oldExpr)})
    | ERightPartial(id, str, oldExpr) =>
      ev("ERightPartial", list{ID.encode(id), string(str), encode(oldExpr)})
    | ELeftPartial(id, str, oldExpr) =>
      ev("ELeftPartial", list{ID.encode(id), string(str), encode(oldExpr)})
    | EPipeTarget(id) => ev("EPipeTarget", list{ID.encode(id)})
    }
  }

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
        (
          "EFloat",
          dv4(
            (a, sign, whole, fraction) => EFloat(a, sign, whole, fraction),
            ID.decode,
            Sign.decode,
            string,
            string,
          ),
        ),
        ("ENull", dv1(x => ENull(x), ID.decode)),
        ("EBlank", dv1(x => EBlank(x), ID.decode)),
        // LetPatternTODO: the `ID.fromInt(0)` here is a hack to ensure
        // roundtripping is consistent as tested by the `fluidExpr` in
        // `TestJsonEncoding.res`. Once we start communicating in new-style `let`s,
        // we can remove this hack.
        (
          "ELet",
          dv4(
            (a, b, c, d) => ELet(a, LPVariable(ID.fromInt(0), b), c, d),
            ID.decode,
            string,
            de,
            de,
          ),
        ),
        (
          "ELetWithPattern",
          dv4((a, b, c, d) => ELet(a, b, c, d), ID.decode, LetPattern.decode, de, de),
        ),
        ("EIf", dv4((a, b, c, d) => EIf(a, b, c, d), ID.decode, de, de, de)),
        ("EInfix", dv4((a, b, c, d) => EInfix(a, b, c, d), ID.decode, Infix.decode, de, de)),
        (
          "ELambda",
          dv3((a, b, c) => ELambda(a, b, c), ID.decode, list(pair(ID.decode, string)), de),
        ),
        ("EFieldAccess", dv3((a, b, c) => EFieldAccess(a, b, c), ID.decode, de, string)),
        ("EVariable", dv2((x, y) => EVariable(x, y), ID.decode, string)),
        (
          "EFnCall",
          dv4(
            (a, b, c, d) => EFnCall(a, b, c, d),
            ID.decode,
            FQFnName.decode,
            list(de),
            SendToRail.decode,
          ),
        ),
        ("EPartial", dv3((a, b, c) => EPartial(a, b, c), ID.decode, string, de)),
        ("ELeftPartial", dv3((a, b, c) => ELeftPartial(a, b, c), ID.decode, string, de)),
        ("ERightPartial", dv3((a, b, c) => ERightPartial(a, b, c), ID.decode, string, de)),
        ("EList", dv2((x, y) => EList(x, y), ID.decode, list(de))),
        (
          "ETuple",
          dv4((x, y1, y2, yRest) => ETuple(x, y1, y2, yRest), ID.decode, de, de, list(de)),
        ),
        ("ERecord", dv2((x, y) => ERecord(x, y), ID.decode, list(pair(string, de)))),
        ("EPipe", dv4((a, b, c, d) => EPipe(a, b, c, d), ID.decode, de, de, list(de))),
        ("EConstructor", dv3((a, b, c) => EConstructor(a, b, c), ID.decode, string, list(de))),
        (
          "EMatch",
          dv3((a, b, c) => EMatch(a, b, c), ID.decode, de, list(pair(MatchPattern.decode, de))),
        ),
        ("EPipeTarget", dv1(a => EPipeTarget(a), ID.decode)),
        (
          "EFeatureFlag",
          dv5((a, b, c, d, e) => EFeatureFlag(a, b, c, d, e), ID.decode, string, de, de, de),
        ),
      },
      j,
    )
  }
}

module AST = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Root(Expr.t)
  let encode = (Root(expr): t) => Expr.encode(expr)
  let decode = j => Root(Expr.decode(j))
}

module Handler = {
  module Spec = {
    module CronInterval = {
      @ppx.deriving(show({with_path: false}))
      type rec t =
        | EveryDay
        | EveryWeek
        | EveryFortnight
        | EveryHour
        | Every12Hours
        | EveryMinute

      let encode = (i: t): Js.Json.t => {
        open Json_encode_extended
        let ev = variant
        switch i {
        | EveryDay => ev("EveryDay", list{})
        | EveryWeek => ev("EveryWeek", list{})
        | EveryFortnight => ev("EveryFortnight", list{})
        | EveryHour => ev("EveryHour", list{})
        | Every12Hours => ev("Every12Hours", list{})
        | EveryMinute => ev("EveryMinute", list{})
        }
      }

      let decode = (j): t => {
        open Json_decode_extended
        let dv0 = variant0
        variants(
          list{
            ("EveryDay", dv0(EveryDay)),
            ("EveryWeek", dv0(EveryWeek)),
            ("EveryFortnight", dv0(EveryFortnight)),
            ("EveryHour", dv0(EveryHour)),
            ("Every12Hours", dv0(Every12Hours)),
            ("EveryMinute", dv0(EveryMinute)),
          },
          j,
        )
      }

      let toString = (i: t): string => {
        switch i {
        | EveryDay => "Daily"
        | EveryWeek => "Weekly"
        | EveryFortnight => "Fortnightly"
        | EveryHour => "Every 1hr"
        | Every12Hours => "Every 12hrs"
        | EveryMinute => "Every 1min"
        }
      }
      let fromString = (i: string): option<t> => {
        switch String.lowercase_ascii(i) {
        | "daily" => Some(EveryDay)
        | "weekly" => Some(EveryWeek)
        | "fortnightly" => Some(EveryFortnight)
        | "every 1hr" => Some(EveryHour)
        | "every 12hrs" => Some(Every12Hours)
        | "every 1min" => Some(EveryMinute)
        | _ => Recover.recover("invalid cron interval", ~debug=i, None)
        }
      }
    }

    module IDs = {
      @ppx.deriving(show({with_path: false}))
      type rec t = {moduleID: ID.t, nameID: ID.t, modifierID: ID.t}

      let encode = ({moduleID, nameID, modifierID}) => {
        open Json_encode_extended
        object_(list{
          ("moduleID", ID.encode(moduleID)),
          ("nameID", ID.encode(nameID)),
          ("modifierID", ID.encode(modifierID)),
        })
      }

      let decode = j => {
        open Json_decode_extended
        {
          moduleID: field("moduleID", ID.decode, j),
          nameID: field("nameID", ID.decode, j),
          modifierID: field("modifierID", ID.decode, j),
        }
      }
      let new = (): t => {
        moduleID: ID.generate(),
        nameID: ID.generate(),
        modifierID: ID.generate(),
      }
    }

    @ppx.deriving(show({with_path: false}))
    type rec t =
      | HTTP(string, string, IDs.t)
      | HTTPBasic(string, string, IDs.t)
      | Worker(string, IDs.t)
      | OldWorker(string, string, IDs.t)
      | Cron(string, option<CronInterval.t>, IDs.t)
      | REPL(string, IDs.t)
      | UnknownHandler(string, string, IDs.t)

    let encode = (spec: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch spec {
      | HTTP(name, mod, ids) => ev("HTTP", list{string(name), string(mod), IDs.encode(ids)})
      | HTTPBasic(name, mod, ids) =>
        ev("HTTPBasic", list{string(name), string(mod), IDs.encode(ids)})
      | Worker(name, ids) => ev("Worker", list{string(name), IDs.encode(ids)})
      | OldWorker(space, name, ids) =>
        ev("OldWorker", list{string(space), string(name), IDs.encode(ids)})
      | Cron(name, interval, ids) =>
        ev("Cron", list{string(name), nullable(CronInterval.encode, interval), IDs.encode(ids)})
      | REPL(name, ids) => ev("REPL", list{string(name), IDs.encode(ids)})
      | UnknownHandler(name, mod, ids) =>
        ev("UnknownHandler", list{string(name), string(mod), IDs.encode(ids)})
      }
    }
    let decode = (j): t => {
      open Json_decode_extended
      let dv2 = variant2
      let dv3 = variant3
      variants(
        list{
          ("HTTP", dv3((a, b, c) => HTTP(a, b, c), string, string, IDs.decode)),
          ("HTTPBasic", dv3((a, b, c) => HTTPBasic(a, b, c), string, string, IDs.decode)),
          ("Worker", dv2((a, b) => Worker(a, b), string, IDs.decode)),
          ("OldWorker", dv3((a, b, c) => OldWorker(a, b, c), string, string, IDs.decode)),
          (
            "Cron",
            dv3((a, b, c) => Cron(a, b, c), string, optional(CronInterval.decode), IDs.decode),
          ),
          ("REPL", dv2((a, b) => REPL(a, b), string, IDs.decode)),
          ("UnknownHandler", dv3((a, b, c) => UnknownHandler(a, b, c), string, string, IDs.decode)),
        },
        j,
      )
    }
    let space = (spec: t): BlankOr.t<string> => {
      switch spec {
      | HTTP(_, _, ids) => F(ids.moduleID, "HTTP")
      | HTTPBasic(_, _, ids) => F(ids.moduleID, "HTTP_BASIC")
      | Worker(_, ids) => F(ids.moduleID, "WORKER")
      | OldWorker(space, _, ids) => F(ids.moduleID, space)
      | Cron(_, _, ids) => F(ids.moduleID, "CRON")
      | REPL(_, ids) => F(ids.moduleID, "REPL")
      | UnknownHandler(_, _, ids) => Blank(ids.moduleID)
      }
    }
    let name = (spec: t): BlankOr.t<string> => {
      switch spec {
      | HTTP(name, _, ids)
      | HTTPBasic(name, _, ids)
      | Worker(name, ids)
      | OldWorker(_, name, ids)
      | Cron(name, _, ids)
      | REPL(name, ids)
      | UnknownHandler(name, _, ids) =>
        if name == "" {
          Blank(ids.nameID)
        } else {
          F(ids.nameID, name)
        }
      }
    }
    let modifier = (spec: t): option<BlankOr.t<string>> => {
      switch spec {
      | HTTP(_, "", ids)
      | HTTPBasic(_, "", ids)
      | UnknownHandler(_, "", ids) =>
        Some(Blank(ids.modifierID))

      | HTTP(_, mod, ids)
      | HTTPBasic(_, mod, ids)
      | UnknownHandler(_, mod, ids) =>
        Some(F(ids.modifierID, mod))

      | Cron(_, Some(interval), ids) => Some(F(ids.modifierID, CronInterval.toString(interval)))
      | Cron(_, None, ids) => Some(Blank(ids.modifierID))

      // These don't have modifiers
      | REPL(_, _)
      | Worker(_, _)
      | OldWorker(_, _, _) =>
        None
      }
    }
    let ids = (spec: t): IDs.t => {
      switch spec {
      | HTTP(_, _, ids)
      | HTTPBasic(_, _, ids)
      | Worker(_, ids)
      | OldWorker(_, _, ids)
      | Cron(_, _, ids)
      | REPL(_, ids)
      | UnknownHandler(_, _, ids) => ids
      }
    }

    let newUnknown = (name: string, modifier: string): t => {
      UnknownHandler(name, modifier, IDs.new())
    }
    let newHTTP = (path: string, modifier: string): t => {
      HTTP(path, modifier, IDs.new())
    }
    let newCron = (name: string, interval: option<CronInterval.t>): t => {
      Cron(name, interval, IDs.new())
    }
    let newWorker = (name: string): t => {
      Worker(name, IDs.new())
    }
    let newREPL = (name: string): t => {
      REPL(name, IDs.new())
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    ast: AST.t,
    spec: Spec.t,
    tlid: TLID.t,
    pos: Pos.t,
  }
  let encode = (h: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(h.tlid)),
      ("pos", Pos.encode(h.pos)),
      ("ast", AST.encode(h.ast)),
      ("spec", Spec.encode(h.spec)),
    })
  }

  let decode = (j): t => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      pos: field("pos", Pos.decode, j),
      ast: field("ast", AST.decode, j),
      spec: field("spec", Spec.decode, j),
    }
  }
}

module DB = {
  module Col = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: option<string>,
      typ: option<DType.t>,
      nameID: ID.t,
      typeID: ID.t,
    }
    let new = (): t => {
      name: None,
      typ: None,
      nameID: ID.generate(),
      typeID: ID.generate(),
    }

    let encode = (col: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", nullable(string, col.name)),
        ("typ", nullable(DType.encode, col.typ)),
        ("nameID", ID.encode(col.nameID)),
        ("typeID", ID.encode(col.typeID)),
      })
    }

    let decode = (j): t => {
      open Json.Decode
      {
        name: field("name", optional(string), j),
        typ: field("typ", optional(DType.decode), j),
        nameID: field("nameID", ID.decode, j),
        typeID: field("typeID", ID.decode, j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tlid: TLID.t,
    name: string,
    nameID: ID.t,
    cols: list<Col.t>,
    version: int,
    pos: Pos.t,
  }

  let encode = (db: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(db.tlid)),
      ("pos", Pos.encode(db.pos)),
      ("name", string(db.name)),
      ("nameID", ID.encode(db.nameID)),
      ("version", int(db.version)),
      ("cols", list(Col.encode, db.cols)),
    })
  }

  let decode = (j): t => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      pos: field("pos", Pos.decode, j),
      name: field("name", string, j),
      nameID: field("nameID", ID.decode, j),
      version: field("version", int, j),
      cols: field("cols", list(Col.decode), j),
    }
  }
}

module UserFunction = {
  module Parameter = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: string,
      nameID: ID.t,
      typ: option<DType.t>,
      typeID: ID.t,
      description: string,
    }

    let encode = (p: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", string(p.name)),
        ("nameID", ID.encode(p.nameID)),
        ("typ", nullable(DType.encode, p.typ)),
        ("typeID", ID.encode(p.typeID)),
        ("description", string(p.description)),
      })
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        name: field("name", string, j),
        nameID: field("nameID", ID.decode, j),
        typ: field("typ", optional(DType.decode), j),
        typeID: field("typeID", ID.decode, j),
        description: field("description", string, j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tlid: TLID.t,
    name: string,
    nameID: ID.t,
    parameters: list<Parameter.t>,
    returnType: DType.t,
    returnTypeID: ID.t,
    description: string,
    infix: bool,
    body: AST.t,
  }

  let encode = (f: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(f.tlid)),
      ("name", string(f.name)),
      ("nameID", ID.encode(f.nameID)),
      ("parameters", list(Parameter.encode, f.parameters)),
      ("returnType", DType.encode(f.returnType)),
      ("returnTypeID", ID.encode(f.returnTypeID)),
      ("description", string(f.description)),
      ("infix", bool(f.infix)),
      ("body", AST.encode(f.body)),
    })
  }
  let decode = (j): t => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      name: field("name", string, j),
      nameID: field("nameID", ID.decode, j),
      parameters: field("parameters", list(Parameter.decode), j),
      returnType: field("returnType", DType.decode, j),
      returnTypeID: field("returnTypeID", ID.decode, j),
      description: field("description", string, j),
      infix: field("infix", bool, j),
      body: field("body", AST.decode, j),
    }
  }
}

module UserType = {
  module RecordField = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: string,
      typ: option<DType.t>,
      nameID: ID.t,
      typeID: ID.t,
    }
    let encode = (f: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", string(f.name)),
        ("typ", nullable(DType.encode, f.typ)),
        ("nameID", ID.encode(f.nameID)),
        ("typeID", ID.encode(f.typeID)),
      })
    }

    let decode = j => {
      open Json.Decode
      {
        name: field("name", string, j),
        typ: field("typ", optional(DType.decode), j),
        nameID: field("nameID", ID.decode, j),
        typeID: field("typeID", ID.decode, j),
      }
    }
  }

  module Definition = {
    @ppx.deriving(show({with_path: false}))
    type rec t = Record(list<RecordField.t>)

    let encode = (d: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch d {
      | Record(fields) => ev("Record", list{list(RecordField.encode)(fields)})
      }
    }

    let decode = j => {
      open Json_decode_extended
      variants(list{("Record", variant1(x => Record(x), list(RecordField.decode)))}, j)
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tlid: TLID.t,
    name: string,
    nameID: ID.t,
    version: int,
    definition: Definition.t,
  }
  let encode = (t: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("tlid", TLID.encode(t.tlid)),
      ("name", string(t.name)),
      ("nameID", ID.encode(t.nameID)),
      ("version", int(t.version)),
      ("definition", Definition.encode(t.definition)),
    })
  }
  let decode = j => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      name: field("name", string, j),
      nameID: field("nameID", ID.decode, j),
      version: field("version", int, j),
      definition: field("definition", Definition.decode, j),
    }
  }
}

module Op = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | SetHandler(TLID.t, Pos.t, Handler.t)
    | CreateDB(TLID.t, Pos.t, string)
    | AddDBCol(TLID.t, ID.t, ID.t)
    | SetDBColName(TLID.t, ID.t, string)
    | SetDBColType(TLID.t, ID.t, string)
    | DeleteTL(TLID.t)
    | MoveTL(TLID.t, Pos.t)
    | SetFunction(UserFunction.t)
    | ChangeDBColName(TLID.t, ID.t, string)
    | ChangeDBColType(TLID.t, ID.t, string)
    | UndoTL(TLID.t)
    | RedoTL(TLID.t)
    | SetExpr(TLID.t, ID.t, Expr.t)
    | TLSavepoint(TLID.t)
    | DeleteFunction(TLID.t)
    | DeleteDBCol(TLID.t, ID.t)
    | RenameDBname(TLID.t, string)
    | CreateDBWithBlankOr(TLID.t, Pos.t, ID.t, string)
    | SetType(UserType.t)
    | DeleteType(TLID.t)

  let tlidOf = (op: t): TLID.t =>
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
    | SetFunction(f) => f.tlid
    | DeleteFunction(tlid) => tlid
    | SetExpr(tlid, _, _) => tlid
    | DeleteDBCol(tlid, _) => tlid
    | RenameDBname(tlid, _) => tlid
    | CreateDBWithBlankOr(tlid, _, _, _) => tlid
    | SetType(ut) => ut.tlid
    | DeleteType(tlid) => tlid
    }

  let encode = (op: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    let tlid = TLID.encode
    let id = ID.encode
    let pos = Pos.encode
    switch op {
    | SetHandler(t, p, h) => ev("SetHandler", list{tlid(t), pos(p), Handler.encode(h)})
    | CreateDB(t, p, name) => ev("CreateDB", list{tlid(t), pos(p), string(name)})
    | AddDBCol(t, cn, ct) => ev("AddDBCol", list{tlid(t), id(cn), id(ct)})
    | SetDBColName(t, i, name) => ev("SetDBColName", list{tlid(t), id(i), string(name)})
    | ChangeDBColName(t, i, name) => ev("ChangeDBColName", list{tlid(t), id(i), string(name)})
    | SetDBColType(t, i, typ) => ev("SetDBColType", list{tlid(t), id(i), string(typ)})
    | ChangeDBColType(t, i, name) => ev("ChangeDBColType", list{tlid(t), id(i), string(name)})
    | DeleteDBCol(t, i) => ev("DeleteDBCol", list{tlid(t), id(i)})
    | TLSavepoint(t) => ev("TLSavepoint", list{tlid(t)})
    | UndoTL(t) => ev("UndoTL", list{tlid(t)})
    | RedoTL(t) => ev("RedoTL", list{tlid(t)})
    | DeleteTL(t) => ev("DeleteTL", list{tlid(t)})
    | MoveTL(t, p) => ev("MoveTL", list{tlid(t), pos(p)})
    | SetFunction(uf) => ev("SetFunction", list{UserFunction.encode(uf)})
    | DeleteFunction(t) => ev("DeleteFunction", list{tlid(t)})
    | SetExpr(t, i, e) => ev("SetExpr", list{tlid(t), id(i), Expr.encode(e)})
    | RenameDBname(t, name) => ev("RenameDBname", list{tlid(t), string(name)})
    | CreateDBWithBlankOr(t, p, i, name) =>
      ev("CreateDBWithBlankOr", list{tlid(t), pos(p), id(i), string(name)})
    | SetType(t) => ev("SetType", list{UserType.encode(t)})
    | DeleteType(t) => ev("DeleteType", list{tlid(t)})
    }
  }
  let decode = (j): t => {
    open Json_decode_extended
    let tlid = TLID.decode
    let id = ID.decode
    let pos = Pos.decode
    variants(
      list{
        (
          "SetHandler",
          variant3((t, p, h) => SetHandler(t, p, {...h, pos: p}), tlid, pos, Handler.decode),
        ),
        ("CreateDB", variant3((t, p, name) => CreateDB(t, p, name), tlid, pos, string)),
        ("AddDBCol", variant3((t, cn, ct) => AddDBCol(t, cn, ct), tlid, id, id)),
        ("SetDBColName", variant3((t, i, name) => SetDBColName(t, i, name), tlid, id, string)),
        (
          "ChangeDBColName",
          variant3((t, i, name) => ChangeDBColName(t, i, name), tlid, id, string),
        ),
        ("SetDBColType", variant3((t, i, typ) => SetDBColType(t, i, typ), tlid, id, string)),
        ("ChangeDBColType", variant3((t, i, typ) => ChangeDBColType(t, i, typ), tlid, id, string)),
        ("DeleteDBCol", variant2((t, i) => DeleteDBCol(t, i), tlid, id)),
        ("TLSavepoint", variant1(t => TLSavepoint(t), tlid)),
        ("UndoTL", variant1(t => UndoTL(t), tlid)),
        ("RedoTL", variant1(t => RedoTL(t), tlid)),
        ("DeleteTL", variant1(t => DeleteTL(t), tlid)),
        ("MoveTL", variant2((t, p) => MoveTL(t, p), tlid, pos)),
        ("SetFunction", variant1(uf => SetFunction(uf), UserFunction.decode)),
        ("DeleteFunction", variant1(t => DeleteFunction(t), tlid)),
        ("SetExpr", variant3((t, i, e) => SetExpr(t, i, e), tlid, id, Expr.decode)),
        ("RenameDBname", variant2((t, name) => RenameDBname(t, name), tlid, string)),
        (
          "CreateDBWithBlankOr",
          variant4((t, p, i, name) => CreateDBWithBlankOr(t, p, i, name), tlid, pos, id, string),
        ),
        ("SetType", variant1(t => SetType(t), UserType.decode)),
        ("DeleteType", variant1(t => DeleteType(t), tlid)),
      },
      j,
    )
  }
}

module Package = {
  module Parameter = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: string,
      typ: DType.t,
      description: string,
    }
    let decode = (j: Js.Json.t): t => {
      open Json.Decode
      {
        name: field("name", string, j),
        typ: field("typ", DType.decode, j),
        description: field("description", string, j),
      }
    }
    let encode = (p: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", string(p.name)),
        ("typ", DType.encode(p.typ)),
        ("description", string(p.description)),
      })
    }
  }

  module Fn = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: FQFnName.PackageFnName.t,
      body: Expr.t,
      parameters: list<Parameter.t>,
      returnType: DType.t,
      description: string,
      author: string,
      deprecated: bool,
      tlid: TLID.t,
    }

    let decode = (j: Js.Json.t): t => {
      open Json.Decode
      {
        name: field("name", FQFnName.PackageFnName.decode, j),
        body: field("body", Expr.decode, j),
        parameters: field("parameters", list(Parameter.decode), j),
        returnType: field("returnType", DType.decode, j),
        description: field("description", string, j),
        author: field("author", string, j),
        deprecated: field("deprecated", bool, j),
        tlid: field("tlid", TLID.decode, j),
      }
    }

    let encode = (f: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", FQFnName.PackageFnName.encode(f.name)),
        ("body", Expr.encode(f.body)),
        ("parameters", list(Parameter.encode, f.parameters)),
        ("returnType", DType.encode(f.returnType)),
        ("description", string(f.description)),
        ("author", string(f.author)),
        ("deprecated", bool(f.deprecated)),
        ("tlid", TLID.encode(f.tlid)),
      })
    }
  }
}
