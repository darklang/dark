// The types that the user sees. For all type definitions, see ProgramTypes.fs

module FQFnName = {
  module StdlibFnName = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {module_: string, function: string, version: int}
    let encode = (n: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("module_", string(n.module_)),
        ("function_", string(n.function)),
        ("version", int(n.version)),
      })
    }
    let decode = j => {
      open Json.Decode
      {
        module_: field("module_", string, j),
        function: field("function_", string, j),
        version: field("version", int, j),
      }
    }
    let toString = (n: t): string => {
      let name = if n.module_ == "" {
        n.function
      } else {
        `${n.module_}::${n.function}`
      }
      if n.version == 0 {
        name
      } else {
        `${name}_v${string_of_int(n.version)}`
      }
    }
  }

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
    let toStdlib = (n: t): StdlibFnName.t => {
      function: n.function,
      version: 0,
      module_: switch n.module_ {
      | None => ""
      | Some(m) => m
      },
    }
  }

  module PackageFnName = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      owner: string,
      package: string,
      module_: string,
      function: string,
      version: int,
    }
    let encode = (n: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("owner", string(n.owner)),
        ("package", string(n.package)),
        ("module_", string(n.module_)),
        ("function_", string(n.function)),
        ("version", int(n.version)),
      })
    }
    let decode = j => {
      open Json.Decode
      {
        owner: field("owner", string, j),
        package: field("package", string, j),
        module_: field("module_", string, j),
        function: field("function_", string, j),
        version: field("version", int, j),
      }
    }
    let toString = (n: t): string =>
      `${n.owner}/${n.package}/${n.module_}::${n.function}_v${string_of_int(n.version)}`
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | User(string)
    | Stdlib(StdlibFnName.t)
    | Package(PackageFnName.t)

  let encode = (n: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch n {
    | User(name) => ev("User", list{string(name)})
    | Stdlib(name) => ev("Stdlib", list{StdlibFnName.encode(name)})
    | Package(name) => ev("Package", list{PackageFnName.encode(name)})
    }
  }

  let decode = (j): t => {
    open Json_decode_extended
    variants(
      list{
        ("User", variant1(name => User(name), string)),
        ("Stdlib", variant1(name => Stdlib(name), StdlibFnName.decode)),
        ("Package", variant1(name => Package(name), PackageFnName.decode)),
      },
      j,
    )
  }

  let toString = (n): string => {
    switch n {
    | User(name) => name
    | Stdlib(std) => StdlibFnName.toString(std)
    | Package(pkg) => PackageFnName.toString(pkg)
    }
  }

  let stdlib = (m: string, f: string, v: int): t => Stdlib({
    module_: m,
    function: f,
    version: v,
  })

  let package = (o: string, p: string, m: string, f: string, v: int): t => Package({
    owner: o,
    package: p,
    module_: m,
    function: f,
    version: v,
  })
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

module Pattern = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    // match id, then pattern id
    | PVariable(ID.t, string)
    | PConstructor(ID.t, string, list<t>)
    // TODO: support char
    | PInteger(ID.t, int64)
    | PBool(ID.t, bool)
    | PString(ID.t, string)
    | PFloat(ID.t, Sign.t, string, string)
    | PNull(ID.t)
    | PBlank(ID.t)

  let rec encode = (pattern: t): Js.Json.t => {
    open Json_encode_extended
    let ep = encode
    let ev = variant
    switch pattern {
    | PVariable(id', name) => ev("PVariable", list{ID.encode(id'), string(name)})
    | PConstructor(id', name, patterns) =>
      ev("PConstructor", list{ID.encode(id'), string(name), list(ep, patterns)})
    | PInteger(id', v) => ev("PInteger", list{ID.encode(id'), int64(v)})
    | PBool(id', v) => ev("PBool", list{ID.encode(id'), bool(v)})
    | PFloat(id', sign, whole, fraction) =>
      ev("PFloat", list{ID.encode(id'), Sign.encode(sign), string(whole), string(fraction)})
    | PString(id', v) => ev("PString", list{ID.encode(id'), string(v)})
    | PNull(id') => ev("PNull", list{ID.encode(id')})
    | PBlank(id') => ev("PBlank", list{ID.encode(id')})
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
        ("PVariable", dv2((a, b) => PVariable(a, b), ID.decode, string)),
        ("PConstructor", dv3((a, b, c) => PConstructor(a, b, c), ID.decode, string, list(decode))),
        ("PInteger", dv2((a, b) => PInteger(a, b), ID.decode, int64)),
        ("PBool", dv2((a, b) => PBool(a, b), ID.decode, bool)),
        ("PString", dv2((a, b) => PString(a, b), ID.decode, string)),
        ("PFloat", dv4((a, b, c, d) => PFloat(a, b, c, d), ID.decode, Sign.decode, string, string)),
        ("PNull", dv1(a => PNull(a), ID.decode)),
        ("PBlank", dv1(a => PBlank(a), ID.decode)),
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

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | EInteger(ID.t, int64)
    | EBool(ID.t, bool)
    | EString(ID.t, string)
    | EFloat(ID.t, Sign.t, string, string)
    | ENull(ID.t)
    | EBlank(ID.t)
    | ELet(ID.t, string, t, t)
    | EIf(ID.t, t, t, t)
    | EBinOp(ID.t, FQFnName.InfixStdlibFnName.t, t, t, SendToRail.t)
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
    | EMatch(ID.t, t, list<(Pattern.t, t)>)
    | EPipeTarget(ID.t)
    | EFeatureFlag(ID.t, string, t, t, t)

  let rec encode = (expr: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch expr {
    | ELet(id, lhs, rhs, body) =>
      ev("ELet", list{ID.encode(id), string(lhs), encode(rhs), encode(body)})
    | EIf(id', cond, ifbody, elsebody) =>
      ev("EIf", list{ID.encode(id'), encode(cond), encode(ifbody), encode(elsebody)})
    | EFnCall(id', name, exprs, r) =>
      ev(
        "EFnCall",
        list{ID.encode(id'), FQFnName.encode(name), list(encode, exprs), SendToRail.encode(r)},
      )
    | EBinOp(id', name, left, right, r) =>
      ev(
        "EBinOp",
        list{
          ID.encode(id'),
          FQFnName.InfixStdlibFnName.encode(name),
          encode(left),
          encode(right),
          SendToRail.encode(r),
        },
      )
    | ELambda(id, vars, body) =>
      ev("ELambda", list{ID.encode(id), list(pair(ID.encode, string), vars), encode(body)})
    | EPipe(id, e1, e2, rest) =>
      ev("EPipe", list{ID.encode(id), encode(e1), encode(e2), list(encode, rest)})
    | EFieldAccess(id, obj, field) =>
      ev("EFieldAccess", list{ID.encode(id), encode(obj), string(field)})
    | EString(id, v) => ev("EString", list{ID.encode(id), string(v)})
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
        list{ID.encode(id), encode(matchExpr), list(pair(Pattern.encode, encode), cases)},
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
        ("ELet", dv4((a, b, c, d) => ELet(a, b, c, d), ID.decode, string, de, de)),
        ("EIf", dv4((a, b, c, d) => EIf(a, b, c, d), ID.decode, de, de, de)),
        (
          "EBinOp",
          dv5(
            (a, b, c, d, e) => EBinOp(a, b, c, d, e),
            ID.decode,
            FQFnName.InfixStdlibFnName.decode,
            de,
            de,
            SendToRail.decode,
          ),
        ),
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
          dv3((a, b, c) => EMatch(a, b, c), ID.decode, de, list(pair(Pattern.decode, de))),
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
        | EveryDay => "EveryDay"
        | EveryWeek => "EveryWeek"
        | EveryFortnight => "EveryFortnight"
        | EveryHour => "EveryHour"
        | Every12Hours => "Every12Hours"
        | EveryMinute => "EveryMinute"
        }
      }
      let fromString = (i: string): option<t> => {
        switch i {
        | "EveryDay" => Some(EveryDay)
        | "EveryWeek" => Some(EveryWeek)
        | "EveryFortnight" => Some(EveryFortnight)
        | "EveryHour" => Some(EveryHour)
        | "Every12Hours" => Some(Every12Hours)
        | "EveryMinute" => Some(EveryMinute)
        | _ => None
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
      | Worker(name, ids) => ev("Worker", list{string(name), IDs.encode(ids)})
      | OldWorker(space, name, ids) =>
        ev("OldWorker", list{string(space), string(name), IDs.encode(ids)})
      | Cron(name, interval, ids) =>
        ev("HTTP", list{string(name), nullable(CronInterval.encode, interval), IDs.encode(ids)})
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
    let space = (spec: t): option<string> => {
      switch spec {
      | HTTP(_, _, _) => Some("HTTP")
      | Worker(_, _) => Some("WORKER")
      | OldWorker(space, _, _) => Some(space)
      | Cron(_, _, _) => Some("CRON")
      | REPL(_, _) => Some("REPL")
      | UnknownHandler(_, _, _) => None
      }
    }
    let name = (spec: t): string => {
      switch spec {
      | HTTP(name, _, _)
      | Worker(name, _)
      | OldWorker(_, name, _)
      | Cron(name, _, _)
      | REPL(name, _)
      | UnknownHandler(name, _, _) => name
      }
    }
    let modifier = (spec: t): option<string> => {
      switch spec {
      | HTTP(_, mod, _)
      | UnknownHandler(_, mod, _) =>
        Some(mod)
      | REPL(_, _)
      | Worker(_, _)
      | OldWorker(_, _, _) =>
        None
      | Cron(_, interval, _) => interval |> Tc.Option.map(~f=CronInterval.toString)
      }
    }
    let ids = (spec: t): IDs.t => {
      switch spec {
      | HTTP(_, _, ids)
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
      ("spec", Spec.encode(h.spec)),
      ("ast", AST.encode(h.ast)),
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
    type rec t = (BlankOr.t<string>, BlankOr.t<string>)

    let encode = (col: t): Js.Json.t => {
      open Json.Encode
      pair(BlankOr.encode(string), BlankOr.encode(string), col)
    }

    let decode = (j): t => {
      open Json.Decode
      // CLEANUP: this is really ugly. Copied from Prelude. We should have a DType here, not a string
      let rec tipe2str = (t: DType.t): string =>
        switch t {
        | TAny => "Any"
        | TInt => "Int"
        | TFloat => "Float"
        | TBool => "Bool"
        | TChar => "Char"
        | TNull => "Null"
        | TCharacter => "Character"
        | TStr => "String"
        | TList => "List"
        | TTuple(_, _, _) => "Tuple"
        | TObj => "Dict"
        | TBlock => "Block"
        | TIncomplete => "Incomplete"
        | TError => "Error"
        | TResp => "Response"
        | TDB => "Datastore"
        | TDate => "Date"
        | TOption => "Option"
        | TPassword => "Password"
        | TUuid => "UUID"
        | TErrorRail => "ErrorRail"
        | TResult => "Result"
        | TDbList(a) => "[" ++ (tipe2str(a) ++ "]")
        | TUserType(name, _) => name
        | TBytes => "Bytes"
        }

      let tipeString = (j): string => map(tipe2str, DType.decode, j)
      tuple2(BlankOr.decode(string), BlankOr.decode(tipeString), j)
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
      ("cols", list(Col.encode, db.cols)),
      ("version", int(db.version)),
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
      name: BlankOr.t<string>,
      typ: BlankOr.t<DType.t>,
      args: list<string>,
      optional: bool,
      description: string,
    }
    let encode = (p: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BlankOr.encode(string, p.name)),
        ("tipe", BlankOr.encode(DType.encode, p.typ)),
        ("block_args", list(string, p.args)),
        ("optional", bool(p.optional)),
        ("description", string(p.description)),
      })
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        name: field("name", BlankOr.decode(string), j),
        typ: field("tipe", BlankOr.decode(DType.decode), j),
        args: field("block_args", list(string), j),
        optional: field("optional", bool, j),
        description: field("description", string, j),
      }
    }
  }
  module Metadata = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: BlankOr.t<string>,
      parameters: list<Parameter.t>,
      description: string,
      returnType: BlankOr.t<DType.t>,
      infix: bool,
    }
    let encode = (f: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BlankOr.encode(string, f.name)),
        ("parameters", list(Parameter.encode, f.parameters)),
        ("description", string(f.description)),
        ("return_type", BlankOr.encode(DType.encode, f.returnType)),
        ("infix", bool(f.infix)),
      })
    }

    let decode = (j): t => {
      open Json_decode_extended
      {
        name: field("name", BlankOr.decode(string), j),
        parameters: field("parameters", list(Parameter.decode), j),
        description: field("description", string, j),
        returnType: field("return_type", BlankOr.decode(DType.decode), j),
        infix: field("infix", bool, j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tlid: TLID.t,
    metadata: Metadata.t,
    ast: AST.t,
  }

  let encode = (uf: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(uf.tlid)),
      ("metadata", Metadata.encode(uf.metadata)),
      ("ast", AST.encode(uf.ast)),
    })
  }
  let decode = (j): t => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      metadata: field("metadata", Metadata.decode, j),
      ast: field("ast", AST.decode, j),
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
    type rec t = UTRecord(list<RecordField.t>)

    let encode = (d: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch d {
      | UTRecord(fields) => ev("UTRecord", list{list(RecordField.encode)(fields)})
      }
    }

    let decode = j => {
      open Json_decode_extended
      variants(list{("UTRecord", variant1(x => UTRecord(x), list(RecordField.decode)))}, j)
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tlid: TLID.t,
    name: BlankOr.t<string>,
    version: int,
    definition: Definition.t,
  }
  let encode = (t: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("tlid", TLID.encode(t.tlid)),
      ("name", BlankOr.encode(string, t.name)),
      ("version", int(t.version)),
      ("definition", Definition.encode(t.definition)),
    })
  }
  let decode = j => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      name: field("name", BlankOr.decode(string), j),
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
    | SetDBColType(t, i, tipe) => ev("SetDBColType", list{tlid(t), id(i), string(tipe)})
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
        ("SetDBColType", variant3((t, i, tipe) => SetDBColType(t, i, tipe), tlid, id, string)),
        (
          "ChangeDBColType",
          variant3((t, i, tipe) => ChangeDBColName(t, i, tipe), tlid, id, string),
        ),
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
      tipe: DType.t,
      description: string,
    }
    let decode = (j: Js.Json.t): t => {
      open Json.Decode
      {
        name: field("name", string, j),
        tipe: field("tipe", DType.decode, j),
        description: field("description", string, j),
      }
    }
    let encode = (pfp: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", string(pfp.name)),
        ("tipe", DType.encode(pfp.tipe)),
        ("description", string(pfp.description)),
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
  }
}
