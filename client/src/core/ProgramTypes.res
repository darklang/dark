// The types that the user sees. For all type definitions, see ProgramTypes.fs

open BaseTypes

module FQFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec stdlibFnName = {module_: string, function: string, version: int}

  @ppx.deriving(show({with_path: false}))
  type rec infixStdlibFnName = {module_: option<string>, function: string}

  @ppx.deriving(show({with_path: false})) type rec userFnName = string

  @ppx.deriving(show({with_path: false}))
  type rec packageFnName = {
    owner: string,
    package: string,
    module_: string,
    function: string,
    version: int,
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | User(userFnName)
    | Stdlib(stdlibFnName)
    | Package(packageFnName)
}

@ppx.deriving(show({with_path: false}))
type rec sign =
  | Positive
  | Negative

module Sign = {
  let toString = (sign: sign): string =>
    switch sign {
    | Positive => ""
    | Negative => "-"
    }
  // Split the string into a sign and a string (removes the sign if present and )
  let split = (whole: string): (sign, string) => {
    if Tc.String.startsWith(~prefix="-", whole) {
      (Negative, Tc.String.dropLeft(~count=1, whole))
    } else if Tc.String.startsWith(~prefix="+", whole) {
      (Positive, Tc.String.dropLeft(~count=1, whole))
    } else {
      (Positive, whole)
    }
  }
  let combine = (sign: sign, whole: string): string => {
    toString(sign) ++ whole
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
    | PFloat(ID.t, sign, string, string)
    | PNull(ID.t)
    | PBlank(ID.t)
    // should this be `t` or `list<t>`
    // (lists are currently heterogeneous)
    | PList(ID.t, list<t>)

  let rec encode = (mid: ID.t, pattern: t): Js.Json.t => {
    open Json_encode_extended
    let fp = encode(mid)
    let ev = variant
    switch pattern {
    /* Warning: A bunch of stuff here seems to expect that the
    second element of the tuples are match id but they are actually
    pattern ids. */
    | PVariable(id', name) => ev("FPVariable", list{ID.encode(mid), ID.encode(id'), string(name)})
    | PConstructor(id', name, patterns) =>
      ev("FPConstructor", list{ID.encode(mid), ID.encode(id'), string(name), list(fp, patterns)})
    | PInteger(id', v) =>
      ev("FPInteger", list{ID.encode(mid), ID.encode(id'), string(Int64.to_string(v))})
    | PBool(id', v) => ev("FPBool", list{ID.encode(mid), ID.encode(id'), bool(v)})
    | PFloat(id', sign, whole, fraction) =>
      ev(
        "FPFloat",
        list{ID.encode(mid), ID.encode(id'), string(Sign.combine(sign, whole)), string(fraction)},
      )
    | PString(id', v) =>
      ev(
        "FPString",
        list{
          object_(list{
            ("matchID", ID.encode(mid)),
            ("patternID", ID.encode(id')),
            ("str", string(v)),
          }),
        },
      )
    | PNull(id') => ev("FPNull", list{ID.encode(mid), ID.encode(id')})
    | PBlank(id') => ev("FPBlank", list{ID.encode(mid), ID.encode(id')})
    | PList(id', patterns) => ev("FPList", list{ID.encode(mid), ID.encode(id'), list(fp, patterns)})
    }
  }

  let rec decode = (j): t => {
    open Json_decode_extended
    let dp = decode
    let dv4 = variant4
    let dv3 = variant3
    let dv2 = variant2
    variants(
      list{
        ("FPVariable", dv3((_, b, c) => PVariable(b, c), ID.decode, ID.decode, string)),
        (
          "FPConstructor",
          dv4((_, b, c, d) => PConstructor(b, c, d), ID.decode, ID.decode, string, list(dp)),
        ),
        ("FPList", dv3((_, b, c) => PList(b, c), ID.decode, ID.decode, list(dp))),
        (
          "FPInteger",
          dv3(
            (_, b, c) => PInteger(b, c),
            ID.decode,
            ID.decode,
            i => i |> string |> Int64.of_string,
          ),
        ),
        ("FPBool", dv3((_, b, c) => PBool(b, c), ID.decode, ID.decode, bool)),
        (
          "FPString",
          recordVariant3(
            (_, patternID, str) => PString(patternID, str),
            ("matchID", ID.decode),
            ("patternID", ID.decode),
            ("str", string),
          ),
        ),
        ("FPFloat", dv4((_, id2, whole, fraction) => {
            let (sign, whole) = if Tc.String.startsWith(~prefix="-", whole) {
              (Negative, Tc.String.dropLeft(~count=1, whole))
            } else {
              (Positive, whole)
            }
            PFloat(id2, sign, whole, fraction)
          }, ID.decode, ID.decode, string, string)),
        ("FPNull", dv2((_, b) => PNull(b), ID.decode, ID.decode)),
        ("FPBlank", dv2((_, b) => PBlank(b), ID.decode, ID.decode)),

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
    | EFloat(ID.t, sign, string, string)
    | ENull(ID.t)
    | EBlank(ID.t)
    | ELet(ID.t, string, t, t)
    | EIf(ID.t, t, t, t)
    | EBinOp(ID.t, string, t, t, SendToRail.t)
    | ELambda(ID.t, list<(ID.t, string)>, t)
    | EFieldAccess(ID.t, t, string)
    | EVariable(ID.t, string)
    | EFnCall(ID.t, string, list<t>, SendToRail.t)
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
    let fe = encode
    let ev = variant
    switch expr {
    | ELet(id', lhs, rhs, body) => ev("ELet", list{ID.encode(id'), string(lhs), fe(rhs), fe(body)})
    | EIf(id', cond, ifbody, elsebody) =>
      ev("EIf", list{ID.encode(id'), fe(cond), fe(ifbody), fe(elsebody)})
    | EFnCall(id', name, exprs, r) =>
      ev("EFnCall", list{ID.encode(id'), string(name), list(fe, exprs), SendToRail.encode(r)})
    | EBinOp(id', name, left, right, r) =>
      ev("EBinOp", list{ID.encode(id'), string(name), fe(left), fe(right), SendToRail.encode(r)})
    | ELambda(id', vars, body) =>
      ev("ELambda", list{ID.encode(id'), list(pair(ID.encode, string), vars), fe(body)})
    | EPipe(id', e1, e2, rest) => ev("EPipe", list{ID.encode(id'), list(fe, list{e1, e2, ...rest})})
    | EFieldAccess(id', obj, field) =>
      ev("EFieldAccess", list{ID.encode(id'), fe(obj), string(field)})
    | EString(id', v) => ev("EString", list{ID.encode(id'), string(v)})
    | EInteger(id', v) => ev("EInteger", list{ID.encode(id'), string(Int64.to_string(v))})
    | EBool(id', v) => ev("EBool", list{ID.encode(id'), bool(v)})
    | EFloat(id', sign, whole, fraction) =>
      ev("EFloat", list{ID.encode(id'), string(Sign.combine(sign, whole)), string(fraction)})
    | ENull(id') => ev("ENull", list{ID.encode(id')})
    | EBlank(id') => ev("EBlank", list{ID.encode(id')})
    | EVariable(id', name) => ev("EVariable", list{ID.encode(id'), string(name)})
    | EList(id', exprs) => ev("EList", list{ID.encode(id'), list(fe, exprs)})
    | ETuple(id', first, second, theRest) =>
      ev("ETuple", list{ID.encode(id'), fe(first), fe(second), list(fe, theRest)})
    | ERecord(id', pairs) => ev("ERecord", list{ID.encode(id'), list(pair(string, fe), pairs)})
    | EFeatureFlag(id', name, cond, a, b) =>
      ev("EFeatureFlag", list{ID.encode(id'), string(name), fe(cond), fe(a), fe(b)})
    | EMatch(id', matchExpr, cases) =>
      ev("EMatch", list{ID.encode(id'), fe(matchExpr), list(pair(Pattern.encode(id'), fe), cases)})
    | EConstructor(id', name, args) =>
      ev("EConstructor", list{ID.encode(id'), string(name), list(fe, args)})
    | EPartial(id', str, oldExpr) => ev("EPartial", list{ID.encode(id'), string(str), fe(oldExpr)})
    | ERightPartial(id', str, oldExpr) =>
      ev("ERightPartial", list{ID.encode(id'), string(str), fe(oldExpr)})
    | ELeftPartial(id', str, oldExpr) =>
      ev("ELeftPartial", list{ID.encode(id'), string(str), fe(oldExpr)})
    | EPipeTarget(id') => ev("EPipeTarget", list{ID.encode(id')})
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
        ("EFloat", dv3((x, whole, fraction) => {
            let (sign, whole) = if Tc.String.startsWith(~prefix="-", whole) {
              (Negative, Tc.String.dropLeft(~count=1, whole))
            } else {
              (Positive, whole)
            }
            EFloat(x, sign, whole, fraction)
          }, ID.decode, string, string)),
        ("ENull", dv1(x => ENull(x), ID.decode)),
        ("EBlank", dv1(x => EBlank(x), ID.decode)),
        ("ELet", dv4((a, b, c, d) => ELet(a, b, c, d), ID.decode, string, de, de)),
        ("EIf", dv4((a, b, c, d) => EIf(a, b, c, d), ID.decode, de, de, de)),
        (
          "EBinOp",
          dv5(
            (a, b, c, d, e) => EBinOp(a, b, c, d, e),
            ID.decode,
            string,
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
          dv4((a, b, c, d) => EFnCall(a, b, c, d), ID.decode, string, list(de), SendToRail.decode),
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
        ("EPipe", dv2((x, y) =>
            switch y {
            | list{} =>
              let (e1, e2) = Recover.recover(
                "decoding a pipe with no exprs",
                ~debug=x,
                (EBlank(ID.generate()), EBlank(ID.generate())),
              )
              EPipe(x, e1, e2, list{})
            | list{e1} =>
              let e2 = Recover.recover(
                "decoding a pipe with only one expr",
                ~debug=x,
                EBlank(ID.generate()),
              )
              EPipe(x, e1, e2, list{})
            | list{e1, e2, ...rest} => EPipe(x, e1, e2, rest)
            }
          , ID.decode, list(de))),
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
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      space: blankOr<string>,
      name: blankOr<string>,
      modifier: blankOr<string>,
    }

    let encode = (spec: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BaseTypes.encodeBlankOr(string, spec.name)),
        ("module", BaseTypes.encodeBlankOr(string, spec.space)),
        ("modifier", BaseTypes.encodeBlankOr(string, spec.modifier)),
        (
          "types",
          object_(list{
            ("input", BaseTypes.encodeBlankOr(int, BaseTypes.Blank(ID.generate()))),
            ("output", BaseTypes.encodeBlankOr(int, BaseTypes.Blank(ID.generate()))),
          }),
        ),
      })
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        space: field("module", BaseTypes.decodeBlankOr(string), j),
        name: field("name", BaseTypes.decodeBlankOr(string), j),
        modifier: field("modifier", BaseTypes.decodeBlankOr(string), j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    ast: AST.t,
    spec: Spec.t,
    tlid: TLID.t,
    pos: pos,
  }
  let encode = (h: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(h.tlid)),
      ("spec", Spec.encode(h.spec)),
      ("ast", AST.encode(h.ast)),
    })
  }

  let decode = (pos, j): t => {
    open Json.Decode
    {
      ast: field("ast", AST.decode, j),
      spec: field("spec", Spec.decode, j),
      tlid: field("tlid", TLID.decode, j),
      pos: pos,
    }
  }
}

module DB = {
  module Col = {
    @ppx.deriving(show({with_path: false}))
    type rec t = (blankOr<string>, blankOr<string>)

    let encode = (col: t): Js.Json.t => {
      open Json.Encode
      pair(BaseTypes.encodeBlankOr(string), BaseTypes.encodeBlankOr(string), col)
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

      let tipeString = (j): string => map(tipe2str, DType.decodeOld, j)
      tuple2(BaseTypes.decodeBlankOr(string), BaseTypes.decodeBlankOr(tipeString), j)
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tlid: TLID.t,
    name: blankOr<string>,
    cols: list<Col.t>,
    version: int,
    pos: pos,
  }

  let encode = (db: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(db.tlid)),
      ("name", BaseTypes.encodeBlankOr(string, db.name)),
      ("cols", list(Col.encode, db.cols)),
      ("version", int(db.version)),
      ("old_migrations", list(int, list{})),
      ("active_migration", null),
    })
  }
  let decode = (pos, j): t => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      name: field("name", BaseTypes.decodeBlankOr(string), j),
      cols: field("cols", list(Col.decode), j),
      version: field("version", int, j),
      pos: pos,
    }
  }
}

module UserFunction = {
  module Parameter = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: blankOr<string>,
      typ: blankOr<DType.t>,
      args: list<string>,
      optional: bool,
      description: string,
    }
    let encode = (p: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BaseTypes.encodeBlankOr(string, p.name)),
        ("tipe", BaseTypes.encodeBlankOr(DType.encode, p.typ)),
        ("block_args", list(string, p.args)),
        ("optional", bool(p.optional)),
        ("description", string(p.description)),
      })
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        name: field("name", BaseTypes.decodeBlankOr(string), j),
        typ: field("tipe", BaseTypes.decodeBlankOr(DType.decodeOld), j),
        args: field("block_args", list(string), j),
        optional: field("optional", bool, j),
        description: field("description", string, j),
      }
    }
  }
  module Metadata = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: blankOr<string>,
      parameters: list<Parameter.t>,
      description: string,
      returnType: blankOr<DType.t>,
      infix: bool,
    }
    let encode = (f: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BaseTypes.encodeBlankOr(string, f.name)),
        ("parameters", list(Parameter.encode, f.parameters)),
        ("description", string(f.description)),
        ("return_type", BaseTypes.encodeBlankOr(DType.encode, f.returnType)),
        ("infix", bool(f.infix)),
      })
    }

    let decode = (j): t => {
      open Json_decode_extended
      {
        name: field("name", BaseTypes.decodeBlankOr(string), j),
        parameters: field("parameters", list(Parameter.decode), j),
        description: field("description", string, j),
        returnType: field("return_type", BaseTypes.decodeBlankOr(DType.decodeOld), j),
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
      name: blankOr<string>,
      typ: blankOr<DType.t>,
    }
    let encode = (f: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BaseTypes.encodeBlankOr(string, f.name)),
        ("tipe", BaseTypes.encodeBlankOr(DType.encode, f.typ)),
      })
    }

    let decode = j => {
      open Json.Decode
      {
        name: field("name", BaseTypes.decodeBlankOr(string), j),
        typ: field("tipe", BaseTypes.decodeBlankOr(DType.decodeOld), j),
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
    name: blankOr<string>,
    version: int,
    definition: Definition.t,
  }
  let encode = (t: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("tlid", TLID.encode(t.tlid)),
      ("name", BaseTypes.encodeBlankOr(string, t.name)),
      ("version", int(t.version)),
      ("definition", Definition.encode(t.definition)),
    })
  }
  let decode = j => {
    open Json.Decode
    {
      tlid: field("tlid", TLID.decode, j),
      name: field("name", BaseTypes.decodeBlankOr(string), j),
      version: field("version", int, j),
      definition: field("definition", Definition.decode, j),
    }
  }
}

module Op = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | SetHandler(TLID.t, pos, Handler.t)
    | CreateDB(TLID.t, pos, string)
    | AddDBCol(TLID.t, ID.t, ID.t)
    | SetDBColName(TLID.t, ID.t, string)
    | SetDBColType(TLID.t, ID.t, string)
    | DeleteTL(TLID.t)
    | MoveTL(TLID.t, pos)
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
    | CreateDBWithBlankOr(TLID.t, pos, ID.t, string)
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
    let pos = BaseTypes.encodePos
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
    let pos = BaseTypes.decodePos
    variants(
      list{
        (
          "SetHandler",
          variant3(
            (t, p, h) => SetHandler(t, p, {...h, pos: p}),
            tlid,
            pos,
            Handler.decode({x: -1286, y: -467}),
          ),
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
