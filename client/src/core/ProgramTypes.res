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
      ufpName: blankOr<string>,
      ufpTipe: blankOr<DType.t>,
      ufpBlock_args: list<string>,
      ufpOptional: bool,
      ufpDescription: string,
    }
    let encode = (p: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BaseTypes.encodeBlankOr(string, p.ufpName)),
        ("tipe", BaseTypes.encodeBlankOr(DType.encode, p.ufpTipe)),
        ("block_args", list(string, p.ufpBlock_args)),
        ("optional", bool(p.ufpOptional)),
        ("description", string(p.ufpDescription)),
      })
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        ufpName: field("name", BaseTypes.decodeBlankOr(string), j),
        ufpTipe: field("tipe", BaseTypes.decodeBlankOr(DType.decodeOld), j),
        ufpBlock_args: field("block_args", list(string), j),
        ufpOptional: field("optional", bool, j),
        ufpDescription: field("description", string, j),
      }
    }
  }
  module Metadata = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      ufmName: blankOr<string>,
      ufmParameters: list<Parameter.t>,
      ufmDescription: string,
      ufmReturnTipe: blankOr<DType.t>,
      ufmInfix: bool,
    }
    let encode = (f: t): Js.Json.t => {
      open Json.Encode
      object_(list{
        ("name", BaseTypes.encodeBlankOr(string, f.ufmName)),
        ("parameters", list(Parameter.encode, f.ufmParameters)),
        ("description", string(f.ufmDescription)),
        ("return_type", BaseTypes.encodeBlankOr(DType.encode, f.ufmReturnTipe)),
        ("infix", bool(f.ufmInfix)),
      })
    }

    let decode = (j): t => {
      open Json_decode_extended
      {
        ufmName: field("name", BaseTypes.decodeBlankOr(string), j),
        ufmParameters: field("parameters", list(Parameter.decode), j),
        ufmDescription: field("description", string, j),
        ufmReturnTipe: field("return_type", BaseTypes.decodeBlankOr(DType.decodeOld), j),
        ufmInfix: field("infix", bool, j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    ufTLID: TLID.t,
    ufMetadata: Metadata.t,
    ufAST: AST.t,
  }

  let encode = (uf: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("tlid", TLID.encode(uf.ufTLID)),
      ("metadata", Metadata.encode(uf.ufMetadata)),
      ("ast", AST.encode(uf.ufAST)),
    })
  }
  let decode = (j): t => {
    open Json.Decode
    {
      ufTLID: field("tlid", TLID.decode, j),
      ufMetadata: field("metadata", Metadata.decode, j),
      ufAST: field("ast", AST.decode, j),
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
