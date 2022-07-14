// The types that are used for execution. For all type definitions, see RuntimeTypes.fs

// CLEANUP: these are just here to allow deriving(show) to work on Belt types. In
// theory we'd move these out, but in practice we actually just need to get rid of deriving(show)
module Belt = {
  include (Belt: module type of Belt with module Map := Belt.Map and module Result := Belt.Result)

  module Result = {
    include Belt.Result

    let pp = (
      okValueFormatter: (Format.formatter, 'okValue) => unit,
      errValueFormatter: (Format.formatter, 'errValue) => unit,
      fmt: Format.formatter,
      value: t<'okValue, 'errValue>,
    ) => {
      switch value {
      | Ok(value) =>
        Format.pp_print_string(fmt, "Ok")
        okValueFormatter(fmt, value)
      | Error(value) =>
        Format.pp_print_string(fmt, "Error")
        errValueFormatter(fmt, value)
      }
    }
  }

  module Map = {
    include (Belt.Map: module type of Belt.Map with module String := Belt.Map.String)

    module String = {
      include Belt.Map.String

      let pp = (
        valueFormatter: (Format.formatter, 'value) => unit,
        fmt: Format.formatter,
        map: t<'value>,
      ) => {
        Format.pp_print_string(fmt, "{ ")
        Belt.Map.String.forEach(map, (key, value) => {
          Format.pp_print_string(fmt, key)
          Format.pp_print_string(fmt, ": ")
          valueFormatter(fmt, value)
          Format.pp_print_string(fmt, ",  ")
        })
        Format.pp_print_string(fmt, "}")
        ()
      }
    }
  }
}

@ppx.deriving(show({with_path: false})) type rec id = ID.t
@ppx.deriving(show({with_path: false})) type rec tlid = TLID.t

module FQFnName = {
  module StdlibFnName = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {module_: string, function: string, version: int}

    let toString = (std: t) => {
      let name = if std.module_ == "" {
        std.function
      } else {
        `${std.module_}::${std.function}`
      }
      if std.version == 0 {
        name
      } else {
        `${name}_v${Belt.Int.toString(std.version)}`
      }
    }

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      {
        // Note: module has no underscore in the API
        module_: field("module", string, j),
        function: field("function", string, j),
        version: field("version", int, j),
      }
    }
  }

  module UserFnName = {
    @ppx.deriving(show({with_path: false})) type rec t = string
    let decode = Json_decode_extended.string
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

    let toString = (pkg: t): string =>
      `${pkg.owner}/${pkg.package}/${pkg.module_}::${pkg.function}_v${Belt.Int.toString(
          pkg.version,
        )}`

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      {
        owner: field("owner", string, j),
        package: field("package", string, j),
        // Note: module has no underscore in the API
        module_: field("module", string, j),
        function: field("function", string, j),
        version: field("version", int, j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | User(UserFnName.t)
    | Stdlib(StdlibFnName.t)
    | Package(PackageFnName.t)

  let toString = (fqfnName: t): string =>
    switch fqfnName {
    | User(name) => name
    | Stdlib(std) => StdlibFnName.toString(std)
    | Package(pkg) => PackageFnName.toString(pkg)
    }
  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    variants(
      list{
        ("User", variant1(name => User(name), UserFnName.decode)),
        ("Stdlib", variant1(name => Stdlib(name), StdlibFnName.decode)),
        ("Package", variant1(name => Package(name), PackageFnName.decode)),
      },
      j,
    )
  }
}

module Pattern = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | PVariable(id, string)
    | PConstructor(id, string, list<t>)
    // TODO: support char
    | PInteger(id, int64)
    | PBool(id, bool)
    | PString(id, string)
    | PFloat(id, float)
    | PNull(id)
    | PBlank(id)
}

module Expr = {
  @ppx.deriving(show({with_path: false}))
  type rec sendToRail =
    | Rail
    | NoRail

  @ppx.deriving(show({with_path: false}))
  type rec isInPipe =
    | InPipe(id)
    | NotInPipe

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | EInteger(id, int64)
    | EBool(id, bool)
    | EString(id, string)
    // | ECharacter(id, string)
    | EFloat(id, float)
    | ENull(id)
    | EBlank(id)
    | ELet(id, string, t, t)
    | EIf(id, t, t, t)
    | ELambda(id, list<(id, string)>, t)
    | EFieldAccess(id, t, string)
    | EVariable(id, string)
    | EApply(id, t, list<t>, isInPipe, sendToRail)
    | EFQFnValue(id, FQFnName.t)
    | EList(id, list<t>)
    | ETuple(id, t, t, list<t>)
    | ERecord(id, list<(string, t)>)
    | EConstructor(id, string, list<t>)
    | EMatch(id, t, list<(Pattern.t, t)>)
    | EFeatureFlag(id, t, t, t)
}

module Dval = {
  @ppx.deriving(show({with_path: false}))
  type rec dvalSource =
    | SourceNone
    | SourceID(TLID.t, ID.t)

  @ppx.deriving(show({with_path: false}))
  and lambdaImpl = {
    parameters: list<(ID.t, string)>,
    symtable: Belt.Map.String.t<t>,
    body: Expr.t,
  }

  @ppx.deriving(show({with_path: false}))
  and dHttp =
    | Redirect(string)
    | Response(int64, list<(string, string)>, t)

  @ppx.deriving(show({with_path: false}))
  and fnValImpl =
    | Lambda(lambdaImpl)
    | FnName(FQFnName.t)

  @ppx.deriving(show({with_path: false}))
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
    | DError((dvalSource, string))
    | DIncomplete(dvalSource)
    | DErrorRail(t)
    | DHttpResponse(dHttp)
    | DDB(string)
    | DDate(string)
    | DPassword(string)
    | DUuid(string)
    | DOption(option<t>)
    | DResult(Belt.Result.t<t, t>)
    | DBytes(bytes)
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
        typ: field("type", DType.decodeNew, j), // Note: "type", not "typ"
        args: field("args", list(string), j),
        description: field("description", string, j),
      }
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
      returnType: field("returnType", DType.decodeNew, j),
      previewable: field("previewable", Previewable.decode, j),
      deprecated: field("deprecated", Deprecation.decode, j),
      isInfix: field("isInfix", bool, j),
      sqlSpec: field("sqlSpec", SqlSpec.decode, j),
    }
  }
}
