// The types that are used for execution. For all type definitions, see RuntimeTypes.fs

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

@ppx.deriving(show) type rec id = ID.t
@ppx.deriving(show) type rec tlid = TLID.t

module FQFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec stdlibFnName = {module_: string, function_: string, version: int}

  @ppx.deriving(show({with_path: false})) type rec userFnName = string

  @ppx.deriving(show({with_path: false}))
  type rec packageFnName = {
    owner: string,
    package: string,
    module_: string,
    function_: string,
    version: int,
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | User(userFnName)
    | Stdlib(stdlibFnName)
    | Package(packageFnName)
}

@ppx.deriving(show({with_path: false}))
type rec isInPipe =
  | InPipe(id)
  | NotInPipe

@ppx.deriving(show({with_path: false}))
type rec pattern =
  | PVariable(id, string)
  | PConstructor(id, string, list<pattern>)
  // TODO: support char
  | PInteger(id, int64)
  | PBool(id, bool)
  | PString(id, string)
  | PFloat(id, float)
  | PNull(id)
  | PBlank(id)

and sendToRail =
  | Rail
  | NoRail

and expr =
  | EInteger(id, int64)
  | EBool(id, bool)
  | EString(id, string)
  // | ECharacter(id, string)
  | EFloat(id, float)
  | ENull(id)
  | EBlank(id)
  | ELet(id, string, expr, expr)
  | EIf(id, expr, expr, expr)
  | ELambda(id, list<(id, string)>, expr)
  | EFieldAccess(id, expr, string)
  | EVariable(id, string)
  | EApply(id, expr, list<expr>, isInPipe, sendToRail)
  | EFQFnValue(id, FQFnName.t)
  | EList(id, list<expr>)
  | ERecord(id, list<(string, expr)>)
  | EConstructor(id, string, list<expr>)
  | EMatch(id, expr, list<(pattern, expr)>)
  | EFeatureFlag(id, expr, expr, expr)

@ppx.deriving(show({with_path: false}))
type rec dtype =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList
  | TObj
  | TIncomplete
  | TError
  | TResp
  | TDB
  | TDate
  | TCharacter
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TUserType(string, int)
  | TBytes
  | TResult
  | TAny
  | TBlock
  | TDbList(dtype)

@ppx.deriving(show({with_path: false}))
type rec dvalSource =
  | FSourceNone
  | FSourceID(TLID.t, ID.t)

@ppx.deriving(show({with_path: false}))
and lambdaImpl = {
  parameters: list<(ID.t, string)>,
  symtable: Belt.Map.String.t<dval>,
  body: expr,
}

@ppx.deriving(show({with_path: false}))
and dHttp =
  | Redirect(string)
  | Response(int64, list<(string, string)>, dval)

@ppx.deriving(show({with_path: false}))
and fnValImpl =
  | Lambda(lambdaImpl)
  | FnName(ProgramTypes.FQFnName.t)

@ppx.deriving(show({with_path: false}))
and dval =
  | DInt(int64)
  | DFloat(float)
  | DBool(bool)
  | DNull
  | DStr(string)
  | DChar(string)
  | DList(list<dval>)
  /* We use Belt.Map.String as Map.String.t has a comparator that doesn't work
   with the cloning algorithm of web workers */
  | DObj(Belt.Map.String.t<dval>)
  | DFnVal(fnValImpl)
  | DError((dvalSource, string))
  | DIncomplete(dvalSource)
  | DErrorRail(dval)
  | DHttpResponse(dHttp)
  | DDB(string)
  | DDate(string)
  | DPassword(string)
  | DUuid(string)
  | DOption(option<dval>)
  | DResult(Belt.Result.t<dval, dval>)
  | DBytes(bytes)
