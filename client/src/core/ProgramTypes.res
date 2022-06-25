// The types that the user sees. For all type definitions, see ProgramTypes.fs

@ppx.deriving(show) type rec id = ID.t
@ppx.deriving(show) type rec tlid = TLID.t

module FQFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec stdlibFnName = {module_: string, function_: string, version: int}

  @ppx.deriving(show({with_path: false}))
  type rec infixStdlibFnName = {module_: option<string>, function_: string}

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
    | PVariable(id, id, string)
    | PConstructor(id, id, string, list<t>)
    // TODO: support char
    // Currently we support u62s; we will support s63s. ints in Bucklescript only support 32 bit ints but we want 63 bit int support
    | PInteger(id, id, string)
    | PBool(id, id, bool)
    | PString({matchID: id, patternID: id, str: string})
    | PFloat(id, id, sign, string, string)
    | PNull(id, id)
    | PBlank(id, id)
}

module Expr = {
  @ppx.deriving(show({with_path: false}))
  type rec sendToRail =
    | Rail
    | NoRail

  // CLEANUP: move comments to LibExecution.ProgramTypes
  @ppx.deriving(show({with_path: false}))
  type rec t =
    // ints in Bucklescript only support 32 bit ints but we want 63 bit int
    // support
    | EInteger(id, string)
    | EBool(id, bool)
    | EString(id, string)
    | EFloat(id, sign, string, string)
    | ENull(id)
    | EBlank(id)
    | ELet(id, string, t, t)
    | EIf(id, t, t, t)
    | EBinOp(id, string, t, t, sendToRail)
    | ELambda(id, list<(id, string)>, t)
    | EFieldAccess(id, t, string)
    | EVariable(id, string)
    | EFnCall(id, string, list<t>, sendToRail)
    | EPartial(id, string, t)
    | ERightPartial(id, string, t)
    | ELeftPartial(id, string, t)
    | EList(id, list<t>)
    | ERecord(id, list<(string, t)>)
    | EPipe(id, list<t>)
    | EConstructor(id, string, list<t>)
    | EMatch(id, t, list<(Pattern.t, t)>)
    | EPipeTarget(id)
    | EFeatureFlag(id, string, t, t, t)
}

module DType = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
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
    | TDbList(t)
}

module AST = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Root(Expr.t)
}
