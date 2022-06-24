// The types that the user sees. For all type definitions, see ProgramTypes.fs

@ppx.deriving(show) type rec id = ID.t
@ppx.deriving(show) type rec tlid = TLID.t

module FQFnName = {
  type stdlibFnName = {module_: string, function_: string, version: int}

  type infixStdlibFnName = {module_: option<string>, function_: string}

  type userFnName = string

  type packageFnName = {
    owner: string,
    package: string,
    module_: string,
    function_: string,
    version: int,
  }

  type t =
    | User(userFnName)
    | Stdlib(stdlibFnName)
    | Package(packageFnName)
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
    | PFloat(id, id, string, string)
    | PNull(id, id)
    | PBlank(id, id)
}

module Expr = {
  @ppx.deriving(show({with_path: false}))
  type rec sign =
    | Positive
    | Negative

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
    | EFnCall(id, FqFnName.t, list<t>, sendToRail)
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
    | TStr
    | TCharacter
    | TBool
    | TFloat
    | TObj
    | TList
    | TAny
    | TNull
    | TBlock
    | TIncomplete
    | TError
    | TResp
    | TDB
    | TDate
    | TPassword
    | TUuid
    | TOption
    | TErrorRail
    | TResult
    | TDbList(t)
    | TUserType(string, int)
    | TBytes
}

module AST = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Root(Expr.t)
}
