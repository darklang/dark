module PT = ProgramTypes
module RT = RuntimeTypes

module Params = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    ops: list<PT.Op.t>,
    opCtr: int,
    clientOpCtrID: string,
  }
  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("ops", list(PT.Op.encode, params.ops)),
      ("opCtr", int(params.opCtr)),
      ("clientOpCtrID", string(params.clientOpCtrID)),
    })
  }
  let decode = (j): t => {
    open Json_decode_extended
    {
      ops: field("ops", list(PT.Op.decode), j),
      opCtr: field("opCtr", int, j),
      clientOpCtrID: field("clientOpCtrID", string, j),
    }
  }
}
module Result = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    handlers: list<PT.Handler.t>,
    deletedHandlers: list<PT.Handler.t>,
    dbs: list<PT.DB.t>,
    deletedDBs: list<PT.DB.t>,
    userFunctions: list<PT.UserFunction.t>,
    deletedUserFunctions: list<PT.UserFunction.t>,
    userTypes: list<PT.UserType.t>,
    deletedUserTypes: list<PT.UserType.t>,
  }
  let decode = (j): t => {
    open Json_decode_extended
    {
      handlers: field("handlers", list(PT.Handler.decode), j),
      deletedHandlers: field("deletedHandlers", list(PT.Handler.decode), j),
      dbs: field("dbs", list(PT.DB.decode), j),
      deletedDBs: field("deletedDBs", list(PT.DB.decode), j),
      userFunctions: field("userFunctions", list(PT.UserFunction.decode), j),
      deletedUserFunctions: field("deletedUserFunctions", list(PT.UserFunction.decode), j),
      userTypes: field("userTypes", list(PT.UserType.decode), j),
      deletedUserTypes: field("deletedUserTypes", list(PT.UserType.decode), j),
    }
  }
}

@ppx.deriving(show({with_path: false}))
type rec t = {result: Result.t}

let decode = (j: Js.Json.t): t => {
  open Json_decode_extended
  {result: field("result", Result.decode, j)}
}
