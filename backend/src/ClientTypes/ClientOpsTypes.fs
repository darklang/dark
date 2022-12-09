module ClientTypes.Ops

type AddOpResultV1 =
  { handlers : List<Program.Handler.T>
    deletedHandlers : List<Program.Handler.T>
    dbs : List<Program.DB.T>
    deletedDBs : List<Program.DB.T>
    userFunctions : List<Program.UserFunction.T>
    deletedUserFunctions : List<Program.UserFunction.T>
    userTypes : List<Program.UserType.T>
    deletedUserTypes : List<Program.UserType.T> }

type AddOpParamsV1 = { ops : List<Program.Op>; opCtr : int; clientOpCtrID : string }
