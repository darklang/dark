module ClientTypes.Ops

type AddOpResultV1 =
  { handlers : List<Program.Handler.T> // replace
    deletedHandlers : List<Program.Handler.T> // replace, see note above
    dbs : List<Program.DB.T> // replace
    deletedDBs : List<Program.DB.T> // replace, see note above
    userFunctions : List<Program.UserFunction.T> // replace
    deletedUserFunctions : List<Program.UserFunction.T>
    userTypes : List<Program.UserType.T>
    deletedUserTypes : List<Program.UserType.T> } // replace, see deleted_toplevels

type AddOpParamsV1 = { ops : List<Program.Op>; opCtr : int; clientOpCtrID : string }

