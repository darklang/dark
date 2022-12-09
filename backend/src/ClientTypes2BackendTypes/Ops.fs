module ClientTypes2BackendTypes.Ops

module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes

module AddOpParamsV1 =
  let toCT (p : LibBackend.Op.AddOpParamsV1) : ClientTypes.Ops.AddOpParamsV1 =
    { ops = p.ops |> List.map CT2Program.Op.toCT
      opCtr = p.opCtr
      clientOpCtrID = p.clientOpCtrID }

  let fromCT (p : ClientTypes.Ops.AddOpParamsV1) : LibBackend.Op.AddOpParamsV1 =
    { ops = p.ops |> List.map CT2Program.Op.fromCT
      opCtr = p.opCtr
      clientOpCtrID = p.clientOpCtrID }

module AddOpResultV1 =
  let toCT (r : LibBackend.Op.AddOpResultV1) : ClientTypes.Ops.AddOpResultV1 =
    { handlers = r.handlers |> List.map CT2Program.Handler.toCT
      deletedHandlers = r.deletedHandlers |> List.map CT2Program.Handler.toCT
      dbs = r.dbs |> List.map CT2Program.DB.toCT
      deletedDBs = r.deletedDBs |> List.map CT2Program.DB.toCT
      userFunctions = r.userFunctions |> List.map CT2Program.UserFunction.toCT
      deletedUserFunctions =
        r.deletedUserFunctions |> List.map CT2Program.UserFunction.toCT
      userTypes = r.userTypes |> List.map CT2Program.UserType.toCT
      deletedUserTypes = r.deletedUserTypes |> List.map CT2Program.UserType.toCT }

  let fromCT (r : ClientTypes.Ops.AddOpResultV1) : LibBackend.Op.AddOpResultV1 =
    { handlers = r.handlers |> List.map CT2Program.Handler.fromCT
      deletedHandlers = r.deletedHandlers |> List.map CT2Program.Handler.fromCT
      dbs = r.dbs |> List.map CT2Program.DB.fromCT
      deletedDBs = r.deletedDBs |> List.map CT2Program.DB.fromCT
      userFunctions = r.userFunctions |> List.map CT2Program.UserFunction.fromCT
      deletedUserFunctions =
        r.deletedUserFunctions |> List.map CT2Program.UserFunction.fromCT
      userTypes = r.userTypes |> List.map CT2Program.UserType.fromCT
      deletedUserTypes = r.deletedUserTypes |> List.map CT2Program.UserType.fromCT }
