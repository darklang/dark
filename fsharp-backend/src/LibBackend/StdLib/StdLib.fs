module LibBackend.StdLib.StdLib

module RT = LibExecution.RuntimeTypes

let fns : List<RT.BuiltInFn> = LibDB.fns @ LibDB2.fns
