/// Types used in many APIs
module ApiServer.ClientTypes

// CLEANUP: before merging, use the shares types from PR 4277.
type Dval = DV of LibExecution.RuntimeTypes.Dval

let toRT (DV dv : Dval) : LibExecution.RuntimeTypes.Dval = dv
let fromRT (dv : LibExecution.RuntimeTypes.Dval) : Dval = DV dv
