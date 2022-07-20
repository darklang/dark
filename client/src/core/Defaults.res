open Prelude

let entryID: string = "entry-box"

let leftButton: int = 0

let centerPos: pos = {x: 475, y: 200}

let origin: pos = {x: 0, y: 0}

let moveSize: int = 50

let pageHeight: int = 400

let pageWidth: int = 500

let unsetCSRF: string = "UNSET_CSRF"

let defaultFunctionsType = {
  builtinFunctions: list{},
  packageFunctions: TLID.Dict.empty,
  allowedFunctions: list{},
  previewUnsafeFunctions: Set.String.empty,
}

let defaultWorkerStats: workerStats = {count: 0, schedule: None}
