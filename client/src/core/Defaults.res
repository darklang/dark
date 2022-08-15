open Prelude

// hmmm: move elsewhere?

let entryID: string = "entry-box"

let leftButton: int = 0

// hmmm
let moveSize: int = 50

// hmmm: better name than 'page'? the settings modal is also a 'page' right?
let pageHeight: int = 400

let pageWidth: int = 500

let unsetCSRF: string = "UNSET_CSRF"

// hmmm: move to Functions.res?
let defaultFunctionsType = {
  builtinFunctions: list{},
  packageFunctions: TLID.Dict.empty,
  allowedFunctions: list{},
  previewUnsafeFunctions: Set.String.empty,
}
