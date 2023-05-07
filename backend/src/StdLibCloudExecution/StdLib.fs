/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibCloudExecution.StdLib

module StdLib = LibExecution.StdLib

// CLEANUP - some of these functions can be run on the client too after the switch to
// F#, esp HttpClient and X509, plus at least some of LibHttpClient*

let fnRenames : StdLib.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames : StdLib.TypeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

let contents =
  StdLib.combine
    [ Libs.DB.contents
      Libs.Event.contents
      Libs.HttpClient.contents // move to StdLibExecution
      Libs.Password.contents ] // move to StdLibExecution?
    fnRenames
    typeRenames
