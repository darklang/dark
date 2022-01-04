module BackendOnlyStdLib.StdLib

// Functions that can only be run on the backend

// CLEANUP - some of these functions can be run on the client too after the switch to
// F#, esp LibJwt, LibCrypto and LibX509, plus at least some of LibHttpClient*

open Prelude

module RT = LibExecution.RuntimeTypes

let fns : List<RT.BuiltInFn> =
  List.concat [ LibDB.fns
                LibCrypto.fns
                LibDarkInternal.fns
                LibEvent.fns
                LibHttpClient0.fns
                LibHttpClient1.fns
                LibHttpClient2.fns
                LibHttpClient3.fns
                LibHttpClient4.fns
                LibHttpClient5.fns
                LibHttpClientAuth.fns
                LibJwt.fns
                LibPassword.fns
                LibStaticAssets.fns
                LibTwilio.fns
                LibX509.fns
                LibDB2.fns ]
