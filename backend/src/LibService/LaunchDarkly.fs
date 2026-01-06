/// LaunchDarkly configuration (feature flags) - STUBBED OUT
/// LaunchDarkly has been removed. This module returns default values.
module LibService.LaunchDarkly

open Prelude

module Internal =
  // Stubbed versions that just return the default/test value

  let boolConfig
    (_name : string)
    (_default_ : bool)
    (testDefault : bool)
    : unit -> bool =
    fun () -> testDefault

  let intConfig
    (_name : string)
    (_default_ : int)
    (testDefault : int)
    : unit -> int =
    fun () -> testDefault

  let floatConfig
    (_name : string)
    (_default_ : float)
    (testDefault : float)
    : unit -> float =
    fun () -> testDefault

  let stringConfig
    (_name : string)
    (_default_ : string)
    (testDefault : string)
    : unit -> string =
    fun () -> testDefault

  let handlerBool
    (_name : string)
    (_default_ : bool)
    (testDefault : bool)
    : CanvasID -> tlid -> bool =
    fun _ _ -> testDefault

  let handlerInt
    (_name : string)
    (_default_ : int)
    (testDefault : int)
    : CanvasID -> tlid -> int =
    fun _ _ -> testDefault

  let handlerFloat
    (_name : string)
    (_default_ : float)
    (testDefault : float)
    : CanvasID -> tlid -> float =
    fun _ _ -> testDefault

  let handlerString
    (_name : string)
    (_default_ : string)
    (testDefault : string)
    : CanvasID -> tlid -> string =
    fun _ _ -> testDefault

  let canvasBool
    (_name : string)
    (_default_ : bool)
    (testDefault : bool)
    : CanvasID -> bool =
    fun _ -> testDefault

  let canvasInt
    (_name : string)
    (_default_ : int)
    (testDefault : int)
    : CanvasID -> int =
    fun _ -> testDefault

  let canvasFloat
    (_name : string)
    (_default_ : float)
    (testDefault : float)
    : CanvasID -> float =
    fun _ -> testDefault

  let canvasString
    (_name : string)
    (_default_ : string)
    (testDefault : string)
    : CanvasID -> string =
    fun _ -> testDefault

  let serviceBool
    (_name : string)
    (_default_ : bool)
    (testDefault : bool)
    : string -> bool =
    fun _ -> testDefault

  let serviceInt
    (_name : string)
    (_default_ : int)
    (testDefault : int)
    : string -> int =
    fun _ -> testDefault

  let serviceFloat
    (_name : string)
    (_default_ : float)
    (testDefault : float)
    : string -> float =
    fun _ -> testDefault

  let serviceString
    (_name : string)
    (_default_ : string)
    (testDefault : string)
    : string -> string =
    fun _ -> testDefault


/// Flush - no-op
let flush () : unit = ()

// --------------
// Handler Flags - per-canvas, per-handler settings
// --------------
let traceSamplingRule =
  Internal.handlerString
    "traces-sampling-rule"
    Config.traceSamplingRuleDefault
    "sample-all"

// --------------
// Canvas Flags - these are per-canvas settings
// --------------
let knownBroken = Internal.canvasBool "canvas-known-broken" false false

// --------------
// Service Flags - may be different for each service
// --------------
let telemetrySamplePercentage =
  Internal.serviceFloat "telemetry-sample-percentage" 100.0 100.0

// --------------
// System flags
// --------------
let healthCheckDomains = Internal.stringConfig "canvas-health-checks" "" ""
