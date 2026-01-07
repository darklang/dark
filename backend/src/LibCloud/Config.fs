/// Provides central place to fetch environment-specific configuration values
[<RequireQualifiedAccess>]
module LibCloud.Config

open Prelude

open LibConfig.ConfigDsl
module Config = LibService.Config

// -------------------------
// Note: if you add or change an env-var in development,
// you'll probably need to restart the dev container.
// -------------------------

// -------------------------
// Root directories - see File.fs and Root type below
// -------------------------

let sourceRootDir = Config.sourceRootDir

let runDir = Config.runDir
let logDir = Config.logDir

let backendDir = $"{sourceRootDir}backend/"
let testdataDir = $"{backendDir}testfiles/data/"
let serializationDir = $"{backendDir}testfiles/serialization-artifacts/"
let webrootDir = $"{backendDir}static/"
let migrationsDir = $"{backendDir}migrations/"

// --------------------
// File security
// --------------------

type Root =
  | Serialization
  | Webroot
  | Testdata
  | Migrations
  | NoCheck

let dir (root : Root) : string =
  match root with
  | Serialization -> serializationDir
  | Webroot -> webrootDir
  | Testdata -> testdataDir
  | Migrations -> migrationsDir
  | NoCheck -> ""


// -------------------------
// Running the server
// -------------------------

let httpclientTimeoutInMs = int "DARK_CONFIG_HTTPCLIENT_TIMEOUT_IN_MS"


// -------------------------
// Serialization
// -------------------------

/// Generate data for serialization tests. We want this to be off in CI so that we
/// don't overwrite the tests before running them. In dev environment, git will tell
/// us if something has changed.
let serializationGenerateTestData =
  bool "DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA"


/// Canvases that are allowed access to the DarkInternal stdlib functions
let allowedDarkInternalCanvasIDs =
  uuids "DARK_CONFIG_ALLOWED_DARK_INTERNAL_CANVAS_IDS"


// -------------------------
// Package Manager
// -------------------------
let packageManagerUrl = string "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL"
