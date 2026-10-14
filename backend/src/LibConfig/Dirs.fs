/// The source tree's directories, and which of them a scoped file read may reach.
///
/// Was `LibCloud.Config` until the cloud world's last three modules left. It extends
/// `LibConfig.Config` (which owns runDir and logDir) with the paths under `backend/` that the
/// migration runner and the test harness read, and with the `Root` allowlist `File.fs` enforces.
[<RequireQualifiedAccess>]
module LibConfig.Dirs

open Prelude

open LibConfig.ConfigDsl
module Config = LibConfig.Config

// -------------------------
// Note: if you add or change an env-var in development,
// you'll probably need to restart the dev container.
// -------------------------

// -------------------------
// Root directories - see File.fs and Root type below
// -------------------------

let sourceRootDir = absoluteDir "DARK_CONFIG_ROOT_DIR"

let runDir = Config.runDir
let logDir = Config.logDir

let backendDir = $"{sourceRootDir}backend/"
let testdataDir = $"{backendDir}testfiles/data/"
let migrationsDir = $"{backendDir}migrations/"

// --------------------
// File security
// --------------------

type Root =
  | Testdata
  | Migrations
  | NoCheck

let dir (root : Root) : string =
  match root with
  | Testdata -> testdataDir
  | Migrations -> migrationsDir
  | NoCheck -> ""


// -------------------------
// Running the server
// -------------------------



