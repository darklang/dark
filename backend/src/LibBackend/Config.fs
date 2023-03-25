/// Provides central place to fetch known configuration values
module LibBackend.Config

open LibService.ConfigDsl

// -------------------------
// Note: if you add an env-var in development, you'll probably need to
// restart the dev container.
// -------------------------

// -------------------------
// Root directories - see File.fs and Root type below
// -------------------------

let runDir = absoluteDir "DARK_CONFIG_RUNDIR"

let backendDir = $"{LibService.Config.rootDir}backend/"

let testdataDir = $"{backendDir}test_appdata/"

let logDir = $"{runDir}logs/"

let serializationDir = $"{backendDir}serialization/"

// -------------------------
// Configurable dirs
// -------------------------
let webrootDir = absoluteDir "DARK_CONFIG_WEBROOT_DIR"

let migrationsDir = absoluteDir "DARK_CONFIG_MIGRATIONS_DIR"

// -------------------------
// Web configuration
// -------------------------
let bwdServerContentHost = string "DARK_CONFIG_BWDSERVER_HOST"

// -------------------------
// Kubernetes
// -------------------------

let httpclientProxyUrl = string "DARK_CONFIG_HTTPCLIENT_TUNNEL_PROXY_URL"

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
let allowTestRoutes = bool "DARK_CONFIG_ALLOW_TEST_ROUTES"

// If we have production data in a non-production environment, we don't want to trigger their workers
let triggerQueueWorkers = bool "DARK_CONFIG_TRIGGER_QUEUE_WORKERS"

// If we have production data in a non-production environment, we don't want to trigger their workers
let triggerCrons = bool "DARK_CONFIG_TRIGGER_CRONS"

// Stop my fans from spinning
let pauseBetweenCronsInMs = int "DARK_CONFIG_PAUSE_BETWEEN_CRONS"

let createAccounts = bool "DARK_CONFIG_CREATE_ACCOUNTS"

// Should we redirect insecure requests
let useHttps = bool "DARK_CONFIG_USE_HTTPS"

// -------------------------
// Rollbar
// -------------------------

let rollbarClientAccessToken =
  (* This is what the rollbar UI calls it *)
  match string "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM" with
  | "none" -> None
  | item -> Some item


let rollbarJs =
  match rollbarClientAccessToken with
  | Some token ->
    Printf.sprintf
      "{captureUncaught:true,verbose:true,enabled:%s,accessToken:'%s',payload:{environment: '%s'}}"
      (if LibService.Config.rollbarEnabled then "true" else "false")
      token
      LibService.Config.rollbarEnvironment
  | _ -> "{enabled:false}"


// -------------------------
// Serialization
// -------------------------

/// Generate data for serialization tests. We want this to be off in CI so that we
/// don't overwrite the tests before running them. In dev environment, git will tell
/// us if something has changed.
let serializationGenerateTestData =
  bool "DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA"

/// When serializing values, check that they types we've explicitly allowed and
/// tested. We want this off in production cases (just in case), but on in
/// development and testing so that we'll catch types we haven't tested.
let serializationGenerateCheckTypes = bool "DARK_CONFIG_SERIALIZATION_CHECK_TYPES"

/// Canvases that are allowed access to the
let allowedDarkInternalCanvasID = uuid "DARK_CONFIG_ALLOWED_DARK_INTERNAL_CANVAS_ID"


// -------------------------
// Pusher
// -------------------------

let pusherID = string "DARK_CONFIG_PUSHER_APP_ID"

let pusherKey = string "DARK_CONFIG_PUSHER_KEY"

let pusherSecret = string "DARK_CONFIG_PUSHER_SECRET"

let pusherCluster = string "DARK_CONFIG_PUSHER_CLUSTER"


// -------------------------
// Queues
// -------------------------
let queuePubSubProjectID = string "DARK_CONFIG_QUEUE_PUBSUB_PROJECT_ID"

let queuePubSubTopicName = string "DARK_CONFIG_QUEUE_PUBSUB_TOPIC_NAME"

let queuePubSubSubscriptionName = string "DARK_CONFIG_QUEUE_PUBSUB_SUBSCRIPTION_NAME"

let queuePubSubCreateTopic = bool "DARK_CONFIG_QUEUE_PUBSUB_CREATE_TOPIC"

let queuePubSubCredentials = credentialsOption "DARK_CONFIG_QUEUE_PUBSUB_CREDENTIALS"


// -------------------------
// Traces
// -------------------------
let traceStorageBucketName = string "DARK_CONFIG_TRACE_STORAGE_BUCKET_NAME"

let traceStorageBaseUri = string "DARK_CONFIG_TRACE_STORAGE_BASE_URI"

let traceStorageCreateBucket = bool "DARK_CONFIG_TRACE_STORAGE_CREATE_BUCKET"

let traceStorageCredentials =
  credentialsOption "DARK_CONFIG_TRACE_STORAGE_CREDENTIALS"


// -------------------------
// Infra
// -------------------------
let publicDomain = string "DARK_CONFIG_PUBLIC_DOMAIN"

let browserReloadEnabled = bool "DARK_CONFIG_BROWSER_RELOAD_ENABLED"

let useLoginDarklangComForLogin = bool "DARK_CONFIG_USE_LOGIN_DARKLANG_COM_FOR_LOGIN"
