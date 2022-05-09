/// Provides central place to fetch known configuration values
module LibBackend.Config

open LibService.ConfigDsl

// -------------------------
// Note: if you add an env-var in development, you'll probably need to
// restart the dev container.
// -------------------------

// -------------------------
// Root directories - see File.fs
// -------------------------

let runDir = absoluteDir "DARK_CONFIG_RUNDIR"

let backendDir = $"{LibService.Config.rootDir}backend/"

let testdataDir = $"{backendDir}test_appdata/"

let testResultDir = $"{runDir}test_results/"

let logDir = $"{runDir}logs/"

let serializationDir = $"{backendDir}serialization/"

let completedTestDir = $"{runDir}completed_tests/"

// -------------------------
// Configurable dirs
// -------------------------
let templatesDir = absoluteDir "DARK_CONFIG_TEMPLATES_DIR"

let webrootDir = absoluteDir "DARK_CONFIG_WEBROOT_DIR"

let migrationsDir = absoluteDir "DARK_CONFIG_MIGRATIONS_DIR"

let binRootDir = absoluteDir "DARK_CONFIG_BIN_ROOT_DIR"

// -------------------------
// Web configuration
// -------------------------
let apiServerServeStaticContent = bool "DARK_CONFIG_APISERVER_SERVE_STATIC_CONTENT"

let apiServerStaticHost = string "DARK_CONFIG_APISERVER_STATIC_HOST"

let cookieDomain = string "DARK_CONFIG_COOKIE_DOMAIN"

let bwdServerContentHost = string "DARK_CONFIG_BWDSERVER_HOST"

// -------------------------
// Kubernetes
// -------------------------

let httpclientProxyUrl = string "DARK_CONFIG_HTTPCLIENT_TUNNEL_PROXY_URL"

// --------------------
// For use in Util
// --------------------

type Root =
  | Log
  | Serialization
  | Templates
  | Webroot
  | CompletedTest
  | Testdata
  | TestResults
  | BinRoot
  | Migrations
  | NoCheck

let dir (root : Root) : string =
  match root with
  | Log -> logDir
  | Serialization -> serializationDir
  | Templates -> templatesDir
  | Webroot -> webrootDir
  | CompletedTest -> completedTestDir
  | BinRoot -> binRootDir
  | Testdata -> testdataDir
  | TestResults -> testResultDir
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

let shouldWriteShapeData = bool "DARK_CONFIG_SAVE_SERIALIZATION_DIGEST"

let showStacktrace = bool "DARK_CONFIG_SHOW_STACKTRACE"

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

let queuePubSubShouldCreate = bool "DARK_CONFIG_QUEUE_PUBSUB_SHOULD_CREATE"


// -------------------------
// Infra
// -------------------------
let publicDomain = string "DARK_CONFIG_PUBLIC_DOMAIN"

let browserReloadEnabled = bool "DARK_CONFIG_BROWSER_RELOAD_ENABLED"

let hashStaticFilenames = bool "DARK_CONFIG_HASH_STATIC_FILENAMES"

let checkTierOneHosts = bool "DARK_CONFIG_CHECK_TIER_ONE_HOSTS"

let staticAssetsBucket = stringOption "DARK_CONFIG_STATIC_ASSETS_BUCKET"
let staticAssetsSaltSuffix = string "DARK_CONFIG_STATIC_ASSETS_SALT_SUFFIX"


let useLoginDarklangComForLogin = bool "DARK_CONFIG_USE_LOGIN_DARKLANG_COM_FOR_LOGIN"
