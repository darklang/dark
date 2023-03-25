variable "service_env_vars" {
  type = map(string)
  default = {
    # Root directories
    "DARK_CONFIG_RUNDIR"   = "/home/dark/gcp-rundir"
    "DARK_CONFIG_ROOT_DIR" = "/home/dark"

    # Important dirs
    "DARK_CONFIG_WEBROOT_DIR"    = "/home/dark/webroot/static"
    "DARK_CONFIG_MIGRATIONS_DIR" = "/home/dark/migrations"

    # Ports

    ## BwdServer
    "DARK_CONFIG_BWDSERVER_BACKEND_PORT"    = "11001"
    "DARK_CONFIG_BWDSERVER_KUBERNETES_PORT" = "11002"

    ## CronChecker
    "DARK_CONFIG_CRONCHECKER_KUBERNETES_PORT" = "12002"

    ## QueueWorker
    "DARK_CONFIG_QUEUEWORKER_KUBERNETES_PORT" = "13002"

    "DARK_CONFIG_ALLOW_TEST_ROUTES"     = "n"
    "DARK_CONFIG_TRIGGER_QUEUE_WORKERS" = "y"
    "DARK_CONFIG_TRIGGER_CRONS"         = "y"
    "DARK_CONFIG_PAUSE_BETWEEN_CRONS"   = "0"
    "DARK_CONFIG_CREATE_ACCOUNTS"       = "n"
    "DARK_CONFIG_USE_HTTPS"             = "y"

    # Serialization
    "DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA" = "n"
    "DARK_CONFIG_SERIALIZATION_CHECK_TYPES"        = "n"

    # Logging
    "DARK_CONFIG_ENV_DISPLAY_NAME" = "production"

    # Rollbar
    "DARK_CONFIG_ROLLBAR_ENABLED"          = "y"
    "DARK_CONFIG_ROLLBAR_ENVIRONMENT"      = "production"
    "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM" = "c7af77e991aa4edd80cf6a576c1e42f5"
    # "#DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM" = "k8s"

    # Honeycomb
    "DARK_CONFIG_TELEMETRY_EXPORTER" = "honeycomb"
    # "#DARK_CONFIG_HONEYCOMB_API_KEY"     = "k8s"
    "DARK_CONFIG_HONEYCOMB_DATASET_NAME" = "kubernetes-bwd-ocaml"
    "DARK_CONFIG_HONEYCOMB_API_ENDPOINT" = "https://api.honeycomb.io:443"

    # Launchdarkly - https://app.launchdarkly.com/settings/projects/default/environments
    # "DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY"   = "k8s"
    "DARK_CONFIG_LAUNCHDARKLY_CLIENT_SIDE_ID" = "627162f9b2bab01530ddc355"

    # Feature flag defaults
    "DARK_CONFIG_TRACE_SAMPLING_RULE_DEFAULT" = "sample-none"

    # DB
    "DARK_CONFIG_DB_DBNAME" = "postgres"
    "DARK_CONFIG_DB_HOST"   = "/cloudsql/balmy-ground-195100:us-west1:dark-west"
    # DARK_CONFIG_DB_USER: k8s
    # DARK_CONFIG_DB_PASSWORD: k8s
    "DARK_CONFIG_DB_POOL_SIZE" = "20"

    # Queue / pubsub
    "DARK_CONFIG_QUEUE_PUBSUB_PROJECT_ID"        = "balmy-ground-195100"
    "DARK_CONFIG_QUEUE_PUBSUB_TOPIC_NAME"        = "topic-queueworker-1"
    "DARK_CONFIG_QUEUE_PUBSUB_SUBSCRIPTION_NAME" = "subscription-queueworker-1"
    "DARK_CONFIG_QUEUE_PUBSUB_CREATE_TOPIC"      = "n"
    # DARK_CONFIG_QUEUE_PUBSUB_CREDENTIALS: k8s

    # Traces / cloud storage
    "DARK_CONFIG_TRACE_STORAGE_BUCKET_NAME"   = "dark-traces"
    "DARK_CONFIG_TRACE_STORAGE_CREATE_BUCKET" = "n"
    #DARK_CONFIG_TRACE_STORAGE_CREDENTIALS=k8s
    "DARK_CONFIG_TRACE_STORAGE_BASE_URI" = "not-used"

    # Httpclient
    "DARK_CONFIG_HTTPCLIENT_TUNNEL_PROXY_URL" = "socks5://tunnel2-service.darklang:1080"

    # Publicly accessible domain
    "DARK_CONFIG_PUBLIC_DOMAIN" = "localhost"

    # Pusher
    # DARK_CONFIG_PUSHER_APP_ID: k8s
    # DARK_CONFIG_PUSHER_KEY: k8s
    # DARK_CONFIG_PUSHER_SECRET: k8s
    # DARK_CONFIG_PUSHER_CLUSTER: k8s

    # Heap analytics
    "DARK_CONFIG_HEAPIO_ID" = "477722926"

    # Other
    "DARK_CONFIG_BROWSER_RELOAD_ENABLED"           = "n"
    "DARK_CONFIG_HASH_STATIC_FILENAMES"            = "y"
    "DARK_CONFIG_USE_LOGIN_DARKLANG_COM_FOR_LOGIN" = "y"

    # Getting started canvas
    "DARK_CONFIG_GETTING_STARTED_CANVAS_NAME"   = "crud"
    "DARK_CONFIG_GETTING_STARTED_CANVAS_SOURCE" = "sample-crud"
  }
}

variable "service_secrets" {
  type = map(string)
  default = {
    # rollbar
    "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM" = "rollbar-post-token"

    # launchdarkly
    "DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY" = "launchdarkly-sdk-api-key"

    # pusher
    "DARK_CONFIG_PUSHER_APP_ID"  = "pusher-app-id"
    "DARK_CONFIG_PUSHER_KEY"     = "pusher-key"
    "DARK_CONFIG_PUSHER_SECRET"  = "pusher-secret"
    "DARK_CONFIG_PUSHER_CLUSTER" = "pusher-cluster"

    # database
    "DARK_CONFIG_DB_USER"     = "cloudsql-username"
    "DARK_CONFIG_DB_PASSWORD" = "cloudsql-password"

    # honeycomb
    "DARK_CONFIG_HONEYCOMB_API_KEY" = "honeycomb-api-key"

    # PubSub - service account JSON file
    "DARK_CONFIG_QUEUE_PUBSUB_CREDENTIALS"  = "queue-pubsub-credentials"
    "DARK_CONFIG_TRACE_STORAGE_CREDENTIALS" = "traces-cloud-storage-credentials"

    # Config
    "DARK_CONFIG_ALLOWED_DARK_INTERNAL_CANVAS_ID" = "allowed-dark-internal-canvas-id"
  }
}
