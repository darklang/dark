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

    "DARK_CONFIG_TRIGGER_QUEUE_WORKERS" = "y"
    "DARK_CONFIG_TRIGGER_CRONS"         = "y"
    "DARK_CONFIG_PAUSE_BETWEEN_CRONS"   = "0"

    # Serialization
    "DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA" = "n"

    # Logging
    "DARK_CONFIG_ENV_DISPLAY_NAME" = "production"

    # Rollbar
    "DARK_CONFIG_ROLLBAR_ENABLED"          = "y"
    "DARK_CONFIG_ROLLBAR_ENVIRONMENT"      = "production"
    "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM" = "e0ce6a9975c7433598a31f13423fa4c3"
    #"DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"

    # Honeycomb
    "DARK_CONFIG_TELEMETRY_EXPORTER" = "honeycomb"
    # DARK_CONFIG_HONEYCOMB_API_KEY
    "DARK_CONFIG_HONEYCOMB_DATASET_NAME" = "backend"
    "DARK_CONFIG_HONEYCOMB_API_ENDPOINT" = "https://api.honeycomb.io:443"

    # Launchdarkly - https://app.launchdarkly.com/settings/projects/default/environments
    # DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY

    # Feature flag defaults
    "DARK_CONFIG_TRACE_SAMPLING_RULE_DEFAULT" = "sample-none"

    # DB
    "DARK_CONFIG_DB_DBNAME" = "postgres"
    "DARK_CONFIG_DB_HOST"   = "/cloudsql/balmy-ground-195100:us-west1:dark-west"
    # DARK_CONFIG_DB_USER
    # DARK_CONFIG_DB_PASSWORD
    "DARK_CONFIG_DB_POOL_SIZE" = "20"

    # Queue / pubsub
    "DARK_CONFIG_QUEUE_PUBSUB_PROJECT_ID"        = "balmy-ground-195100"
    "DARK_CONFIG_QUEUE_PUBSUB_TOPIC_NAME"        = "topic-queue-ai"
    "DARK_CONFIG_QUEUE_PUBSUB_SUBSCRIPTION_NAME" = "topic-queue-ai-sub"
    "DARK_CONFIG_QUEUE_PUBSUB_CREATE_TOPIC"      = "n"
    # DARK_CONFIG_QUEUE_PUBSUB_CREDENTIALS

    # Traces / cloud storage
    "DARK_CONFIG_TRACE_STORAGE_BUCKET_NAME"   = "dark-traces-ai"
    "DARK_CONFIG_TRACE_STORAGE_CREATE_BUCKET" = "n"
    # DARK_CONFIG_TRACE_STORAGE_CREDENTIALS
    "DARK_CONFIG_TRACE_STORAGE_BASE_URI" = "not-used"

    # Httpclient
    "DARK_CONFIG_HTTPCLIENT_TUNNEL_PROXY_URL" = "socks5://tunnel2-service.darklang:1080"

    # Pusher
    # DARK_CONFIG_PUSHER_APP_ID: k8s
    # DARK_CONFIG_PUSHER_KEY: k8s
    # DARK_CONFIG_PUSHER_SECRET: k8s
    # DARK_CONFIG_PUSHER_CLUSTER: k8s

    # Heap analytics
    "DARK_CONFIG_HEAPIO_ID" = "477722926"
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
