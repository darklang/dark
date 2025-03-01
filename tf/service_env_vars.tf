# We keep env vars and secrets together and sort them so that the order is the
# same on each update. Otherwise, it's hard to read the changes in the
# terraform plan output.
# Abstracting this code seems to be impossible in terraform, so in the future
# we should write this code in dark and generate the service_env_vars from it
variable "service_env_vars" {
  type = map(object({
    value  = optional(string)
    secret = optional(string)
  }))
  default = {
    # Root directories
    "DARK_CONFIG_RUNDIR"   = { value : "/home/dark/gcp-rundir" }
    "DARK_CONFIG_ROOT_DIR" = { value : "/home/dark" }

    # Important dirs
    "DARK_CONFIG_WEBROOT_DIR"    = { value : "/home/dark/webroot/static" }
    "DARK_CONFIG_MIGRATIONS_DIR" = { value : "/home/dark/migrations" }

    # Ports

    ## BwdServer
    "DARK_CONFIG_BWDSERVER_BACKEND_PORT"    = { value : "11001" }
    "DARK_CONFIG_BWDSERVER_KUBERNETES_PORT" = { value : "11002" }

    ## CronChecker
    "DARK_CONFIG_CRONCHECKER_KUBERNETES_PORT" = { value : "12002" }

    ## QueueWorker
    "DARK_CONFIG_QUEUEWORKER_KUBERNETES_PORT" = { value : "13002" }

    "DARK_CONFIG_TRIGGER_QUEUE_WORKERS" = { value : "y" }
    "DARK_CONFIG_TRIGGER_CRONS"         = { value : "y" }
    "DARK_CONFIG_PAUSE_BETWEEN_CRONS"   = { value : "0" }

    ## ProdExec
    "DARK_CONFIG_PRODEXEC_PORT"            = { value : "8080" }
    "DARK_CONFIG_PRODEXEC_CHISEL_USERNAME" = { secret : "prodexec-chisel-username" }
    "DARK_CONFIG_PRODEXEC_CHISEL_PASSWORD" = { secret : "prodexec-chisel-password" }
    "DARK_CONFIG_PRODEXEC_SSH_PASSWORD"    = { secret : "prodexec-ssh-password" }

    # Http
    "DARK_CONFIG_HTTPCLIENT_TIMEOUT_IN_MS" = { value : "10000" }

    # Serialization
    "DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA" = { value : "n" }

    # Logging
    "DARK_CONFIG_ENV_DISPLAY_NAME" = { value : "production" }
    "DARK_CONFIG_DEFAULT_LOGGER"   = { value : "none" } # We use telemetry instead

    # Rollbar
    "DARK_CONFIG_ROLLBAR_ENABLED"          = { value : "y" }
    "DARK_CONFIG_ROLLBAR_ENVIRONMENT"      = { value : "production" }
    "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM" = { value : "e0ce6a9975c7433598a31f13423fa4c3" }
    "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM" = { secret : "rollbar-post-token" }

    # Honeycomb
    "DARK_CONFIG_TELEMETRY_EXPORTER"     = { value : "honeycomb" }
    "DARK_CONFIG_HONEYCOMB_API_KEY"      = { secret : "honeycomb-api-key" }
    "DARK_CONFIG_HONEYCOMB_DATASET_NAME" = { value : "backend" }
    "DARK_CONFIG_HONEYCOMB_API_ENDPOINT" = { value : "https://api.honeycomb.io:443" }

    # Launchdarkly - https://app.launchdarkly.com/settings/projects/default/environments
    "DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY" = { secret : "launchdarkly-sdk-api-key" }


    # Feature flag defaults
    "DARK_CONFIG_TRACE_SAMPLING_RULE_DEFAULT" = { value : "sample-none" }

    # Queue / pubsub
    "DARK_CONFIG_QUEUE_PUBSUB_PROJECT_ID"        = { value : "darklang-next" }
    "DARK_CONFIG_QUEUE_PUBSUB_TOPIC_NAME"        = { value : "topic-queue" }
    "DARK_CONFIG_QUEUE_PUBSUB_SUBSCRIPTION_NAME" = { value : "topic-queue-sub" }
    "DARK_CONFIG_QUEUE_PUBSUB_CREATE_TOPIC"      = { value : "n" }
    "DARK_CONFIG_QUEUE_PUBSUB_CREDENTIALS"       = { secret : "queue-pubsub-credentials" }

    # Traces / cloud storage
    "DARK_CONFIG_TRACE_STORAGE_BUCKET_NAME"   = { value : "darklang-traces" }
    "DARK_CONFIG_TRACE_STORAGE_CREATE_BUCKET" = { value : "n" }
    "DARK_CONFIG_TRACE_STORAGE_CREDENTIALS"   = { secret : "traces-cloud-storage-credentials" }
    "DARK_CONFIG_TRACE_STORAGE_BASE_URI"      = { value : "not-used" }

    # Pusher
    "DARK_CONFIG_PUSHER_APP_ID"  = { secret : "pusher-app-id" }
    "DARK_CONFIG_PUSHER_KEY"     = { secret : "pusher-key" }
    "DARK_CONFIG_PUSHER_SECRET"  = { secret : "pusher-secret" }
    "DARK_CONFIG_PUSHER_CLUSTER" = { secret : "pusher-cluster" }


    # Heap analytics
    "DARK_CONFIG_HEAPIO_ID" = { value : "477722926" }

    # Package Manager
    "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL" = { value : "https://packages.darklang.com" }

    # Config
    "DARK_CONFIG_ALLOWED_DARK_INTERNAL_CANVAS_IDS" = { secret : "allowed-dark-internal-canvas-ids" }
  }
}
