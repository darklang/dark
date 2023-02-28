module Tests.TestConfig

open LibService.ConfigDsl

// testing
let bwdServerBackendPort = int "DARK_CONFIG_TEST_BWDSERVER_BACKEND_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_TEST_BWDSERVER_KUBERNETES_PORT"
let httpBaseClientPort = int "DARK_CONFIG_TEST_HTTPBASECLIENT_SERVER_PORT"
