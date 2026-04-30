module Tests.TestConfig

open LibConfig.ConfigDsl

// testing
let bwdServerBackendPort = int "DARK_CONFIG_TEST_BWDSERVER_BACKEND_PORT"
let httpClientPort = int "DARK_CONFIG_TEST_HTTPCLIENT_SERVER_PORT"
