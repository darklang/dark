module Tests.TestConfig

open LibService.ConfigDsl

// testing
let bwdServerBackendPort = int "DARK_CONFIG_TEST_BWDSERVER_BACKEND_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_TEST_BWDSERVER_KUBERNETES_PORT"
let apiServerBackendPort = int "DARK_CONFIG_TEST_APISERVER_BACKEND_PORT"
let apiServerNginxPort = int "DARK_CONFIG_TEST_APISERVER_NGINX_PORT"
let apiServerKubernetesPort = int "DARK_CONFIG_TEST_APISERVER_KUBERNETES_PORT"
let httpClientPort = int "DARK_CONFIG_TEST_HTTPCLIENT_SERVER_PORT"
let httpBaseClientPort = int "DARK_CONFIG_TEST_HTTPBASECLIENT_SERVER_PORT"
