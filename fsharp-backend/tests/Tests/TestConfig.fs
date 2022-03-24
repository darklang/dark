module Tests.TestConfig

open LibService.ConfigDsl

// testing
let ocamlServerNginxPort = int "DARK_CONFIG_TEST_OCAMLSERVER_NGINX_PORT"
let bwdServerBackendPort = int "DARK_CONFIG_TEST_BWDSERVER_BACKEND_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_TEST_BWDSERVER_KUBERNETES_PORT"
let httpClientPort = int "DARK_CONFIG_TEST_HTTPCLIENT_SERVER_PORT"
