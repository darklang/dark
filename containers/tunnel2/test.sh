#!/usr/bin/env bash

red="\033[0;31m"
green="\033[0;32m"
reset="\033[0m"

EXIT=0

function expect_success() {
  if curl --insecure --connect-timeout 3 $1 > /dev/null 2>&1 ; then
    echo -e "${green}success: able to connect to ${1}${reset}"
  else
    echo -e "${red}FAIL: unable to connect to ${1}, expected to be able${reset}"
    return 1
  fi
}
function expect_not_there () {
  if curl --insecure --connect-timeout 3 $1 > /dev/null 2>&1 ; then
    echo -e "${red}FAIL: ${1} shouldn't even exist${reset}"
    return 1
  else
    echo -e "${green}success: non-existant destination ${1} wasn't there${reset}"
  fi
}

function expect_failure() {
  if curl --insecure --connect-timeout 3 $1 > /dev/null 2>&1 ; then
    echo -e "${red}FAIL: able to connect to ${1}, expected to be unable${reset}"
    return 1
  else
    echo -e "${green}success: unable to connect to ${1}${reset}"
  fi
}

# Run them all in parallel

expect_success https://google.com &
expect_success http://google.com &
expect_success https://142.251.40.238 &
expect_success http://142.251.40.238 &

# metadata server
expect_failure http://metadata &
expect_not_there https://metadata &
expect_not_there http://metadata.internal.google &
expect_not_there https://metadata.internal.google &
expect_failure http://169.254.169.254 &
expect_not_there https://169.254.169.254 &

# kubernetes server
expect_not_there http://kubernetes.darklang.svc &
expect_failure https://kubernetes.darklang.svc &
expect_not_there http://kubernetes.default.svc &
expect_failure https://kubernetes.default.svc &
expect_not_there "http://$KUBERNETES_SERVICE_HOST" &
expect_failure "https://$KUBERNETES_SERVICE_HOST" &

expect_success https://microsoft.com &
expect_success http://microsoft.com &

# Wait for them to complete
for job in $(jobs -p); do
  wait $job || (( EXIT+=1 ))
done

exit $EXIT