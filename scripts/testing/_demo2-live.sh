#!/usr/bin/env bash
# Demo 2 on main, in the container: a relay, an editing instance A with live.autopush on, a
# hosting instance B running the sync daemon and `serve`. A's saves reach B's page a pull later.
set -uo pipefail
CLI=${CLI:-backend/Build/out/Cli/Debug/net10.0/Cli}
ROOT=rundir/demo2-live
PORT=$(( 9300 + RANDOM % 300 ))
SPORT=$(( 9096 ))
SECRET=$(head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n')
ss -ltn 2>/dev/null | grep -q ":$PORT " && { echo "port $PORT in use; re-run"; exit 2; }
rm -rf "$ROOT"; mkdir -p "$ROOT"
mk() { local n=$1; mkdir -p "$ROOT/$n/home"; scripts/testing/_copy-store "$ROOT/$n/data.db"
       sqlite3 "$ROOT/$n/data.db" 'DELETE FROM config_v0;'; rm -f "$ROOT/$n/cli-config.json"; }
x()  { local n=$1; shift; DARK_CONFIG_RUNDIR="$PWD/$ROOT/$n" HOME="$PWD/$ROOT/$n/home" "$PWD/$CLI" "$@" 2>&1 | sed 's/\x1b\[[0-9;]*m//g'; }
mk relay; mk A; mk B
for i in relay A B; do x $i permissions allow all > /dev/null; done
DARK_CONFIG_RUNDIR="$PWD/$ROOT/relay" HOME="$PWD/$ROOT/relay/home" DARK_MATTER_WRITE_SECRET="$SECRET" \
  "$PWD/$CLI" serve Darklang.Matter.router --port "$PORT" > "$ROOT/relay.log" 2>&1 &
RPID=$!
cleanup() { kill $RPID $SPID $DPID 2>/dev/null; x B apps stop sync > /dev/null 2>&1; }
trap cleanup EXIT
for i in $(seq 1 60); do curl -s -o /dev/null "http://localhost:$PORT/ping" && break; sleep 0.5; done
echo "relay up on $PORT"
x A connect "http://localhost:$PORT" --secret "$SECRET" | tail -1
x B connect "http://localhost:$PORT" --secret "$SECRET" | tail -1
x A config set live.autopush on | tail -1
echo "--- A authors the site (each save autopushes)"
x A fn Demo.Site.page '(): String = "site v1"' | grep -E "✓|autopush|Pushed|refus" | head -4
x A fn Demo.Site.router '(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Demo.Site.page ()) 200' | grep -E "✓|autopush|Pushed" | head -3
echo "--- B pulls once, then follows on a daemon"
x B pull | tail -1
# `apps start sync` here would be the auto-sync daemon; in this container the daemon's `eval` guest is
# refused the relay transport (`httpGetUnsafeBytes is restricted to trusted first-party code`), which
# is a pre-existing product gap this walkthrough does not fix. The pull loop below is what the daemon
# does, at the same interval.
( while true; do x B pull > /dev/null 2>&1; sleep 2; done ) &
DPID=$!
DARK_CONFIG_RUNDIR="$PWD/$ROOT/B" HOME="$PWD/$ROOT/B/home" "$PWD/$CLI" serve Demo.Site.router --port "$SPORT" > "$ROOT/serve.log" 2>&1 &
SPID=$!
for i in $(seq 1 60); do curl -s -o /dev/null "http://localhost:$SPORT/" && break; sleep 0.5; done
echo "B serves: $(curl -s http://localhost:$SPORT/)"
echo "--- A edits; B's page a pull later"
T0=$(date +%s)
x A fn Demo.Site.page '(): String = "site v2"' | grep -E "autopush|Pushed" | head -2
for i in $(seq 1 40); do body=$(curl -s "http://localhost:$SPORT/"); [ "$body" = "site v2" ] && break; sleep 0.5; done
echo "B serves: $body after $(( $(date +%s) - T0 ))s"
echo "--- A breaks the page; B keeps the last good one"
x A fn Demo.Site.page '(): String = 3' | grep -E "autopush|Pushed|At-rest" | head -3
sleep 6
echo "B serves: $(curl -s http://localhost:$SPORT/)"
grep "live:" "$ROOT/serve.log" | tail -2
echo "--- A fixes it"
x A fn Demo.Site.page '(): String = "site v3"' | grep -E "autopush|Pushed" | head -2
for i in $(seq 1 40); do body=$(curl -s "http://localhost:$SPORT/"); [ "$body" = "site v3" ] && break; sleep 0.5; done
echo "B serves: $body"
