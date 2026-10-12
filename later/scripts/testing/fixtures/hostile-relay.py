#!/usr/bin/env python3
"""A relay that answers badly, so the sync and branch clients can be tested against one.

Used by the sync-hostile-relay gate (`scripts/testing/gates sync-hostile-relay`). The mode is
chosen by the PORT, so several can run at once and each test names the failure it wants:

  9081 truncated JSON
  9082 valid JSON, wrong shape
  9084 200 with an EMPTY body
  9085 500 on everything
  9086 never answers at all
  9087 holds far more ops than the client, but serves nothing past the client's cursor
  9088 dies mid-pull: page 1 lands, the connection drops on page 2, and it is back for the next pull
  9089 speaks a wire format from the future (formatVersion 99): the peer that upgraded before you did

An empty 200 must not get "Pushed N ops" out of the client, which would record the push as done and
never resend.

Every mode answers /ping before it consults the mode, so a caller can wait for the fixture to bind.
"""
import sys, time
from http.server import BaseHTTPRequestHandler, HTTPServer

MODE = int(sys.argv[1])
DROPPED_ONCE = [False]

class H(BaseHTTPRequestHandler):
    def log_message(self, *a): pass

    def _send(self, code, body):
        b = body.encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        self.wfile.write(b)

    def answer(self):
        p = self.path
        if p.startswith("/ping"):
            return self._send(200, "pong")
        if MODE == 9086:
            time.sleep(600); return
        if MODE == 9085:
            return self._send(500, "boom")
        if MODE == 9084:
            return self._send(200, "")
        if MODE == 9081:
            return self._send(200, '{"count":12,"ops":[{"id":"aaa')          # truncated
        if MODE == 9082:
            return self._send(200, '{"unexpected":true,"ops":"not-a-list"}')  # wrong shape
        if MODE == 9087:
            # A relay the client HAS been talking to (same instance id), holding many more ops than the
            # client, that answers "nothing past your cursor". That is the shape of a cursor pointing
            # past ops the local log lost, and the client must rewind rather than pull zero forever.
            if p.startswith("/sync/head"):
                return self._send(200, '{"count":999999999,"maxTs":"2026-01-01T00:00:00Z"}')
            # Every page, whatever the cursor: nothing past it, and a cursor of 0 to say so.
            return self._send(200, '{"formatVersion":1,"darkBuild":"x","kernelHash":"x",'
                                   '"owner":"inst-hostile-9087","cursor":0,"ops":[],"commits":[]}')
        if MODE == 9088:
            # A relay restarting under a pull. Page 1 carries one op (unreadable to any build, so it is
            # stored inert and nothing folds); page 2's connection drops once, the way a restart looks
            # from the client; every request after that answers normally. The client must not record the
            # relay's position off a pull that lost a page, and the next pull must finish the job.
            if p.startswith("/sync/head"):
                return self._send(200, '{"count":1,"maxTs":"2026-01-01T00:00:00.000Z"}')
            if "sinceSeq=0" in p:
                return self._send(200, ('{"formatVersion":1,"darkBuild":"x","kernelHash":"x",'
                                        '"owner":"inst-hostile-9088","cursor":1,"ops":['
                                        '{"id":"9088aaaa-0000-4000-8000-000000000001","blobHex":"ff3907",'
                                        '"ts":"2026-01-01T00:00:00.000Z","author":"hostile","commit":""}],"commits":[]}'))
            if not DROPPED_ONCE[0]:
                DROPPED_ONCE[0] = True
                self.connection.close(); return
            return self._send(200, ('{"formatVersion":1,"darkBuild":"x","kernelHash":"x",'
                                    '"owner":"inst-hostile-9088","cursor":1,"ops":[],"commits":[]}'))
        if MODE == 9089:
            if p.startswith("/sync/head"):
                return self._send(200, '{"count":1,"maxTs":"2026-01-01T00:00:00.000Z"}')
            return self._send(200, ('{"formatVersion":99,"darkBuild":"future","kernelHash":"x",'
                                    '"owner":"inst-hostile-9089","cursor":1,"ops":[],"commits":[]}'))
        return self._send(200, "{}")

    def do_GET(self): self.answer()
    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        self.rfile.read(n)
        self.answer()

HTTPServer(("127.0.0.1", MODE), H).serve_forever()
