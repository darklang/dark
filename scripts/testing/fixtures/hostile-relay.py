#!/usr/bin/env python3
"""A relay that answers badly, so the sync and branch clients can be tested against one.

Used by `scripts/testing/test-sync-hostile-relay`. The mode is chosen by the PORT, so several can run
at once and each test names the failure it wants:

  9081 truncated JSON             9084 200 with an EMPTY body
  9082 valid JSON, wrong shape    9085 500 on everything
  9083 an absurd count            9086 never answers at all

The empty-body mode is the one that mattered: a relay answering 200 with nothing readable used to get a
cheerful "Pushed N ops" out of the client, which then recorded the push as done and never sent them again.
"""
import sys, time
from http.server import BaseHTTPRequestHandler, HTTPServer

MODE = int(sys.argv[1])

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
        if MODE == 9083:
            return self._send(200, '{"count":99999999999999999999,"maxTs":"","ops":[]}')
        return self._send(200, "{}")

    def do_GET(self): self.answer()
    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        self.rfile.read(n)
        self.answer()

HTTPServer(("127.0.0.1", MODE), H).serve_forever()
