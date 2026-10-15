#!/usr/bin/env python3
"""
Interactive CLI test using pty module (no pexpect dependency).
Works on both host (with pexpect) and rM2 (pty fallback).
"""
import sys, os, time, select, signal

try:
    import pexpect
    USE_PEXPECT = True
except ImportError:
    import pty, subprocess
    USE_PEXPECT = False

BINARY = sys.argv[1] if len(sys.argv) > 1 else "./clis/darklang-alpha-2c6d956fc3-linux-x64"
STARTUP_WAIT = float(sys.argv[2]) if len(sys.argv) > 2 else 3.0
CHAR_DELAY = float(sys.argv[3]) if len(sys.argv) > 3 else 0.2

def run_pexpect():
    child = pexpect.spawn(BINARY, timeout=30)
    time.sleep(STARTUP_WAIT)
    for c in "help":
        child.send(c)
        time.sleep(CHAR_DELAY)
    child.send("\r")
    time.sleep(STARTUP_WAIT)
    for c in "quit":
        child.send(c)
        time.sleep(CHAR_DELAY)
    child.send("\r")
    time.sleep(2)
    child.close()

def run_pty():
    master, slave = pty.openpty()
    proc = subprocess.Popen(
        [BINARY],
        stdin=slave, stdout=slave, stderr=slave,
        close_fds=True
    )
    os.close(slave)
    time.sleep(STARTUP_WAIT)

    def send(text):
        os.write(master, text.encode())

    def drain():
        """Read any available output to prevent buffer blocking"""
        while select.select([master], [], [], 0.1)[0]:
            try:
                os.read(master, 4096)
            except OSError:
                break

    for c in "help":
        send(c)
        time.sleep(CHAR_DELAY)
        drain()
    send("\r")
    time.sleep(STARTUP_WAIT)
    drain()

    for c in "quit":
        send(c)
        time.sleep(CHAR_DELAY)
        drain()
    send("\r")
    time.sleep(2)
    drain()

    proc.terminate()
    try:
        proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()
    os.close(master)

if __name__ == "__main__":
    if USE_PEXPECT:
        run_pexpect()
    else:
        run_pty()
    print("done")
