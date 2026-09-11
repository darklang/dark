#!/usr/bin/env python3
"""A platform in another language, for testing that the spawn path works.

Python rather than C so this is a text fixture with nothing to build, and still genuinely
external: it speaks the wire and knows nothing about Dark.

Wire, both directions: a 4-byte little-endian length, then that many bytes.
  request  = the builtin NAME (varint length, then UTF-8), varint arg count,
             then each arg as a Dval
  response = 1 status byte (0 ok, 1 error), then a Dval

Dval encoding, only the cases this fixture needs:
  DUnit   = 0
  DInt64  = 8,  then 8 bytes little-endian
  DString = 14, then varint byte length, then UTF-8
Varint is .NET's 7-bit encoded int: low 7 bits per byte, high bit means continue.
"""
import struct
import sys


def read_exact(n):
    buf = b""
    while len(buf) < n:
        chunk = sys.stdin.buffer.read(n - len(buf))
        if not chunk:
            return None
        buf += chunk
    return buf


def read_string(data, pos):
    length, pos = read_varint(data, pos)
    return data[pos : pos + length].decode("utf-8"), pos + length


def read_varint(data, pos):
    value, shift = 0, 0
    while True:
        b = data[pos]
        pos += 1
        value |= (b & 0x7F) << shift
        if not b & 0x80:
            return value, pos
        shift += 7


def write_varint(out, value):
    while value >= 128:
        out.append((value | 128) & 0xFF)
        value >>= 7
    out.append(value)


def write_string(out, text):
    encoded = text.encode("utf-8")
    out.append(14)
    write_varint(out, len(encoded))
    out.extend(encoded)


def write_int64(out, value):
    out.append(8)
    out.extend(struct.pack("<q", value))


def main():
    counter = 0
    while True:
        header = read_exact(4)
        if header is None:
            return
        (length,) = struct.unpack("<I", header)
        body = read_exact(length)
        if body is None:
            return

        fn, pos = read_string(body, 0)
        argc, pos = read_varint(body, pos)

        out = bytearray()
        out.append(0)  # ok

        # argc is 1, not 0: a Dark builtin taking nothing still declares a `unit` parameter, so a
        # unit Dval (tag 0) arrives on the wire. Worth knowing before writing a plugin.
        if fn == "echoCounter" and argc == 1:
            # Impure on purpose: state living outside the runtime.
            counter += 1
            write_int64(out, counter)
        elif fn == "echoShout" and argc == 1 and body[pos] == 14:
            pos += 1
            slen, pos = read_varint(body, pos)
            text = body[pos : pos + slen].decode("utf-8")
            write_string(out, text.upper())
        elif fn == "echoCrash" and argc == 1:
            # Deliberately fall over, so the host's crash handling can be tested.
            sys.exit(1)
        else:
            out = bytearray()
            out.append(1)  # error
            write_string(out, f"no such builtin '{fn}', or wrong arity")

        sys.stdout.buffer.write(struct.pack("<I", len(out)))
        sys.stdout.buffer.write(bytes(out))
        sys.stdout.buffer.flush()


main()
