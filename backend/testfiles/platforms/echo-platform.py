#!/usr/bin/env python3
"""A platform in another language, for testing that the spawn path works.

Python rather than C so this is a text fixture with nothing to build, and still genuinely
external: it speaks the wire and knows nothing about Dark.

Wire, both directions: a 4-byte little-endian length, then that many bytes.
  hello    = varint wire version, platform name, varint type count,
             then that many (symbolic type name, content hash) pairs
  hi back  = varint wire version this plugin speaks
  frame    = blob table, then the payload
  request  = the builtin NAME (varint length, then UTF-8), varint arg count,
             then each arg as a Dval
  response = 1 status byte (0 ok, 1 error), then a Dval

The blob table is how BYTES cross: a varint count, then that many entries of a hash
(varint length, then UTF-8), a varint byte count, and the bytes. A blob inside the
payload is a REFERENCE to one of those hashes, and a reference whose hash is not in
the table names bytes the receiver was expected to already hold.

The hello frame is how a plugin can return anything but a primitive. A record or an
enum carries the type's CONTENT HASH, which a plugin has no way to know: that is the
same reason a manifest names types symbolically. The host resolves those names
against its own store and hands the answers over once, at startup.

Dval encoding, only the cases this fixture needs:
  DUnit   = 0
  DInt64  = 8,  then 8 bytes little-endian
  DString = 14, then varint byte length, then UTF-8
  DEnum   = 21, then the source and runtime type hashes (each a string), then the
            type arguments, then the case name, then the case's fields
  DBlob   = 24, then the hash as a string, then the length as 8 bytes little-endian
A type argument is 0 for unknown, or 1 followed by a known type (14 is String).
Varint is .NET's 7-bit encoded int: low 7 bits per byte, high bit means continue.
"""
import hashlib
import socket
import time
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


def read_table(data, pos):
    """The bytes travelling beside this frame, by hash."""
    count, pos = read_varint(data, pos)
    table = {}
    for _ in range(count):
        hash_, pos = read_string(data, pos)
        length, pos = read_varint(data, pos)
        table[hash_] = data[pos : pos + length]
        pos += length
    return table, pos


def write_table(out, table):
    write_varint(out, len(table))
    for hash_, blob in table.items():
        encoded = hash_.encode("utf-8")
        write_varint(out, len(encoded))
        out.extend(encoded)
        write_varint(out, len(blob))
        out.extend(blob)


def read_blob_ref(data, pos, table):
    """A blob argument: a reference into the table this frame arrived with."""
    assert data[pos] == 24
    pos += 1
    hash_, pos = read_string(data, pos)
    pos += 8  # the length, which the bytes themselves already tell us
    return table[hash_], pos


def write_blob(out, table, blob):
    """Put the bytes in the table and reference them from the payload."""
    hash_ = hashlib.sha256(blob).hexdigest()
    table[hash_] = blob
    out.append(24)
    encoded = hash_.encode("utf-8")
    write_varint(out, len(encoded))
    out.extend(encoded)
    out.extend(struct.pack("<q", len(blob)))


def write_ok_string(out, result_hash, text):
    """`Ok text` as a `Result<String, String>`, using the hash the host handed over."""
    out.append(21)
    encoded = result_hash.encode("utf-8")
    write_varint(out, len(encoded))
    out.extend(encoded)
    write_varint(out, len(encoded))
    out.extend(encoded)
    write_varint(out, 2)  # two type arguments
    out.extend([1, 14, 1, 14])  # each one known, each one String
    write_varint(out, 2)
    out.extend(b"Ok")
    write_varint(out, 1)  # one field in this case
    write_string(out, text)


def hello():
    """Read what the host says at startup, and say what we speak back."""
    header = read_exact(4)
    (length,) = struct.unpack("<I", header)
    body = read_exact(length)
    version, pos = read_varint(body, 0)
    _name, pos = read_string(body, pos)
    count, pos = read_varint(body, pos)
    types = {}
    for _ in range(count):
        name, pos = read_string(body, pos)
        hash_, pos = read_string(body, pos)
        types[name] = hash_

    reply = bytearray()
    write_varint(reply, 1)
    sys.stdout.buffer.write(struct.pack("<I", len(reply)))
    sys.stdout.buffer.write(bytes(reply))
    sys.stdout.buffer.flush()
    return version, types


def main():
    _version, types = hello()
    counter = 0
    while True:
        header = read_exact(4)
        if header is None:
            return
        (length,) = struct.unpack("<I", header)
        body = read_exact(length)
        if body is None:
            return

        table, pos = read_table(body, 0)
        fn, pos = read_string(body, pos)
        argc, pos = read_varint(body, pos)

        returning = {}
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
        elif fn == "echoBytes" and argc == 1:
            # Bytes in and bytes out, which is the case the at-rest encoding cannot express and
            # the table exists for.
            blob, pos = read_blob_ref(body, pos, table)
            write_blob(out, returning, blob.upper())
        elif fn == "echoResult" and argc == 1 and body[pos] == 14:
            # A `Result`, which is an enum, which needs the type's hash. The manifest named
            # `Darklang.Stdlib.Result.Result` and the host resolved it for us at startup.
            pos += 1
            slen, pos = read_varint(body, pos)
            text = body[pos : pos + slen].decode("utf-8")
            write_ok_string(out, types["Darklang.Stdlib.Result.Result"], text.upper())
        elif fn == "echoReach" and argc == 1:
            # Tries the network, which this platform never asked for. What comes back says
            # whether the host confined it.
            try:
                socket.create_connection(("1.1.1.1", 80), timeout=3).close()
                write_string(out, "reached the network")
            except OSError as e:
                write_string(out, f"{e.errno}")
        elif fn == "echoWrongType" and argc == 1:
            # The manifest promises a String. Send an Int64 and see whether anybody checks.
            write_int64(out, 42)
        elif fn == "echoOversized" and argc == 1:
            # Announce a frame far larger than the host will read, and send none of it. The length
            # prefix is the PLATFORM's number, and nothing obliges it to be a sane one.
            sys.stdout.buffer.write(struct.pack("<i", 200000000))
            sys.stdout.buffer.flush()
            time.sleep(3600)
        elif fn == "echoNegative" and argc == 1:
            # Not a size at all.
            sys.stdout.buffer.write(struct.pack("<i", -1))
            sys.stdout.buffer.flush()
            time.sleep(3600)
        elif fn == "echoHang" and argc == 1:
            # Alive and silent, which is not the same failure as crashing. A crash closes the pipe
            # and the host sees the read end; this answers nothing at all, forever.
            while True:
                time.sleep(3600)
        elif fn == "echoCrash" and argc == 1:
            # Deliberately fall over, so the host's crash handling can be tested.
            sys.exit(1)
        else:
            returning = {}
            out = bytearray()
            out.append(1)  # error
            write_string(out, f"no such builtin '{fn}', or wrong arity")

        frame = bytearray()
        write_table(frame, returning)
        frame.extend(out)
        sys.stdout.buffer.write(struct.pack("<I", len(frame)))
        sys.stdout.buffer.write(bytes(frame))
        sys.stdout.buffer.flush()


main()
