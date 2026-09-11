#!/usr/bin/env python3
"""Outbound HTTP, in a process that is not the runtime.

The pilot for step 4 of the out-of-process plan, and deliberately not a port of the
linked `HttpClient`: this is a few dozen lines of Python that knows the wire and
nothing else. What it demonstrates is that a REAL effectful platform, with real
types and a well-known effect, can live outside, which is the only way a sandbox
around network egress is possible at all.

What it is not: a replacement for `HttpClient`. It cannot claim `httpClientRequest`,
because two platforms may not claim one name, so it has a name of its own. Replacing
a linked platform is a separate question and needs a person to ask for it out loud.

Wire, both directions: a 4-byte little-endian length, then that many bytes.
  hello    = varint wire version, platform name, varint type count,
             then that many (symbolic type name, content hash) pairs
  hi back  = varint wire version this plugin speaks
  frame    = blob table, then the payload
  request  = builtin name, varint arg count, then each arg as a Dval
  response = 1 status byte (0 ok, 1 error), then a Dval

Dval tags used here: 14 string, 17 list, 18 tuple, 20 record, 21 enum, 24 blob,
25 arbitrary-precision int. A record's fields are written in SORTED key order,
because the host reads them into a map and writes them back out the same way.
"""
import hashlib
import http.client
import struct
import sys
import urllib.parse


def read_exact(n):
    buf = b""
    while len(buf) < n:
        chunk = sys.stdin.buffer.read(n - len(buf))
        if not chunk:
            return None
        buf += chunk
    return buf


def read_varint(data, pos):
    value, shift = 0, 0
    while True:
        b = data[pos]
        pos += 1
        value |= (b & 0x7F) << shift
        if not b & 0x80:
            return value, pos
        shift += 7


def read_string(data, pos):
    length, pos = read_varint(data, pos)
    return data[pos : pos + length].decode("utf-8"), pos + length


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


def write_raw_string(out, text):
    """A length-prefixed string with no Dval tag: type hashes and record field names."""
    encoded = text.encode("utf-8")
    write_varint(out, len(encoded))
    out.extend(encoded)


def read_table(data, pos):
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
        write_raw_string(out, hash_)
        write_varint(out, len(blob))
        out.extend(blob)


def read_dval_string(data, pos):
    assert data[pos] == 14, f"expected a string, got tag {data[pos]}"
    return read_string(data, pos + 1)


def read_headers(data, pos):
    """List<Tuple<String, String>>, as the host writes it."""
    assert data[pos] == 17, f"expected a list, got tag {data[pos]}"
    pos += 1
    pos = skip_value_type(data, pos)
    count, pos = read_varint(data, pos)
    headers = []
    for _ in range(count):
        assert data[pos] == 18, f"expected a tuple, got tag {data[pos]}"
        pos += 1
        name, pos = read_dval_string(data, pos)
        value, pos = read_dval_string(data, pos)
        rest, pos = read_varint(data, pos)
        assert rest == 0, "a header is a pair"
        headers.append((name, value))
    return headers, pos


def skip_value_type(data, pos):
    """A list carries its element type, which we do not need but must step over."""
    tag = data[pos]
    pos += 1
    if tag == 0:  # unknown
        return pos
    return skip_known_type(data, pos)


def skip_known_type(data, pos):
    tag = data[pos]
    pos += 1
    if tag == 17 or tag == 20 or tag == 24:  # list, db, stream: one inner type
        return skip_value_type(data, pos)
    if tag == 18:  # tuple: two, then a list of the rest
        pos = skip_value_type(data, pos)
        pos = skip_value_type(data, pos)
        count, pos = read_varint(data, pos)
        for _ in range(count):
            pos = skip_value_type(data, pos)
        return pos
    if tag == 21:  # custom: a hash, then type arguments
        _hash, pos = read_string(data, pos)
        count, pos = read_varint(data, pos)
        for _ in range(count):
            pos = skip_value_type(data, pos)
        return pos
    if tag == 22:  # dict: key and value
        pos = skip_value_type(data, pos)
        return skip_value_type(data, pos)
    return pos  # every scalar is just its tag


def read_blob(data, pos, table):
    assert data[pos] == 24, f"expected a blob, got tag {data[pos]}"
    pos += 1
    hash_, pos = read_string(data, pos)
    pos += 8
    return table.get(hash_, b""), pos


def write_blob(out, returning, blob):
    hash_ = hashlib.sha256(blob).hexdigest()
    returning[hash_] = blob
    out.append(24)
    write_raw_string(out, hash_)
    out.extend(struct.pack("<q", len(blob)))


def write_int(out, value):
    out.append(25)
    write_raw_string(out, str(value))


def write_string_pair_list(out, pairs):
    out.append(17)
    out.extend([1, 18, 1, 14, 1, 14, 0])  # known tuple of two known strings, no rest
    write_varint(out, len(pairs))
    for name, value in pairs:
        out.append(18)
        write_string(out, name)
        write_string(out, value)
        write_varint(out, 0)


def write_response(out, returning, types, status, headers, body):
    """A `Stdlib.HttpClient.Response`, built from the hash the host handed over."""
    response_hash = types["Darklang.Stdlib.HttpClient.Response"]
    out.append(20)
    write_raw_string(out, response_hash)
    write_raw_string(out, response_hash)
    write_varint(out, 0)  # no type arguments
    write_varint(out, 3)  # three fields, in sorted key order
    write_raw_string(out, "body")
    write_blob(out, returning, body)
    write_raw_string(out, "headers")
    write_string_pair_list(out, headers)
    write_raw_string(out, "statusCode")
    write_int(out, status)


def open_enum(out, types, case):
    result_hash = types["Darklang.Stdlib.Result.Result"]
    out.append(21)
    write_raw_string(out, result_hash)
    write_raw_string(out, result_hash)
    write_varint(out, 2)
    out.extend([0, 0])  # both type arguments unknown; the host knows the signature
    write_raw_string(out, case)
    write_varint(out, 1)


# One connection per host, kept open. Measured, not assumed: opening a fresh TCP
# connection per call costs about 3 ms against a server on loopback, which swamps
# the 150 us the pipe costs and would have made the whole pilot look like the wire
# was the problem. A platform that reaches the network has to pool, exactly as the
# platform it sits beside does.
connections = {}


def connection_for(parts):
    key = (parts.scheme, parts.hostname, parts.port)
    existing = connections.get(key)
    if existing is not None:
        return existing
    if parts.scheme == "https":
        made = http.client.HTTPSConnection(parts.hostname, parts.port, timeout=30)
    else:
        made = http.client.HTTPConnection(parts.hostname, parts.port, timeout=30)
    connections[key] = made
    return made


def fetch(method, uri, headers, body):
    parts = urllib.parse.urlsplit(uri)
    path = parts.path or "/"
    if parts.query:
        path = f"{path}?{parts.query}"

    def once():
        conn = connection_for(parts)
        conn.request(method, path, body=body or None, headers=dict(headers))
        response = conn.getresponse()
        # Read the body before the next request, or the connection cannot be reused.
        return (response.status, list(response.headers.items()), response.read())

    try:
        return once()
    except (http.client.HTTPException, OSError):
        # A pooled connection the server closed while idle. One retry on a fresh one,
        # which is what every pooling client does and the reason this is not just a dict.
        connections.pop((parts.scheme, parts.hostname, parts.port), None)
        return once()


def hello():
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
        out.append(0)

        if fn == "fetchRequest" and argc == 4:
            method, pos = read_dval_string(body, pos)
            uri, pos = read_dval_string(body, pos)
            headers, pos = read_headers(body, pos)
            request_body, pos = read_blob(body, pos, table)
            try:
                status, response_headers, response_body = fetch(
                    method, uri, headers, request_body
                )
                open_enum(out, types, "Ok")
                write_response(
                    out, returning, types, status, response_headers, response_body
                )
            except Exception as e:
                # Everything the network can do to you arrives here as a value, because a
                # platform falling over should look like a Result and not like a crash.
                open_enum(out, types, "Error")
                write_string(out, f"{type(e).__name__}: {e}")
        else:
            returning = {}
            out = bytearray()
            out.append(1)
            write_string(out, f"no such builtin '{fn}', or wrong arity")

        frame = bytearray()
        write_table(frame, returning)
        frame.extend(out)
        sys.stdout.buffer.write(struct.pack("<I", len(frame)))
        sys.stdout.buffer.write(bytes(frame))
        sys.stdout.buffer.flush()


main()
