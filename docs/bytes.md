# Bytes

`Bytes` holds binary data, such as an image, a file's contents, or an HTTP body.

You use the same type and functions whether the data is in memory or saved:

```dark
let first = Stdlib.Bytes.fromString "hello"
let second = Stdlib.Bytes.fromList [104uy, 101uy, 108uy, 108uy, 111uy]
first == second // true
```

Freshly created bytes stay in memory. Saving a value containing bytes stores
those bytes automatically. There is no separate user-facing persistent type
or command for switching storage forms.

Saving bytes does not change their equality, hash, or sort position. Equal
contents compare equal, including inside lists, records, and tuples. The
normal value display is `<Bytes: N bytes>` for either storage form.

Reflection also exposes one Bytes value, without revealing how it is stored.
Converting a reflected value back preserves its contents, including bytes that
have never been saved. Getting the length uses metadata and never loads the
contents from storage.

Internally, in-memory bytes are called ephemeral and saved bytes are persistent.
Persistent values reference immutable data by its SHA-256 content hash and
length. In-memory values calculate and cache that same hash when a comparison,
hash, sort, or save needs it. Creating or reading temporary bytes does not
require hashing or saving them. Comparisons do not read from or write to storage.
Trace replay checks reuse the cached hash too.

Sorting uses content hashes and lengths, not alphabetical or byte-by-byte order.
The resulting order is stable across storage forms.

Bytes can be dictionary keys. Independently created bytes with the same contents
find the same entry, including after saving and loading the dictionary. The first
key comparison or hash can scan the contents; later operations reuse the cached hash.

### Why Bytes are allowed as keys

A dictionary key means the same thing as `==`, for every value. Bytes compare by
content, so they meet that rule the same way strings and lists do. Functions and
streams stay forbidden because they have no content equality.

Reflection also depends on this. A reflected dictionary stores its entries as
`Dict<Dval, Dval>`, and a reflected `Dval` can hold `DBytes`. If Bytes were
forbidden as keys, the type checker would reject `Dval` as a key type, and with it
the reflection types and everything that uses them. Restoring the ban in the
checker made 191 more declarations fail across the tree, including `reflect`.

Two costs to keep in mind:

- The first time an unsaved Bytes value is used as a key, its whole payload is
  hashed. A multi-megabyte key costs one full scan, once.
- A dictionary keyed by Bytes iterates in content-hash order, which looks
  arbitrary. Do not rely on it matching the order the keys were added or their
  byte order.

Saving a dictionary rewrites any unsaved Bytes keys into saved ones
(`Dval.rewriteWith`), so the saved dictionary finds the same entries.

## Allocation check

`scripts/perf/workloads/probes/bytes.dark` measures creation, first comparison,
and cached comparison separately. On the Linux ARM64 AOT build on October 16,
2026, hashing a newly created value added 400 bytes of allocation compared with
creating it and comparing it with itself. This was the same for 16-byte and
64-KiB inputs. Cached comparisons allocated 320 bytes per probe iteration for
both sizes, including the interpreter and loop overhead (the baseline was
152 bytes). Two runs gave the same numbers.

These are allocation measurements, not timing measurements. The first hash
still scans the entire payload; later comparisons reuse it.
