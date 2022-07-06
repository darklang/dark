# Serialization

Dark has a lot of serializers. At time of writing:

- binary serializers to store user code
- many, many dval serializers to show code to users, save code in the DB, respond to
  API requests, etc. These are mostly handwritten formats (and are often buggy as a result).
- the "Vanilla" Json serializer which serializes arbitrary types
- the "OCamlCompatible" Json serializer which is used in situations where it needs to
  be compatible with the old OCaml json serializers - we would like to replace this
  with the Vanilla serializer in all cases

## Goals

Our goals with serializers are:

- never change a serialization format in a way that could break users' code
- deprecate and remove old/broken serialization formats
- remove the OCamlCompatible serializer

## Testing

To accomplish these, we have robust serialization tests, implemented in Serialization.Tests.fs

For each serialization format in Dark, we store serialized data from that format, to
ensure it never changes. The serialized data should be quite extensive, covering as
many cases as we can reasonably think of.

They help solve our goals in the following ways.

### Goal: never changing formats

If the format changes, we want to know about it. The saved serialized data ensures this: if we change something, there's a git diff to tell us something changed, and we can choose to commit it if that's a backward-compatible change or we determine that it's otherwise OK.

#### Vanilla / OCamlCompatible

It's especially easy to accidentally change the format in Vanilla/OCamlCompatible
serializers, as those formats just use existing types. To ensure backward
compatibility, we:

- require serialization types to be explicitly listed (using
  `Json.Vanilla.allow<type>()`). Attempting to serialize or deserialize a type that
  isn't explicitly allowed will throw an exception.

- require a test case for each top-level type that will be serialized. That way if we
  change the toplevel type, we'll see the change to the format. Tests will fail if an allowed type does not have a test case, or if there is a test case for a type that hasn't been allowed

#### Binary Serialization

There is a serialized file for an extensive test case.

#### Other serializers

TBD

### Goal: removing the OCamlCompatible serializer

The serialized test data will allow us to see places in which we can change the
serializer directly over to the Vanilla one.

### Goal: deprecating old formats

By listing all the formats, we can occasionally go through them to see if any case be
removed.