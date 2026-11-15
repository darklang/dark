# Serialization

Dark has a lot of serializers. At time of writing:

- binary serializers to store user code
- many, many dval serializers to show code to users, save code in the DB, respond to
  API requests, etc. These are mostly handwritten formats (and are often buggy as a result).

## Goals

Our goals with serializers are:

- never change a serialization format in a way that could break users' code

## Testing

`backend/tests/Tests/Serialization.Binary.Tests.fs` round-trips each binary serializer over the
values in `Serialization.TestValues.fs`, and `Serialization.DarkTypes.Tests.fs` does the same for
the ProgramTypes and RuntimeTypes mappings to Dark types.
