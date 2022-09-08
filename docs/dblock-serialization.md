In Jan 2020, we changed the dblock internal representation.

Before, it was a partially applied, in-memory only, OCaml representation,
which was not possible to serialize.

Afterwards, it was a combination of param names, an expr for the body, and a
symtable, all of which could be serialized.

However, we made the explicit decision
(https://github.com/darklang/dark/pull/1780) to not allow dblocks to be
deserialized in a user-visible way. In discussing it, we realized there real
challenges:

- users receiving malicious Dark code in HTTP requests from strangers
- users saving values in queues or databases, that may not deserialize
  properly later in newer versions of dark
- users saving malicious code in the DB that only appears later.

As such, we don't want users to be able to arbitrarily save lambdas at the
moment. This may change as we develop the product and realize that there are
good reasons to support it.

Right now (Jan 2020), we only serialize dblocks that we send to the user via
analysis. So we don't save them in events/workers/queues, stored events, or
user data (datastores). They only serialize via the automatic yojson
serialization to pass between the AnalysisWorker and the client App.

If we change this, we will likely need to do some form of versioning.
