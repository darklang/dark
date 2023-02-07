# Deploy lock

This directory also hosts scripts that affect the deploy lock. We only allow one
deploy to go out at once, and we want to strictly ensure that they are deployed in
the order they were merged (so that a slow build does not overwrite the deploy from a
later build that happened to faster). Each deploy adds a file, which we call a
lock-identifier, to a dark canvas at the start of a build that will
be deployed. Later on, when it is time to deploy, the build will check if it owns the
earliest lock-identifier; if so it will deploy. If it doesn't, it will wait a
reasonable amount of time, before failing.

There are two key parts to making this successful:

- add the lock identifiers at the start of the build. This ensures that fast builds
  that start later cannot outpace slow builds.

- using commit timestamps: we assume that commit timestamps are correctly ordered.
  I'm not sure this is reliable as GitHub creates the merge commits, and it is possible
  that there is clock skew between github servers. So we'll have to see how that works
  out.
