## Other ways to run the dev container

### Just serve it, not constantly recompiling

- Run `scripts/builder --compile --serve`

Note that if you're using a VS Code devcontainer, this will happen automatically and
you don't need to run it.

### (Not) Rebuilding the dev container

If you pull a commit with a Dockerfile update, and then restart your
`scripts/builder` script -- it will rebuild as much of the container as possible.

If you don't want to rebuild the container, use `NEVER_REBUILD_DOCKER=1 scripts/builder ...`
to make the the build script use the last built one.

In another shell you can now kick off a `scripts/builder --compile` to rebuild the container
in parallel with your currently working one.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker
invocations, use the old+running container as opposed to attempting to use the
container that has an in progress build.
