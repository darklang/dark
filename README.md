# dockerfile
Build our docker container

# Garbage collection (manual)
We decided against automated GCing of these images:
1. AWS offers an age-since-pushed lifecycle policy, but if this repo is
   low-volatility, that doesn't help us. (If darklang/dark depends on the same
CI image for a long time, we'd risk losing that image.)
2. AWS offers a "keep last N" policy, but we could easily accidentally blow past
that in a flurry of experiments as happened the weekend of 2020-05-30
3. A lifecycle policy based on last-pulled-in="some period of time" would be
   ideal, but AWS does not have that

So instead we'll not GC unless/until we see disk space becoming a problem.

When that happens, we can look at darklang/dark, and specfically, preserve any
images mentioned in `.circleci/config.yml` - strings matching
`500377317163.dkr.ecr.us-east-1.amazonaws.com/dark-ci` will be relevant tags -
and delete anything not there in some not-yet-defined period of time. (Say, the
last 30 days.)
