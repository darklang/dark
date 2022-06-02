# Deployments (and shipit)

Deployments in Dark are almost all done through shipit, a command-based python script
that handles deploying k8s services.

# Deployment

Each service is configured using a `shipit.yaml` file in the subdirectory. The keys of this file are:

- `k8s.namespace`:
  The kubernetes namespace that this service is in. Other files referred to by shipit should also be in this namespace, and have their namespace set in their files to match.

- `k8s.manually-deployed.configs`:
  pure config files that are manually deployed. The vast majority of config files
  should use this, so that we can watch them go out and check that they actually
  work. These are used with `deploy config apply-manually` and `deploy config diff`

- `k8s.manually-deployed.custom-diff`:
  A command to diff the configs in this service.

- `k8s.manually-deployed.custom-apply`:
  A list of commands to apply the configs in this service

- `k8s.manually-deployed.custom-post-apply`:
  A list of commands to run after other steps are run. Useful for restarting services.

- `k8s.manually-deployed.configmaps`:
  A dict of config maps to deploy

- `k8s.manually-deployed.configmaps.CONFIGMAPNAME.text-file`:
  Deploy the config map from a text file

- `k8s.manually-deployed.configmaps.CONFIGMAPNAME.env-file`:
  Env file to use to create/replace a configmap

- `k8s.release.configmaps`:
  A list of config maps to deploy

- `k8s.release.versioned-configmaps.CONFIGMAP.text-file`:
  A filename pointing to a text file: a configmap named CONFIGMAP-HASH (where HASH is
  the hash of the file) will be created from this. Configmap names are put in the
  template using `{VERSIONED-CONFIGMAP:name}`

- `k8s.release.versioned-configmaps.CONFIGMAP.env-file`:
  A filename pointing to an env-file: a configmap named CONFIGMAP-HASH (where HASH is
  the hash of the file) will be created from this. Configmap names are put in the
  template using `{VERSIONED-CONFIGMAP:name}`

- `k8s.release.config-template`:
  Template file for a release of this deployment. During a deploy, the template is
  filled with vars from `containers` (automatically derived), `builtins`, and
  `expected-args` (which are filled in from the command line using `--arg`).

- `k8s.release.containers`:
  List of containers used in this deployment. The container name must match a
  container in `../containers/` and the ID will be provided as a template var.

- `k8s.release.builtins`:
  List of builtins to replace in the template. Currently the only builtin is
  `CLOUDSQL_INSTANCE_NAME`.

- `k8s.release.expected-args`:
  These arguments are expected to be passed on the command line to `release push` using `--arg=`.

# Commands:

- `manual apply [--dry-run=server] [single-service]`
- `manual diff [services]`
- `containers build [services] --save-manifest=MANIFEST-FILE.json`
- `containers pull [services] --save-manifest=MANIFEST-FILE.json`
- `containers push [services]`
- `containers list [services]`
- `release current-manifest --save-manifest=MANIFEST-FILE.json`
- `release diff [services]`
- `release push [--dry-run] [--arg ARG] --manifest MANIFEST-FILE.json [services]`
- `release prepare [--arg ARG] --manifest MANIFEST-FILE.json [services]`
- `validate [services]`

# Deploy lock

This directory also hosts scripts that affect the deploy lock. We only allow one
deploy to go out at once, and we want to strictly ensure that they are deployed in
the order they were merged (so that a slow build does not overwrite the deploy from a
later build that happened to faster). Each deploy adds a file, which we call a
lock-identifier, to a Google Cloud Storage bucket at the start of a build that will
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
