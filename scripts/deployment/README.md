# Deployments (and shipit)

Deployments in Dark are almost all done through shipit, a command-based python script
that handles deploying k8s services.

# Deployment

Each service is configured using a `shipit.yaml` file in the subdirectory. The keys of this file are:

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
  A list of config maps to deploy

- `k8s.manually-deployed.configmaps.[].name`:
  The k8s name of the config map

- `k8s.manually-deployed.configmaps.[].from-file`:
  Deploy the config map from a file

- `k8s.manually-deployed.configmaps.[].from-file.key`:
  The key for the file, optional, will be derived by k8s otherwise (uses `basename` at
  time of writing.)

- `k8s.manually-deployed.configmaps.[].from-file.filename`:
  The file to be put in the keymap

- `k8s.manually-deployed.configmaps.[].from-env`:
  Deploy the config map from an env-file (not implemented yet)

- `k8s.release.configmaps`:
  A list of config maps to deploy

- `k8s.release.configmaps.[].name`:
  The k8s name of the config map

- `k8s.release.configmaps.[].from-file`:
  Deploy the config map from a file

- `k8s.release.configmaps.[].from-file.key`:
  The key for the file, optional, will be derived by k8s otherwise (uses `basename` at
  time of writing.)

- `k8s.release.configmaps.[].from-file.filename`:
  The file to be put in the keymap

- `k8s.release.configmaps.[].from-env`:
  Deploy the config map from an env-file (not implemented yet)

- `k8s.release.template`:
  Template file of a release. During a deploy, the template is filled with vars from
  `containers` (automatically derived), `builtins`, and `expected-args` (which are
  filled in from the command line using `--arg`).

- `k8s.deployment.containers`:
  List of containers used in this deployment. The container name must match a
  container in `../containers/` and the ID will be provided as a template var.

- `k8s.deployment.builtins`:
  List of builtins to replace in the template. Currently the only builtin is
  `CLOUDSQL_INSTANCE_NAME`.

- `k8s.deployment.expected-args`:
  These arguments are expected to be passed on the command line to `deploy deployment apply` using `--arg=`.

# Commands (`*` is not implemented yet):

- config apply-manually [single-service] (TODO: honeymarker and rollbar deploy)
- config diff [services]
- containers build [services]
- containers pull [services]
- containers push [services]
- containers show-manifest [services] > MANIFEST-FILE.json
- containers list [services]
- release prepare [services] --arg CHANGE_CAUSE='reason' --manifest=MANIFEST-FILE.json
- release diff [services]
- release push [services] (TODO: honeymarker and rollbar deploy)
