There is one directory in this for each _conceptual_ service we're running in
production. Some _conceptual_ services are multiple k8s deployments (such as
custom-domains) but they're kept together as they're part of one logical
concept.

# Deployment

Deployments and such are managed by scripts/deployment/deploy. Each service is configured using a `deploy.yaml` file in the subdirectory. The keys of this file are:

- `k8s.manually-deployed-config.configs`:
  pure config files that are manually deployed. The vast majority of config files
  should be this, so that we can watch them go out and check that they actually
  work. These are used with `deploy config apply-manually` and `deploy config diff`

- `k8s.manually-deployed-config.custom-diff`:
  A command to diff the configs in this service.

- `k8s.manually-deployed-config.custom-apply`:
  A list of commands to apply the configs in this service

- `k8s.manually-deployed-config.custom-post-apply`:
  A list of commands to run after other steps are run. Useful for restarting services.

- `k8s.manually-deployed-config.config-maps`:
  A list of config maps to deploy

- `k8s.manually-deployed-config.config-maps.[].name`:
  The k8s name of the config map

- `k8s.manually-deployed-config.config-maps.[].from-file`:
  Deploy the config map from a file

- `k8s.manually-deployed-config.config-maps.[].from-file.key`:
  The key for the file, optional, will be derived by k8s otherwise (uses basename at
  time of writing.)

- `k8s.manually-deployed-config.config-maps.[].from-file.filename`:
  The file to be put in the keymap

- `k8s.manually-deployed-config.config-maps.[].from-env`:
  Deploy the config map from an env-file (not implemented yet)

- `k8s.deployment.template`:
  Template file of a deployment. During a deploy, the template is filled with vars
  from `containers` (automatically derived) and `template-vars` (filled in from
  command line or from default)

- `k8s.deployment.containers`:
  List of containers used in this deployment. The container name must match a
  container in `../containers/` and the ID will be provided as a template var.

- `k8s.deployment.builtins`:
  List of builtins to replace in the template. Currently the only builtin is
  `CLOUDSQL_INSTANCE_NAME`.

- `k8s.deployment.expected-args`:
  These arguments are expected to be passed on the command line to `deploy deployment apply` using `--arg=`.

# Commands (`*` is not implemented yet):

- config apply-manually [services] (TODO: honeymarker and rollbar deploy)
- config diff [services]
- \*config dry-run [services]
- containers build [services]
- containers pull [services]
- containers push [services]
- containers show-manifest [services]
- containers list [services]
- deployment diff [services] --args CHANGE_CAUSE='reason' --manifest=FILE
- deployment dry-run [services] --args CHANGE_CAUSE='reason' --manifest=FILE
- \*deployment apply [services] --args CHANGE_CAUSE='reason' --manifest=FILE (TODO: honeymarker and rollbar deploy)
- \*deployment status [services]
- \*test [services]
