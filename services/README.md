There is one directory in this for each _conceptual_ service we're running in
production. Some _conceptual_ services are multiple k8s deployments (such as
custom-domains) but they're kept together as they're part of one logical
concept.

# Deployment

Deployments and such are managed by scripts/deployment/deploy. Each service is configured using a `deploy.yaml` file in the subdirectory. The keys of this file are:

- `k8s.manually-deployed-files`:
  pure config files that are manually deployed. The vast majority of config files
  should be this, so that we can watch them go out and check that they actually
  work. These are used with `deploy config-manually-deploy` and `deploy config-check`

- `k8s.deployment.template`:
  Template file of a deployment. This template file is filled with values such as the
  ID of containers, and some other values (TODO). During a deploy, the template is
  filled with vars from `containers`, `template-vars` and the command line. Missing
  template vars causes an error, before being deployed.

- `k8s.deployment.containers`:
  List of containers used in this deployment. The container name must match a
  container in `../containers/` and the ID will be provided as a template var.

# Commands:

- config deploy-manually [services]
- config diff [services]
- containers build [services]
- containers pull [services]
- containers list [services]
- deployment diff [services]
- deployment deploy [services]
- deployment status [services]
- test [services]
