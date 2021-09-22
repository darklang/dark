There is one directory in this for each _conceptual_ service we're running in
production. Some _conceptual_ services are multiple k8s deployments (such as
custom-domains) but they're kept together as they're part of one logical
concept. All deployments are managed by `shipit`, see [scripts/deployment/README.md].
