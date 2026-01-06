# Trim apt Installs

**Status**: [x] Complete

## Review Current apt Packages

After removing Docker, GCloud, Java, etc., review remaining packages in `Dockerfile`.

## Packages to KEEP (definitely needed)

```
# Core tools
curl, apt-transport-https, ca-certificates, gpg, gpg-agent
rsync, git, wget, sudo, locales, git-restore-mtime
jq, parallel, vim, unzip, file
less, pv, htop

# .NET dependencies
libc6, libgcc1, libgssapi-krb5-2, libicu74, libssl3, libstdc++6, zlib1g

# Build tools
build-essential  # For tree-sitter parser

# SQLite
sqlite3

# Node.js
nodejs

# Python
python3-pip, python3-setuptools, python3-dev, python3-venv

# Sodium (crypto)
libsodium-dev, libssl-dev, zlib1g-dev

# Misc
bash-completion, net-tools, psmisc
```

## Packages to REVIEW

| Package | Current Use | Decision |
|---------|-------------|----------|
| `ntp` | Time sync | REVIEW - needed in container? |
| `openssh-server` | SSH | REMOVE - not needed |
| `dnsutils` | DNS lookup | REVIEW - debugging only? |
| `sshpass` | SSH auth | REMOVE - was for prodexec |
| `expect` | CLI tests | KEEP - needed for tests |

## Packages Already Removed (in other tasks)

- `docker-ce`, `docker-buildx-plugin`
- `google-cloud-sdk`, `google-cloud-sdk-pubsub-emulator`, `google-cloud-sdk-gke-gcloud-auth-plugin`
- `openjdk-11-jdk`

## Steps

1. [ ] Review each package in Dockerfile apt install
2. [ ] Remove `openssh-server` (not needed)
3. [ ] Remove `sshpass` (was for prodexec)
4. [ ] Review and decide on `ntp`, `dnsutils`
5. [ ] Consolidate apt install blocks if possible
6. [ ] Run `./scripts/run-backend-tests`
7. [ ] Commit: `trim: remove unused apt packages`

## Commit Message Template

```
trim: remove unused apt packages

- Remove openssh-server (not needed in dev container)
- Remove sshpass (was for ProdExec SSH)
- [Remove ntp/dnsutils if decided]

Reduces container size and build time.
```

## Notes

- Be conservative - it's easier to remove too little than break the build
- Some packages may be transitive dependencies
- Test thoroughly after changes
