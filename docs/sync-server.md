# Running a sync server

A sync server keeps one Dark store that everyone's machines sync with. Each person gets a token; each machine runs `dark sync add <url> --token -` once and stays in step from then on. Your drafts go only to your own machines. What you commit goes to everyone.

The server is `dark sync serve --listen <addr>`: the same `dark`, on its own store. It speaks plain HTTP on one port, with a WebSocket per connected machine at `/sync` and a health check at `/health`. TLS is done in front of it, by Fly's proxy or by Caddy on a VM, so the server never holds a certificate.

The files are in `deploy/sync-server/`:

| File | What it is |
|---|---|
| `Dockerfile` | the image: Ubuntu 24.04, a release `dark`, the store in `/data` |
| `entrypoint.sh` | starts the server as user `dark`, from the settings below |
| `prepare` | puts the `dark` the image ships beside the Dockerfile |
| `fly.toml` | Fly.io: one machine, one volume, TLS at Fly's edge |
| `gcp/compose.yaml`, `gcp/Caddyfile` | one VM (Google Compute Engine or any Docker host), Caddy for TLS |

## Settings

The CLI reads no environment variables; the entrypoint turns these into `dark sync serve` flags.

| Variable | Default | What it sets |
|---|---|---|
| `DARK_SYNC_LISTEN` | `:8080` | where to listen |
| `DARK_SYNC_BACKUP_EVERY_HOURS` | `6` | how often the store is backed up to `/data/backups` |
| `DARK_SYNC_BACKUP_KEEP` | `28` | how many of those backups are kept (a week) |
| `DARK_SYNC_MAX_FRAME_MB` | `256` | the largest message the server takes |
| `DARK_SYNC_BOOTSTRAP_PERSON`, `DARK_SYNC_BOOTSTRAP_TOKEN` | | a token kept on every start, so the first person can connect before anyone has a shell |
| `DARK_SYNC_MAINTENANCE` | | when set, the container starts without serving (to restore a backup) |

## Build the image

Every option below starts from one `dark` binary for linux-x64.

```
scripts/build/build-release-cli-exes.sh --runtimes=linux-x64   # in the dev container
deploy/sync-server/prepare --built                              # or: prepare <path to dark>
docker build -t dark-sync deploy/sync-server
```

## Deploy on Fly.io

Pick an app name and set it in `fly.toml`. Then:

```
cd deploy/sync-server
fly apps create <app>
fly volumes create dark_sync_data --region yul --size 3

# The first token: made here, kept on the server only as a hash.
token="dsync_$(openssl rand -base64 32 | tr '+/' '-_' | tr -d '=')"
fly secrets set DARK_SYNC_BOOTSTRAP_PERSON=stachu DARK_SYNC_BOOTSTRAP_TOKEN="$token"
fly deploy

curl https://<app>.fly.dev/health
printf '%s\n' "$token" | dark sync add https://<app>.fly.dev --token -
```

The health check answers JSON: `status`, the sync `format`, the `build`, the store's id, its journal position and how many sessions are open.

## Deploy on Google Cloud

Use one Compute Engine VM with a persistent disk. Cloud Run is not an option: the store is one SQLite file that needs a local disk and one writer, and Cloud Run's volumes are network file systems; its request timeout also cuts a session after an hour at most.

```
gcloud compute disks create dark-sync --size 10GB --zone us-east1-b
gcloud compute instances create dark-sync --zone us-east1-b --machine-type e2-small \
  --image-family debian-12 --image-project debian-cloud \
  --disk name=dark-sync,device-name=dark-sync,mode=rw,auto-delete=no \
  --tags https-server
gcloud compute firewall-rules create dark-sync-https --allow tcp:80,tcp:443 --target-tags https-server
gcloud compute resource-policies create snapshot-schedule dark-sync-daily \
  --region us-east1 --daily-schedule --start-time 04:00 --max-retention-days 14
gcloud compute disks add-resource-policies dark-sync --zone us-east1-b \
  --resource-policies dark-sync-daily
```

Point a DNS name (say `sync.example.com`) at the VM's address. On the VM, with Docker installed:

```
sudo mkfs.ext4 -m 0 /dev/disk/by-id/google-dark-sync        # first time only
sudo mkdir -p /mnt/disks/dark-sync
echo '/dev/disk/by-id/google-dark-sync /mnt/disks/dark-sync ext4 discard,defaults 0 2' | sudo tee -a /etc/fstab
sudo mount -a
sudo mkdir -p /mnt/disks/dark-sync/data /mnt/disks/dark-sync/caddy

# The first token, as on Fly. The file is root's alone.
printf 'DARK_SYNC_BOOTSTRAP_PERSON=stachu\nDARK_SYNC_BOOTSTRAP_TOKEN=%s\n' "$token" \
  | sudo tee /mnt/disks/dark-sync/server.env >/dev/null
sudo chmod 600 /mnt/disks/dark-sync/server.env

docker load < dark-sync.tar          # the image, built as above and copied over
cd deploy/sync-server/gcp
DARK_SYNC_DOMAIN=sync.example.com docker compose up -d
```

Caddy gets and renews the certificate itself.

## Add a person

On the server (`fly ssh console -C "dark-sync-entrypoint ..."` on Fly; `docker compose exec dark-sync dark-sync-entrypoint ...` on a VM):

```
dark-sync-entrypoint sync token new oceanoak --label laptop
```

It prints the token once, on stdout, and keeps only its hash. Send it to them over something private. On their machine:

```
dark sync add https://<server> --token -       # paste it, then Enter
```

A person can have a token per machine (`--label` says which), or one for all of them. `--read-only` makes a token that takes everything committed and sends nothing.

## Rotate or revoke a token

```
dark-sync-entrypoint sync token list
dark-sync-entrypoint sync token new stachu --label laptop      # the new one
dark-sync-entrypoint sync token revoke 336d783d                # the old one, by id
```

`revoke` takes an id or a person's name (all their tokens). A revoked token's open sessions end within half a minute, and its machine says so at its next command: `sync: <server> refused this machine's token: it was revoked.` There, `dark sync off` and `dark sync add <url> --token -` with the new token.

## Back up and restore

The server backs its store up every 6 hours into `/data/backups`, keeping a week, each one a whole SQLite file taken while it runs. Fly also snapshots the volume daily (`snapshot_retention` in `fly.toml`); on GCP, the snapshot schedule above does.

A backup now, and a copy of it off the server:

```
dark-sync-entrypoint sync backup /data/backups/manual.db
fly ssh sftp get /data/backups/manual.db                         # Fly
gcloud compute scp dark-sync:/mnt/disks/dark-sync/data/backups/manual.db .   # GCP
```

To restore, stop serving, restore, serve again:

```
fly secrets set DARK_SYNC_MAINTENANCE=1                           # restarts without serving
fly ssh console -C "dark-sync-entrypoint sync restore /data/backups/dark-sync-20261119-060000.db"
fly secrets unset DARK_SYNC_MAINTENANCE
```

On a VM, `docker compose stop dark-sync`, then `docker compose run --rm dark-sync sync restore <file>`, then `docker compose start dark-sync`.

`restore` checks the file is a whole store, keeps the current one beside it (`data.db.before-restore-<time>`), and gives the store a new identity. Every machine then compares whole stores with it once, and sends back whatever the backup lacks, so work made after the backup isn't lost while some machine still holds it.

## Upgrade

Build the image from the new `dark` and deploy it (`fly deploy`, or `docker load` and `docker compose up -d`). The server stops on SIGTERM: it takes no new connections, ends its sessions between imports, and exits; machines reconnect and resume where they were. The store is migrated on start, as any `dark` migrates its store.

When the sync format changes, a machine on the old `dark` is told `the server runs a dark that speaks another sync format` and syncs nothing until it is upgraded too; its work stays on it meanwhile.

## Watch it

The server writes one JSON object a line to stdout (`fly logs`, `docker compose logs dark-sync`). Every line has `at` and `event`:

| Event | When |
|---|---|
| `listening` | started: the address, store id, format, build, live tokens, backup directory |
| `session-start`, `session-end` | a machine connected or left: session id, person, token id, how it ended, how long |
| `session` | what a session did: `hello from laptop (stachu, token 336d783d)`, bundles sent and received with sizes and times |
| `notice` | a conflict the server recorded (nobody acts on these; the people's machines record their own) |
| `refused` | a connection turned away: `unknown-token`, `revoked` or `format` |
| `backup`, `backup-pruned`, `backup-failed` | the scheduled backups |
| `signal`, `stopping`, `stopped` | a SIGTERM or SIGINT; the sessions still open as it stops; the exit after they ended |

## How it's kept safe

- Tokens are 32 random bytes. The server keeps only their SHA-256, so neither a backup nor a copy of the store lets anyone in. On a machine, the token is a file only its user can read (`~/.darklang/sync/token`), and no command prints it.
- A token never crosses the internet in the clear: `dark sync add` refuses `http://` except to this machine, a private network or a tailnet.
- A store belongs to the first person it syncs as; another person's token can't use it, so nobody receives someone else's drafts by claiming their store.
- A machine whose clock runs more than 5 minutes ahead of the server's has its changes turned away until the clock is right, and is told why. Otherwise it would win every edit made at the same time, and pull everyone's clocks forward with it.
- The server takes no message over `DARK_SYNC_MAX_FRAME_MB`, and only one import at a time.
