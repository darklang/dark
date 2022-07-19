# Running the client against production

If you want to run your local client against the production server, we have a way to do that using ngrok

Note: `<your-username>` below should be your Darklang username.
It's intended to disambiguate between users so we don't run over each other's
tunnels. It might also be a good idea to use some random digits as a suffix so
people can't get access to our local servers by guessing our usernames.

## Set up `ngrok` to connect to the server

- Join our ngrok account by asking Paul
- install ngrok: `brew cask install ngrok` on OS X, or follow instructions at
  https://ngrok.com/download for Linux. (The snap installer is broken.)
- authorize your ngrok client: `ngrok authtoken YOURTOKEN` (found at
  https://dashboard.ngrok.com/auth)
- run your tunnel:
  `ngrok http darklang.localhost:8000 --hostname=darklang-<your-username>.ngrok.io`

This exposes your `localhost:8000` server at the forwarded public address, and
should show something like:

```
ngrok

Visit http://localhost:4040/ to inspect, replay, and modify your requests

Session Status                online
Account                       Dark (Plan: Basic)
Version                       3.0.6
Region                        United States (us)
Latency                       -
Web Interface                 http://127.0.0.1:4040
Forwarding                    https://darklang-dark.ngrok.io -> http://darklang.localhost:8000

Connections                   ttl     opn     rt1     rt5     p50     p90
                              0       0       0.00    0.00    0.00    0.00
```

## Run your local client on darklang.com URLs

Now that you're connected, load a production canvas, including a query param of
`localhost-assets=<your-username>` in the URL.
i.e. `https://darklang.com/a/<canvas-name>?localhost-assets=<your-username>`

Static assets will then be loaded from `darklang-<username>.ngrok.io` instead
of `static.darklang.com`. You can verify that static assets are being loaded
via ngrok by:
- checking the "Connections" in the `ngrok` terminal window
- checking `ngrok`'s "Web Interface" at the URL noted in the console
- checking your browser's dev console for the request URLs used
- running a tail on the server logs: `tail -f rundir/logs/server.log`.

Note that this URL is public as long as you have the `ngrok` tunnel open, so
you can showcase your local `client` changes to others this way.

## Simplify the `ngrok` command

You can simplify the `ngrok` command to `ngrok start darklang` by adding to your
  `~/.ngrok2/ngrok.yml`:
```yaml
tunnels:
  darklang:
    proto: http
    addr: 8000
    subdomain: darklang-<your-username>
```