## Running the client against production

If you want to run your local client against the production server, we have a way to do that using ngrok:

- Join our ngrok account by asking Paul
- install ngrok: `brew cask install ngrok` on OS X, or follow instructions at
  https://ngrok.com/download for Linux. (The snap installer is broken.)
- authorize your ngrok client: `ngrok authtoken YOURTOKEN` (found at
  https://dashboard.ngrok.com/auth)
- run your tunnel: `ngrok http 8000 --hostname=darklang-<username>.ngrok.io`
- You can simplify this to `ngrok start darklang` by adding to your
  `~/.ngrok2/ngrok.yml`:
```yaml
tunnels:
  darklang:
    proto: http
    addr: 8000
    subdomain: darklang-<username>
```

Use the queryparam "localhost-assets=<username>" to load static assets from darklang-<username>.ngrok.io instead of static.darklang.com.

You can check if it's going through via the ngrok console (which logs requests), and by tailing the server logs: `tail -f rundir/logs/server.log`.


