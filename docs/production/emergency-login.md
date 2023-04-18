# Emergency Login

If login is broken because auth0 is down or the ops-login canvas (which backs
login.darklang.com) is down, on-call will still need a way to log in.

To do this, run:

- `scripts/production/connect-to-exec host`
- `./app/ProdExec emergency-login <username>`

This will:

- inserts a row into the `session` table in postgres and
- prints a session key, along with instructions for use

To use:

- you will need the cookie-inspector extension installed:
  https://chrome.google.com/webstore/detail/cookie-inspector/jgbbilmfbammlbbhmmgaagdkbkepnijn
- You'll need to be on darklang.com (or darklang.localhost, for local env login) for the following steps
- open the dev console, go to the Cookies tab, right click the table and select
  Add Cookie
- Name = `__session`, Value and Domain are printed by the script above
- Click Submit

With said cookie set, you are now logged in.
