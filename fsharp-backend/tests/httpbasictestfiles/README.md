CLEANUP separate PR to:
- create `httphandlertestfiles` dir
- move `httptestfiles` to `httphandlertestfiles/http`
  - except for the `README.md`, which can just sit at `httphandlertestfiles`
- move _these_ files to `httphandlertestfiles/httpbasic`

(This is only a separate file so a PR doesn't look too noisy, moving files around.)

See in `httptestfiles/README.md` for how these files are parsed/used.