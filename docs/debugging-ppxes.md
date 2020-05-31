## Debugging ppx stuff

PPX is an ocaml preprocessor we use. The ppx libraries are all pretty
opaque. To read the preprocessed output, run dune with the --verbose
flag to see the commands run. You'll see something that looks like this:

```
./.ppx/landmarks.ppx+lwt.ppx+ppx_bin_prot+ppx_deriving.std+ppx_deriving_yojson+ppx_fields_conv+ppx_sexp_conv+ppx_pipebang/ppx.exe --dump-ast --cookie 'library-name="dark"' -o lib/types.pp.ml --impl lib/types.ml
```

Run that without the --dump-ast and then look at the .pp.ml file to find
the preprocessed version.


