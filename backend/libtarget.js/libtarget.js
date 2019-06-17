//Provides: dark_targetjs_digest384
function dark_targetjs_digest384(s) {
  var b64 = analysiswrapper.sha2.SHA384(s).toString("base64");
  // the built-in Buffer.toString uses a different alphabet than the server.
  return b64.replace(/\//g, "_").replace(/\+/g, "-");
}

//Provides: dark_targetjs_digest256
function dark_targetjs_digest256(s) {
  var b64 = analysiswrapper.sha2.SHA256(s).toString("base64");
  // the built-in Buffer.toString uses a different alphabet than the server.
  return b64.replace(/\//g, "_").replace(/\+/g, "-");
}

// Temporary, should be fixed in js_of_ocaml >3.2.1:
// https://github.com/ocsigen/js_of_ocaml/issues/693
// https://github.com/ocsigen/js_of_ocaml/commit/390fd5f1d2bbaed8d9da3df2a77a6b67f248ebbb

//Provides: caml_restore_raw_backtrace
function caml_restore_raw_backtrace(exn, bt) {
  return 0;
}
