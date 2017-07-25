open Ocamlbuild_plugin

let () =
  dispatch (function
      | Before_options ->
        Options.use_ocamlfind := true
      | After_rules | After_options | Before_rules
      | Before_hygiene | After_hygiene -> ())
