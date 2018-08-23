open Js_of_ocaml

type js_string = Js.js_string Js.t

let () =
  Libfrontend.Init.init ();
  Js.export "darkAnalysis"
    (object%js
       method performAnalysis (tlids: js_string) : js_string =
         let tlids = Js.to_string tlids in
         let result = Libfrontend.Init.perform_analysis tlids in
         Js.string result
     end);
  ()
