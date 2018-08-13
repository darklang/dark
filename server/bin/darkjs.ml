let () =
  Libfrontend.Init.init ();
  Js.export "darkAnalysis"
    (object%js
       method performAnalysis tlids =
         Libfrontend.Init.perform_analysis tlids
     end);
  ()
