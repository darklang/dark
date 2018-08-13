let () =
  Libfrontend.Init.init ();
  Js.export "darkAnalysis"
    (object%js
       method performAnalysis (tlids:string) =
         Libfrontend.Init.perform_analysis tlids
     end);
  ()
