module App

(* open LibExecution *)

printfn "App"
printfn "%s" ((LibExecution.Runtime.DStr "test").ToString())
