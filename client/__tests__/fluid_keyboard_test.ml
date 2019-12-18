open Tc
open Tester
open FluidKeyboard

let run () =
  describe "key handling" (fun () ->
      testAll
        "roundtrips every printable ASCII character"
        (List.range 32 126)
        (fun i ->
          let c = Char.fromCode i |> Option.valueExn in
          expect (fromChar c |> toChar) |> toEqual (Some c) ) ) ;
  ()
