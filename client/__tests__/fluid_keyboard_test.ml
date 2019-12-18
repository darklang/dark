open! Tc
open Jest
open Expect
open FluidKeyboard

let () =
  describe "key handling" (fun () ->
      testAll
        "fromChar and toChar roundtrip every printable ASCII character"
        (List.range 32 126)
        (fun i ->
          let c = Char.fromCode i |> Option.valueExn in
          expect (fromChar c |> toChar) |> toEqual (Some c) ) ) ;
  ()
