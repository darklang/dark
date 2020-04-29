open Tester
open Prelude
open FluidPrinter
open Fluid_test_data
module E = FluidExpression

let run () =
  describe "toTokens' converts expressions to tokens" (fun () ->
      test "field access keeps parentBlockID" (fun () ->
          let parentID = Some (gid ()) in
          let tokens = (toTokens' ~parentID aField Builder.empty).tokens in
          expect
            ( List.find tokens ~f:(fun t ->
                  match t with
                  | TVariable (_, _, pbid) when pbid = parentID ->
                      true
                  | _ ->
                      false)
            |> Option.isSome )
          |> toEqual true)) ;
  ()
