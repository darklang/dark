open Tester
open Prelude
open FluidPrinter
open Fluid_test_data
module E = FluidExpression

let run () =
  let hasTokenMatch ~(f : fluidToken -> bool) (tokens : fluidToken list) =
    expect (List.any tokens ~f) |> toEqual true
  in
  describe "toTokens' converts expressions to tokens" (fun () ->
      test "field access keeps parentBlockID" (fun () ->
          let parentID = Some (gid ()) in
          let tokens = (toTokens' ~parentID aField Builder.empty).tokens in
          tokens
          |> hasTokenMatch ~f:(fun t ->
                 match t with
                 | TVariable (_, _, pbid) when pbid = parentID ->
                     true
                 | _ ->
                     false)) ;
      test "empty object literal does not have parentBlockID" (fun () ->
          let tokens = (toTokens' emptyRecord Builder.empty).tokens in
          tokens
          |> hasTokenMatch ~f:(fun t ->
                 match t with
                 | TRecordOpen (_, None) | TRecordClose (_, None) ->
                     true
                 | _ ->
                     false)) ;
      test "object literal keeps parentBlockID in fields" (fun () ->
          let parentBlockID = gid () in
          let expr = E.ERecord (parentBlockID, [recordRow1]) in
          let tokens = (toTokens' expr Builder.empty).tokens in
          tokens
          |> hasTokenMatch ~f:(fun t ->
                 match t with
                 | TRecordFieldname _ ->
                     FluidToken.parentBlockID t = Some parentBlockID
                 | _ ->
                     false)) ;
      test "empty list literal does not have parentBlockID" (fun () ->
          let tokens = (toTokens' emptyList Builder.empty).tokens in
          tokens
          |> hasTokenMatch ~f:(fun t ->
                 match t with
                 | TListOpen (_, None) | TListClose (_, None) ->
                     true
                 | _ ->
                     false)) ;
      test "list literal keeps parentBlockID in the items" (fun () ->
          let parentBlockID = gid () in
          let expr = E.EList (parentBlockID, [fiftySix; seventyEight]) in
          let tokens = (toTokens' expr Builder.empty).tokens in
          tokens
          |> hasTokenMatch ~f:(fun t ->
                 match t with
                 | TInteger _ ->
                     FluidToken.parentBlockID t = Some parentBlockID
                 | _ ->
                     false))) ;
  ()
