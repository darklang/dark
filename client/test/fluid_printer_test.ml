open Tester
open Prelude
open FluidTokenizer
open Fluid_test_data
module E = FluidExpression

let run () =
  let hasTokenMatch ~(f : fluidToken -> bool) (e : E.t) =
    let tokens = (toTokens' e Builder.empty).tokens in
    expect (List.any tokens ~f) |> toEqual true
  in
  describe "toTokens' converts expressions to tokens" (fun () ->
      test "field access keeps parentBlockID" (fun () ->
          let parentID = Some (gid ()) in
          let tokens = (toTokens' ~parentID aField Builder.empty).tokens in
          expect
            (List.any tokens ~f:(function
                | TVariable (_, _, pbid) when pbid = parentID ->
                    true
                | _ ->
                    false))
          |> toEqual true) ;
      test "empty object literal does not have parentBlockID" (fun () ->
          emptyRecord
          |> hasTokenMatch ~f:(function
                 | TRecordOpen (_, None) | TRecordClose (_, None) ->
                     true
                 | _ ->
                     false)) ;
      test "object literal keeps parentBlockID in fields" (fun () ->
          let parentBlockID = gid () in
          E.ERecord (parentBlockID, [recordRow1])
          |> hasTokenMatch ~f:(function
                 | TRecordFieldname d when d.parentBlockID = Some parentBlockID
                   ->
                     true
                 | _ ->
                     false)) ;
      test "empty list literal does not have parentBlockID" (fun () ->
          emptyList
          |> hasTokenMatch ~f:(function
                 | TListOpen (_, None) | TListClose (_, None) ->
                     true
                 | _ ->
                     false)) ;
      test "list literal keeps parentBlockID in the items" (fun () ->
          let parentBlockID = gid () in
          E.EList (parentBlockID, [fiftySix; seventyEight])
          |> hasTokenMatch ~f:(function
                 | TInteger (_, _, pid) when pid = Some parentBlockID ->
                     true
                 | _ ->
                     false))) ;
  ()
