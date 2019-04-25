open! Tc
open Types
open Jest
open Expect
open Viewport

let () =
  describe "isEnclosed" (fun () ->
      test "box inside of viewport" (fun () ->
          let viewport = ({x = 0; y = 0}, {w = 800; h = 600}) in
          let box = ({x = 10; y = 20}, {w = 500; h = 250}) in
          expect (isEnclosed viewport box) |> toBe true ) ;
      test "box outside of viewport" (fun () ->
          let viewport = ({x = 0; y = 0}, {w = 800; h = 600}) in
          let box = ({x = 10; y = -900}, {w = 500; h = 250}) in
          expect (isEnclosed viewport box) |> toBe false ) ;
      test "box partially in viewport should return true" (fun () ->
          let viewport = ({x = 0; y = 0}, {w = 800; h = 600}) in
          let box = ({x = 700; y = 20}, {w = 500; h = 250}) in
          expect (isEnclosed viewport box) |> toBe true ) ;
      () ) ;
  ()
