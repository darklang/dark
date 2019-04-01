(*
open! Tc
open Types
open Jest
open Expect
open Prelude
open Toplevel
module B = Blank

let () =
  describe "Toplevel" (fun () ->
      let h1tlid = gtlid () in
      let handlerData =
        { ast = B.new_ ()
        ; spec =
            { module_ = B.newF "JOB"
            ; name = B.newF "processOrder"
            ; modifier = B.new_ () }
        ; tlid = h1tlid }
      in
      let dbtlid = gtlid () in
      let dbdata =
        { dbTLID = dbtlid
        ; dbName = Blank.newF "Books"
        ; cols = []
        ; version = 0
        ; oldMigrations = []
        ; activeMigration = None }
      in
      let toplevels =
        [ {id = h1tlid; pos = {x = 0; y = 0}; data = TLHandler handlerData}
        ; {id = dbtlid; pos = {x = 0; y = 0}; data = TLDB dbdata} ]
      in
      test "findDBNamed : has db with name" (fun () ->
          let tl =
            match findDBNamed "Books" toplevels with
            | Some tl ->
                tl.id = dbtlid
            | None ->
                false
          in
          expect tl |> toEqual true ) ;
      test "findDBNamed : does not have db with name" (fun () ->
          let tl =
            match findDBNamed "DVDs" toplevels with
            | Some tl ->
                tl.id = dbtlid
            | None ->
                false
          in
          expect tl |> toEqual false ) ;
      test "findEventNamed : has event with name and space" (fun () ->
          let tl =
            match findEventNamed "JOB" "processOrder" toplevels with
            | Some tl ->
                tl.id = h1tlid
            | None ->
                false
          in
          expect tl |> toEqual true ) ;
      test
        "findEventNamed : has event with space, but name does not match"
        (fun () ->
          let tl =
            match findEventNamed "JOB" "sendEmail" toplevels with
            | Some tl ->
                tl.id = h1tlid
            | None ->
                false
          in
          expect tl |> toEqual false ) ;
      () )
*)