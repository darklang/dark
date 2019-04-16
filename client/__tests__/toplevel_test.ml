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
      test "keyForHandlerSpec" (fun () ->
          expect
            (keyForHandlerSpec handlerData.spec.module_ handlerData.spec.name)
          |> toEqual "JOB:processOrder" ) ;
      test "dbsByName" (fun () ->
          expect (dbsByName toplevels)
          |> toEqual
               (StrDict.insert ~key:"Books" ~value:(dbtlid, []) StrDict.empty)
      ) ;
      test "handlersByName" (fun () ->
          expect (handlersByName toplevels)
          |> toEqual
               (StrDict.insert
                  ~key:"JOB:processOrder"
                  ~value:h1tlid
                  StrDict.empty) ) ;
      () ) *)
