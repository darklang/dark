open! Tc
open Types
open Jest
open Expect
open Prelude
open Introspect
module TL = Toplevel
module B = Blank

let () =
  describe "Introspect" (fun () ->
      let h1tlid = gtlid () in
      let h1data =
        { ast = B.new_ ()
        ; spec =
            { space = B.newF "JOB"
            ; name = B.newF "processOrder"
            ; modifier = B.new_ () }
        ; hTLID = h1tlid
        ; pos = {x = 0; y = 0} }
      in
      let h2tlid = gtlid () in
      let h2data =
        { ast =
            B.newF
              (FnCall
                 ( B.newF "DB::deleteAll_v1"
                 , [B.newF (Variable "Books")]
                 , NoRail ))
        ; spec =
            { space = B.newF "HTTP"
            ; name = B.newF "/hello"
            ; modifier = B.newF "GET" }
        ; hTLID = h2tlid
        ; pos = {x = 0; y = 0} }
      in
      let dbtlid = gtlid () in
      let dbdata =
        { dbTLID = dbtlid
        ; dbName = Blank.newF "Books"
        ; cols = []
        ; version = 0
        ; oldMigrations = []
        ; activeMigration = None
        ; pos = {x = 0; y = 0} }
      in
      let dbs = TD.fromList [(dbdata.dbTLID, dbdata)] in
      let handlers =
        TD.fromList [(h1data.hTLID, h1data); (h2data.hTLID, h2data)]
      in
      test "dbsByName" (fun () ->
          expect (dbsByName dbs)
          |> toEqual (StrDict.insert ~key:"Books" ~value:dbtlid StrDict.empty)
      ) ;
      test "handlersByName" (fun () ->
          let v =
            handlers |> handlersByName |> StrDict.get ~key:"JOB:processOrder"
          in
          expect v |> toEqual (Some h1tlid) ) ;
      test "findUsagesInAST" (fun () ->
          let handlers = handlersByName handlers in
          let databases = dbsByName dbs in
          let usages =
            match findUsagesInAST h2tlid databases handlers h2data.ast with
            | [(tlid, toTLID)] ->
                tlid = h2tlid && toTLID = dbtlid
            | _ ->
                false
          in
          expect usages |> toEqual true ) ;
      test "tlidsToUpdateUsage" (fun () ->
          let fntlid = gtlid () in
          let ops =
            [ SetHandler (h1tlid, {x = 0; y = 0}, h1data)
            ; SetExpr (h1tlid, gid (), B.new_ ())
            ; SetFunction
                { ufTLID = fntlid
                ; ufMetadata =
                    { ufmName = B.newF "trollClean"
                    ; ufmParameters = []
                    ; ufmDescription = "can users put docs here?"
                    ; ufmReturnTipe = B.new_ ()
                    ; ufmInfix = false }
                ; ufAST = B.new_ () } ]
          in
          expect (tlidsToUpdateUsage ops) |> toEqual [h1tlid; fntlid] ) ;
      () ) ;
  ()
