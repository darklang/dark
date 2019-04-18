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
            { module_ = B.newF "JOB"
            ; name = B.newF "processOrder"
            ; modifier = B.new_ () }
        ; tlid = h1tlid }
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
            { module_ = B.newF "HTTP"
            ; name = B.newF "/hello"
            ; modifier = B.newF "GET" }
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
        [ {id = h1tlid; pos = {x = 0; y = 0}; data = TLHandler h1data}
        ; {id = dbtlid; pos = {x = 0; y = 0}; data = TLDB dbdata}
        ; {id = h2tlid; pos = {x = 0; y = 0}; data = TLHandler h2data} ]
      in
      test "keyForHandlerSpec" (fun () ->
          expect (keyForHandlerSpec h1data.spec.module_ h1data.spec.name)
          |> toEqual "JOB:processOrder" ) ;
      test "dbsByName" (fun () ->
          expect (dbsByName toplevels)
          |> toEqual (StrDict.insert ~key:"Books" ~value:dbtlid StrDict.empty)
      ) ;
      test "handlersByName" (fun () ->
          let handlerKeys = StrDict.keys (handlersByName toplevels) in
          expect (List.member ~value:"JOB:processOrder" handlerKeys)
          |> toEqual true ) ;
      test "findUsagesInAST" (fun () ->
          let handlers = handlersByName toplevels in
          let databases = dbsByName toplevels in
          let usages =
            match findUsagesInAST h2tlid databases handlers h2data.ast with
            | [(tlid, toTLID, _)] ->
                tlid = h2tlid && toTLID = dbtlid
            | _ ->
                false
          in
          expect usages |> toEqual true ) ;
      test "tlidsToUpdateMeta" (fun () ->
          let tlid1 = gtlid () in
          let tlid2 = gtlid () in
          let ops =
            [ DeleteDBCol (tlid1, gid ())
            ; SetFunction
                { ufTLID = tlid2
                ; ufMetadata =
                    { ufmName = B.newF "trollClean"
                    ; ufmParameters = []
                    ; ufmDescription = "can users put docs here?"
                    ; ufmReturnTipe = B.new_ ()
                    ; ufmInfix = false }
                ; ufAST = B.new_ () }
            ; AddDBCol (tlid1, gid (), gid ()) ]
          in
          expect (tlidsToUpdateMeta ops) |> toEqual [tlid1; tlid2] ) ;
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
      () )
