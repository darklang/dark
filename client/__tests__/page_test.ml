open! Tc
open Types
open Jest
open Expect
open Prelude
module B = Blank
module D = Defaults

let defaultTLID = gtlid ()

let defaultExpr = B.new_ ()

let defaultPos = {x = 0; y = 0}

let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultExpr)
    ?(pos = defaultPos)
    ?(space : string option = None)
    () : toplevel =
  let space =
    match space with None -> B.new_ () | Some name -> B.newF name
  in
  let spec = {space; name = B.new_ (); modifier = B.new_ ()} in
  TLHandler {ast = expr; spec; hTLID = tlid; pos}


let () =
  describe "calculatePanOffset" (fun () ->
      let m = D.defaultModel in
      let tl = aHandler ~pos:{x = 500; y = 500} () in
      test "do not update canvasProps if center=false" (fun () ->
          let page = FocusedHandler (defaultTLID, false) in
          let newM = Page.calculatePanOffset m tl page in
          expect (m.canvasProps.offset = newM.canvasProps.offset) |> toBe true
      ) ;
      test "update canvasProps if center=true" (fun () ->
          let page = FocusedHandler (defaultTLID, true) in
          let newM = Page.calculatePanOffset m tl page in
          expect (m.canvasProps.offset = newM.canvasProps.offset) |> toBe false
      ) ;
      () ) ;
  ()
