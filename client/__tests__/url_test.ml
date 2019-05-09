open! Tc
open Types
open Jest
open Expect
open Prelude
open Url
module B = Blank
module D = Defaults

let defaultTLID = gtlid ()

let defaultExpr = B.new_ ()

let defaultPos = {x = 0; y = 0}

let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultExpr)
    ?(pos = defaultPos)
    ?(module_ : string option = None)
    () : toplevel =
  let module_ =
    match module_ with None -> B.new_ () | Some name -> B.newF name
  in
  let spec = {module_; name = B.new_ (); modifier = B.new_ ()} in
  {id = tlid; pos; data = TLHandler {ast = expr; spec; tlid}}


let () =
  describe "calculatePanOffset" (fun () ->
      let m = D.defaultModel in
      let tl = aHandler ~pos:{x = 500; y = 500} () in
      test "do not update canvasProps if center=false" (fun () ->
          let page = FocusedHandler (defaultTLID, false) in
          let newM = calculatePanOffset m tl page in
          expect (m.canvasProps.offset = newM.canvasProps.offset) |> toBe true
      ) ;
      test "update canvasProps if center=true" (fun () ->
          let page = FocusedHandler (defaultTLID, true) in
          let newM = calculatePanOffset m tl page in
          expect (m.canvasProps.offset = newM.canvasProps.offset) |> toBe false
      ) ;
      () ) ;
  ()
