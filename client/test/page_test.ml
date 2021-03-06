open Tester
open! Tc
open Prelude
module B = BlankOr
module D = Defaults

let defaultTLID = gtlid ()

let defaultExpr = B.new_ ()

let defaultFluidExpr = FluidExpression.EBlank (gid ())

let defaultPos = {x = 0; y = 0}

let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultFluidExpr)
    ?(pos = defaultPos)
    ?(space : string option = None)
    () : toplevel =
  let space = match space with None -> B.new_ () | Some name -> B.newF name in
  let spec = {space; name = B.new_ (); modifier = B.new_ ()} in
  TLHandler {ast = FluidAST.ofExpr expr; spec; hTLID = tlid; pos}


let run () =
  describe "calculatePanOffset" (fun () ->
      let m = D.defaultModel in
      let tl = aHandler ~pos:{x = 500; y = 500} () in
      test "do not update canvasProps if center=false" (fun () ->
          let page = FocusedHandler (defaultTLID, None, false) in
          let newM = Page.calculatePanOffset m tl page in
          expect (m.canvasProps.offset = newM.canvasProps.offset)
          |> toEqual true) ;
      test "update canvasProps if center=true" (fun () ->
          let page = FocusedHandler (defaultTLID, None, true) in
          let newM = Page.calculatePanOffset m tl page in
          expect (m.canvasProps.offset = newM.canvasProps.offset)
          |> toEqual false) ;
      ()) ;
  ()
