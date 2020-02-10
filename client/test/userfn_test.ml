open Tester
open! Tc
open Prelude
open UserFunctions
module B = BlankOr
module D = Defaults

let defaultTLID = gtlid ()

let defaultFluidExpr = FluidExpression.EBlank (gid ())

let defaultPos = {x = 0; y = 0}

let defaultFnName = "myFun"

let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultFluidExpr)
    ?(pos = defaultPos)
    ?(space : string option = None)
    () : toplevel =
  let space = match space with None -> B.new_ () | Some name -> B.newF name in
  let spec = {space; name = B.new_ (); modifier = B.new_ ()} in
  TLHandler {ast = expr; spec; hTLID = tlid; pos}


let aFn
    ?(tlid = defaultTLID)
    ?(name = defaultFnName)
    ?(params = [])
    ?(expr = defaultFluidExpr)
    () : toplevel =
  TLFunc
    { ufTLID = tlid
    ; ufMetadata =
        { ufmName = F (gid (), name)
        ; ufmParameters = params
        ; ufmDescription = ""
        ; ufmReturnTipe = F (gid (), TAny)
        ; ufmInfix = false }
    ; ufAST = expr }


let run () =
  describe "canDelete" (fun () ->
      test "can delete a function with no used-in references" (fun () ->
          expect (canDelete [] defaultTLID) |> toEqual true) ;
      test
        "cannot delete a function with used-in references from elsewhere"
        (fun () ->
          let caller = aHandler ~tlid:(TLID "1") () in
          expect (canDelete [caller] defaultTLID) |> toEqual false) ;
      test "can delete if only used-in references are itself" (fun () ->
          let fn = aFn () in
          expect (canDelete [fn] defaultTLID) |> toEqual true) ;
      test
        "cannot delete if any one of the used-in references is from elsewhere"
        (fun () ->
          let fn = aFn () in
          let caller = aHandler ~tlid:(TLID "1") () in
          expect (canDelete [fn; caller] defaultTLID) |> toEqual false) ;
      ()) ;
  ()
