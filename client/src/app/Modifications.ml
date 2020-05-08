(* This file will contain the functions that we deprecate modifications with.
  It should have the same scoping as Main.ml, but not the access to Main.
  (modules) < (files) < Modifications.ml < Main.ml
  It is here instead of main because Main.ml is already overwhelming, it will be nice if we can reduce Main.ml to just the barebones app-architecture code. *)
open Prelude
module TL = Toplevel

let updateASTCache (m : model) (tlid : TLID.t) (str : string) : model =
  let searchCache =
    m.searchCache |> TLIDDict.update ~tlid ~f:(fun _ -> Some str)
  in
  {m with searchCache}


(* Sends updates to ops, modifies model, sends request for new analysis *)
let fullstackASTUpdate
    ?(mFn : model -> model = fun m -> m) (tl : toplevel) (ast : FluidAST.t) :
    modification =
  (* Let's keep ops-related mods as is.
    For now we want to focus on deprecating client-model updating mods
  *)
  let opsMod = TL.setASTOpMod tl ast in
  let f m =
    let tlid = TL.id tl in
    (* All model updates happens here *)
    let newM =
      (* Apply model update function passed by caller *)
      let m0 = mFn m in
      (* Updates model AST directly instead of waiting for API callback *)
      let m1 = TL.updateModelWithAST m0 tlid ast in
      (* Updates AST cache to allow code to be searchable *)
      if m1.fluidState.activeEditor = MainEditor tlid
      then
        ast
        |> FluidAST.toExpr
        |> FluidPrinter.eToHumanString
        |> updateASTCache m1 tlid
      else m1
    in
    let cmd =
      match Analysis.getSelectedTraceID newM tlid with
      | Some traceID ->
          Analysis.requestAnalysis newM tlid traceID
      | None ->
          Tea.Cmd.none
    in
    (newM, cmd)
  in
  Many [opsMod; ReplaceAllModificationsWithThisOne f]
