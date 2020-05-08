(* Has the same scoping as Main.ml made so we don't crowd Main *)
open Prelude
module TL = Toplevel

let updateASTCache (m : model) (tlid : TLID.t) (str : string) : model =
  let searchCache =
    m.searchCache |> TLIDDict.update ~tlid ~f:(fun _ -> Some str)
  in
  {m with searchCache}


(* Sends updates to ops, modifies model, sends request for new analysis *)
let fullstackASTUpdate (tl : toplevel) (ast : FluidAST.t) : modification =
  let opsMod = TL.setASTOpMod tl ast in
  let f m0 =
    let tlid = TL.id tl in
    let newM =
      (* All model updates happens here *)
      let m1 = TL.updateModelWithAST m0 tlid ast in
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
  (* Lets make sure we deprecate all in-app modifications before we touch  *)
  Many [opsMod; ReplaceAllModificationsWithThisOne f]
