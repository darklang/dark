open! Porting
open Prelude
open Types

(* Dark *)
module P = Pointer
module TL = Toplevel

let copy (tl : toplevel) (mp : pointerData option) : modification =
  match tl.data with
  | TLDB _ ->
      NoChange
  | TLHandler h ->
    ( match mp with
    | None ->
        CopyToClipboard (Some (PExpr h.ast))
    | Some p ->
        CopyToClipboard (TL.find tl (P.toID p)) )
  | TLFunc f ->
    ( match mp with
    | None ->
        CopyToClipboard (Some (PExpr f.ufAST))
    | Some p ->
        CopyToClipboard (TL.find tl (P.toID p)) )


let cut (tl : toplevel) (p : pointerData) : modification =
  let pid = P.toID p in
  let pred = TL.getPrevBlank tl (Some p) |> Option.map P.toID in
  match tl.data with
  | TLDB _ ->
      NoChange
  | TLHandler _ ->
      let newClipboard = TL.find tl pid in
      let newH = TL.delete tl p (gid ()) |> TL.asHandler |> deOption "cut" in
      Many
        [ CopyToClipboard newClipboard
        ; RPC ([SetHandler (tl.id, tl.pos, newH)], FocusNext (tl.id, pred)) ]
  | TLFunc _ ->
      let newClipboard = TL.find tl pid in
      let newF =
        TL.delete tl p (gid ()) |> TL.asUserFunction |> deOption "cut"
      in
      Many
        [ CopyToClipboard newClipboard
        ; RPC ([SetFunction newF], FocusNext (tl.id, pred)) ]


let paste (m : model) (tl : toplevel) (id : id) : modification =
  match m.clipboard with
  | None ->
      NoChange
  | Some pd ->
      let cloned = TL.clonePointerData pd in
      ( match tl.data with
      | TLDB _ ->
          NoChange
      | TLHandler h ->
          let newAst = AST.replace (TL.findExn tl id) cloned h.ast in
          if newAst = h.ast
          then NoChange (* paste doesn't always make sense *)
          else
            RPC
              ( [SetHandler (tl.id, tl.pos, {h with ast = newAst})]
              , FocusExact (tl.id, P.toID cloned) )
      | TLFunc f ->
          let newAst = AST.replace (TL.findExn tl id) cloned f.ufAST in
          if newAst = f.ufAST
          then NoChange (* -- paste doesn't always make sense *)
          else
            RPC
              ( [SetFunction {f with ufAST = newAst}]
              , FocusExact (tl.id, P.toID cloned) ) )


let peek (m : model) : clipboard = Option.map TL.clonePointerData m.clipboard

let newFromClipboard (m : model) (pos : pos) : modification =
  let nid = gtlid () in
  let ast =
    match peek m with
    | None ->
        Blank.new_ ()
    | Some a ->
      (match a with PExpr exp -> exp | _ -> Blank.new_ ())
  in
  let spec = Entry.newHandlerSpec () in
  let handler = {ast; spec; tlid = nid} in
  RPC ([SetHandler (nid, pos, handler)], FocusNext (nid, None))
