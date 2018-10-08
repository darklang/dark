module P = Pointer
open Prelude
module TL = Toplevel
open Types

let copy m tl mp =
  match tl.data with
  | TLDB _ -> NoChange
  | TLHandler h -> (
    match mp with
    | Nothing -> CopyToClipboard (Just <| PExpr h.ast)
    | Just p -> CopyToClipboard (TL.find tl (P.toID p)) )
  | TLFunc f -> (
    match mp with
    | Nothing -> CopyToClipboard (Just <| PExpr f.ast)
    | Just p -> CopyToClipboard (TL.find tl (P.toID p)) )

let cut m tl p =
  let pid = P.toID p in
  let pred = TL.getPrevBlank tl (Just p) |> Maybe.map P.toID in
  match tl.data with
  | TLDB _ -> NoChange
  | TLHandler h ->
      let newClipboard = TL.find tl pid in
      let newH =
        TL.delete tl p (gid ()) |> TL.asHandler |> Option.getExn "cut"
      in
      Many
        [ CopyToClipboard newClipboard
        ; RPC ([SetHandler (tl.id, tl.pos, newH)], FocusNext (tl.id, pred)) ]
  | TLFunc f ->
      let newClipboard = TL.find tl pid in
      let newF =
        TL.delete tl p (gid ()) |> TL.asUserFunction |> Option.getExn "cut"
      in
      Many
        [ CopyToClipboard newClipboard
        ; RPC ([SetFunction newF], FocusNext (tl.id, pred)) ]

let paste m tl id =
  match m.clipboard with
  | Nothing -> NoChange
  | Just pd -> (
      let cloned = TL.clonePointerData pd in
      match tl.data with
      | TLDB _ -> NoChange
      | TLHandler h ->
          let newAst = AST.replace (TL.findExn tl id) cloned h.ast in
          if newAst == h.ast then NoChange
          else
            RPC
              ( [SetHandler (tl.id, tl.pos, {h with ast= newAst})]
              , FocusExact (tl.id, P.toID cloned) )
      | TLFunc f ->
          let newAst = AST.replace (TL.findExn tl id) cloned f.ast in
          if newAst == f.ast then NoChange
          else
            RPC
              ( [SetFunction {f with ast= newAst}]
              , FocusExact (tl.id, P.toID cloned) ) )

let peek m = Maybe.map TL.clonePointerData m.clipboard

let newFromClipboard m pos =
  let nid = gtlid () in
  let ast =
    match peek m with
    | Nothing -> Blank.new_ ()
    | Just a -> ( match a with PExpr exp -> exp | _ -> Blank.new_ () )
  in
  let spec = Entry.newHandlerSpec () in
  let handler = {ast; spec; tlid= nid} in
  RPC ([SetHandler (nid, pos, handler)], FocusNext (nid, Nothing))
