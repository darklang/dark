open Tc
open Types

let tlidStrOfReference (r : tlReference) : string =
  match r with
  | OutReferenceDB (TLID tlid, _, _, _) ->
      tlid
  | OutReferenceEvent (TLID tlid, _, _, _) ->
      tlid


let idOfReference (r : tlReference) : id =
  match r with
  | OutReferenceDB (_, _, _, id) ->
      id
  | OutReferenceEvent (_, _, _, id) ->
      id


let shouldUpdateReferences (ops : op list) =
  List.any
    ~f:(fun op ->
      match op with
      | SetHandler _ ->
          true
      | SetExpr _ ->
          true
      | SetFunction _ ->
          true
      | _ ->
          false )
    ops
