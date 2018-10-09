open Tea
open! Porting
module B = Blank
module P = Pointer
open Prelude
open Types

let delete (pd : pointerData) (spec : handlerSpec) (newID : id) : handlerSpec =
  replace pd (P.emptyD_ newID (P.typeOf pd)) spec

let replace (p : pointerData) (dt : pointerData) (spec : handlerSpec) :
    handlerSpec =
  { spec with
    types=
      { input= replaceInType p dt spec.types.input
      ; output= replaceInType p dt spec.types.output } }

let replaceInType (pd : pointerData) (replacement : pointerData)
    (dt : darkType) : darkType =
  let _ =
    todo {msg= "replaceInType"; pointerData= pd; replacement; darkType= dt}
  in
  if B.toID dt = P.toID pd then
    match replacement with PDarkType t -> t | _ -> dt
  else
    match dt with
    | F (id, DTObj ts) ->
        let newTs =
          ts
          |> List.map (fun (n, t) ->
                 let newN =
                   match replacement with
                   | PDarkTypeField name ->
                       if P.toID pd = B.toID n then name else n
                   | _ -> n
                 in
                 let newT =
                   match replacement with
                   | PDarkType tipe -> replaceInType pd replacement t
                   | _ -> t
                 in
                 (newN, newT) )
        in
        F (id, DTObj newTs)
    | _ -> dt

let allData (t : darkType) : pointerData list = []

let childrenOf (id : id) (t : darkType) : pointerData list =
  if B.toID t = id then
    match t with
    | F (_, DTObj ts) ->
        ts
        |> List.map (fun (n, dt) -> [PDarkTypeField n; PDarkType dt])
        |> List.concat
    | _ -> []
  else
    match t with
    | F (_, DTObj ts) ->
        ts |> List.map (fun (n, dt) -> childrenOf id dt) |> List.concat
    | _ -> []

let siblings (p : pointerData) (t : darkType) : pointerData list =
  match t with
  | F (_, DTObj ts) ->
      let result =
        ts
        |> List.map (fun (n, dt) -> [PDarkTypeField n; PDarkType dt])
        |> List.concat
      in
      if List.member p result then result
      else ts |> List.map (fun (_, dt) -> siblings p dt) |> List.concat
  | _ -> []
