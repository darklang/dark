open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer

let spaceOf (hs : handlerSpec) : handlerSpace =
  let spaceOfStr s =
    let lwr = String.toUpper s in
    match lwr with
    | "HTTP" ->
        HSHTTP
    | "CRON" ->
        HSCron
    | "WORKER" ->
        HSWorker
    | "REPL" ->
        HSRepl
    | _ ->
        HSDeprecatedOther
  in
  match hs.space with Blank _ -> HSDeprecatedOther | F (_, s) -> spaceOfStr s


let visibleModifier (hs : handlerSpec) : bool =
  match spaceOf hs with
  | HSHTTP | HSCron ->
      true
  | HSRepl | HSDeprecatedOther | HSWorker ->
      false


let replaceEventModifier
    (search : ID.t) (replacement : string blankOr) (hs : handlerSpec) :
    handlerSpec =
  {hs with modifier = B.replace search replacement hs.modifier}


let replaceEventName
    (search : ID.t) (replacement : string blankOr) (hs : handlerSpec) :
    handlerSpec =
  {hs with name = B.replace search replacement hs.name}


let replaceEventSpace
    (search : ID.t) (replacement : string blankOr) (hs : handlerSpec) :
    handlerSpec =
  {hs with space = B.replace search replacement hs.space}


let replace (search : ID.t) (replacement : string blankOr) (hs : handlerSpec) :
    handlerSpec =
  hs
  |> replaceEventModifier search replacement
  |> replaceEventName search replacement
  |> replaceEventSpace search replacement


let blankOrData (spec : handlerSpec) : blankOrData list =
  [PEventSpace spec.space; PEventModifier spec.modifier; PEventName spec.name]


let firstBlankOrData (spec : handlerSpec) : ID.t option =
  spec |> blankOrData |> List.head |> Option.map ~f:Pointer.toID


let firstBlank (spec : handlerSpec) : ID.t option =
  spec
  |> blankOrData
  |> List.filter ~f:Pointer.isBlank
  |> List.head
  |> Option.map ~f:Pointer.toID


let lastBlankOrData (spec : handlerSpec) : ID.t option =
  spec |> blankOrData |> List.reverse |> List.head |> Option.map ~f:Pointer.toID


let lastBlank (spec : handlerSpec) : ID.t option =
  spec
  |> blankOrData
  |> List.filter ~f:Pointer.isBlank
  |> List.reverse
  |> List.head
  |> Option.map ~f:Pointer.toID
