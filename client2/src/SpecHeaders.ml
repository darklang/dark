open Tea
open! Porting
module B = Blank
module P = Pointer
open Types

let spaceOf hs =
  let spaceOfStr s =
    let lwr = String.toLower s in
    if lwr = "cron" then HSCron else if lwr = "http" then HSHTTP else HSOther
  in
  match hs.module_ with Blank _ -> HSEmpty | F (_, s) -> spaceOfStr s

let visibleModifier hs =
  match spaceOf hs with
  | HSHTTP -> true
  | HSCron -> true
  | HSOther -> false
  | HSEmpty -> true

let replaceEventModifier search replacement hs =
  {hs with modifier= B.replace search replacement hs.modifier}

let replaceEventName search replacement hs =
  {hs with name= B.replace search replacement hs.name}

let replaceEventSpace search replacement hs =
  {hs with module_= B.replace search replacement hs.module_}

let replace search replacement hs =
  hs
  |> replaceEventModifier search replacement
  |> replaceEventName search replacement
  |> replaceEventSpace search replacement

let delete pd hs newID = replace (P.toID pd) (Blank newID) hs

let allData spec =
  [PEventName spec.name; PEventSpace spec.module_; PEventModifier spec.modifier]
