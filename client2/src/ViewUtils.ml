open Tea
open! Porting
module B = Blank
module Attrs = Html.Attributes
module JSD = Json.Decode
module JSDP = Json.Decode.Pipeline
module P = Pointer
open Prelude
module TL = Toplevel
open Types

type viewState =
  { tl: toplevel
  ; cursorState: cursorState
  ; tlid: tlid
  ; hovering: id option
  ; ac: autocomplete
  ; handlerSpace: handlerSpace
  ; showEntry: bool
  ; showLivevalue: bool
  ; dbLocked: bool
  ; currentResults: analysisResults
  ; traces: trace list
  ; analyses: analyses
  ; ufns: userFunction list
  ; relatedBlankOrs: id list
  ; tooWide: bool
  ; executingFunctions: id list
  ; tlCursors: tLCursors
  ; testVariants: variantTest list
  ; featureFlags: flagsVS
  ; handlerLocked: bool
  ; canvasName: string
  ; userContentHost: string }

let createVS (m : model) (tl : toplevel) : viewState =
  { tl
  ; cursorState= unwrapCursorState m.cursorState
  ; tlid= tl.id
  ; hovering=
      m.hovering |> List.head
      |> Option.andThen (fun i ->
             match idOf m.cursorState with
             | Some cur -> (
               match TL.find tl i with
               | Some (PExpr exp) ->
                   let cursorSubsumedByHover =
                     exp |> AST.allData |> List.map P.toID |> List.member cur
                   in
                   if cursorSubsumedByHover then None else Some i
               | _ -> if cur = i then None else Some i )
             | _ -> Some i )
  ; ac= m.complete
  ; showEntry= true
  ; showLivevalue= true
  ; handlerSpace= TL.spaceOf tl |> Option.withDefault HSOther
  ; dbLocked= DB.isLocked m tl.id
  ; ufns= m.userFunctions
  ; currentResults= Analysis.getCurrentAnalysisResults m tl.id
  ; traces= Analysis.getTraces m tl.id
  ; analyses= m.analyses
  ; relatedBlankOrs=
      ( match unwrapCursorState m.cursorState with
      | Entering (Filling (_, id)) -> (
        match TL.find tl id with
        | Some (PVarBind (F (_, var))) as pd -> (
          match TL.getParentOf tl (deOption "impossible" pd) with
          | Some (PExpr e) -> (
            match e with
            | F (_, Let (_, _, body)) -> AST.uses var body |> List.map B.toID
            | F (_, Lambda (_, body)) -> AST.uses var body |> List.map B.toID
            | _ -> [] )
          | _ -> [] )
        | _ -> [] )
      | _ -> [] )
  ; tooWide= false
  ; executingFunctions=
      List.filter (fun (tlid, id) -> tlid = tl.id) m.executingFunctions
      |> List.map (fun (tlid, id) -> id)
  ; tlCursors= m.tlCursors
  ; testVariants= m.tests
  ; featureFlags= m.featureFlags
  ; handlerLocked= isLocked tl.id m
  ; canvasName= m.canvasName
  ; userContentHost= m.userContentHost }

let fontAwesome (name : string) : msg Html.html =
  Html.i [Html.class' ("fa fa-" ^ name)] []

let eventNoPropagation (event : string) (constructor : mouseEvent -> msg) :
    msg Html.attribute =
  Html.onWithOptions event
    {stopPropagation= true; preventDefault= false}
    (decodeClickEvent constructor)

let eventNoDefault (event : string) (constructor : mouseEvent -> msg) :
    msg Html.attribute =
  Html.onWithOptions event
    {stopPropagation= false; preventDefault= true}
    (decodeClickEvent constructor)

let nothingMouseEvent (name : string) : msg Html.attribute =
  eventNoPropagation name NothingClick

let decodeClickEvent (fn : mouseEvent -> 'a) : 'a JSD.decoder =
  let _ = "type annotation" in
  let toA px py button = fn {pos= {vx= px; vy= py}; button} in
  JSDP.decode toA
  |> JSDP.required "pageX" JSD.int
  |> JSDP.required "pageY" JSD.int
  |> JSDP.required "button" JSD.int

let placeHtml (m : model) (pos : pos) (html : msg Html.html) : msg Html.html =
  let div class_ subs = Html.div [Html.class' class_] subs in
  Html.div
    [ Html.class' "node"
    ; Attrs.style
        [ ("left", string_of_int pos.x ^ "px")
        ; ("top", string_of_int pos.y ^ "px") ] ]
    [html]

let inCh (w : int) : string = w |> string_of_int |> fun s -> s ^ "ch"

let widthInCh (w : int) : msg Html.attribute =
  w |> inCh |> fun w_ -> Attrs.style [("width", w_)]

let blankOrLength (b : string blankOr) : int =
  match b with Blank _ -> 6 | F (_, str) -> String.length str

let visualStringLength (string : string) : int =
  string |> Regex.replace "\t" "        " |> String.length

let approxWidth (e : expr) : int =
  match e with Blank _ -> 6 | F (_, ne) -> approxNWidth ne

let approxNWidth (ne : nExpr) : int =
  match ne with
  | Value v -> toString v |> String.length
  | Variable name -> String.length name
  | Let (lhs, rhs, body) ->
      max (blankOrLength lhs + approxWidth rhs + 4 + 3) (approxWidth body)
  | If (cond, ifbody, elsebody) ->
      3
      |> ( + ) (approxWidth cond)
      |> max (approxWidth ifbody + 2)
      |> max (approxWidth elsebody + 2)
  | FnCall (name, exprs, _) ->
      let sizes =
        exprs |> List.map approxWidth |> List.map (( + ) 1) |> List.sum
      in
      String.length name + sizes
  | Lambda (vars, expr) -> max (approxWidth expr) 7
  | Thread exprs ->
      exprs |> List.map approxWidth |> List.maximum |> Option.withDefault 2
      |> ( + ) 1
  | FieldAccess (obj, field) -> approxWidth obj + 1 + blankOrLength field
  | ListLiteral exprs ->
      exprs |> List.map approxWidth
      |> List.map (( + ) 2)
      |> List.sum |> ( + ) 4
  | ObjectLiteral pairs ->
      pairs
      |> List.map (fun (k, v) -> blankOrLength k + approxWidth v)
      |> List.map (( + ) 2)
      |> List.map (( + ) 2)
      |> List.maximum |> Option.withDefault 0 |> ( + ) 4
  | FeatureFlag (msg, cond, a, b) -> max (approxWidth a) (approxWidth b) + 1

let isLocked (tlid : tlid) (m : model) : bool =
  List.member tlid m.lockedHandlers

let viewFnName (fnName : fnName) (extraClasses : string list) : msg Html.html =
  let pattern = regex "(\\w+::)?(\\w+)_v(\\d+)" in
  let matches = Regex.find (Regex.AtMost 1) pattern fnName in
  let name, version =
    match List.head matches with
    | Some m ->
        let name =
          match m.submatches with
          | [_; Some fn; None] -> fn
          | [_; Some fn; Some modName] -> modName ^ fn
          | _ -> fnName
        in
        let version =
          match m.submatches with [Some v; _; _] -> v | _ -> "0"
        in
        (name, version)
    | None -> (fnName, "0")
  in
  Html.div
    [Html.class' (String.join " " ("versioned-function" :: extraClasses))]
    [ Html.span [Html.class' "name"] [Html.text name]
    ; Html.span [Html.class' "version"] [Html.text ("v" ^ version)] ]
