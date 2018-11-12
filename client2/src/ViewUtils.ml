open Types
open Porting
open Prelude


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

let isLocked (tlid : tlid) (m : model) : bool =
  List.member tlid m.lockedHandlers

let createVS (m : model) (tl : toplevel) : viewState =
  { tl
  ; cursorState= unwrapCursorState m.cursorState
  ; tlid= tl.id
  ; hovering=
      m.hovering |> List.head
      |> Option.andThen (fun i ->
             match idOf m.cursorState with
             | Some cur -> (
               match Toplevel.find tl i with
               | Some (PExpr exp) ->
                   let cursorSubsumedByHover =
                     exp |> AST.allData |> List.map Pointer.toID |> List.member cur
                   in
                   if cursorSubsumedByHover then None else Some i
               | _ -> if cur = i then None else Some i )
             | _ -> Some i )
  ; ac= m.complete
  ; showEntry= true
  ; showLivevalue= true
  ; handlerSpace= Toplevel.spaceOf tl |> Option.withDefault HSOther
  ; dbLocked= DB.isLocked m tl.id
  ; ufns= m.userFunctions
  ; currentResults= Analysis.getCurrentAnalysisResults m tl.id
  ; traces= Analysis.getTraces m tl.id
  ; analyses= m.analyses
  ; relatedBlankOrs=
      ( match unwrapCursorState m.cursorState with
      | Entering (Filling (_, id)) -> (
        match Toplevel.find tl id with
        | Some (PVarBind (F (_, var))) as pd -> (
          match Toplevel.getParentOf tl (deOption "impossible" pd) with
          | Some (PExpr e) -> (
            match e with
            | F (_, Let (_, _, body)) -> AST.uses var body |> List.map Blank.toID
            | F (_, Lambda (_, body)) -> AST.uses var body |> List.map Blank.toID
            (* TODO(match): match *)
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

let decodeClickEvent (fn : mouseEvent -> 'a) j : 'a =
  let module JSD = Json_decode_extended in
  fn
    { mePos = { vx = JSD.field "pageX" JSD.int j
              ; vy = JSD.field "pageY" JSD.int j
              }
    ; button = (JSD.field "button" JSD.int j)
    }

let eventNoPropagation ~(key: string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Patched_tea_html.onWithOptions ~key event
    {stopPropagation= true; preventDefault= false}
    (Decoders.wrapDecoder (decodeClickEvent constructor))

let eventNoDefault ~(key: string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Patched_tea_html.onWithOptions ~key event
    {stopPropagation= false; preventDefault= true}
    (Decoders.wrapDecoder (decodeClickEvent constructor))

let nothingMouseEvent (name : string) : msg Vdom.property =
  eventNoPropagation ~key:"" name (fun e -> NothingClick e)

let placeHtml (m : model) (pos : pos) (html : msg Html.html) : msg Html.html =
  let div class_ subs = Html.div [Html.class' class_] subs in
  Html.div
    [ Html.class' "node"
    ; Html.styles
        [ ("left", string_of_int pos.x ^ "px")
        ; ("top", string_of_int pos.y ^ "px") ] ]
    [html]

let inCh (w : int) : string = w |> string_of_int |> fun s -> s ^ "ch"

let widthInCh (w : int) : msg Vdom.property =
  w |> inCh |> Html.style "width"

let blankOrLength ~(f: 'a -> int) (b : 'a blankOr) : int =
  match b with
  | Blank _ -> 6
  | F (_, value) -> f value

let strBlankOrLength (b : string blankOr) : int =
  blankOrLength ~f:String.length b

let visualStringLength (string : string) : int =
  string |> Regex.replace "\t" "        " |> String.length

let rec approxWidth (e : expr) : int =
  match e with Blank _ -> 6 | F (_, ne) -> approxNWidth ne

and approxNWidth (ne : nExpr) : int =
  match ne with
  | Value v -> toString v |> String.length
  | Variable name -> String.length name
  | Let (lhs, rhs, body) ->
      max (strBlankOrLength lhs + approxWidth rhs + 4 + 3) (approxWidth body)
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
  | FieldAccess (obj, field) -> approxWidth obj + 1 + strBlankOrLength field
  | ListLiteral exprs ->
      exprs |> List.map approxWidth
      |> List.map (( + ) 2)
      |> List.sum |> ( + ) 4
  | ObjectLiteral pairs ->
      pairs
      |> List.map (fun (k, v) -> strBlankOrLength k + approxWidth v)
      |> List.map (( + ) 2)
      |> List.map (( + ) 2)
      |> List.maximum |> Option.withDefault 0 |> ( + ) 4
  | FeatureFlag (msg, cond, a, b) -> max (approxWidth a) (approxWidth b) + 1
  | Match (matchExpr, cases) ->
    let rec pWidth p =
      (match p with
       | PLiteral l -> String.length l
       | PVariable v -> String.length v
       | PConstructor (name, args) ->
         String.length name
         + List.sum (List.map (blankOrLength ~f:pWidth) args)
         + List.length args)
    in
    let caseWidth (p, e) =
      (blankOrLength ~f:pWidth p)
      + 2 (* indent *)
      + 4 (* arrow and spaces *)
      + approxWidth e
    in
    List.maximum
      ((6 + approxWidth matchExpr) :: List.map caseWidth cases)
    |> Option.withDefault 0


let viewFnName (fnName : fnName) (extraClasses : string list) : msg Html.html =
  let pattern = Js.Re.fromString "(\\w+::)?(\\w+)_v(\\d+)" in
  let mResult = Js.Re.exec fnName pattern in
  let name, version =
    match mResult with
    | Some result ->
      let captures =
        result
        |> Js.Re.captures
        |> Belt.List.fromArray
        |> List.map Js.toOption
      in
      let name =
        match captures with
        | [_; None; Some fn; _] -> fn
        | [_; Some modName; Some fn; _] -> modName ^ fn
        | _ -> fnName
      in
      let version =
        match captures with
        | [_; _; _; Some v] -> v
        | _ -> "0"
      in
      (name, version)
    | None -> (fnName, "0")
  in
  Html.div
    [Html.class' (String.join " " ("versioned-function" :: extraClasses))]
    [ Html.span [Html.class' "name"] [Html.text name]
    ; Html.span [Html.class' "version"] [Html.text ("v" ^ version)] ]
