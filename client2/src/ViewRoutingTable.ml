open! Porting
open Prelude
open Types
open ViewUtils

module B = Blank
module TL = Toplevel

type viewState = ViewUtils.viewState
type htmlConfig = ViewBlankOr.htmlConfig
let idConfigs = ViewBlankOr.idConfigs
let fontAwesome = ViewUtils.fontAwesome
let wc = ViewBlankOr.wc
let nested = ViewBlankOr.nested
let atom = ViewBlankOr.atom
let keyword = ViewBlankOr.keyword


type collapseVerbs = CollapseVerbs | DontCollapseVerbs

and showLink = ShowLink | DontShowLink

and showUndo = ShowUndo | DontShowUndo


let span (class_ : string) (subs : msg Html.html list) : msg Html.html =
  Html.span [Html.class' class_] subs

let text (class_ : string) (msg : string) : msg Html.html =
  span class_ [Html.text msg]

let div (class_ : string) (subs : msg Html.html list) : msg Html.html =
  Html.div [Html.class' class_] subs


type entry =
  { name: string option
  ; prefix: string list
  ; verbs: (string * pos) list
  ; tlid: tlid }

let missingEventSpaceDesc : string = "<missing event space>"

let missingEventRouteDesc : string = "<missing route>"

let spaceName (tl : toplevel) : string option =
  match tl.data with TLHandler h -> h.spec.module_ |> B.toMaybe | _ -> None

let splitBySpace (tls : toplevel list) : (string * toplevel list) list =
  let spaceName_ tl =
    tl |> TL.asHandler
    |> Option.map (fun x -> x.spec)
    |> Option.map (fun x -> x.module_)
    |> Option.andThen B.toMaybe
    |> Option.withDefault missingEventSpaceDesc
  in
  tls
  |> List.sortBy spaceName_
  |> List.groupWhile (fun a b -> spaceName_ a = spaceName_ b)
  |> List.map (fun hs ->
         let space =
           hs |> List.head |> deOption "splitBySpace" |> spaceName_
         in
         (space, hs) )

let tl2entry (tls : toplevel list) : entry list =
  tls
  |> List.filterMap (fun tl ->
         match TL.asHandler tl with
         | Some h -> Some (tl.pos, tl.id, h)
         | None -> None )
  |> List.map (fun (pos, tlid, h) ->
         { name= (match h.spec.name with F (_, s) -> Some s | Blank _ -> None)
         ; prefix= []
         ; verbs=
             ( match h.spec.modifier with
             | F (_, s) -> [(s, pos)]
             | Blank _ -> [("_", pos)] )
         ; tlid } )
  |> List.sortBy (fun c -> Option.withDefault "ZZZZZZ" c.name)

let collapseByVerb (es : entry list) : entry list =
  es
  |> List.groupWhile (fun a b -> a.name = b.name)
  |> List.map
       (List.foldr
          (fun curr list ->
            if curr.name = None then curr :: list
            else
              match list with
              | [] -> [curr]
              | prev :: rest ->
                  let new_ = {prev with verbs= prev.verbs @ curr.verbs} in
                  new_ :: rest )
          [])
  |> List.concat

let rec prefixify (hs : entry list) : entry list =
  match hs with
  | [] -> hs
  | [_] -> hs
  | h :: rest -> (
    match h.name with
    | None -> h :: prefixify rest
    | Some name -> (
        let len = String.length name in
        let _ = "type annotation" in
        let makePrefix h2 =
          match h2.name with
          | Some name2 ->
              let newName = String.dropLeft len name2 in
              {h2 with name= Some newName; prefix= h.prefix @ [name]}
          | _ -> h2
        in
        let isPrefixOf h2 =
          match h2.name with
          | None -> false
          | Some n2 -> String.startsWith name n2
        in
        match List.splitWhen (fun h2 -> not (isPrefixOf h2)) rest with
        | None -> h :: (rest |> List.map makePrefix |> prefixify)
        | Some (matched, unmatched) ->
            h :: (prefixify <| List.map makePrefix matched @ unmatched) ) )

let ordering (a : string) (b : string) : int =
  match (a, b) with
  | "HTTP", _ -> -1
  | _, "HTTP" -> 1
  | _ ->
      if a = missingEventRouteDesc then 1
      else if b = missingEventRouteDesc then -1
      else compare a b

let buttonLink ~(key: string) (content : msg Html.html) (handler : msg) (page : page option) :
    msg Html.html =
  let href =
    page |> Option.map (fun p -> Html.href (Url.urlFor p)) |> Option.toList
  in
  let event =
    match page with
    | None -> ViewUtils.eventNoDefault ~key "click" (fun _ -> handler)
    | Some _ ->
      ViewUtils.eventNoPropagation ~key "click" (fun _ -> handler)
  in
  Html.a ([event; Html.class' "button-link"] @ href) [content]

let undoButton (tlid : tlid) (page : page) : msg Html.html =
  buttonLink ~key:("rt" ^ showTLID tlid) (text "undo" "Restore") (RestoreToplevel tlid) (Some page)

let tlLink (pos : pos) (class_ : string) (name : string) : msg Html.html =
  Url.linkFor (Toplevels pos) class_ [Html.text name]

let fnLink (fn : userFunction) (isUsed : bool) (text_ : string) : msg Html.html
    =
  Url.linkFor
    (Fn (fn.ufTLID, Defaults.centerPos))
    (if isUsed then "default-link" else "default-link unused")
    [Html.text text_]

(* TODO: refactor `addHandler` from weird tuple to better API for passing msg
 * + the cache key for the vdom *)
let header (name : string) (list : 'a list) (addHandler : (string * msg) option) :
    msg Html.html =
  Html.summary [Html.class' "header"]
    [ text "title" name
    ; text "parens" "("
    ; text "count" (list |> List.length |> string_of_int)
    ; text "parens" ")"
    ; ( match addHandler with
      | Some (key, msg) -> buttonLink ~key (fontAwesome "plus-circle") msg None
      | None -> text "" "" ) ]

let section (name : string) (entries : 'a list) (addHandler : (string * msg) option)
    (routes : msg Html.html) : msg Html.html =
  if List.length entries = 0 then
    Html.div
      [Html.class' "routing-section empty"]
      [header name entries addHandler; routes]
  else
    Html.details
      [Html.class' "routing-section"]
      [header name entries addHandler; routes]

let viewGroup (m : model) (showLink : showLink) (showUndo : showUndo)
    ((spacename, entries) : string * entry list) : msg Html.html =
  let def s = Option.withDefault missingEventRouteDesc s in
  let externalLink h =
    if showLink = ShowLink && List.member "GET" (List.map Tuple.first h.verbs)
    then
      match h.name with
      | Some n ->
          let target = String.join "" (h.prefix @ [n]) in
          Html.a
            [ Html.class' "external"
            ; Html.href
                ( "//"
                ^ Tea.Http.encodeUri m.canvasName
                ^ "." ^ m.userContentHost ^ target )
            ; Html.target "_blank" ]
            [fontAwesome "external-link-alt"]
      | None -> Html.div [] []
    else Html.div [] []
  in
  let verbs e =
    e.verbs
    |> List.map (fun (verb, pos) -> tlLink pos "verb-link" verb)
    |> List.intersperse (Html.text ",")
  in
  let entryHtml e =
    let pos =
      e.verbs |> List.head |> deOption "viewGroup/entryHtml" |> Tuple.second
    in
    div "handler"
      ( [ div "name"
            (List.map (text "prefix") e.prefix @ [Html.text (def e.name)])
        ; div "extra" [span "verbs" (verbs e); externalLink e] ]
      @ if showUndo = ShowUndo then [undoButton e.tlid (Toplevels pos)] else []
      )
  in
  let routes = div "routes" (List.map entryHtml entries) in
  let distinctEntries =
    entries |> List.map (fun x -> x.verbs) |> List.concat
  in
  let button =
    if spacename = "HTTP" && showUndo <> ShowUndo
    then Some ("crh", CreateRouteHandler)
    else None
  in
  section spacename distinctEntries button routes

let viewRoutes (m : model) (collapse : collapseVerbs) (showLink : showLink)
    (showUndo : showUndo) : msg Html.html list =
  let tls = m.toplevels in
  tls |> splitBySpace
  |> List.sortWith (fun (a, _) (b, _) -> ordering a b)
  |> List.map (Tuple.mapSecond tl2entry)
  |> (fun entries ->
       if collapse = CollapseVerbs then
         List.map (Tuple.mapSecond collapseByVerb) entries
       else entries )
  |> List.map (Tuple.mapSecond prefixify)
  |> List.map (viewGroup m showLink showUndo)

let viewRestorableDBs (tls : toplevel list) : msg Html.html =
  let dbs =
    tls
    |> List.filter (fun tl -> TL.asDB tl <> None)
    |> List.map (fun tl -> (tl.pos, tl.id, TL.asDB tl |> deOption "asDB"))
    |> List.sortBy (fun (_, _, db) -> db.dbName)
  in
  let dbHtml (pos, tlid, db) =
    div "simple-route" [text "name" db.dbName; undoButton tlid (Toplevels pos)]
  in
  let routes = div "dbs" (List.map dbHtml dbs) in
  section "DBs" dbs None routes

let viewDeletedTLs_ (m : model) : msg Html.html =
  let tls = m.deletedToplevels in
  let routes = viewRoutes m DontCollapseVerbs DontShowLink ShowUndo in
  let dbs = viewRestorableDBs tls in
  let h = header "Deleted" tls None in
  Html.details [Html.class' "routing-section deleted"] ([h] @ routes @ [dbs])

let viewDeletedTLs =
  Cache.cache1 (fun m -> m.deletedToplevels) viewDeletedTLs_

let view404s_ (f404s : fourOhFour list) : msg Html.html =
  let fofToKey fof =
    fof.space ^ "-" ^ fof.path ^ "-" ^ fof.modifier
  in
  let theCreateLink fof =
    buttonLink ~key:(fofToKey fof) (fontAwesome "plus-circle") (CreateHandlerFrom404 fof) None
  in
  let theDeleteLink fof =
    buttonLink ~key:(fofToKey fof) (fontAwesome "minus-circle") (Delete404 fof) None
  in
  let fofHtml ({space; path; modifier} as fof) =
    div "fof"
      [ text "path" path
      ; (if space = "HTTP" then text "" "" else text "space" space)
      ; text "modifier" modifier
      ; theCreateLink fof
      ; theDeleteLink fof ]
  in
  let routes = div "404s" (List.map fofHtml f404s) in
  section "404s" f404s None routes

let view404s =
  Cache.cache1 (fun f404s -> f404s) view404s_

let viewDBs_ (tls : toplevel list) : msg Html.html =
  let dbs =
    tls
    |> List.filter (fun tl -> TL.asDB tl <> None)
    |> List.map (fun tl -> (tl.pos, TL.asDB tl |> deOption "asDB"))
    |> List.sortBy (fun (_, db) -> db.dbName)
  in
  let dbHtml (pos, db) =
    div "simple-route" [span "name" [tlLink pos "default-link" db.dbName]]
  in
  let routes = div "dbs" (List.map dbHtml dbs) in
  section "DBs" dbs None routes

let viewDBs =
  Cache.cache1 (fun tls -> TL.dbs tls) viewDBs_


let viewUserFunctions_ (m : model) : msg Html.html =
  let fns =
    m.userFunctions |> List.filter (fun fn -> B.isF fn.ufMetadata.ufmName)
  in
  let fnNamedLink fn name =
    let useCount = Refactor.countFnUsage m name in
    if useCount = 0 then
      [ span "name" [fnLink fn false name]
      ; buttonLink
          ~key:("duf-" ^ (showTLID fn.ufTLID))
          (fontAwesome "minus-circle")
          (DeleteUserFunction fn.ufTLID) None ]
    else
      let countedName = name ^ " (" ^ string_of_int useCount ^ ")" in
      [span "name" [fnLink fn true countedName]]
  in
  let fnHtml fn =
    div "simple-route"
      (let fnName = B.asF fn.ufMetadata.ufmName in
       match fnName with
       | Some name -> fnNamedLink fn name
       | None -> [span "name" [fnLink fn true "should be filtered by here"]])
  in
  let routes = div "fns" (List.map fnHtml fns) in
  section "Functions" fns (Some ("cf", CreateFunction)) routes

let viewUserFunctions =
  Cache.cache1 (fun m -> (m.userFunctions, m.toplevels)) viewUserFunctions_

let viewRoutingTable_ (m : model) : msg Html.html =
  let sections =
    viewRoutes m CollapseVerbs ShowLink DontShowUndo
    @ [viewDBs m.toplevels] @ [view404s m.f404s] @ [viewUserFunctions m]
    @ [viewDeletedTLs m]
  in
  let html =
    Html.div
      [ Html.class' "viewing-table"
      ; nothingMouseEvent "mouseup"
      ; ViewUtils.eventNoPropagation
          ~key:"ept"
          "mouseenter"
          (fun _ -> EnablePanning false)
      ; ViewUtils.eventNoPropagation
          ~key:"epf"
          "mouseleave"
          (fun _ -> EnablePanning true)
      ]
      sections
  in
  Html.div [Html.id "sidebar-left"] [html]

let rtCacheKey m =
  (m.toplevels, m.f404s, m.userFunctions, m.deletedToplevels)
let viewRoutingTable m =
  Cache.cache1 rtCacheKey viewRoutingTable_ m


