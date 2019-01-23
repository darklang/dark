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

type collapseVerbs =
  | CollapseVerbs
  | DontCollapseVerbs

and showLink =
  | ShowLink
  | DontShowLink

and showUndo =
  | ShowUndo
  | DontShowUndo

(* ---------------------------------- *)
(* Html *)
(* ---------------------------------- *)
let span (class_ : string) (subs : msg Html.html list) : msg Html.html =
  Html.span [Html.class' class_] subs


let text (class_ : string) (msg : string) : msg Html.html =
  span class_ [Html.text msg]


let div (class_ : string) (subs : msg Html.html list) : msg Html.html =
  Html.div [Html.class' class_] subs


type entry =
  { name : string option
  ; verbs : (string * pos) list
  ; tlid : tlid }

let missingEventSpaceDesc : string = "Undefined"

let missingEventRouteDesc : string = "Undefined"

let spaceName (tl : toplevel) : string option =
  match tl.data with TLHandler h -> h.spec.module_ |> B.toMaybe | _ -> None


let splitBySpace (tls : toplevel list) : (string * toplevel list) list =
  let spaceName_ tl =
    tl
    |> TL.asHandler
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
         | Some h ->
             Some (tl.pos, tl.id, h)
         | None ->
             None )
  |> List.map (fun (pos, tlid, h) ->
         { name =
             (match h.spec.name with F (_, s) -> Some s | Blank _ -> None)
         ; verbs =
             ( match h.spec.modifier with
             | F (_, s) ->
                 [(s, pos)]
             | Blank _ ->
                 [("_", pos)] )
         ; tlid } )
  |> List.sortBy (fun c -> Option.withDefault "ZZZZZZ" c.name)


let collapseByVerb (es : entry list) : entry list =
  es
  |> List.groupWhile (fun a b -> a.name = b.name)
  |> List.map
       (List.foldr
          (fun curr list ->
            if curr.name = None
            then curr :: list
            else
              match list with
              | [] ->
                  [curr]
              | prev :: rest ->
                  let new_ = {prev with verbs = prev.verbs @ curr.verbs} in
                  new_ :: rest )
          [])
  |> List.concat


let ordering (a : string) (b : string) : int =
  match (a, b) with
  (* to the start *)
  | "HTTP", _ ->
      -1
  | _, "HTTP" ->
      1
  (* to the end *)
  | _ ->
      if a = missingEventRouteDesc
      then 1
      else if b = missingEventRouteDesc
      then -1
      else compare a b


let buttonLink
    ~(key : string)
    (content : msg Html.html)
    (handler : msg)
    (page : page option) : msg Html.html =
  let href =
    page |> Option.map (fun p -> Html.href (Url.urlFor p)) |> Option.toList
  in
  let event =
    match page with
    | None ->
        ViewUtils.eventNoDefault ~key "click" (fun _ -> handler)
    | Some _ ->
        ViewUtils.eventNoPropagation ~key "click" (fun _ -> handler)
  in
  Html.a ([event; Html.class' "button-link"] @ href) [content]


let undoButton (tlid : tlid) (page : page) : msg Html.html =
  buttonLink
    ~key:("rt" ^ showTLID tlid)
    (text "undo" "Restore")
    (RestoreToplevel tlid)
    (Some page)


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
let header (name : string) (count : int) (addHandler : (string * msg) option) :
    msg Html.html =
  Html.summary
    [Html.class' "header"]
    [ text "title" name
    ; text "parens" "("
    ; text "count" (count |> string_of_int)
    ; text "parens" ")"
    ; ( match addHandler with
      | Some (key, msg) ->
          buttonLink ~key (fontAwesome "plus-circle") msg None
      | None ->
          text "" "" ) ]


let section
    (name : string)
    (entries : 'a list)
    (addHandler : (string * msg) option)
    (routes : msg Html.html) : msg Html.html =
  if List.length entries = 0
  then
    Html.div
      [Html.class' "routing-section empty"]
      [header name 0 addHandler; routes]
  else
    Html.details
      [Html.class' "routing-section"]
      [header name (List.length entries) addHandler; routes]


let viewGroup
    (m : model)
    (showLink : showLink)
    (showUndo : showUndo)
    ((spacename, entries) : string * entry list) : msg Html.html =
  let def s = Option.withDefault missingEventRouteDesc s in
  let externalLink h =
    if showLink = ShowLink && List.member "GET" (List.map Tuple.first h.verbs)
    then
      match h.name with
      | Some n ->
          Html.a
            [ Html.class' "external"
            ; Html.href
                ( "//"
                ^ Tea.Http.encodeUri m.canvasName
                ^ "."
                ^ m.userContentHost
                ^ n )
            ; Html.target "_blank" ]
            [fontAwesome "external-link-alt"]
      | None ->
          Html.div [] []
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
    div
      "handler"
      ( [ div "name" [Html.text (def e.name)]
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


let viewRoutes
    (m : model)
    (tls : toplevel list)
    (spaceName : string)
    (filterFunc : (toplevel -> bool))
    (collapse : collapseVerbs)
    (showLink : showLink)
    (showUndo : showUndo) : msg Html.html list =
  (* if the list is empty, ensure we display a routing table entry anyway *)
  let handleEmpty entries =
    if List.isEmpty entries
    then [spaceName, []]
    else entries
  in
  tls
  |> List.filter filterFunc
  |> splitBySpace
  |> handleEmpty
  |> List.sortWith (fun (a, _) (b, _) -> ordering a b)
  |> List.map (Tuple.mapSecond tl2entry)
  |> (fun entries ->
       if collapse = CollapseVerbs
       then List.map (Tuple.mapSecond collapseByVerb) entries
       else entries )
  |> List.map (viewGroup m showLink showUndo)


let view404s_ (f404s : fourOhFour list) : msg Html.html =
  let fofToKey fof = fof.space ^ "-" ^ fof.path ^ "-" ^ fof.modifier in
  let theCreateLink fof =
    buttonLink
      ~key:(fofToKey fof)
      (fontAwesome "plus-circle")
      (CreateHandlerFrom404 fof)
      None
  in
  let theDeleteLink fof =
    buttonLink
      ~key:(fofToKey fof)
      (fontAwesome "minus-circle")
      (Delete404 fof)
      None
  in
  let fofHtml ({space; path; modifier} as fof) =
    div
      "fof"
      [ text "path" path
      ; (if space = "HTTP" then text "" "" else text "space" space)
      ; text "modifier" modifier
      ; theCreateLink fof
      ; theDeleteLink fof ]
  in
  let routes = div "404s" (List.map fofHtml f404s) in
  section "404s" f404s None routes


let view404s = Cache.cache1 (fun f404s -> f404s) view404s_

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


let viewDBs = Cache.cache1 (fun tls -> TL.dbs tls) viewDBs_

let viewDeletedUserFunctions (m : model) : msg Html.html =
  let fns =
    m.deletedUserFunctions
    |> List.filter (fun fn -> B.isF fn.ufMetadata.ufmName)
  in
  let fnNamedLink fn name = [span "name" [fnLink fn false name]] in
  let fnHtml fn =
    div
      "simple-route"
      (let fnName = B.asF fn.ufMetadata.ufmName in
       match fnName with
       | Some name ->
           fnNamedLink fn name
       | None ->
           [span "name" [fnLink fn true "should be filtered by here"]])
  in
  let routes = div "fns" (List.map fnHtml fns) in
  section "Functions" fns (Some ("cf", CreateFunction)) routes


let viewUserFunctions_ (m : model) : msg Html.html =
  let fns =
    m.userFunctions |> List.filter (fun fn -> B.isF fn.ufMetadata.ufmName)
  in
  let fnNamedLink fn name =
    let useCount = Refactor.countFnUsage m name in
    if useCount = 0
    then
      [ span "name" [fnLink fn false name]
      ; buttonLink
          ~key:("duf-" ^ showTLID fn.ufTLID)
          (fontAwesome "minus-circle")
          (DeleteUserFunction fn.ufTLID)
          None ]
    else
      let countedName = name ^ " (" ^ string_of_int useCount ^ ")" in
      [span "name" [fnLink fn true countedName]]
  in
  let fnHtml fn =
    div
      "simple-route"
      (let fnName = B.asF fn.ufMetadata.ufmName in
       match fnName with
       | Some name ->
           fnNamedLink fn name
       | None ->
           [span "name" [fnLink fn true "should be filtered by here"]])
  in
  let routes = div "fns" (List.map fnHtml fns) in
  section "Functions" fns (Some ("cf", CreateFunction)) routes


let viewUserFunctions =
  Cache.cache1 (fun m -> (m.userFunctions, m.toplevels)) viewUserFunctions_


let viewDeletedTLs_ (m : model) : msg Html.html =
  let tls = m.deletedToplevels in
  let httpTLs = viewRoutes m tls "HTTP" TL.isHTTPHandler DontCollapseVerbs DontShowLink ShowUndo in
  let cronTLs = viewRoutes m tls "CRON" TL.isCronHandler DontCollapseVerbs DontShowLink ShowUndo in
  let customEventSpaceTLs = viewRoutes m tls "Custom Event Space" TL.isCustomEventSpaceHandler DontCollapseVerbs DontShowLink ShowUndo in
  let undefinedEventSpaceTLs = viewRoutes m tls "Undefined" TL.isUndefinedEventSpaceHandler DontCollapseVerbs DontShowLink ShowUndo in
  let dbs = viewRestorableDBs tls in
  let fns = viewDeletedUserFunctions m in
  let count = List.length m.deletedUserFunctions + List.length tls in
  let h = header "Deleted" count None in
  Html.details
    [Html.class' "routing-section deleted"]
    ([h] @ httpTLs @ [dbs; fns] @ cronTLs @ customEventSpaceTLs @ undefinedEventSpaceTLs)


let viewDeletedTLs =
  Cache.cache1
    (fun m -> (m.deletedToplevels, m.deletedUserFunctions))
    viewDeletedTLs_


let viewRoutingTable_ (m : model) : msg Html.html =
  let sections =
      viewRoutes m m.toplevels "HTTP" TL.isHTTPHandler CollapseVerbs ShowLink DontShowUndo
    @ [viewDBs m.toplevels]
    @ [viewUserFunctions m]
    @ viewRoutes m m.toplevels "CRON" TL.isCronHandler CollapseVerbs ShowLink DontShowUndo
    @ viewRoutes m m.toplevels "Custom Event Space" TL.isCustomEventSpaceHandler CollapseVerbs ShowLink DontShowUndo
    @ viewRoutes m m.toplevels "Undefined" TL.isUndefinedEventSpaceHandler CollapseVerbs ShowLink DontShowUndo
    @ [view404s m.f404s]
    @ [viewDeletedTLs m]
  in
  let html =
    Html.div
      [ Html.class' "viewing-table"
      ; nothingMouseEvent "mouseup"
      ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
            EnablePanning false )
      ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
            EnablePanning true ) ]
      sections
  in
  Html.div [Html.id "sidebar-left"] [html]


let rtCacheKey m = (m.toplevels, m.f404s, m.userFunctions, m.deletedToplevels)

let viewRoutingTable m = Cache.cache1 rtCacheKey viewRoutingTable_ m
