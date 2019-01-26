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

let missingEventSpaceDesc : string = "Undefined"

let missingEventRouteDesc : string = "Undefined"

type newentry =
  { name : string
  ; tlid : tlid
  ; used : bool
  ; class' : string
  ; destination : page option
  ; minusButton : (int option * msg) option
  ; plusButton : msg option
  ; verb : string option
  ; externalLink : handlerSpec option }

and category =
  { count : int
  ; name : string
  ; plusButton : (string * msg) option
  ; classname : string
  ; entries : somename list }

and somename =
  | Category of category
  | Entry of newentry

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


let openAttr (m : model) (name : string) =
  if Tc.StrSet.has m.routingTableOpenDetails ~value:name
  then
    (* This was surprisingly fickle. To work, it needed to use the same key for
     * both sides of the condition, and I needed to omit Vdom.noProp which
     * would seem natural. *)
    [ Vdom.attribute "" "open" "true"
    ; ViewUtils.eventNoPropagation ~key:("rtod-" ^ name) "click" (fun _ ->
          MarkRoutingTableOpen (false, name) ) ]
  else
    [ (* Vdom.noProp - see comment above *)
      ViewUtils.eventNoPropagation ~key:("rtod-" ^ name) "click" (fun _ ->
          MarkRoutingTableOpen (true, name) ) ]


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
    (m : model)
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
      ([Html.class' "routing-section"] @ openAttr m name)
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
  section m spacename distinctEntries button routes


let viewRoutes
    (m : model)
    (tls : toplevel list)
    (spaceName : string)
    (filterFunc : toplevel -> bool)
    (collapse : collapseVerbs)
    (showLink : showLink)
    (showUndo : showUndo) : msg Html.html list =
  (* if the list is empty, ensure we display a routing table entry anyway *)
  let handleEmpty entries =
    if List.isEmpty entries then [(spaceName, [])] else entries
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


let httpCategory (_m : model) (tls : toplevel list) : category =
  let handlers = tls |> Tc.List.filter ~f:TL.isHTTPHandler in
  { count = List.length handlers
  ; name = "HTTP"
  ; plusButton = Some ("crh", CreateRouteHandler)
  ; classname = "http"
  ; entries =
      Tc.List.map handlers ~f:(fun tl ->
          let h = tl |> TL.asHandler |> deOption "httpCategory/entry" in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Tc.Option.withDefault ~default:missingEventRouteDesc
            ; used = true
            ; tlid = h.tlid
            ; class' = "default-link"
            ; destination = Some (Toplevels tl.pos)
            ; minusButton = None
            ; plusButton = None
            ; externalLink = Some h.spec
            ; verb = h.spec.modifier |> Blank.toMaybe } ) }


let cronCategory (_m : model) (tls : toplevel list) : category =
  let handlers = tls |> Tc.List.filter ~f:TL.isCronHandler in
  { count = List.length handlers
  ; name = "CRON"
  ; plusButton = None
  ; classname = "cron"
  ; entries =
      Tc.List.map handlers ~f:(fun tl ->
          let h = tl |> TL.asHandler |> deOption "cronCategory/entry" in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Tc.Option.withDefault ~default:missingEventRouteDesc
            ; used = true
            ; tlid = h.tlid
            ; class' = "default-link"
            ; destination = Some (Toplevels tl.pos)
            ; minusButton = None
            ; plusButton = None
            ; externalLink = None
            ; verb = None } ) }


let dbCategory (_m : model) (tls : toplevel list) : category =
  let dbs =
    tls
    |> List.filter (fun tl -> TL.asDB tl <> None)
    |> List.map (fun tl -> (tl.pos, TL.asDB tl |> deOption "asDB"))
    |> List.sortBy (fun (_, db) -> db.dbName)
  in
  let entries =
    Tc.List.map dbs ~f:(fun (pos, db) ->
        Entry
          { name = db.dbName
          ; tlid = db.dbTLID
          ; used = false
          ; class' = "default-link"
          ; destination = Some (Toplevels pos)
          ; minusButton = None
          ; externalLink = None
          ; verb = None
          ; plusButton = None } )
  in
  { count = List.length dbs
  ; name = "DBs"
  ; classname = "dbs"
  ; plusButton = None
  ; entries }


let undefinedCategory (_m : model) (tls : toplevel list) : category =
  let handlers = tls |> Tc.List.filter ~f:TL.isUndefinedEventSpaceHandler in
  { count = List.length handlers
  ; name = missingEventSpaceDesc
  ; plusButton = None
  ; classname = missingEventSpaceDesc
  ; entries =
      Tc.List.map handlers ~f:(fun tl ->
          let h = tl |> TL.asHandler |> deOption "undefinedCategory/entry" in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Tc.Option.withDefault ~default:missingEventRouteDesc
            ; used = true
            ; tlid = h.tlid
            ; class' = "default-link"
            ; destination = Some (Toplevels tl.pos)
            ; minusButton = None
            ; plusButton = None
            ; externalLink = None
            ; verb = None } ) }


let eventCategories (_m : model) (tls : toplevel list) : category list =
  let groups =
    tls
    |> Tc.List.filter ~f:TL.isCustomEventSpaceHandler
    |> splitBySpace
    |> List.sortWith (fun (a, _) (b, _) -> ordering a b)
  in
  Tc.List.map groups ~f:(fun (name, handlers) ->
      { count = List.length handlers
      ; name
      ; plusButton = None
      ; classname = name
      ; entries =
          Tc.List.map handlers ~f:(fun tl ->
              let h = tl |> TL.asHandler |> deOption "eventCategories/entry" in
              Entry
                { name =
                    h.spec.name
                    |> Blank.toMaybe
                    |> Tc.Option.withDefault ~default:missingEventRouteDesc
                ; used = true
                ; tlid = h.tlid
                ; class' = "default-link"
                ; destination = Some (Toplevels tl.pos)
                ; minusButton = None
                ; plusButton = None
                ; externalLink = None
                ; verb = None } ) } )


let f404Category (m : model) : category =
  let f404s = m.f404s in
  { count = List.length f404s
  ; name = "404s"
  ; plusButton = None
  ; classname = "fof"
  ; entries =
      Tc.List.map f404s ~f:(fun ({space; path; modifier} as fof) ->
          Entry
            { name = (if space = "HTTP" then path else space ^ "::" ^ path)
            ; used = true
            ; tlid = TLID "no-tlid-for-404"
            ; class' = "default-link"
            ; destination = None
            ; minusButton = Some (None, Delete404 fof)
            ; plusButton = Some (CreateHandlerFrom404 fof)
            ; externalLink = None
            ; verb = Some modifier } ) }


let userFunctionCategory (m : model) (ufs : userFunction list) : category =
  let fns = ufs |> List.filter (fun fn -> B.isF fn.ufMetadata.ufmName) in
  let entries =
    Tc.List.map fns ~f:(fun fn ->
        let name =
          fn.ufMetadata.ufmName |> Blank.toMaybe |> deOption "userFunction"
        in
        let useCount = Refactor.countFnUsage m name in
        let minusButton =
          if useCount = 0
          then Some (Some useCount, DeleteUserFunction fn.ufTLID)
          else None
        in
        Entry
          { name
          ; tlid = fn.ufTLID
          ; used = useCount > 0
          ; class' = "fn"
          ; minusButton
          ; destination = Some (Fn (fn.ufTLID, Defaults.centerPos))
          ; plusButton = None
          ; verb = None
          ; externalLink = None } )
  in
  { count = List.length fns
  ; name = "Functions"
  ; classname = "fns"
  ; plusButton = Some ("cf", CreateFunction)
  ; entries }


let rec count (s : somename) : int =
  match s with
  | Entry _ ->
      1
  | Category c ->
      c.entries |> Tc.List.map ~f:count |> Tc.List.sum


let deletedCategory (m : model) : category =
  let tls =
    m.deletedToplevels |> Tc.List.sortBy ~f:(fun tl -> TL.sortkey tl)
  in
  let ufns =
    m.deletedUserFunctions
    |> Tc.List.sortBy ~f:(fun fn ->
           fn.ufMetadata.ufmName |> Blank.toMaybe |> Option.withDefault "" )
  in
  let cats =
    [ httpCategory m tls
    ; dbCategory m tls
    ; userFunctionCategory m ufns
    ; cronCategory m tls ]
    @ eventCategories m tls
    @ [undefinedCategory m tls]
  in
  { count = cats |> Tc.List.map ~f:(fun c -> count (Category c)) |> Tc.List.sum
  ; name = "Deleted"
  ; plusButton = None
  ; classname = "deleted"
  ; entries = Tc.List.map cats ~f:(fun x -> Category x) }


let view404s_ (m : model) (f404s : fourOhFour list) : msg Html.html =
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
  section m "404s" f404s None routes


let view404s m =
  Cache.cache1 (fun f404s -> (f404s, m.routingTableOpenDetails)) (view404s_ m)


let viewRestorableDBs (m : model) (tls : toplevel list) : msg Html.html =
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
  section m "DBs" dbs None routes


let viewDBs_ (m : model) (tls : toplevel list) : msg Html.html =
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
  section m "DBs" dbs None routes


let viewDBs m =
  Cache.cache1
    (fun tls -> (TL.dbs tls, m.routingTableOpenDetails))
    (viewDBs_ m)


let deletedUserFunctionsEntries (m : model) : category =
  let fns =
    m.deletedUserFunctions
    |> List.filter (fun fn -> B.isF fn.ufMetadata.ufmName)
  in
  let entries =
    Tc.List.map fns ~f:(fun fn ->
        Entry
          { name =
              fn.ufMetadata.ufmName |> Blank.toMaybe |> Option.withDefault ""
          ; tlid = fn.ufTLID
          ; used = false
          ; class' = "fn"
          ; minusButton = None
          ; destination = Some (Fn (fn.ufTLID, Defaults.centerPos))
          ; plusButton = None
          ; verb = None
          ; externalLink = None } )
  in
  { count = List.length fns
  ; name = "Functions"
  ; classname = "fns"
  ; plusButton = None
  ; entries }


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
  section m "Functions" fns None routes


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
  section m "Functions" fns (Some ("cf", CreateFunction)) routes


let viewUserFunctions =
  Cache.cache1
    (fun m -> (m.userFunctions, m.toplevels, m.routingTableOpenDetails))
    viewUserFunctions_


let viewDeletedTLs_ (m : model) : msg Html.html =
  let tls = m.deletedToplevels in
  let httpTLs =
    viewRoutes
      m
      tls
      "HTTP"
      TL.isHTTPHandler
      DontCollapseVerbs
      DontShowLink
      ShowUndo
  in
  let cronTLs =
    viewRoutes
      m
      tls
      "CRON"
      TL.isCronHandler
      DontCollapseVerbs
      DontShowLink
      ShowUndo
  in
  let customEventSpaceTLs =
    viewRoutes
      m
      tls
      "Custom Event Space"
      TL.isCustomEventSpaceHandler
      DontCollapseVerbs
      DontShowLink
      ShowUndo
  in
  let undefinedEventSpaceTLs =
    viewRoutes
      m
      tls
      "Undefined"
      TL.isUndefinedEventSpaceHandler
      DontCollapseVerbs
      DontShowLink
      ShowUndo
  in
  let dbs = viewRestorableDBs m tls in
  let fns = viewDeletedUserFunctions m in
  let count = List.length m.deletedUserFunctions + List.length tls in
  let h = header "Deleted" count None in
  Html.details
    ([Html.class' "routing-section deleted"] @ openAttr m "deleted")
    ( [h]
    @ httpTLs
    @ [dbs; fns]
    @ cronTLs
    @ customEventSpaceTLs
    @ undefinedEventSpaceTLs )


let viewDeletedTLs =
  Cache.cache1
    (fun m ->
      (m.deletedToplevels, m.deletedUserFunctions, m.routingTableOpenDetails)
      )
    viewDeletedTLs_


let entry2html (m : model) (e : newentry) : msg Html.html =
  (* TODO: restore button *)
  let ext =
    Tc.Option.map
      ~f:(fun l -> ViewCode.externalLink l m.canvasName m.userContentHost)
      e.externalLink
    |> Tc.Option.withDefault ~default:[]
  in
  let mainlink =
    match e.destination with
    | Some dest ->
        [Url.linkFor dest "verb-link" [Html.text e.name]]
    | _ ->
        [Html.text e.name]
  in
  let verb =
    match (e.destination, e.verb) with
    | Some dest, Some v ->
        [Url.linkFor dest "verb-link" [Html.text v]]
    | None, Some v ->
        [Html.text v]
    | _ ->
        []
  in
  let minuslink =
    match e.minusButton with
    | Some (None, msg) | Some (Some 0, msg) ->
        [ buttonLink
            ~key:(e.class' ^ showTLID e.tlid)
            (fontAwesome "minus-circle")
            msg
            None ]
    | Some (Some count, _) ->
        [Html.text (" (" ^ string_of_int count ^ ")")]
    | None ->
        []
  in
  let pluslink =
    match e.plusButton with
    | Some msg ->
        [ buttonLink
            ~key:(e.name ^ "-plus")
            (fontAwesome "plus-circle")
            msg
            None ]
    | None ->
        []
  in
  Html.div
    [Html.class' "simple-route"]
    ( [Html.span [Html.class' "name"] mainlink]
    @ verb
    @ ext
    @ pluslink
    @ minuslink )


let rec somename2html (m : model) (s : somename) : msg Html.html =
  match s with Category c -> category2html m c | Entry e -> entry2html m e


and category2html (m : model) (c : category) : msg Html.html =
  let header =
    Html.summary
      [Html.class' "header"]
      [ text "title" c.name
      ; text "parens" "("
      ; text "count" (c.count |> string_of_int)
      ; text "parens" ")"
      ; ( match c.plusButton with
        | Some (key, msg) ->
            buttonLink ~key (fontAwesome "plus-circle") msg None
        | None ->
            text "" "" ) ]
  in
  let routes = Tc.List.map ~f:(somename2html m) c.entries in
  (* TODO: readd openAttr *)
  if List.length c.entries = 0
  then Html.div [Html.class' "routing-section empty"] (header :: routes)
  else Html.details [Html.class' "routing-section"] (header :: routes)


let viewRoutingTable_ (m : model) : msg Html.html =
  let sections =
    viewRoutes
      m
      m.toplevels
      "HTTP"
      TL.isHTTPHandler
      CollapseVerbs
      ShowLink
      DontShowUndo
    @ [viewDBs m m.toplevels]
    @ [viewUserFunctions m]
    @ viewRoutes
        m
        m.toplevels
        "CRON"
        TL.isCronHandler
        CollapseVerbs
        ShowLink
        DontShowUndo
    @ viewRoutes
        m
        m.toplevels
        "Custom Event Space"
        TL.isCustomEventSpaceHandler
        CollapseVerbs
        ShowLink
        DontShowUndo
    @ viewRoutes
        m
        m.toplevels
        "Undefined"
        TL.isUndefinedEventSpaceHandler
        CollapseVerbs
        ShowLink
        DontShowUndo
    @ [view404s m m.f404s]
    @ [viewDeletedTLs m]
  in
  let tls = m.toplevels |> Tc.List.sortBy ~f:(fun tl -> TL.sortkey tl) in
  let ufns =
    m.userFunctions
    |> Tc.List.sortBy ~f:(fun fn ->
           fn.ufMetadata.ufmName |> Blank.toMaybe |> Option.withDefault "" )
  in
  let cats =
    [ httpCategory m tls
    ; dbCategory m tls
    ; userFunctionCategory m ufns
    ; cronCategory m tls ]
    @ eventCategories m tls
    @ [undefinedCategory m tls; f404Category m; deletedCategory m]
  in
  let html =
    Html.div
      [ Html.class' "viewing-table"
      ; nothingMouseEvent "mouseup"
      ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
            EnablePanning false )
      ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
            EnablePanning true ) ]
      (Tc.List.map ~f:(category2html m) cats @ sections)
  in
  Html.div [Html.id "sidebar-left"] [html]


let rtCacheKey m =
  ( m.toplevels
  , m.f404s
  , m.userFunctions
  , m.deletedToplevels
  , m.routingTableOpenDetails )


let viewRoutingTable m = Cache.cache1 rtCacheKey viewRoutingTable_ m
