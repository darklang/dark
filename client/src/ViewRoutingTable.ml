open! Porting
open Prelude
open Types
open ViewUtils
module B = Blank
module TL = Toplevel

let missingEventSpaceDesc : string = "Undefined"

let missingEventRouteDesc : string = "Undefined"

type entry =
  { name : string
  ; tlid : tlid
  ; used : bool
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
  ; entries : item list }

and item =
  | Category of category
  | Entry of entry

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
            ; destination = Some (Toplevels tl.pos)
            ; minusButton = None
            ; plusButton = None
            ; externalLink = None
            ; verb = None } ) }


let splitBySpace (tls : toplevel list) : (string * toplevel list) list =
  let spaceName_ tl =
    tl
    |> TL.asHandler
    |> Option.map (fun x -> x.spec.module_)
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


let eventCategories (_m : model) (tls : toplevel list) : category list =
  let groups =
    tls |> Tc.List.filter ~f:TL.isCustomEventSpaceHandler |> splitBySpace
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


let rec count (s : item) : int =
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
  let cats =
    Tc.List.map cats ~f:(fun c ->
        { c with
          plusButton = None (* only allow new entries on the main category *)
        ; classname =
            (* dont open/close in lockstep with parent *)
            "deleted-" ^ c.classname
        ; entries =
            Tc.List.map c.entries ~f:(function
                | Entry e ->
                    Entry
                      { e with
                        plusButton = Some (RestoreToplevel e.tlid)
                      ; minusButton = None
                      ; externalLink = None }
                | c ->
                    c ) } )
  in
  { count = cats |> Tc.List.map ~f:(fun c -> count (Category c)) |> Tc.List.sum
  ; name = "Deleted"
  ; plusButton = None
  ; classname = "deleted"
  ; entries = Tc.List.map cats ~f:(fun c -> Category c) }


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


let entry2html (m : model) (e : entry) : msg Html.html =
  let ext =
    Tc.Option.map
      ~f:(fun l -> ViewCode.externalLink l m.canvasName m.userContentHost)
      e.externalLink
    |> Tc.Option.withDefault ~default:[]
  in
  let mainlink =
    match e.destination with
    | Some dest ->
        [Url.linkFor dest "default-link" [Html.text e.name]]
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
            ~key:("entry-" ^ showTLID e.tlid)
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
    [Html.class' "simple-route handler"]
    ( [Html.span [Html.class' "name"] mainlink]
    @ verb
    @ ext
    @ pluslink
    @ minuslink )


let rec item2html (m : model) (s : item) : msg Html.html =
  match s with Category c -> category2html m c | Entry e -> entry2html m e


and category2html (m : model) (c : category) : msg Html.html =
  let text cl t = Html.span [Html.class' cl] [Html.text t] in
  let isOpen = Tc.StrSet.has m.routingTableOpenDetails ~value:c.classname in
  let openEventHandler =
    ViewUtils.eventNoPropagation
      ~key:((if isOpen then "cheh-true-" else "cheh-false-") ^ c.classname)
      "click"
      (fun _ -> MarkRoutingTableOpen (not isOpen, c.classname))
  in
  let openAttr =
    if isOpen then [Vdom.attribute "" "open" ""] else [Vdom.noProp]
  in
  let header =
    Html.summary
      [Html.class' "header"; openEventHandler]
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
  let routes = Tc.List.map ~f:(item2html m) c.entries in
  if List.length c.entries = 0
  then
    Html.div
      [Html.class' ("routing-section empty " ^ c.classname)]
      (header :: routes)
  else
    Html.details (Html.class' "routing-section" :: openAttr) (header :: routes)


let viewRoutingTable_ (m : model) : msg Html.html =
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
      (Tc.List.map ~f:(category2html m) cats)
  in
  Html.div [Html.id "sidebar-left"] [html]


let rtCacheKey m =
  ( m.toplevels
  , m.userFunctions
  , m.f404s
  , m.deletedToplevels
  , m.deletedUserFunctions
  , m.routingTableOpenDetails )


let viewRoutingTable m =
  (* Cache.cache1 rtCacheKey viewRoutingTable_ m *)
  viewRoutingTable_ m
