open Tc
open Prelude
open Types
open ViewUtils
module B = Blank
module TL = Toplevel
module TD = TLIDDict

let missingEventSpaceDesc : string = "Undefined"

let missingEventRouteDesc : string = "Undefined"

type entry =
  { name : string
  ; tlid : tlid
  ; destination : page option (* where to go when clicked *)
  ; uses : int option
  ; minusButton : msg option
  ; plusButton : msg option
  ; killAction :
      msg option
      (* if this is in the deleted section, what does minus do? *)
  ; verb : string option }

and category =
  { count : int
  ; name : string
  ; plusButton : msg option
  ; classname : string
  ; entries : item list }

and item =
  | Category of category
  | Entry of entry

let buttonLink ~(key : string) (content : msg Html.html) (handler : msg) :
    msg Html.html =
  let event = ViewUtils.eventNeither ~key "click" (fun _ -> handler) in
  Html.a [event; Html.class' "button-link"] [content]


let htmlObject (src : string) =
  Html.node
    "object"
    [Vdom.attribute "" "data" src; Html.type' "image/svg+xml"]
    []


let categoryIcon (name : string) : msg Html.html list =
  match String.toLower name with
  | "http" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/http.svg")]
  | "dbs" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/db.svg")]
  | "fns" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/fn.svg")]
  | "deleted" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/deleted.svg")]
  | "static" ->
      [fontAwesome "file"]
  | "types" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/types.svg")]
  | "cron" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/cron.svg")]
  | "repl" ->
      [fontAwesome "terminal"]
  | "worker" ->
      [fontAwesome "wrench"]
  | "fof" ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/fof.svg")]
  | "groups" ->
      [fontAwesome "object-group"]
  | _ ->
      [htmlObject ("//" ^ Native.Ext.staticHost () ^ "/icons/undefined.svg")]


let handlerCategory
    (filter : toplevel -> bool)
    (name : string)
    (action : omniAction)
    (hs : handler list) : category =
  let handlers = hs |> List.filter ~f:(fun h -> filter (TLHandler h)) in
  { count = List.length handlers
  ; name = String.toUpper name
  ; plusButton = Some (CreateRouteHandler action)
  ; classname = String.toLower name
  ; entries =
      List.map handlers ~f:(fun h ->
          let tlid = h.hTLID in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Option.withDefault ~default:missingEventRouteDesc
            ; uses = None
            ; tlid
            ; destination = Some (FocusedHandler (tlid, true))
            ; minusButton = None
            ; killAction = Some (ToplevelDeleteForever tlid)
            ; plusButton = None
            ; verb =
                ( if TL.isHTTPHandler (TLHandler h)
                then B.toMaybe h.spec.modifier
                else None ) } ) }


let httpCategory (handlers : handler list) : category =
  handlerCategory TL.isHTTPHandler "HTTP" (NewHTTPHandler None) handlers


let cronCategory (handlers : handler list) : category =
  handlerCategory TL.isCronHandler "CRON" (NewCronHandler None) handlers


let replCategory (handlers : handler list) : category =
  handlerCategory TL.isReplHandler "REPL" (NewReplHandler None) handlers


let workerCategory (handlers : handler list) : category =
  handlerCategory
    (fun tl ->
      TL.isWorkerHandler tl
      || (* Show the old workers here for now *)
         TL.isDeprecatedCustomHandler tl )
    "WORKER"
    (NewWorkerHandler None)
    handlers


let dbCategory (m : model) (dbs : db list) : category =
  let entries =
    List.map dbs ~f:(fun db ->
        let uses =
          match db.dbName with
          | Blank _ ->
              0
          | F (_, name) ->
              Refactor.dbUseCount m name
        in
        let minusButton =
          if (not (DB.isLocked m db.dbTLID)) && uses = 0
          then Some (ToplevelDelete db.dbTLID)
          else None
        in
        Entry
          { name = B.valueWithDefault "Untitled DB" db.dbName
          ; tlid = db.dbTLID
          ; uses = Some uses
          ; destination = Some (FocusedDB (db.dbTLID, true))
          ; minusButton
          ; killAction = Some (ToplevelDeleteForever db.dbTLID)
          ; verb = None
          ; plusButton = None } )
  in
  { count = List.length dbs
  ; name = "Databases"
  ; classname = "dbs"
  ; plusButton = Some CreateDBTable
  ; entries }


let f404Category (m : model) : category =
  let f404s =
    m.f404s |> List.uniqueBy ~f:(fun f -> f.space ^ f.path ^ f.modifier)
  in
  { count = List.length f404s
  ; name = "404s"
  ; plusButton = None
  ; classname = "fof"
  ; entries =
      List.map f404s ~f:(fun ({space; path; modifier} as fof) ->
          Entry
            { name = (if space = "HTTP" then path else space ^ "::" ^ path)
            ; uses = None
            ; tlid = TLID "no-tlid-for-404"
            ; destination = None
            ; minusButton = Some (Delete404RPC fof)
            ; killAction = None
            ; plusButton = Some (CreateHandlerFrom404 fof)
            ; verb = Some modifier } ) }


let userFunctionCategory (m : model) (ufs : userFunction list) : category =
  let fns = ufs |> List.filter ~f:(fun fn -> B.isF fn.ufMetadata.ufmName) in
  let entries =
    List.map fns ~f:(fun fn ->
        let name =
          fn.ufMetadata.ufmName |> Blank.toMaybe |> deOption "userFunction"
        in
        let minusButton =
          if Refactor.usedFn m name
          then None
          else Some (DeleteUserFunction fn.ufTLID)
        in
        Entry
          { name
          ; tlid = fn.ufTLID
          ; uses = Some (Refactor.fnUseCount m name)
          ; minusButton
          ; killAction = Some (DeleteUserFunctionForever fn.ufTLID)
          ; destination = Some (FocusedFn fn.ufTLID)
          ; plusButton = None
          ; verb = None } )
  in
  { count = List.length fns
  ; name = "Functions"
  ; classname = "fns"
  ; plusButton = Some CreateFunction
  ; entries }


let userTipeCategory (m : model) (tipes : userTipe list) : category =
  let tipes = tipes |> List.filter ~f:(fun t -> B.isF t.utName) in
  let entries =
    List.map tipes ~f:(fun tipe ->
        let name = tipe.utName |> Blank.toMaybe |> deOption "userTipe name" in
        let minusButton =
          if Refactor.usedTipe m name
          then None
          else Some (DeleteUserType tipe.utTLID)
        in
        Entry
          { name
          ; tlid = tipe.utTLID
          ; uses = Some (Refactor.tipeUseCount m name)
          ; minusButton
          ; killAction = Some (DeleteUserTypeForever tipe.utTLID)
          ; destination = Some (FocusedType tipe.utTLID)
          ; plusButton = None
          ; verb = None } )
  in
  { count = List.length tipes
  ; name = "Types"
  ; classname = "types"
  ; plusButton = Some CreateType
  ; entries }


let groupCategory (m : model) (groups : group list) : category =
  let groups = groups |> List.filter ~f:(fun (g : group) -> B.isF g.name) in
  let entries =
    List.map groups ~f:(fun (group : group) ->
        let name = group.name |> Blank.toMaybe |> deOption "group name" in
        let minusButton =
          let hasMembers = List.length group.members > 0 in
          if Refactor.usedGroup m name || hasMembers
          then None
          else Some (DeleteGroup group.gTLID)
        in
        Entry
          { name
          ; tlid = group.gTLID
          ; uses = None
          ; minusButton
          ; killAction = Some (DeleteGroupForever group.gTLID)
          ; destination = Some (FocusedGroup (group.gTLID, true))
          ; plusButton = None
          ; verb = None } )
  in
  { count = List.length groups
  ; name = "Groups"
  ; classname = "groups"
  ; plusButton = Some CreateGroup
  ; entries }


let rec count (s : item) : int =
  match s with
  | Entry _ ->
      1
  | Category c ->
      c.entries |> List.map ~f:count |> List.sum


let standardCategories m hs dbs ufns tipes groups =
  let hs =
    hs |> TD.values |> List.sortBy ~f:(fun tl -> TL.sortkey (TLHandler tl))
  in
  let dbs =
    dbs |> TD.values |> List.sortBy ~f:(fun tl -> TL.sortkey (TLDB tl))
  in
  let ufns =
    ufns |> TD.values |> List.sortBy ~f:(fun tl -> TL.sortkey (TLFunc tl))
  in
  let tipes =
    tipes |> TD.values |> List.sortBy ~f:(fun tl -> TL.sortkey (TLTipe tl))
  in
  let groups =
    groups |> TD.values |> List.sortBy ~f:(fun tl -> TL.sortkey (TLGroup tl))
  in
  [ httpCategory hs
  ; dbCategory m dbs
  ; userFunctionCategory m ufns
  ; userTipeCategory m tipes
  ; workerCategory hs
  ; cronCategory hs
  ; replCategory hs
  ; groupCategory m groups ]


let deletedCategory (m : model) : category =
  let cats =
    standardCategories
      m
      m.deletedHandlers
      m.deletedDBs
      m.deletedUserFunctions
      m.deletedUserTipes
      m.deletedGroups
    |> List.map ~f:(fun c ->
           { c with
             plusButton =
               None (* only allow new entries on the main category *)
           ; classname =
               (* dont open/close in lockstep with parent *)
               "deleted-" ^ c.classname
           ; entries =
               List.map c.entries ~f:(function
                   | Entry e ->
                       Entry
                         { e with
                           plusButton = Some (RestoreToplevel e.tlid)
                         ; uses = None
                         ; minusButton = e.killAction
                         ; destination = None }
                   | c ->
                       c ) } )
  in
  { count = cats |> List.map ~f:(fun c -> count (Category c)) |> List.sum
  ; name = "Deleted"
  ; plusButton = None
  ; classname = "deleted"
  ; entries = List.map cats ~f:(fun c -> Category c) }


let entry2html ~hovering (m : model) (e : entry) : msg Html.html =
  let name = e.name in
  let destinationLink page classes name =
    Url.linkFor page classes [Html.text name]
  in
  let mainlink =
    Html.span
      [Html.class' "name"]
      ( match e.destination with
      | Some dest ->
          let cl =
            if e.uses = Some 0 then "default-link unused" else "default-link"
          in
          [destinationLink dest cl name]
      | _ ->
          [Html.text name] )
  in
  let verb =
    match (e.destination, e.verb) with
    | Some dest, Some v ->
        [destinationLink dest "verb verb-link" v]
    | None, Some v ->
        [Html.span [Html.class' "verb"] [Html.text v]]
    | _ ->
        [Html.span [Html.class' "verb"] []]
  in
  let httpMethod = match e.verb with Some v -> v | None -> "" in
  let iconspacer = [Html.div [Html.class' "icon-spacer"] []] in
  let minuslink =
    (* This prevents the delete button appearing in the hover view.
     * We'll add it back in for 404s specifically at some point *)
    if hovering
    then Vdom.noNode
    else
      Html.div
        [Html.class' "delete"]
        ( match e.minusButton with
        | Some msg ->
            if m.permission = Some ReadWrite
            then
              [ buttonLink
                  ~key:("entry-" ^ showTLID e.tlid)
                  (fontAwesome "times-circle")
                  msg ]
            else []
        | None ->
            iconspacer )
  in
  let pluslink =
    match e.plusButton with
    | Some msg ->
        if m.permission = Some ReadWrite
        then [buttonLink ~key:(e.name ^ "-plus") (fontAwesome "plus") msg]
        else []
    | None ->
        iconspacer
  in
  let auxViews =
    Html.div
      [Html.classList [("aux", true); (httpMethod, true)]]
      (verb @ pluslink)
  in
  let selected = Some e.tlid = tlidOf m.cursorState in
  Html.div
    [Html.classList [("simple-item handler", true); ("selected", selected)]]
    [minuslink; mainlink; auxViews]


let deploy2html (d : staticDeploy) : msg Html.html =
  let statusString =
    match d.status with Deployed -> "Deployed" | Deploying -> "Deploying"
  in
  Html.div
    [Html.class' "simple-item deploy"]
    [ Html.div
        [Html.class' "deploy-status"]
        [ Html.a
            [Html.href d.url; Html.target "_blank"; Html.class' "hash"]
            [Html.text d.deployHash]
        ; Html.span
            [ Html.classList
                [ ("status", true)
                ; ( "success"
                  , match d.status with Deployed -> true | _ -> false ) ] ]
            [Html.text statusString] ]
    ; Html.span
        [Html.class' "datetime"]
        [Html.text (Js.Date.toUTCString d.lastUpdate)] ]


(* Category Views *)

let categoryTitle (name : string) (classname : string) : msg Html.html =
  let icon =
    Html.div
      [ Html.class' "header-icon"
      ; Html.title name
      ; Vdom.attribute "" "role" "img"
      ; Vdom.attribute "" "alt" name ]
      (categoryIcon classname)
  in
  let text cl t = Html.span [Html.class' cl] [Html.text t] in
  Html.div [Html.class' "title"] [icon; text "title" name]


let categoryOpenCloseHelpers (m : model) (classname : string) (count : int) :
    msg Vdom.property * msg Vdom.property =
  let isOpen = StrSet.has m.routingTableOpenDetails ~value:classname in
  let openEventHandler =
    ViewUtils.eventNoPropagation
      ~key:((if isOpen then "cheh-true-" else "cheh-false-") ^ classname)
      "click"
      (fun _ -> MarkRoutingTableOpen (not isOpen, classname))
  in
  let openAttr =
    if isOpen && count <> 0 then Vdom.attribute "" "open" "" else Vdom.noProp
  in
  (openEventHandler, openAttr)


let deployStats2html (m : model) : msg Html.html =
  let entries = m.staticDeploys in
  let count = List.length entries in
  let openEventHandler, openAttr =
    categoryOpenCloseHelpers m "deploys" count
  in
  let header =
    let title = categoryTitle "Static Assets" "static" in
    let deployLatest =
      if count <> 0
      then entries |> List.take ~count:1 |> List.map ~f:deploy2html
      else []
    in
    Html.summary
      [openEventHandler]
      [Html.div [Html.class' "header"] (title :: deployLatest)]
  in
  let deploys =
    if count > 1
    then entries |> List.drop ~count:1 |> List.map ~f:deploy2html
    else []
  in
  let classes =
    Html.classList
      [("sidebar-section", true); ("deploys", true); ("empty", count = 0)]
  in
  (if count = 0 then Html.div else Html.details)
    [classes; openAttr]
    (header :: deploys)


let rec item2html ~hovering (m : model) (s : item) : msg Html.html =
  match s with
  | Category c ->
      category2html m c
  | Entry e ->
      entry2html ~hovering m e


and category2html (m : model) (c : category) : msg Html.html =
  let openEventHandler, openAttr =
    categoryOpenCloseHelpers m c.classname c.count
  in
  let header =
    let title = categoryTitle c.name c.classname in
    let plusButton =
      match c.plusButton with
      | Some msg ->
          if m.permission = Some ReadWrite
          then
            [ buttonLink
                ~key:("plus-" ^ c.classname)
                (fontAwesome "plus-circle")
                msg ]
          else []
      | None ->
          []
    in
    Html.summary
      [Html.class' "headerSummary"; openEventHandler]
      [Html.div [Html.class' "header"] (title :: plusButton)]
  in
  let entries = List.map ~f:(item2html ~hovering:false m) c.entries in
  let classes =
    Html.classList
      [("sidebar-section", true); (c.classname, true); ("empty", c.count = 0)]
  in
  (if c.count = 0 then Html.div else Html.details)
    [classes; openAttr]
    (header :: entries)


let closedCategory2html (m : model) (c : category) : msg Html.html =
  let plusButton =
    match c.plusButton with
    | Some msg ->
        if m.permission = Some ReadWrite
        then
          [ buttonLink
              ~key:("plus-" ^ c.classname)
              (fontAwesome "plus-circle")
              msg ]
        else []
    | None ->
        []
  in
  let hoverView =
    let entries = List.map ~f:(item2html ~hovering:true m) c.entries in
    if c.count = 0 then [] else [Html.div [Html.class' "hover"] entries]
  in
  let icon =
    Html.div
      [ Html.classList [("header-icon", true); ("empty", c.count = 0)]
      ; Vdom.attribute "" "role" "img"
      ; Vdom.attribute "" "alt" c.name ]
      (categoryIcon c.classname)
  in
  Html.div
    [Html.class' "collapsed"]
    ([Html.div [Html.class' "collapsed-icon"] (icon :: plusButton)] @ hoverView)


let closedDeployStats2html (m : model) : msg Html.html =
  let entries = m.staticDeploys in
  let count = List.length entries in
  let hoverView =
    if count > 0
    then
      let deploys = List.map ~f:deploy2html entries in
      [Html.div [Html.class' "hover"] deploys]
    else [Vdom.noNode]
  in
  let icon =
    Html.div
      [ Html.classList [("header-icon", true); ("empty", count = 0)]
      ; Vdom.attribute "" "role" "img"
      ; Vdom.attribute "" "alt" "Static Assets" ]
      (categoryIcon "static")
  in
  Html.div
    [Html.class' "collapsed"]
    ([Html.div [Html.class' "collapsed-icon"] [icon]] @ hoverView)


let toggleSidebar (m : model) : msg Html.html =
  let event =
    ViewUtils.eventNeither ~key:"toggle-sidebar" "click" (fun _ ->
        ToggleSideBar )
  in
  let button icon tooltip =
    Html.a [Html.class' "button-link"; Html.title tooltip] [icon; icon]
  in
  let toggleBtn =
    if m.sidebarOpen
    then button (fontAwesome "chevron-left") "Collapse sidebar"
    else button (fontAwesome "chevron-right") "Expand sidebar"
  in
  let toggleSide =
    Html.div
      [event; Html.class' "toggle-container"]
      [ Html.p [] [Html.text "Collapse sidebar"]
      ; Html.div
          [ Html.classList
              [("toggle-button", true); ("closed", not m.sidebarOpen)] ]
          [toggleBtn] ]
  in
  toggleSide


let stateInfoTohtml (key : string) (value : msg Html.html) : msg Html.html =
  Html.div
    [Html.class' "state-info-row"]
    [ Html.p [Html.class' "key"] [Html.text key]
    ; Html.p [Html.class' "sep"] [Html.text ":"]
    ; Html.p [Html.class' "value"] [value] ]


let adminDebuggerView (m : model) : msg Html.html =
  let environmentName =
    if m.environment == "prodclone" then "clone" else m.environment
  in
  let pageToString pg =
    match pg with
    | Architecture ->
        "Architecture"
    | FocusedFn tlid ->
        Printf.sprintf "Fn (TLID %s)" (deTLID tlid)
    | FocusedHandler (tlid, _) ->
        Printf.sprintf "Handler (TLID %s)" (deTLID tlid)
    | FocusedDB (tlid, _) ->
        Printf.sprintf "DB (TLID %s)" (deTLID tlid)
    | FocusedType tlid ->
        Printf.sprintf "Type (TLID %s)" (deTLID tlid)
    | FocusedGroup (tlid, _) ->
        Printf.sprintf "Group (TLID %s)" (deTLID tlid)
  in
  let flagText =
    "["
    ^ (m.tests |> List.map ~f:show_variantTest |> String.join ~sep:", ")
    ^ "]"
  in
  let environment =
    Html.span [Html.class' "environment"] [Html.text environmentName]
  in
  let stateInfo =
    Html.div
      [Html.class' "state-info"]
      [ stateInfoTohtml "env" (Html.text m.environment)
      ; stateInfoTohtml "flags" (Html.text flagText)
      ; stateInfoTohtml "cursor" (Html.text (pageToString m.currentPage)) ]
  in
  let toggleTimer =
    Html.div
      [ ViewUtils.eventNoPropagation ~key:"tt" "mouseup" (fun _ -> ToggleTimers)
      ; Html.class' "checkbox-row" ]
      [ Html.input'
          [Html.type' "checkbox"; Html.checked (m.timersEnabled == false)]
          []
      ; Html.p [] [Html.text "Enable Timers"] ]
  in
  let debugger =
    Html.a
      [ Html.href (ViewScaffold.debuggerLinkLoc ())
      ; Html.class' "state-info-row debugger" ]
      [ Html.text
          (if Url.isDebugging then "Disable Debugger" else "Enable Debugger")
      ]
  in
  let saveTestButton =
    Html.a
      [ ViewUtils.eventNoPropagation ~key:"stb" "mouseup" (fun _ ->
            SaveTestButton )
      ; Html.class' "state-info-row save-state" ]
      [Html.text "SAVE STATE FOR INTEGRATION TEST"]
  in
  let hoverView =
    [ Html.div
        [Html.class' "hover admin-state"]
        [stateInfo; toggleTimer; debugger; saveTestButton] ]
  in
  let icon =
    Html.div
      [ Html.class' "header-icon admin-settings"
      ; Html.title "Admin"
      ; Vdom.attribute "" "role" "img"
      ; Vdom.attribute "" "alt" "Admin" ]
      [fontAwesome "cog"]
  in
  Html.div
    [Html.class' "collapsed admin"]
    [ Html.div
        [Html.class' ("collapsed-icon " ^ m.environment)]
        ([environment; icon] @ hoverView) ]


let viewSidebar_ (m : model) : msg Html.html =
  let isClosed : bool = not m.sidebarOpen in
  let cats =
    standardCategories m m.handlers m.dbs m.userFunctions m.userTipes m.groups
    @ [f404Category m; deletedCategory m]
  in
  let showAdminDebugger =
    if isClosed && m.isAdmin then adminDebuggerView m else Vdom.noNode
  in
  let showCategories =
    if isClosed then closedCategory2html else category2html
  in
  let showDeployStats =
    if isClosed then closedDeployStats2html else deployStats2html
  in
  let status =
    match m.error.message with
    | Some _ when m.isAdmin ->
        Html.div
          [ Html.classList
              [("error-status error", true); ("opened", m.error.showDetails)]
          ]
          [ Html.a
              [ Html.class' "link"
              ; Html.href "#"
              ; ViewUtils.eventNoPropagation
                  ~key:(string_of_bool m.error.showDetails)
                  "mouseup"
                  (fun _ -> ShowErrorDetails (not m.error.showDetails) ) ]
              [ Html.text
                  ( if m.error.showDetails
                  then "Hide details"
                  else "See details" ) ] ]
    | _ ->
        Html.div [Html.class' "error-status"] [Html.text "Dark"]
  in
  let html =
    Html.div
      [ Html.classList [("viewing-table", true); ("isClosed", isClosed)]
      ; nothingMouseEvent "mouseup"
      ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
            EnablePanning false )
      ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
            EnablePanning true ) ]
      ( [toggleSidebar m]
      @ [ Html.div
            [Html.classList [("groups-closed", isClosed); ("groups", true)]]
            ( List.map ~f:(showCategories m) cats
            @ [showDeployStats m; showAdminDebugger] )
        ; status ] )
  in
  Html.div [Html.id "sidebar-left"] [html]


let rtCacheKey m =
  ( m.handlers
    |> TD.mapValues ~f:(fun (h : handler) -> (h.pos, TL.sortkey (TLHandler h)))
  , m.dbs |> TD.mapValues ~f:(fun (db : db) -> (db.pos, TL.sortkey (TLDB db)))
  , m.userFunctions |> TD.mapValues ~f:(fun f -> f.ufMetadata.ufmName)
  , m.userTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.f404s
  , m.sidebarOpen
  , m.deletedHandlers
    |> TD.mapValues ~f:(fun (h : handler) -> TL.sortkey (TLHandler h))
  , m.deletedDBs
    |> TD.mapValues ~f:(fun (db : db) -> (db.pos, TL.sortkey (TLDB db)))
  , m.deletedUserFunctions |> TD.mapValues ~f:(fun f -> f.ufMetadata.ufmName)
  , m.deletedUserTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.routingTableOpenDetails
  , m.staticDeploys
  , m.unlockedDBs
  , m.usedDBs
  , m.usedFns
  , m.usedGroups
  , m.userTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.deletedUserTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.groups
  , m.deletedGroups
    |> TD.mapValues ~f:(fun (g : group) -> TL.sortkey (TLGroup g))
  , tlidOf m.cursorState
  , m.environment
  , m.timersEnabled
  , m.error
  , m.currentPage )


let viewSidebar m = Cache.cache1 rtCacheKey viewSidebar_ m
