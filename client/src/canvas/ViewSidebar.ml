open Prelude
open ViewUtils
module B = BlankOr
module TL = Toplevel
module TD = TLIDDict
module Cmd = Tea.Cmd

let missingEventSpaceDesc : string = "Undefined"

let missingEventRouteDesc : string = "Undefined"

let delPrefix : string = "deleted-"

type identifier =
  | Tlid of TLID.t
  | Other of string

type onClickAction =
  | Destination of page
  | SendMsg of msg
  | DoNothing

let tlidOfIdentifier identifier : TLID.t option =
  match identifier with Tlid tlid -> Some tlid | Other _ -> None


let entryKeyFromIdentifier identifier : string =
  match identifier with
  | Tlid tlid ->
      "entry-" ^ TLID.toString tlid
  | Other s ->
      "entry-" ^ s


type entry =
  { name : string
  ; identifier : identifier
  ; onClick : onClickAction
  ; uses : int option
  ; minusButton : msg option
  ; plusButton : msg option
  ; killAction : msg option
        (* if this is in the deleted section, what does minus do? *)
  ; verb : string option }

and category =
  { count : int
  ; name : string
  ; plusButton : msg option
  ; iconAction : msg option
  ; classname : string
  ; tooltip : tooltip option
  ; entries : item list }

and item =
  | Category of category
  | Entry of entry

let rec count (s : item) : int =
  match s with
  | Entry _ ->
      1
  | Category c ->
      c.entries |> List.map ~f:count |> List.sum


let iconButton
    ~(key : string) ~(icon : string) ~(classname : string) (handler : msg) :
    msg Html.html =
  let event = ViewUtils.eventNeither ~key "click" (fun _ -> handler) in
  Html.div [event; Html.class' ("icon-button " ^ classname)] [fontAwesome icon]


let categoryIcon_ (name : string) : msg Html.html list =
  let darkIcon = ViewUtils.darkIcon in
  (* Deleted categories have a deleted- prefix, with which are not valid fontaweome icons *)
  match
    name |> String.toLower |> Regex.replace ~re:(Regex.regex delPrefix) ~repl:""
  with
  | "http" ->
      [darkIcon "http"]
  | "dbs" ->
      [darkIcon "db"]
  | "fns" ->
      [darkIcon "fn"]
  | "deleted" ->
      [darkIcon "deleted"]
  | "package-manager" ->
      [fontAwesome "box-open"]
  | "static" ->
      [fontAwesome "file"]
  | "types" ->
      [darkIcon "types"]
  | "cron" ->
      [darkIcon "cron"]
  | "repl" ->
      [fontAwesome "terminal"]
  | "worker" ->
      [fontAwesome "wrench"]
  | "fof" ->
      [darkIcon "fof"]
  | "group" ->
      [fontAwesome "object-group"]
  | _ when String.contains ~substring:"pm-author" name ->
      [fontAwesome "user"]
  | _ when String.contains ~substring:"pm-package" name ->
      [fontAwesome "cubes"]
  | _ ->
      [darkIcon "undefined"]


let categoryButton ?(props = []) (name : string) (description : string) :
    msg Html.html =
  Html.div
    ( [ Html.class' "category-icon"
      ; Html.title description
      ; Vdom.attribute "" "role" "img"
      ; Vdom.attribute "" "alt" description ]
    @ props )
    (categoryIcon_ name)


let setTooltips (tooltip : tooltip) (entries : 'a list) : tooltip option =
  if entries = [] then Some tooltip else None


let handlerCategory
    (filter : toplevel -> bool)
    (name : string)
    (action : omniAction)
    (iconAction : msg option)
    (tooltip : tooltip)
    (hs : handler list) : category =
  let handlers = hs |> List.filter ~f:(fun h -> filter (TLHandler h)) in
  { count = List.length handlers
  ; name
  ; plusButton = Some (CreateRouteHandler action)
  ; classname = String.toLower name
  ; iconAction
  ; tooltip = setTooltips tooltip handlers
  ; entries =
      List.map handlers ~f:(fun h ->
          let tlid = h.hTLID in
          Entry
            { name =
                h.spec.name
                |> B.toOption
                |> Option.withDefault ~default:missingEventRouteDesc
            ; uses = None
            ; identifier = Tlid tlid
            ; onClick = Destination (FocusedHandler (tlid, None, true))
            ; minusButton = None
            ; killAction = Some (ToplevelDeleteForever tlid)
            ; plusButton = None
            ; verb =
                ( if TL.isHTTPHandler (TLHandler h)
                then B.toOption h.spec.modifier
                else None ) }) }


let httpCategory (handlers : handler list) : category =
  handlerCategory
    TL.isHTTPHandler
    "HTTP"
    (NewHTTPHandler None)
    (Some GoToArchitecturalView)
    Http
    handlers


let cronCategory (handlers : handler list) : category =
  handlerCategory
    TL.isCronHandler
    "Cron"
    (NewCronHandler None)
    (Some GoToArchitecturalView)
    Cron
    handlers


let replCategory (handlers : handler list) : category =
  handlerCategory
    TL.isReplHandler
    "REPL"
    (NewReplHandler None)
    None
    Repl
    handlers


let workerCategory (handlers : handler list) : category =
  handlerCategory
    (fun tl ->
      TL.isWorkerHandler tl
      || (* Show the old workers here for now *)
      TL.isDeprecatedCustomHandler tl)
    "Worker"
    (NewWorkerHandler None)
    (Some GoToArchitecturalView)
    Worker
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
        let minusButton = None in
        Entry
          { name = B.valueWithDefault "Untitled DB" db.dbName
          ; identifier = Tlid db.dbTLID
          ; uses = Some uses
          ; onClick = Destination (FocusedDB (db.dbTLID, true))
          ; minusButton
          ; killAction = Some (ToplevelDeleteForever db.dbTLID)
          ; verb = None
          ; plusButton = None })
  in
  { count = List.length dbs
  ; name = "Datastores"
  ; classname = "dbs"
  ; plusButton = Some CreateDBTable
  ; iconAction = Some GoToArchitecturalView
  ; tooltip = setTooltips Datastore entries
  ; entries }


let f404Category (m : model) : category =
  let f404s =
    (* Generate set of deleted handler specs, stringified *)
    let deletedHandlerSpecs =
      m.deletedHandlers
      |> TLIDDict.values
      |> List.map ~f:(fun h ->
             let space = B.valueWithDefault "" h.spec.space in
             let name = B.valueWithDefault "" h.spec.name in
             let modifier = B.valueWithDefault "" h.spec.modifier in
             (* Note that this concatenated string gets compared to `space ^ path ^ modifier` later.
              * h.spec.name and f404.path are the same thing, with different names. Yes this is confusing.*)
             space ^ name ^ modifier)
      |> StrSet.fromList
    in
    m.f404s
    |> List.uniqueBy ~f:(fun f -> f.space ^ f.path ^ f.modifier)
    (* Don't show 404s for deleted handlers *)
    |> List.filter ~f:(fun f ->
           not
             (StrSet.has
                ~value:(f.space ^ f.path ^ f.modifier)
                deletedHandlerSpecs))
  in
  { count = List.length f404s
  ; name = "404s"
  ; plusButton = None
  ; classname = "fof"
  ; iconAction = None
  ; tooltip = setTooltips FourOhFour f404s
  ; entries =
      List.map f404s ~f:(fun ({space; path; modifier; _} as fof) ->
          Entry
            { name = (if space = "HTTP" then path else space ^ "::" ^ path)
            ; uses = None
            ; identifier = Other (fof.space ^ fof.path ^ fof.modifier)
            ; onClick = SendMsg (CreateHandlerFrom404 fof)
            ; minusButton = Some (Delete404APICall fof)
            ; killAction = None
            ; plusButton = Some (CreateHandlerFrom404 fof)
            ; verb = (if space = "WORKER" then None else Some modifier) }) }


let userFunctionCategory (m : model) (ufs : userFunction list) : category =
  let fns = ufs |> List.filter ~f:(fun fn -> B.isF fn.ufMetadata.ufmName) in
  let entries =
    List.filterMap fns ~f:(fun fn ->
        Option.map (B.toOption fn.ufMetadata.ufmName) ~f:(fun name ->
            let tlid = fn.ufTLID in
            let usedIn = Introspect.allUsedIn tlid m in
            let minusButton = None in
            Entry
              { name
              ; identifier = Tlid tlid
              ; uses = Some (List.length usedIn)
              ; minusButton
              ; killAction = Some (DeleteUserFunctionForever tlid)
              ; onClick = Destination (FocusedFn (tlid, None))
              ; plusButton = None
              ; verb = None }))
  in
  { count = List.length fns
  ; name = "Functions"
  ; classname = "fns"
  ; plusButton = Some CreateFunction
  ; iconAction = Some GoToArchitecturalView
  ; tooltip = setTooltips Function entries
  ; entries }


let userTipeCategory (m : model) (tipes : userTipe list) : category =
  let tipes = tipes |> List.filter ~f:(fun t -> B.isF t.utName) in
  let entries =
    List.filterMap tipes ~f:(fun tipe ->
        Option.map (B.toOption tipe.utName) ~f:(fun name ->
            let minusButton =
              if Refactor.usedTipe m name
              then None
              else Some (DeleteUserType tipe.utTLID)
            in
            Entry
              { name
              ; identifier = Tlid tipe.utTLID
              ; uses = Some (Refactor.tipeUseCount m name)
              ; minusButton
              ; killAction = Some (DeleteUserTypeForever tipe.utTLID)
              ; onClick = Destination (FocusedType tipe.utTLID)
              ; plusButton = None
              ; verb = None }))
  in
  { count = List.length tipes
  ; name = "Types"
  ; classname = "types"
  ; plusButton = Some CreateType
  ; iconAction = None
  ; tooltip = None
  ; entries }


let groupCategory (groups : group list) : category =
  let groups = groups |> List.filter ~f:(fun (g : group) -> B.isF g.gName) in
  let entries =
    List.filterMap groups ~f:(fun (group : group) ->
        Option.map (B.toOption group.gName) ~f:(fun name ->
            let minusButton =
              let hasMembers = List.length group.members > 0 in
              if hasMembers then None else Some (DeleteGroup group.gTLID)
            in
            Entry
              { name
              ; identifier = Tlid group.gTLID
              ; uses = None
              ; minusButton
              ; killAction = Some (DeleteGroupForever group.gTLID)
              ; onClick = Destination (FocusedGroup (group.gTLID, true))
              ; plusButton = None
              ; verb = None }))
  in
  { count = List.length groups
  ; name = "Groups"
  ; classname = "group"
  ; plusButton = Some CreateGroup
  ; iconAction = None
  ; tooltip = None
  ; entries }


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
  let groupCategory =
    if VariantTesting.variantIsActive m GroupVariant
    then [groupCategory groups]
    else []
  in
  (* We want to hide user defined types for users who arent already using them
    since there is currently no way to use them other than as a function param.
    we should show user defined types once the user can use them more *)
  let tipes =
    if List.length tipes == 0 then [] else [userTipeCategory m tipes]
  in
  let catergories =
    [ httpCategory hs
    ; workerCategory hs
    ; cronCategory hs
    ; replCategory hs
    ; dbCategory m dbs
    ; userFunctionCategory m ufns ]
    @ tipes
  in
  catergories @ groupCategory


let packageManagerCategory (pmfns : packageFns) : category =
  let getFnnameEntries (moduleList : packageFn list) : item list =
    let fnnames =
      moduleList
      |> List.sortBy ~f:(fun f -> f.module_)
      |> List.uniqueBy ~f:(fun fn -> fn.fnname)
    in
    fnnames
    |> List.map ~f:(fun fn ->
           Entry
             { name =
                 fn.module_ ^ "::" ^ fn.fnname ^ "_v" ^ string_of_int fn.version
             ; identifier = Tlid fn.pfTLID
             ; onClick = Destination (FocusedPackageManagerFn fn.pfTLID)
             ; uses = None
             ; minusButton = None
             ; plusButton = None
             ; killAction = None
             ; verb = None })
  in
  let getPackageEntries (userList : packageFn list) : item list =
    let uniquePackages =
      userList
      |> List.sortBy ~f:(fun f -> f.package)
      |> List.uniqueBy ~f:(fun fn -> fn.package)
    in
    uniquePackages
    |> List.map ~f:(fun fn ->
           let packageList =
             userList |> List.filter ~f:(fun f -> fn.package = f.package)
           in
           Category
             { count = List.length uniquePackages
             ; name = fn.package
             ; plusButton = None
             ; iconAction = None
             ; classname = "pm-package" ^ fn.package
             ; tooltip = None
             ; entries = getFnnameEntries packageList })
  in
  let uniqueauthors =
    pmfns
    |> TD.values
    |> List.sortBy ~f:(fun f -> f.user)
    |> List.uniqueBy ~f:(fun fn -> fn.user)
  in
  let getAuthorEntries =
    uniqueauthors
    |> List.map ~f:(fun fn ->
           let authorList =
             pmfns |> TD.values |> List.filter ~f:(fun f -> fn.user = f.user)
           in
           Category
             { count = List.length uniqueauthors
             ; name = fn.user
             ; plusButton = None
             ; iconAction = None
             ; classname = "pm-author" ^ fn.user
             ; tooltip = None
             ; entries = getPackageEntries authorList })
  in
  { count = List.length uniqueauthors
  ; name = "Package Manager"
  ; plusButton = None
  ; iconAction = None
  ; classname = "package-manager"
  ; tooltip = Some PackageManager
  ; entries = getAuthorEntries }


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
             plusButton = None (* only allow new entries on the main category *)
           ; classname =
               (* dont open/close in lockstep with parent *)
               delPrefix ^ c.classname
           ; entries =
               List.map c.entries ~f:(function
                   | Entry e ->
                       let actionOpt =
                         e.identifier
                         |> tlidOfIdentifier
                         |> Option.map ~f:(fun tlid -> RestoreToplevel tlid)
                       in
                       Entry
                         { e with
                           plusButton = actionOpt
                         ; uses = None
                         ; minusButton = e.killAction
                         ; onClick =
                             actionOpt
                             |> Option.map ~f:(fun msg -> SendMsg msg)
                             |> Option.withDefault ~default:DoNothing }
                   | c ->
                       c) })
  in
  let showTooltip = List.filter ~f:(fun c -> c.entries <> []) cats in
  { count = cats |> List.map ~f:(fun c -> count (Category c)) |> List.sum
  ; name = "Deleted"
  ; plusButton = None
  ; classname = "deleted"
  ; iconAction = None
  ; tooltip = setTooltips Deleted showTooltip
  ; entries = List.map cats ~f:(fun c -> Category c) }


let viewEmptyCategory (c : category) : msg Html.html =
  let name =
    match c.classname with
    | "http" ->
        "HTTP handlers"
    | "cron" | "worker" | "repl" ->
        c.name ^ "s"
    | _ ->
        c.name
  in
  Html.div [Html.class' "simple-item empty"] [Html.text ("No " ^ name)]


let viewEntry (m : model) (e : entry) : msg Html.html =
  let name = e.name in
  let isSelected =
    tlidOfIdentifier e.identifier = CursorState.tlidOf m.cursorState
  in
  let linkItem =
    let verb =
      match e.verb with
      | Some v ->
          Html.span [Html.class' ("verb " ^ v)] [Html.text v]
      | _ ->
          Vdom.noNode
    in
    match e.onClick with
    | Destination dest ->
        let cls = "toplevel-link" ^ if isSelected then " selected" else "" in
        let path = Html.span [Html.class' "path"] [Html.text name] in
        Html.span
          [Html.class' "toplevel-name"]
          [Url.linkFor dest cls [path; verb]]
    | SendMsg msg ->
        let cls = "toplevel-msg" in
        let path = Html.span [Html.class' "path"] [Html.text name] in
        let action =
          if m.permission = Some ReadWrite
          then
            ViewUtils.eventNeither
              ~key:(name ^ "-clicked-msg")
              "click"
              (fun _ -> msg)
          else Vdom.noProp
        in
        Html.span
          [Html.class' "toplevel-name"; action]
          [Html.span [Html.class' cls] [path; verb]]
    | DoNothing ->
        Html.span [Html.class' "toplevel-name"] [Html.text name; verb]
  in
  let iconspacer = Html.div [Html.class' "icon-spacer"] [] in
  let minuslink =
    (* This prevents the delete button appearing in the hover view.
     * We'll add it back in for 404s specifically at some point *)
    if m.permission = Some Read
    then iconspacer
    else
      match e.minusButton with
      | Some msg ->
          iconButton
            ~key:(entryKeyFromIdentifier e.identifier)
            ~icon:"minus-circle"
            ~classname:"delete-button"
            msg
      | None ->
          iconspacer
  in
  let pluslink =
    match e.plusButton with
    | Some msg ->
        if m.permission = Some ReadWrite
        then
          iconButton
            ~key:(e.name ^ "-plus")
            ~icon:"plus-circle"
            ~classname:"add-button"
            msg
        else iconspacer
    | None ->
        iconspacer
  in
  Html.div [Html.class' "simple-item"] [minuslink; linkItem; pluslink]


let viewDeploy (d : staticDeploy) : msg Html.html =
  let statusString =
    match d.status with Deployed -> "Deployed" | Deploying -> "Deploying"
  in
  let copyBtn =
    Html.div
      [ Html.class' "icon-button copy-hash"
      ; ViewUtils.eventNeither "click" ~key:("hash-" ^ d.deployHash) (fun m ->
            ClipboardCopyLivevalue ("\"" ^ d.deployHash ^ "\"", m.mePos)) ]
      [fontAwesome "copy"]
  in
  Html.div
    [Html.class' "simple-item deploy"]
    [ Html.div
        [Html.class' "hash"]
        [ Html.a [Html.href d.url; Html.target "_blank"] [Html.text d.deployHash]
        ; copyBtn ]
    ; Html.div
        [ Html.classList
            [ ("status", true)
            ; ("success", match d.status with Deployed -> true | _ -> false) ]
        ]
        [Html.text statusString]
    ; Html.div
        [Html.class' "timestamp"]
        [Html.text (Js.Date.toUTCString d.lastUpdate)] ]


let categoryName (name : string) : msg Html.html =
  Html.span [Html.class' "category-name"] [Html.text name]


let categoryOpenCloseHelpers
    (s : sidebarState) (classname : string) (count : int) :
    msg Vdom.property * msg Vdom.property =
  let isOpen = StrSet.has s.openedCategories ~value:classname in
  let isDetailed = s.mode = DetailedMode in
  let isSubCat = String.contains ~substring:delPrefix classname in
  let openEventHandler =
    if isDetailed || isSubCat
    then
      ViewUtils.eventNoPropagation
        ~key:((if isOpen then "cheh-true-" else "cheh-false-") ^ classname)
        "click"
        (fun _ -> SidebarMsg (MarkCategoryOpen (not isOpen, classname)))
    else Vdom.noProp
  in
  let openAttr =
    if isOpen && count <> 0 then Vdom.attribute "" "open" "" else Vdom.noProp
  in
  (openEventHandler, openAttr)


let viewDeployStats (m : model) : msg Html.html =
  let entries = m.staticDeploys in
  let count = List.length entries in
  let openEventHandler, openAttr =
    categoryOpenCloseHelpers m.sidebarState "deploys" count
  in
  let openAttr =
    if m.sidebarState.mode = AbridgedMode
    then Vdom.attribute "" "open" ""
    else openAttr
  in
  let title = categoryName "Static Assets" in
  let summary =
    let tooltip =
      let description, details, action, tipClass =
        Tooltips.getTooltipViewInfo StaticAssets
      in
      Tooltips.viewToolTipT
        ~direction:Bottom
        ~tipClass:(Some tipClass)
        ~shouldShow:(m.tooltip = Some StaticAssets)
        description
        details
        action
    in
    let openTooltip =
      if count = 0
      then
        ViewUtils.eventNoPropagation
          ~key:"open-tooltip-deploys"
          "click"
          (fun _ -> ToolTipMsg (Open StaticAssets))
      else Vdom.noProp
    in
    let header =
      Html.div
        [Html.class' "category-header"; openTooltip]
        [categoryButton "static" "Static Assets"; title]
    in
    let deployLatest =
      if count <> 0
      then entries |> List.take ~count:1 |> List.map ~f:viewDeploy
      else []
    in
    Html.summary
      [openEventHandler; Html.class' "category-summary"]
      (tooltip :: header :: deployLatest)
  in
  let deploys =
    if List.length entries > 0
    then entries |> List.map ~f:viewDeploy
    else
      [ Html.div
          [Html.class' "simple-item empty"]
          [Html.text "No Static deploys"] ]
  in
  let content =
    Html.div
      [ Html.class' "category-content"
      ; eventNoPropagation ~key:"cat-close-deploy" "mouseleave" (fun _ ->
            SidebarMsg ResetSidebar) ]
      (title :: deploys)
  in
  let classes =
    Html.classList
      [("sidebar-category", true); ("deploys", true); ("empty", count = 0)]
  in
  Html.details ~unique:"deploys" [classes; openAttr] [summary; content]


let rec viewItem (m : model) (s : item) : msg Html.html =
  match s with
  | Category c ->
      if c.count > 0 then viewCategory m c else Vdom.noNode
  | Entry e ->
      viewEntry m e


and viewCategory (m : model) (c : category) : msg Html.html =
  let openEventHandler, openAttr =
    categoryOpenCloseHelpers m.sidebarState c.classname c.count
  in
  let openTooltip, tooltipView =
    match c.tooltip with
    | Some tt ->
        let description, details, action, tipClass = Tooltips.getTooltipViewInfo tt in
        let view =
          Tooltips.viewToolTipT
            ~direction:Bottom
            ~tipClass:(Some tipClass)
            ~shouldShow:(m.tooltip = Some tt)
            description
            details
            action
        in
        ( ViewUtils.eventNoPropagation
            ~key:("open-tooltip-" ^ c.classname)
            "click"
            (fun _ -> ToolTipMsg (Open tt))
        , view )
    | None ->
        (Vdom.noProp, Vdom.noNode)
  in
  let openAttr =
    if m.sidebarState.mode = AbridgedMode
    then Vdom.attribute "" "open" ""
    else openAttr
  in
  let isSubCat = String.contains ~substring:delPrefix c.classname in
  let title = categoryName c.name in
  let summary =
    let plusButton =
      match c.plusButton with
      | Some msg ->
          if m.permission = Some ReadWrite
          then
            iconButton
              ~key:("plus-" ^ c.classname)
              ~icon:"plus-circle"
              ~classname:"create-tl-icon"
              msg
          else Vdom.noNode
      | None ->
          Vdom.noNode
    in
    let catIcon =
      let props =
        match c.iconAction with
        | Some ev when m.sidebarState.mode = AbridgedMode && not isSubCat ->
            [eventNeither ~key:"return-to-arch" "click" (fun _ -> ev)]
        | Some _ | None ->
            [Vdom.noProp]
      in
      categoryButton c.classname c.name ~props
    in
    let header =
      Html.div [Html.class' "category-header"; openTooltip] [catIcon; title]
    in
    Html.summary
      [Html.class' "category-summary"; openEventHandler]
      [tooltipView; header; plusButton]
  in
  let content =
    let entries =
      if c.count > 0
      then List.map ~f:(viewItem m) c.entries
      else [viewEmptyCategory c]
    in
    Html.div
      [ Html.class' "category-content"
      ; eventNoPropagation
          ~key:("cat-close-" ^ c.classname)
          "mouseleave"
          (fun _ ->
            if not isSubCat
            then SidebarMsg ResetSidebar
            else IgnoreMsg "sidebar-category-close") ]
      (categoryName c.name :: entries)
  in
  let classes =
    Html.classList
      [("sidebar-category", true); (c.classname, true); ("empty", c.count = 0)]
  in
  Html.details ~unique:c.classname [classes; openAttr] [summary; content]


let viewToggleBtn (isDetailed : bool) : msg Html.html =
  let event =
    ViewUtils.eventNeither ~key:"toggle-sidebar" "click" (fun _ ->
        SidebarMsg ToggleSidebarMode)
  in
  let description =
    if isDetailed then "Collapse sidebar" else "Expand sidebar"
  in
  let icon =
    let view' iconName =
      Html.span [Html.class' "icon"] [fontAwesome iconName; fontAwesome iconName]
    in
    if isDetailed then view' "chevron-left" else view' "chevron-right"
  in
  let label = Html.span [Html.class' "label"] [Html.text description] in
  let alt = if isDetailed then Vdom.noProp else Html.title description in
  Html.div [event; Html.class' "toggle-sidebar-btn"; alt] [label; icon]


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
    | FocusedPackageManagerFn tlid ->
        Printf.sprintf "Package Manager Fn (TLID %s)" (TLID.toString tlid)
    | FocusedFn (tlid, _) ->
        Printf.sprintf "Fn (TLID %s)" (TLID.toString tlid)
    | FocusedHandler (tlid, _, _) ->
        Printf.sprintf "Handler (TLID %s)" (TLID.toString tlid)
    | FocusedDB (tlid, _) ->
        Printf.sprintf "DB (TLID %s)" (TLID.toString tlid)
    | FocusedType tlid ->
        Printf.sprintf "Type (TLID %s)" (TLID.toString tlid)
    | FocusedGroup (tlid, _) ->
        Printf.sprintf "Group (TLID %s)" (TLID.toString tlid)
    | SettingsModal tab ->
        Printf.sprintf
          "SettingsModal (tab %s)"
          (SettingsViewTypes.settingsTabToText tab)
  in
  let flagText =
    "["
    ^ (m.tests |> List.map ~f:show_variantTest |> String.join ~sep:", ")
    ^ "]"
  in
  let environment =
    Html.div
      [Html.class' ("environment " ^ environmentName)]
      [Html.text environmentName]
  in
  let stateInfo =
    Html.div
      [Html.class' "state-info"]
      [ stateInfoTohtml "env" (Html.text m.environment)
      ; stateInfoTohtml "flags" (Html.text flagText)
      ; stateInfoTohtml "page" (Html.text (pageToString m.currentPage))
      ; stateInfoTohtml
          "cursorState"
          (Html.text (show_cursorState m.cursorState)) ]
  in
  let toggleTimer =
    Html.div
      [ ViewUtils.eventNoPropagation ~key:"tt" "mouseup" (fun _ ->
            ToggleEditorSetting
              (fun es -> {es with runTimers = not es.runTimers}))
      ; Html.class' "checkbox-row" ]
      [ Html.input'
          [Html.type' "checkbox"; Html.checked m.editorSettings.runTimers]
          []
      ; Html.label [] [Html.text "Run Timers"] ]
  in
  let toggleFluidDebugger =
    Html.div
      [ ViewUtils.eventNoPropagation ~key:"tt" "mouseup" (fun _ ->
            ToggleEditorSetting
              (fun es -> {es with showFluidDebugger = not es.showFluidDebugger}))
      ; Html.class' "checkbox-row" ]
      [ Html.input'
          [ Html.type' "checkbox"
          ; Html.checked m.editorSettings.showFluidDebugger ]
          []
      ; Html.label [] [Html.text "Show Fluid Debugger"] ]
  in
  let toggleHandlerASTs =
    Html.div
      [ ViewUtils.eventNoPropagation ~key:"tgast" "mouseup" (fun _ ->
            ToggleEditorSetting
              (fun es -> {es with showHandlerASTs = not es.showHandlerASTs}))
      ; Html.class' "checkbox-row" ]
      [ Html.input'
          [Html.type' "checkbox"; Html.checked m.editorSettings.showHandlerASTs]
          []
      ; Html.label [] [Html.text "Show Handler ASTs"] ]
  in
  let debugger =
    Html.a
      [ Html.href (ViewScaffold.debuggerLinkLoc m)
      ; Html.class' "state-info-row debugger" ]
      [ Html.text
          ( if m.teaDebuggerEnabled
          then "Disable Debugger"
          else "Enable Debugger" ) ]
  in
  let variantLinks =
    if m.isAdmin
    then
      List.map VariantTesting.availableAdminVariants ~f:(fun variant ->
          let name = VariantTesting.nameOf variant in
          let enabled = VariantTesting.variantIsActive m variant in
          Html.a
            [ Html.href (ViewScaffold.flagLinkLoc name enabled)
            ; Html.class' "state-info-row" ]
            [ Html.text
                ( if enabled
                then "Disable " ^ name ^ " variant"
                else "Enable " ^ name ^ " variant" ) ])
    else []
  in
  let saveTestButton =
    Html.a
      [ ViewUtils.eventNoPropagation ~key:"stb" "mouseup" (fun _ ->
            SaveTestButton)
      ; Html.class' "state-info-row save-state" ]
      [Html.text "SAVE STATE FOR INTEGRATION TEST"]
  in
  let hoverView =
    Html.div
      [Html.class' "category-content"]
      ( [ stateInfo
        ; toggleTimer
        ; toggleFluidDebugger
        ; toggleHandlerASTs
        ; debugger ]
      @ variantLinks
      @ [saveTestButton] )
  in
  let icon =
    Html.div
      [ Html.class' "category-icon"
      ; Html.title "Admin"
      ; Vdom.attribute "" "role" "img"
      ; Vdom.attribute "" "alt" "Admin" ]
      [fontAwesome "cog"]
  in
  let sectionIcon =
    Html.div [Html.class' "category-summary"] [icon; environment]
  in
  Html.div [Html.class' "sidebar-category admin"] [sectionIcon; hoverView]


let update (msg : sidebarMsg) : modification =
  match msg with
  | ToggleSidebarMode ->
      ReplaceAllModificationsWithThisOne
        (fun m ->
          let mode =
            match m.sidebarState.mode with
            | DetailedMode ->
                AbridgedMode
            | AbridgedMode ->
                DetailedMode
          in
          ({m with sidebarState = {m.sidebarState with mode}}, Cmd.none))
  | ResetSidebar ->
      ReplaceAllModificationsWithThisOne (Viewport.enablePan true)
  | MarkCategoryOpen (shouldOpen, key) ->
      ReplaceAllModificationsWithThisOne
        (fun m ->
          let openedCategories =
            if shouldOpen
            then StrSet.add ~value:key m.sidebarState.openedCategories
            else StrSet.remove ~value:key m.sidebarState.openedCategories
          in
          ( {m with sidebarState = {m.sidebarState with openedCategories}}
          , Cmd.none ))


let viewSidebar_ (m : model) : msg Html.html =
  let packageManager = [packageManagerCategory m.functions.packageFunctions] in
  let cats =
    standardCategories m m.handlers m.dbs m.userFunctions m.userTipes m.groups
    @ [f404Category m; deletedCategory m]
    @ packageManager
  in
  let isDetailed =
    match m.sidebarState.mode with DetailedMode -> true | _ -> false
  in
  let showAdminDebugger =
    if (not isDetailed) && m.isAdmin then adminDebuggerView m else Vdom.noNode
  in
  let content =
    let categories =
      List.map ~f:(viewCategory m) cats @ [viewDeployStats m; showAdminDebugger]
    in
    Html.div
      [ Html.classList
          [ ("viewing-table", true)
          ; ("detailed", isDetailed)
          ; ("abridged", not isDetailed) ] ]
      (viewToggleBtn isDetailed :: categories)
  in
  Html.div
    [ Html.id "sidebar-left"
      (* Block opening the omnibox here by preventing canvas pan start *)
    ; nothingMouseEvent "mousedown"
    ; ViewUtils.eventNoPropagation ~key:"click-sidebar" "click" (fun _ ->
          ToolTipMsg Close)
    ; ViewUtils.eventNoPropagation ~key:"ept" "mouseover" (fun _ ->
          EnablePanning false)
    ; ViewUtils.eventNoPropagation ~key:"epf" "mouseout" (fun _ ->
          EnablePanning true) ]
    [content]


let rtCacheKey m =
  ( m.handlers
    |> TD.mapValues ~f:(fun (h : handler) -> (h.pos, TL.sortkey (TLHandler h)))
  , m.dbs |> TD.mapValues ~f:(fun (db : db) -> (db.pos, TL.sortkey (TLDB db)))
  , m.userFunctions |> TD.mapValues ~f:(fun f -> f.ufMetadata.ufmName)
  , m.userTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.f404s
  , m.sidebarState
  , m.deletedHandlers
    |> TD.mapValues ~f:(fun (h : handler) -> TL.sortkey (TLHandler h))
  , m.deletedDBs
    |> TD.mapValues ~f:(fun (db : db) -> (db.pos, TL.sortkey (TLDB db)))
  , m.deletedUserFunctions |> TD.mapValues ~f:(fun f -> f.ufMetadata.ufmName)
  , m.deletedUserTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.staticDeploys
  , m.unlockedDBs
  , m.usedDBs
  , m.usedFns
  , m.userTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.deletedUserTipes |> TD.mapValues ~f:(fun t -> t.utName)
  , m.groups
  , m.deletedGroups
    |> TD.mapValues ~f:(fun (g : group) -> TL.sortkey (TLGroup g))
  , CursorState.tlidOf m.cursorState
  , m.environment
  , m.editorSettings
  , m.permission
  , m.currentPage
  , m.functions.packageFunctions |> TD.mapValues ~f:(fun t -> t.user)
  , m.tooltip )
  |> Option.some


let viewSidebar m = ViewCache.cache1m rtCacheKey viewSidebar_ m
