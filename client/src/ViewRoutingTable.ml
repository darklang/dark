open Tc
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
  ; destination : page option (* where to go when clicked *)
  ; uses : int option
  ; minusButton : msg option
  ; plusButton : msg option
  ; killAction :
      msg option
      (* if this is in the deleted section, what does minus do? *)
  ; verb : string option
  ; externalLink : handlerSpec option
  (* for http handlers to link out *) }

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


let httpCategory (_m : model) (tls : toplevel list) : category =
  let handlers = tls |> List.filter ~f:TL.isHTTPHandler in
  { count = List.length handlers
  ; name = "HTTP"
  ; plusButton = Some (CreateRouteHandler (Some "HTTP"))
  ; classname = "http"
  ; entries =
      List.map handlers ~f:(fun tl ->
          let h = tl |> TL.asHandler |> deOption "httpCategory/entry" in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Option.withDefault ~default:missingEventRouteDesc
            ; uses = None
            ; tlid = h.tlid
            ; destination = Some (FocusedHandler (tl.id, true))
            ; minusButton = Some (ToplevelDelete tl.id)
            ; killAction = Some (ToplevelDeleteForever tl.id)
            ; plusButton = None
            ; externalLink = Some h.spec
            ; verb = h.spec.modifier |> Blank.toMaybe } ) }


let cronCategory (_m : model) (tls : toplevel list) : category =
  let handlers = tls |> List.filter ~f:TL.isCronHandler in
  { count = List.length handlers
  ; name = "CRON"
  ; plusButton = Some (CreateRouteHandler (Some "CRON"))
  ; classname = "cron"
  ; entries =
      List.map handlers ~f:(fun tl ->
          let h = tl |> TL.asHandler |> deOption "cronCategory/entry" in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Option.withDefault ~default:missingEventRouteDesc
            ; uses = None
            ; tlid = h.tlid
            ; destination = Some (FocusedHandler (tl.id, true))
            ; minusButton = Some (ToplevelDelete tl.id)
            ; killAction = Some (ToplevelDeleteForever tl.id)
            ; plusButton = None
            ; externalLink = None
            ; verb = None } ) }


let dbCategory (m : model) (tls : toplevel list) : category =
  let dbs =
    tls
    |> List.filter ~f:(fun tl -> TL.asDB tl <> None)
    |> List.map ~f:(fun tl -> (TL.asDB tl |> deOption "dbCategory", tl.pos))
    |> List.sortBy ~f:(fun (db, _) -> B.valueWithDefault "" db.dbName)
  in
  let entries =
    List.map dbs ~f:(fun (db, _) ->
        let uses =
          match db.dbName with
          | Partial _ | Blank _ ->
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
          ; externalLink = None
          ; verb = None
          ; plusButton = None } )
  in
  { count = List.length dbs
  ; name = "DBs"
  ; classname = "dbs"
  ; plusButton = Some CreateDBTable
  ; entries }


let undefinedCategory (_m : model) (tls : toplevel list) : category =
  let handlers = tls |> List.filter ~f:TL.isUndefinedEventSpaceHandler in
  { count = List.length handlers
  ; name = missingEventSpaceDesc
  ; plusButton = Some (CreateRouteHandler None)
  ; classname = missingEventSpaceDesc
  ; entries =
      List.map handlers ~f:(fun tl ->
          let h = tl |> TL.asHandler |> deOption "undefinedCategory/entry" in
          Entry
            { name =
                h.spec.name
                |> Blank.toMaybe
                |> Option.withDefault ~default:missingEventRouteDesc
            ; uses = None
            ; tlid = h.tlid
            ; destination = Some (FocusedHandler (tl.id, true))
            ; minusButton = Some (ToplevelDelete tl.id)
            ; killAction = Some (ToplevelDeleteForever tl.id)
            ; plusButton = None
            ; externalLink = None
            ; verb = None } ) }


let splitBySpace (tls : toplevel list) : (string * toplevel list) list =
  let spaceName_ tl =
    tl
    |> TL.asHandler
    |> Option.map ~f:(fun x -> x.spec.module_)
    |> Option.andThen ~f:B.toMaybe
    |> Option.withDefault ~default:missingEventSpaceDesc
  in
  tls
  |> List.sortBy ~f:spaceName_
  |> List.groupWhile ~f:(fun a b -> spaceName_ a = spaceName_ b)
  |> List.map ~f:(fun hs ->
         let space =
           hs |> List.head |> deOption "splitBySpace" |> spaceName_
         in
         (space, hs) )


let eventCategories (_m : model) (tls : toplevel list) : category list =
  let groups =
    tls |> List.filter ~f:TL.isCustomEventSpaceHandler |> splitBySpace
  in
  List.map groups ~f:(fun (name, handlers) ->
      { count = List.length handlers
      ; name
      ; plusButton = Some (CreateRouteHandler (Some name))
      ; classname = name
      ; entries =
          List.map handlers ~f:(fun tl ->
              let h = tl |> TL.asHandler |> deOption "eventCategories/entry" in
              Entry
                { name =
                    h.spec.name
                    |> Blank.toMaybe
                    |> Option.withDefault ~default:missingEventRouteDesc
                ; uses = None
                ; tlid = h.tlid
                ; destination = Some (FocusedHandler (tl.id, true))
                ; minusButton = Some (ToplevelDelete tl.id)
                ; killAction = Some (ToplevelDeleteForever tl.id)
                ; plusButton = None
                ; externalLink = None
                ; verb = None } ) } )


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
            ; externalLink = None
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
          ; verb = None
          ; externalLink = None } )
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
          ; verb = None
          ; externalLink = None } )
  in
  { count = List.length tipes
  ; name = "Types"
  ; classname = "types"
  ; plusButton = Some CreateType
  ; entries }


let rec count (s : item) : int =
  match s with
  | Entry _ ->
      1
  | Category c ->
      c.entries |> List.map ~f:count |> List.sum


let deletedCategory (m : model) : category =
  let tls = m.deletedToplevels |> List.sortBy ~f:(fun tl -> TL.sortkey tl) in
  let ufns =
    m.deletedUserFunctions
    |> List.sortBy ~f:(fun fn ->
           fn.ufMetadata.ufmName
           |> Blank.toMaybe
           |> Option.withDefault ~default:"" )
  in
  let tipes =
    m.deletedUserTipes
    |> List.sortBy ~f:(fun t ->
           t.utName |> Blank.toMaybe |> Option.withDefault ~default:"" )
  in
  let cats =
    [ httpCategory m tls
    ; dbCategory m tls
    ; userFunctionCategory m ufns
    ; userTipeCategory m tipes
    ; cronCategory m tls ]
    @ eventCategories m tls
    @ [undefinedCategory m tls]
  in
  let cats =
    List.map cats ~f:(fun c ->
        { c with
          plusButton = None (* only allow new entries on the main category *)
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
                      ; destination = None
                      ; externalLink = None }
                | c ->
                    c ) } )
  in
  { count = cats |> List.map ~f:(fun c -> count (Category c)) |> List.sum
  ; name = "Deleted"
  ; plusButton = None
  ; classname = "deleted"
  ; entries = List.map cats ~f:(fun c -> Category c) }


let entry2html (m : model) (e : entry) : msg Html.html =
  let ext =
    Option.map
      ~f:(fun l -> ViewCode.externalLink l m.canvasName m.userContentHost)
      e.externalLink
    |> Option.withDefault ~default:[]
  in
  let name =
    match e.uses with
    | Some count ->
        e.name ^ " (" ^ string_of_int count ^ ")"
    | _ ->
        e.name
  in
  let destinationLink page classes name =
    Url.linkFor page classes [Html.text name]
  in
  let mainlink =
    match e.destination with
    | Some dest ->
        let cl =
          if e.uses = Some 0 then "default-link unused" else "default-link"
        in
        [destinationLink dest cl name]
    | _ ->
        [Html.text name]
  in
  let verb =
    let noExt = if List.isEmpty ext then " no-ext" else "" in
    match (e.destination, e.verb) with
    | Some dest, Some v ->
        [destinationLink dest ("verb verb-link" ^ noExt) v]
    | None, Some v ->
        [Html.span [Html.class' ("verb" ^ noExt)] [Html.text v]]
    | _ ->
        []
  in
  let iconspacer = [Html.div [Html.class' "icon-spacer"] []] in
  let minuslink =
    match e.minusButton with
    | Some msg ->
        [buttonLink ~key:("entry-" ^ showTLID e.tlid) (fontAwesome "times") msg]
    | None ->
        iconspacer
  in
  let pluslink =
    match e.plusButton with
    | Some msg ->
        [buttonLink ~key:(e.name ^ "-plus") (fontAwesome "plus") msg]
    | None ->
        iconspacer
  in
  let auxViews =
    Html.div [Html.class' "aux"] (verb @ ext @ minuslink @ pluslink)
  in
  let selected = Some e.tlid = tlidOf m.cursorState in
  Html.div
    [Html.classList [("simple-route handler", true); ("selected", selected)]]
    [Html.span [Html.class' "name"] mainlink; auxViews]


let deploy2html (d : staticDeploy) : msg Html.html =
  let statusString =
    match d.status with Deployed -> "Deployed" | Deploying -> "Deploying"
  in
  Html.div
    [Html.class' "simple-route deploy"]
    [ Html.span
        [Html.class' "datetime"]
        [Html.text (Js.Date.toUTCString d.lastUpdate)]
    ; Html.a
        [Html.href d.url; Html.target "_blank"; Html.class' "hash"]
        [Html.text d.deployHash]
    ; Html.span
        [ Html.classList
            [ ("status", true)
            ; ("success", match d.status with Deployed -> true | _ -> false) ]
        ]
        [Html.text statusString] ]


(* Category Views *)

let categoryTitle (name : string) (count : int) : msg Html.html list =
  let text cl t = Html.span [Html.class' cl] [Html.text t] in
  [ text "title" name
  ; text "parens" "("
  ; text "count" (count |> string_of_int)
  ; text "parens" ")" ]


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
    let title = categoryTitle "Static Assets" count in
    let deployLatest =
      if count <> 0
      then entries |> List.take ~count:1 |> List.map ~f:deploy2html
      else []
    in
    Html.summary [Html.class' "header"; openEventHandler] (title @ deployLatest)
  in
  let routes =
    if count > 1
    then entries |> List.drop ~count:1 |> List.map ~f:deploy2html
    else []
  in
  let classes =
    Html.classList
      [("routing-section", true); ("deploys", true); ("empty", count = 0)]
  in
  (if count = 0 then Html.div else Html.details)
    [classes; openAttr]
    (header :: routes)


let rec item2html (m : model) (s : item) : msg Html.html =
  match s with Category c -> category2html m c | Entry e -> entry2html m e


and category2html (m : model) (c : category) : msg Html.html =
  let openEventHandler, openAttr =
    categoryOpenCloseHelpers m c.classname c.count
  in
  let header =
    let title = categoryTitle c.name c.count in
    let plusButton =
      match c.plusButton with
      | Some msg ->
          [ buttonLink
              ~key:("plus-" ^ c.classname)
              (fontAwesome "plus-circle")
              msg ]
      | None ->
          []
    in
    Html.summary [Html.class' "header"; openEventHandler] (title @ plusButton)
  in
  let routes = List.map ~f:(item2html m) c.entries in
  let classes =
    Html.classList
      [("routing-section", true); (c.classname, true); ("empty", c.count = 0)]
  in
  (if c.count = 0 then Html.div else Html.details)
    [classes; openAttr]
    (header :: routes)


let viewRoutingTable_ (m : model) : msg Html.html =
  let tls = m.toplevels |> List.sortBy ~f:(fun tl -> TL.sortkey tl) in
  let ufns =
    m.userFunctions
    |> List.sortBy ~f:(fun fn ->
           fn.ufMetadata.ufmName
           |> Blank.toMaybe
           |> Option.withDefault ~default:"" )
  in
  let uts =
    m.userTipes
    |> List.sortBy ~f:(fun t ->
           t.utName |> Blank.toMaybe |> Option.withDefault ~default:"" )
  in
  let cats =
    [ httpCategory m tls
    ; dbCategory m tls
    ; userFunctionCategory m ufns
    ; userTipeCategory m uts
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
      (List.map ~f:(category2html m) cats @ [deployStats2html m])
  in
  Html.div [Html.id "sidebar-left"] [html]


let rtCacheKey m =
  ( m.toplevels |> List.map ~f:(fun tl -> (tl.pos, TL.sortkey tl))
  , m.userFunctions |> List.map ~f:(fun f -> f.ufMetadata.ufmName)
  , m.f404s
  , m.deletedToplevels |> List.map ~f:(fun tl -> (tl.pos, TL.sortkey tl))
  , m.deletedUserFunctions |> List.map ~f:(fun f -> f.ufMetadata.ufmName)
  , m.routingTableOpenDetails
  , m.staticDeploys
  , m.unlockedDBs
  , m.usedDBs
  , m.usedFns
  , m.userTipes |> List.map ~f:(fun t -> t.utName)
  , m.deletedUserTipes |> List.map ~f:(fun t -> t.utName)
  , tlidOf m.cursorState )


let viewRoutingTable m = Cache.cache1 rtCacheKey viewRoutingTable_ m
