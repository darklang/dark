open Prelude
module TL = Toplevel
module B = BlankOr

let dbColsView (cols : dbColumn list) : msg Html.html =
  let colView col =
    match col with
    | F (_, nm), F (_, ty) ->
        let html =
          Html.div
            [Html.class' "field"]
            [ Html.div [Html.class' "name"] [Html.text nm]
            ; Html.div [Html.class' "type"] [Html.text ty] ]
        in
        Some html
    | _ ->
        None
  in
  Html.div [Html.class' "fields"] (List.filterMap ~f:colView cols)


let fnParamsView (params : userFunctionParameter list) : msg Html.html =
  let paramView p =
    let name =
      Html.span
        [ Html.classList
            [("name", true); ("has-blanks", BlankOr.isBlank p.ufpName)] ]
        [Html.text (BlankOr.valueWithDefault "no name" p.ufpName)]
    in
    let ptype =
      Html.span
        [ Html.classList
            [("type", true); ("has-blanks", BlankOr.isBlank p.ufpTipe)] ]
        [ Html.text
            ( match p.ufpTipe with
            | F (_, v) ->
                Runtime.tipe2str v
            | Blank _ ->
                "no type" ) ]
    in
    Html.div [Html.class' "field"] [name; ptype]
  in
  Html.div [Html.class' "fields"] (List.map ~f:paramView params)


let packageFnParamsView (params : packageFnParameter list) : msg Html.html =
  let paramView (p : packageFnParameter) =
    let name = Html.span [Html.classList [("name", true)]] [Html.text p.name] in
    let ptype =
      Html.span
        [Html.classList [("type", true)]]
        [Html.text (Runtime.tipe2str p.tipe)]
    in
    Html.div [Html.class' "field"] [name; ptype]
  in
  Html.div [Html.class' "fields"] (List.map ~f:paramView params)


let hoveringRefProps
    (originTLID : TLID.t) (originIDs : ID.t list) ~(key : string) =
  [ ViewUtils.eventNoPropagation
      ~key:(key ^ "-in_" ^ TLID.toString originTLID)
      "mouseenter"
      (fun _ -> SetHoveringReferences (originTLID, originIDs))
  ; ViewUtils.eventNoPropagation
      ~key:(key ^ "-out_" ^ TLID.toString originTLID)
      "mouseleave"
      (fun _ -> SetHoveringReferences (originTLID, [])) ]


let dbView
    (originTLID : TLID.t)
    (originIDs : ID.t list)
    (tlid : TLID.t)
    (name : string)
    (cols : dbColumn list)
    (direction : string) : msg Html.html =
  Html.div
    ( [ Html.class' ("ref-block db " ^ direction)
      ; ViewUtils.eventNoPropagation
          ~key:("ref-db-link" ^ TLID.toString tlid)
          "click"
          (fun _ -> GoTo (FocusedDB (tlid, true))) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-db-hover" )
    [ Html.div
        [Html.class' "dbheader"]
        [ ViewUtils.fontAwesome "database"
        ; Html.span [Html.class' "dbname"] [Html.text name] ]
    ; dbColsView cols ]


let handlerView
    (originTLID : TLID.t)
    (originIDs : ID.t list)
    (tlid : TLID.t)
    (space : string)
    (name : string)
    (modifier : string option)
    (direction : string) : msg Html.html =
  let modifier_ =
    match modifier with
    | Some "_" | None ->
        Vdom.noNode
    | Some m ->
        Html.div [Html.class' "spec"] [Html.text m]
  in
  Html.div
    ( [ Html.class' ("ref-block handler " ^ direction)
      ; ViewUtils.eventNoPropagation
          ~key:("ref-handler-link" ^ TLID.toString tlid)
          "click"
          (fun _ -> GoTo (FocusedHandler (tlid, None, true))) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-handler-hover" )
    [ Html.div [Html.class' "spec space"] [Html.text space]
    ; Html.div [Html.class' "spec"] [Html.text name]
    ; modifier_ ]


let fnView
    (originTLID : TLID.t)
    (originIDs : ID.t list)
    (tlid : TLID.t)
    (name : string)
    (params : userFunctionParameter list)
    (direction : string) : msg Html.html =
  let header =
    [ViewUtils.darkIcon "fn"; Html.span [Html.class' "fnname"] [Html.text name]]
  in
  Html.div
    ( [ Html.class' ("ref-block fn " ^ direction)
      ; ViewUtils.eventNoPropagation
          ~key:("ref-fn-link" ^ TLID.toString tlid)
          "click"
          (fun _ -> GoTo (FocusedFn (tlid, None))) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-fn-hover" )
    [Html.div [Html.class' "fnheader"] header; fnParamsView params]


let packageFnView
    (originTLID : TLID.t)
    (originIDs : ID.t list)
    (tlid : TLID.t)
    (name : string)
    (params : packageFnParameter list)
    (direction : string) : msg Html.html =
  (* Spec is here: https://www.notion.so/darklang/PM-Function-References-793d95469dfd40d5b01c2271cb8f4a0f *)
  let header =
    [ ViewUtils.fontAwesome "box-open"
    ; Html.span [Html.class' "fnname"] [Html.text name] ]
  in
  Html.div
    (* TODO(JULIAN): Make the icon color correct based on if you can edit it! *)
    ( [ Html.class' ("ref-block fn " ^ direction)
      ; ViewUtils.eventNoPropagation
          ~key:("ref-fn-link" ^ TLID.toString tlid)
          "click"
          (fun _ -> GoTo (FocusedPackageManagerFn tlid)) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-fn-hover" )
    [Html.div [Html.class' "fnheader"] header; packageFnParamsView params]


let tipeView
    (originTLID : TLID.t)
    (originIDs : ID.t list)
    (tlid : TLID.t)
    (name : string)
    (_version : int)
    (direction : string) : msg Html.html =
  let header =
    [ ViewUtils.darkIcon "type"
    ; Html.span [Html.class' "tipename"] [Html.text name] ]
  in
  Html.div
    ( [ Html.class' ("ref-block tipe " ^ direction)
      ; ViewUtils.eventNoPropagation
          ~key:("ref-tipe-link" ^ TLID.toString tlid)
          "click"
          (fun _ -> GoTo (FocusedType tlid)) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-tipe-hover" )
    [Html.div [Html.class' "tipeheader"] header]


let renderView originalTLID direction (tl, originalIDs) =
  match tl with
  | TLDB {dbTLID; dbName = F (_, name); cols; _} ->
      dbView originalTLID originalIDs dbTLID name cols direction
  | TLHandler
      {hTLID; spec = {space = F (_, space); name = F (_, name); modifier}; _} ->
      handlerView
        originalTLID
        originalIDs
        hTLID
        space
        name
        (B.toOption modifier)
        direction
  | TLFunc
      {ufTLID; ufMetadata = {ufmName = F (_, name); ufmParameters; _}; ufAST = _}
    ->
      fnView originalTLID originalIDs ufTLID name ufmParameters direction
  | TLPmFunc pFn ->
      let name = pFn |> PackageManager.extendedName in
      packageFnView
        originalTLID
        originalIDs
        pFn.pfTLID
        name
        pFn.parameters
        direction
  | TLTipe {utTLID; utName = F (_, name); utVersion; utDefinition = _} ->
      tipeView originalTLID originalIDs utTLID name utVersion direction
  | _ ->
      Vdom.noNode


let allUsagesView
    (tlid : TLID.t) (uses : toplevel list) (refs : (toplevel * ID.t list) list)
    : msg Html.html list =
  let refersTo = List.map ~f:(renderView tlid "refers-to") refs in
  let usedIn =
    List.map ~f:(fun use -> (renderView tlid "used-in") (use, [])) uses
  in
  usedIn @ refersTo
