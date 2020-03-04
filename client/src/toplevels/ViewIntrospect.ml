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
          (fun _ -> GoTo (FocusedHandler (tlid, true))) ]
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
    [ Html.div [Html.class' "fnicon"] [ViewUtils.svgIconFn "#599ab2"]
    ; Html.span [Html.class' "fnname"] [Html.text name] ]
  in
  Html.div
    ( [ Html.class' ("ref-block fn " ^ direction)
      ; ViewUtils.eventNoPropagation
          ~key:("ref-fn-link" ^ TLID.toString tlid)
          "click"
          (fun _ -> GoTo (FocusedFn tlid)) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-fn-hover" )
    [Html.div [Html.class' "fnheader"] header; fnParamsView params]


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
