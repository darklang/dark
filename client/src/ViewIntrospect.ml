open Tc
open Prelude
open Types
module TL = Toplevel
module B = Blank

let dbColsView (cols : dbColumn list) : msg Html.html =
  let colView col =
    match col with
    | F (_, nm), F (_, ty) ->
        let html =
          Html.div
            [Html.class' "col"]
            [ Html.div [Html.class' "name"] [Html.text nm]
            ; Html.div [Html.class' "type"] [Html.text ty] ]
        in
        Some html
    | _ ->
        None
  in
  Html.div [Html.class' "cols"] (List.filterMap ~f:colView cols)


let hoveringRefProps (originTLID : tlid) (originIDs : id list) ~(key : string)
    =
  [ ViewUtils.eventNoPropagation
      ~key:(key ^ "-in_" ^ showTLID originTLID)
      "mouseenter"
      (fun _ -> SetHoveringReferences (originTLID, originIDs))
  ; ViewUtils.eventNoPropagation
      ~key:(key ^ "-out_" ^ showTLID originTLID)
      "mouseleave"
      (fun _ -> SetHoveringReferences (originTLID, [])) ]


let dbView
    (originTLID : tlid)
    (originIDs : id list)
    (targetTLID : tlid)
    (name : string)
    (cols : dbColumn list) : msg Html.html =
  Html.div
    ( [ Html.class' "ref-block db"
      ; ViewUtils.nothingMouseEvent "click"
      ; ViewUtils.eventNoPropagation
          ~key:("ref-db-link" ^ showTLID targetTLID)
          "mouseup"
          (fun _ -> GoTo (FocusedDB (targetTLID, true))) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-db-hover" )
    [Html.span [Html.class' "dbtitle"] [Html.text name]; dbColsView cols]


let handlerView
    (originTLID : tlid)
    (originIDs : id list)
    (targetTLID : tlid)
    (space : string)
    (name : string)
    (modifier : string option) : msg Html.html =
  let modifier_ =
    match modifier with
    | Some "_" | None ->
        []
    | Some m ->
        [Html.div [Html.class' "spec"] [Html.text m]]
  in
  Html.div
    ( [ Html.class' "ref-block handler"
      ; ViewUtils.nothingMouseEvent "click"
      ; ViewUtils.eventNoPropagation
          ~key:("ref-handler-link" ^ showTLID targetTLID)
          "mouseup"
          (fun _ -> GoTo (FocusedHandler (targetTLID, true))) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-handler-hover" )
    ( [ Html.div [Html.class' "spec"] [Html.text space]
      ; Html.div [Html.class' "spec"] [Html.text name] ]
    @ modifier_ )


let fnView
    (originTLID : tlid)
    (originIDs : id list)
    (targetTLID : tlid)
    (name : string)
    (params : userFunctionParameter list) : msg Html.html =
  let header =
    [ Html.div [Html.class' "fnicon"] [ViewUtils.svgIconFn "#666"]
    ; Html.span [Html.class' "fnname"] [Html.text name] ]
  in
  let paramView p =
    let name =
      Html.span
        [Html.classList [("has-blanks", Blank.isBlank p.ufpName)]]
        [Html.text (Blank.valueWithDefault "no name" p.ufpName)]
    in
    let ptype =
      Html.span
        [Html.classList [("has-blanks", Blank.isBlank p.ufpTipe)]]
        [ Html.text
            ( match p.ufpTipe with
            | F (_, v) ->
                Runtime.tipe2str v
            | Blank _ ->
                "no type" ) ]
    in
    Html.div [Html.class' "fnparam"] [name; Html.text ":"; ptype]
  in
  Html.div
    ( [ Html.class' "ref-block fn"
      ; ViewUtils.nothingMouseEvent "click"
      ; ViewUtils.eventNoPropagation
          ~key:("ref-fn-link" ^ showTLID targetTLID)
          "mouseup"
          (fun _ -> GoTo (FocusedFn targetTLID)) ]
    @ hoveringRefProps originTLID originIDs ~key:"ref-fn-hover" )
    [ Html.div [Html.class' "fnheader"] header
    ; Html.div [Html.class' "fnparams"] (List.map ~f:paramView params) ]


let renderView originalTLID (tl, originalIDs) =
  match tl with
  | TLDB {dbTLID; dbName = F (_, name); cols} ->
      dbView originalTLID originalIDs dbTLID name cols
  | TLHandler
      {hTLID; spec = {space = F (_, space); name = F (_, name); modifier}} ->
      handlerView
        originalTLID
        originalIDs
        hTLID
        space
        name
        (B.toMaybe modifier)
  | TLFunc {ufTLID; ufMetadata = {ufmName = F (_, name); ufmParameters}} ->
      fnView originalTLID originalIDs ufTLID name ufmParameters
  | _ ->
      Vdom.noNode


let refersToViews (tlid : tlid) (refs : (toplevel * id list) list) :
    msg Html.html =
  let topOffset =
    List.head refs
    |> Option.andThen ~f:(fun (tl, _) ->
           let id = tl |> TL.id |> showTLID in
           Native.Ext.querySelector (".id-" ^ id) )
    |> Option.andThen ~f:(fun e -> Some (Native.Ext.offsetTop e))
    |> Option.withDefault ~default:0
  in
  Html.div
    [ Html.class' "usages"
    ; Html.styles [("top", string_of_int (topOffset - 16) ^ "px")] ]
    (List.map ~f:(renderView tlid) refs)


let usedInViews (tlid : tlid) (uses : toplevel list) : msg Html.html =
  let uses = List.map ~f:(fun use -> (use, [])) uses in
  Html.div [Html.class' "used-in"] (List.map ~f:(renderView tlid) uses)
