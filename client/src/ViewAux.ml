open Tc
open Prelude
open Types

let dbColsView (cols : dBColumn list) : msg Html.html =
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


let dbView (tlid : tlid) (name : string) (cols : dBColumn list) :
    msg Html.html =
  Html.div
    [ Html.class' "ref-block db"
    ; ViewUtils.eventNoPropagation
        ~key:("ref-db-link" ^ showTLID tlid)
        "click"
        (fun _ -> GoTo (FocusedDB tlid)) ]
    [Html.span [Html.class' "dbtitle"] [Html.text name]; dbColsView cols]


let eventView (tlid : tlid) (space : string) (name : string) : msg Html.html =
  Html.div
    [ Html.class' "ref-block emit"
    ; ViewUtils.eventNoPropagation
        ~key:("ref-emit-link" ^ showTLID tlid)
        "click"
        (fun _ -> GoTo (FocusedHandler tlid)) ]
    [ Html.div [Html.class' "spec"] [Html.text space]
    ; Html.div [Html.class' "spec"] [Html.text name] ]


let idOfReference (r : tlReference) : id =
  match r with
  | OutReferenceDB (_, _, _, id) ->
      id
  | OutReferenceHandler (_, _, _, _, id) ->
      id


let referenceViews (refs : tlReference list) : msg Html.html =
  let topOffset =
    match List.head refs with
    | Some r ->
        let id = idOfReference r in
        let el = Native.Ext.querySelector (".id-" ^ showID id) in
        (match el with Some e -> Native.Ext.offsetTop e | None -> 0)
    | None ->
        0
  and renderView r =
    match r with
    | OutReferenceDB (tlid, name, cols, _) ->
        dbView tlid name cols
    | OutReferenceHandler (tlid, space, _, name, _) ->
        eventView tlid space name
  in
  Html.div
    [ Html.class' "references"
    ; Html.styles [("top", string_of_int (topOffset - 16) ^ "px")] ]
    (List.map ~f:renderView refs)
