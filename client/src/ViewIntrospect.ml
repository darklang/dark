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


let dbView (tlid : tlid) (name : string) (cols : dBColumn list) : msg Html.html
    =
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

let handlerView (tlid : tlid) (space : string) (name : string) (modifier : string option) : msg Html.html =
  let modifier_ =
    match modifier with
    | Some m ->
      [Html.div [Html.class' "spec"] [Html.text m]]
    | None ->
      []
  in
  Html.div
    [ Html.class' "ref-block handler"
    ; ViewUtils.eventNoPropagation
        ~key:("ref-handler-link" ^ showTLID tlid)
        "click"
        (fun _ -> GoTo (FocusedHandler tlid)) ]
    ([ Html.div [Html.class' "spec"] [Html.text space]
    ; Html.div [Html.class' "spec"] [Html.text name]
    ] @ modifier_)

let fnView (tlid : tlid) (name : string) : msg Html.html =
  Html.div
    [ Html.class' "ref-block fn"
    ; ViewUtils.eventNoPropagation
        ~key:("ref-fn-link" ^ showTLID tlid)
        "click"
        (fun _ -> GoTo (FocusedFn tlid)) ]
    [Html.span [Html.class' "fnname"] [Html.text name]]


let referenceViews (refs : tlReference list) : msg Html.html =
  let topOffset =
    List.head refs
    |> Option.andThen ~f:(fun r -> Introspect.idOfReference r)
    |> Option.andThen ~f:(fun id -> Native.Ext.querySelector (".id-" ^ showID id) )
    |> Option.andThen ~f:(fun e -> Some (Native.Ext.offsetTop e))
    |> Option.withDefault ~default:0
  and renderView r =
    match r with
    | OutReferenceDB (tlid, name, cols, _) ->
        dbView tlid name cols
    | OutReferenceEvent (tlid, space, name, _) ->
        eventView tlid space name
    | InReferenceHandler (tlid, space, name, modifier) ->
        handlerView tlid space name modifier
    | InReferenceFunction (tlid, name, _) ->
        fnView tlid name

  in
  Html.div
    [ Html.class' "references"
    ; Html.styles [("top", string_of_int (topOffset - 16) ^ "px")] ]
    (List.map ~f:renderView refs)
