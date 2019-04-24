open Tc
open Prelude
open Types

let idOfRefersTo = Introspect.idOfRefersTo

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


let handlerView
    (tlid : tlid) (space : string) (name : string) (modifier : string option) :
    msg Html.html =
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
    ( [ Html.div [Html.class' "spec"] [Html.text space]
      ; Html.div [Html.class' "spec"] [Html.text name] ]
    @ modifier_ )


let fnView (tlid : tlid) (name : string) (params : userFunctionParameter list)
    : msg Html.html =
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
    [ Html.class' "ref-block fn"
    ; ViewUtils.eventNoPropagation
        ~key:("ref-fn-link" ^ showTLID tlid)
        "click"
        (fun _ -> GoTo (FocusedFn tlid)) ]
    [ Html.div [Html.class' "fnheader"] header
    ; Html.div [Html.class' "fnparams"] (List.map ~f:paramView params) ]


let refersToViews (refs : refersTo list) : msg Html.html =
  let topOffset =
    List.head refs
    |> Option.andThen ~f:(fun r ->
           let id = idOfRefersTo r in
           Native.Ext.querySelector (".id-" ^ showID id) )
    |> Option.andThen ~f:(fun e -> Some (Native.Ext.offsetTop e))
    |> Option.withDefault ~default:0
  and renderView r =
    match r with
    | ToDB (tlid, name, cols, _) ->
        dbView tlid name cols
    | ToEvent (tlid, space, name, _) ->
        eventView tlid space name
  in
  Html.div
    [ Html.class' "usages"
    ; Html.styles [("top", string_of_int (topOffset - 16) ^ "px")] ]
    (List.map ~f:renderView refs)


let usedInViews (uses : usedIn list) : msg Html.html =
  let renderView r =
    match r with
    | InHandler (tlid, space, name, modifier) ->
        handlerView tlid space name modifier
    | InFunction (tlid, name, params) ->
        fnView tlid name params
  in
  Html.div [Html.class' "used-in"] (List.map ~f:renderView uses)
