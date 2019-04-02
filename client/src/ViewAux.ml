open Tc
open Prelude
open Types

let rDBColView (cols : dBColumn list) : msg Html.html =
  let colView col =
    match col with
    | F (_, nm), F (_, ty) ->
        Some
          (Html.div
             [Html.class' "col"]
             [ Html.div [Html.class' "name"] [Html.text nm]
             ; Html.div [Html.class' "type"] [Html.text ty] ])
    | _ ->
        None
  in
  Html.div [Html.class' "cols"] (List.filterMap ~f:colView cols)


let referenceViews (refs : tlReference list) : msg Html.html list =
  let linkToDB tlid =
    ViewUtils.eventNoPropagation
      ~key:("ref-db-link" ^ showTLID tlid)
      "click"
      (fun _ -> GoTo (FocusedDB tlid))
  and linkToEmit tlid =
    ViewUtils.eventNoPropagation
      ~key:("ref-emit-link" ^ showTLID tlid)
      "click"
      (fun _ -> GoTo (FocusedHandler tlid))
  and calcTopOffset id =
    match Native.Ext.querySelector (".id-" ^ showID id) with
    | Some e ->
        Native.Ext.offsetTop e
    | None ->
        0
  in
  let renderDB tlid id name cols =
    Html.div
      [ Html.class' "tlref db"
      ; Html.styles [("top", string_of_int (calcTopOffset id) ^ "px")]
      ; linkToDB tlid ]
      [Html.span [Html.class' "dbtitle"] [Html.text name]; rDBColView cols]
  and renderEvent tlid id space name =
    Html.div
      [ Html.class' "tlref emit"
      ; Html.styles [("top", string_of_int (calcTopOffset id) ^ "px")]
      ; linkToEmit tlid ]
      [ Html.div [Html.class' "spec"] [Html.text space]
      ; Html.div [Html.class' "spec"] [Html.text name] ]
  in
  refs
  |> List.map ~f:(fun r ->
         match r with
         | OutReferenceDB (tlid, name, cols, id) ->
             renderDB tlid id name cols
         | OutReferenceHandler (tlid, space, _, name, id) ->
             renderEvent tlid id space name )
