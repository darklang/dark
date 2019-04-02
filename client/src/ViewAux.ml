open Tc
open Prelude
open Types

let referenceViews (refs : tlReference list) : msg Html.html list =
  let renderDB tlid id name =
    let topOffset =
      match Native.Ext.querySelector (".id-" ^ showID id) with
      | Some e ->
          Native.Ext.offsetTop e
      | None ->
          0
    in
    Html.div
      [ Html.class' "tlref db"
      ; Html.styles [("top", string_of_int topOffset ^ "px")]
      ; ViewUtils.eventNoPropagation
          ~key:("ref-db-link" ^ showTLID tlid)
          "click"
          (fun _ -> GoTo (FocusedDB tlid)) ]
      [Html.span [Html.class' "dbtitle"] [Html.text name]]
  and renderEvent tlid id space name =
    let topOffset =
      match Native.Ext.querySelector (".id-" ^ showID id) with
      | Some e ->
          Native.Ext.offsetTop e
      | None ->
          0
    in
    Html.div
      [ Html.class' "tlref emit"
      ; Html.styles [("top", string_of_int topOffset ^ "px")]
      ; ViewUtils.eventNoPropagation
          ~key:("ref-emit-link" ^ showTLID tlid)
          "click"
          (fun _ -> GoTo (FocusedHandler tlid)) ]
      [ Html.div [Html.class' "spec"] [Html.text space]
      ; Html.div [Html.class' "spec"] [Html.text name] ]
  in
  refs
  |> List.map ~f:(fun r ->
         match r with
         | ReferenceDB (_, tlid, name, _, id) ->
             renderDB tlid id name
         | ReferenceHandler (_, tlid, space, _, name, id) ->
             renderEvent tlid id space name )
