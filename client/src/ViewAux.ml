open Tc
open Prelude
open Types

let referenceViews (refs : tlReference list) : msg Html.html list =
  let renderDB id name =
    let topOffset =
      match Native.Ext.querySelector (".id-" ^ showID id) with
      | Some e ->
          Native.Ext.offsetTop e
      | None ->
          0
    in
    Html.div
      [ Html.class' "tlref db"
      ; Html.styles [("top", string_of_int topOffset ^ "px")] ]
      [Html.text name]
  and renderEvent id space name =
    let topOffset =
      match Native.Ext.querySelector (".id-" ^ showID id) with
      | Some e ->
          Native.Ext.offsetTop e
      | None ->
          0
    in
    Html.div
      [ Html.class' "tlref db"
      ; Html.styles [("top", string_of_int topOffset ^ "px")] ]
      [Html.text (space ^ " " ^ name)]
  in
  refs
  |> List.map ~f:(fun r ->
         match r with
         | ReferenceDB (_, _, name, _, id) ->
             renderDB id name
         | ReferenceHandler (_, _, space, _, name, id) ->
             renderEvent id space name )
