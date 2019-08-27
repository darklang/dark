open Tc
open Prelude
open Types

let msgLink ~(key : string) (content : msg Html.html) (handler : msg) :
    msg Html.html =
  let event = ViewUtils.eventNeither ~key "mouseup" (fun _ -> handler) in
  Html.a [event; Html.class' ""] [content]


let html (m : model) =
  if m.showTopbar = false
  then []
  else if VariantTesting.isFluidV2 m.tests
  then
    let fluid = VariantTesting.isFluidForCustomers m.tests in
    let fluidUrl =
      let qp =
        ("fluid", not fluid) :: Url.queryParams
        |> List.uniqueBy ~f:(fun (k, _) -> k)
        |> List.filter ~f:(fun (_, v) -> v)
        |> List.map ~f:(fun (k, _) -> k ^ "=1")
        |> String.join ~sep:"&"
        |> fun s -> "?" ^ s
      in
      let loc = {(Tea.Navigation.getLocation ()) with search = qp} in
      loc.protocol ^ "//" ^ loc.host ^ loc.pathname ^ loc.search ^ loc.hash
    in
    [ Html.div
        [Html.styles []; Html.classList [("topbar", true)]]
        [ Html.a
            [ Html.href fluidUrl
            ; ViewUtils.eventNoPropagation
                ~key:"toggle-fluid"
                "mouseup"
                (fun _ -> IgnoreMsg ) ]
            [ Html.text
                (* NB: right now, this "old editor" link will take you
                       * from ?fluidv2=1&fluid=1 -> ?fluidv2=1, so it won't take
                       * you to the structured editor, it'll just put the
                       * status/debug box back.  Once we remove the fluidv2
                       * feature flag - that is, ship it to customers - then
                       * it'll take you from fluid to not-fluid *)
                ( if fluid
                then "Back to the old editor"
                else "Try our new editor!" ) ]
        ; Html.text " "
        ; msgLink ~key:"hide-topbar" (Html.text "(hide)") HideTopbar ] ]
  else []
