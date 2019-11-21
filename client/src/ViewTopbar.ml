open Tc
open Prelude
open Types

let msgLink ~(key : string) (content : msg Html.html) (handler : msg) :
    msg Html.html =
  let event = ViewUtils.eventNeither ~key "mouseup" (fun _ -> handler) in
  Html.a [event; Html.class' ""] [content]


let html (m : model) =
  if m.showTopbar && VariantTesting.isFluid m.tests
  then
    let nonFluidUrl =
      let qp =
        Url.queryParams
        |> List.uniqueBy ~f:(fun (k, _) -> k)
        |> List.filter ~f:(fun (_, v) -> v)
        |> List.map ~f:(fun (k, _) -> k ^ "=1")
        |> List.cons "fluid=0"
        |> String.join ~sep:"&"
        |> fun s -> "?" ^ s
      in
      let loc = {(Tea.Navigation.getLocation ()) with search = qp} in
      loc.protocol ^ "//" ^ loc.host ^ loc.pathname ^ loc.search ^ loc.hash
    in
    [ Html.div
        [Html.styles []; Html.classList [("topbar", true)]]
        [ Html.text
            "This is our new \"Fluid\" editor. Typing code should mostly feel just like text. "
        ; Html.br []
        ; Html.a
            [ Html.href nonFluidUrl
            ; ViewUtils.eventNoPropagation
                ~key:"toggle-fluid"
                "mouseup"
                (fun _ -> IgnoreMsg ) ]
            [Html.text "(go to the old editor)"]
        ; Html.text " "
        ; Html.a
            [ Html.href
                "https://darkcommunity.slack.com/archives/CQWEKP85V/p1574372251002600"
            ; ViewUtils.eventNoPropagation
                ~key:"toggle-fluid"
                "mouseup"
                (fun _ -> IgnoreMsg ) ]
            [Html.text "(send us feedback!)"]
        ; Html.br []
        ; msgLink ~key:"hide-topbar" (Html.text "(hide)") HideTopbar ] ]
  else []
