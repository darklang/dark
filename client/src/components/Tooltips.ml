(* NOTE: This will change in the future to pretty tool tips, this is just an inbetween state *)
open Prelude

type tooltipSource =
  | Http
  | Worker
  | Cron
  | Repl
  | Datastore
  | Function
  | FourOhFour
  | Deleted
  | PackageManager
  | StaticAssets

and toolTipDirection =
  | Left
  | Right
  | Top
  | Bottom

and tooltipContent =
  { title : string
  ; details : string option
  ; action : (string * msg) option
  ; align : toolTipDirection
  ; tipAlignment : string }

let update (tooltipState : tooltipState) (msg : toolTipMsg) : modification =
  let tooltipState =
    let currentTooltip = tooltipState.tooltip in
    match msg with
    | OpenTooltip tt
      when (not (Option.isSome currentTooltip)) || Some tt <> currentTooltip ->
        {tooltip = Some tt}
    | OpenTooltip _ | Close ->
        {tooltip = None}
    | OpenLink url ->
        Native.Window.openUrl url "_blank" ;
        {tooltip = None}
  in
  Many
    [ ReplaceAllModificationsWithThisOne
        (fun m -> ({m with tooltipState}, Tea.Cmd.none)) ]


let tooltipToTooltipSource (t : tooltip) : tooltipSource =
  match t with
  | Http ->
      Http
  | Worker ->
      Worker
  | Cron ->
      Cron
  | Repl ->
      Repl
  | Datastore ->
      Datastore
  | Function ->
      Function
  | FourOhFour ->
      FourOhFour
  | Deleted ->
      Deleted
  | PackageManager ->
      PackageManager
  | StaticAssets ->
      StaticAssets


let generateContent (t : tooltipSource) : tooltipContent =
  match t with
  | Http ->
      { title = "Click the plus sign to create a REST API endpoint."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg
                (OpenLink "https://darklang.github.io/docs/first-api-endpoint")
            )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | Worker ->
      { title =
          "Click the plus sign to create a worker to process asynchronous tasks."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg
                (OpenLink "https://darklang.github.io/docs/first-worker") )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | Cron ->
      { title = "Click the plus sign to create a scheduled job."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg (OpenLink "https://darklang.github.io/docs/first-cron")
            )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | Repl ->
      { title = "Click the plus sign to create a general purpose coding block."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg (OpenLink "https://darklang.github.io/docs/first-repl")
            )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | Datastore ->
      { title = "Click to create a key-value store."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg
                (OpenLink "https://darklang.github.io/docs/first-datastore") )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | Function ->
      { title = "Click to create a reusable block of code."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg
                (OpenLink "https://darklang.github.io/docs/first-function") )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | FourOhFour ->
      { title =
          "Attempts to hit endpoints that do not yet have handlers appear here."
      ; details =
          Some
            "If you're looking for a 404 but not seeing it in this list, check the 'Deleted' section of the sidebar."
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg
                (OpenLink
                   "https://darklang.github.io/docs/trace-driven-development")
            )
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | Deleted ->
      { title = "Deleted handlers appear here."
      ; details = None
      ; action = None
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | PackageManager ->
      { title =
          "A list of built-in Dark functions. Click on the name of the function to preview it."
      ; details =
          Some
            "To use the function in your canvas, start typing its name in your handler and select it from autocomplete."
      ; action = None
      ; align = Bottom
      ; tipAlignment = "align-left" }
  | StaticAssets ->
      { title = "Learn more about hosting static assets here."
      ; details = None
      ; action =
          Some
            ( "Learn More"
            , ToolTipMsg
                (OpenLink "https://darklang.github.io/docs/static-assets") )
      ; align = Bottom
      ; tipAlignment = "align-left" }


let viewToolTip ~(shouldShow : bool) (t : tooltipContent) : msg Html.html =
  if shouldShow
  then
    let viewDesc = Html.h1 [Html.class' "description"] [Html.text t.title] in
    let viewDetail =
      match t.details with
      | Some txt ->
          Html.p [Html.class' "details"] [Html.text txt]
      | None ->
          Vdom.noNode
    in
    let viewBtn =
      match t.action with
      | Some (text, action) ->
          Html.button
            [ ViewUtils.eventNoPropagation
                ~key:("close-settings-" ^ text)
                "click"
                (fun _ -> action) ]
            [Html.p [] [Html.text text]]
      | None ->
          Vdom.noNode
    in
    let directionToClass =
      match t.align with
      | Top ->
          "above"
      | Bottom ->
          "below"
      | Left ->
          "left-of"
      | Right ->
          "right-of"
    in
    Html.div
      [Html.class' "tooltipWrapper"]
      [ Html.div
          [Html.class' ("tooltips " ^ directionToClass)]
          [ Html.div [Html.class' "content"] [viewDesc; viewDetail; viewBtn]
          ; Html.div [Html.class' ("tip " ^ t.tipAlignment)] [] ] ]
  else Vdom.noNode
