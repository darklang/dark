(* NOTE: This will change in the future to pretty tool tips, this is just an inbetween state *)
open Prelude

let update (currentTooltip : tooltip option) (msg : toolTipMsg) : modification =
  let tooltip =
    match msg with
    | Open _ when not (Option.isSome currentTooltip) ->
        currentTooltip
    | Open _ | Close ->
        None
    | OpenLink url ->
        Native.Window.openUrl url "_blank" ;
        None
  in
  Many
    [ ReplaceAllModificationsWithThisOne
        (fun m -> ({m with tooltip}, Tea.Cmd.none)) ]


let getTooltipViewInfo (tooltip : tooltip) :
    string * string option * (string * msg) option * string =
  match tooltip with
  | Http ->
      ( "Click the plus sign to create a REST API endpoint."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg
              (OpenLink "https://darklang.github.io/docs/first-api-endpoint") )
      , "align-left" )
  | Worker ->
      ( "Click the plus sign to create a worker to process asynchronous tasks."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg (OpenLink "https://darklang.github.io/docs/first-worker")
          )
      , "align-left" )
  | Cron ->
      ( "Click the plus sign to create a scheduled job."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg (OpenLink "https://darklang.github.io/docs/first-cron")
          )
      , "align-left" )
  | Repl ->
      ( "Click the plus sign to create a general purpose coding block."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg (OpenLink "https://darklang.github.io/docs/first-repl")
          )
      , "align-left" )
  | Datastore ->
      ( "Click to create a key-value store."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg
              (OpenLink "https://darklang.github.io/docs/first-datastore") )
      , "align-left" )
  | Function ->
      ( "Click to create a reusable block of code."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg
              (OpenLink "https://darklang.github.io/docs/first-function") )
      , "align-left" )
  | FourOhFour ->
      ( "Attempts to hit endpoints that do not yet have handlers appear here."
      , Some
          "If you're looking for a 404 but not seeing it in this list, check the 'Deleted' section of the sidebar."
      , Some
          ( "Learn More"
          , ToolTipMsg
              (OpenLink
                 "https://darklang.github.io/docs/trace-driven-development") )
      , "align-left" )
  | Deleted ->
      ("Deleted handlers appear here.", None, None, "align-left")
  | PackageManager ->
      ( "A list of built-in Dark functions. Click on the name of the function to preview it."
      , Some
          "To use the function in your canvas, start typing its name in your handler and select it from autocomplete."
      , None
      , "align-left" )
  | StaticAssets ->
      ( "Learn more about hosting static assets here."
      , None
      , Some
          ( "Learn More"
          , ToolTipMsg
              (OpenLink "https://darklang.github.io/docs/static-assets") )
      , "align-left" )


let viewToolTip
    ~(direction : toolTipDirection)
    (currentTooltip : tooltip option)
    (tooltip : tooltip) : msg Html.html =
  let showTooltip =
    match currentTooltip with Some tt when tt == tooltip -> true | _ -> false
  in
  if showTooltip
  then
    let description, details, action, tipClass = getTooltipViewInfo tooltip in
    let viewDesc =
      Html.h1 [Html.class' "description"] [Html.text description]
    in
    let viewDetail =
      match details with
      | Some txt ->
          Html.p [Html.class' "details"] [Html.text txt]
      | None ->
          Vdom.noNode
    in
    let viewBtn =
      match action with
      | Some (text, action) ->
          Html.button
            [ ViewUtils.eventNoPropagation
                ~key:"close-settings-modal"
                "click"
                (fun _ -> action) ]
            [Html.p [] [Html.text text]]
      | None ->
          Vdom.noNode
    in
    let directionToClass =
      match direction with
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
          ; Html.div [Html.class' ("tip " ^ tipClass)] [] ] ]
  else Vdom.noNode
