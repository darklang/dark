open Prelude
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

let onEvent = ViewUtils.onEvent

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let viewText = ViewBlankOr.viewText

let enterable = ViewBlankOr.Enterable

let idConfigs = ViewBlankOr.idConfigs

let wc = ViewBlankOr.wc

let moveParams (fn : userFunction) (oldPos : int) (newPos : int) : userFunction
    =
  let ufmParameters =
    fn.ufMetadata.ufmParameters |> List.reorder ~oldPos ~newPos
  in
  {fn with ufMetadata = {fn.ufMetadata with ufmParameters}}


let update (m : model) (msg : fnpMsg) : modification =
  let currentUserFn, mods =
    match msg with
    | ParamDragStart index ->
        ({m.currentUserFn with draggingParamIndex = Some index}, [])
    | ParamDragDone ->
        ({m.currentUserFn with draggingParamIndex = None}, [])
    | ParamEntersSpace index ->
        ({m.currentUserFn with dragOverSpaceIndex = Some index}, [])
    | ParamLeavesSpace ->
        ({m.currentUserFn with dragOverSpaceIndex = None}, [])
    | Reset ->
        (Defaults.defaultFnSpace, [])
    | ParamDropIntoSpace newPos ->
        Page.tlidOf m.currentPage
        |> Option.andThen ~f:(fun tlid -> TLIDDict.get ~tlid m.userFunctions)
        |> Option.pair m.currentUserFn.draggingParamIndex
        |> Option.map ~f:(fun (oldPos, fn) ->
               let newFn = moveParams fn oldPos newPos in
               let updateArgs =
                 match fn.ufMetadata.ufmName with
                 | F (_, name) ->
                     Refactor.reorderFnCallArgs m fn.ufTLID name oldPos newPos
                 | Blank _ ->
                     []
               in
               let justMovedParam =
                 List.getAt ~index:newPos newFn.ufMetadata.ufmParameters
                 |> Option.map ~f:(fun p -> B.toID p.ufpName)
               in
               let fnM =
                 { justMovedParam
                 ; draggingParamIndex = None
                 ; dragOverSpaceIndex = None }
               in
               (fnM, AddOps ([SetFunction newFn], FocusNoChange) :: updateArgs))
        |> Option.withDefault ~default:(m.currentUserFn, [])
  in
  if List.isEmpty mods
  then TweakModel (fun m -> {m with currentUserFn})
  else Many (mods @ [TweakModel (fun m -> {m with currentUserFn})])


let viewKillParameterBtn (uf : userFunction) (p : userFunctionParameter) :
    msg Html.html =
  let freeVariables =
    uf.ufAST |> AST.freeVariables |> List.map ~f:Tuple2.second
  in
  let canDeleteParameter pname =
    List.member ~value:pname freeVariables |> not
  in
  let buttonContent allowed =
    if allowed
    then
      Html.div
        [ Html.class' "parameter-btn allowed"
        ; ViewUtils.eventNoPropagation
            ~key:
              ( "dufp-"
              ^ showTLID uf.ufTLID
              ^ "-"
              ^ (p.ufpName |> B.toID |> showID) )
            "click"
            (fun _ -> DeleteUserFunctionParameter (uf.ufTLID, p)) ]
        [fontAwesome "times-circle"]
    else
      Html.div
        [ Html.class' "parameter-btn disallowed"
        ; Html.title
            "Can't delete parameter because it is used in the function body" ]
        [fontAwesome "times-circle"]
  in
  match p.ufpName with
  | F (_, pname) ->
      buttonContent (canDeleteParameter pname)
  | _ ->
      buttonContent true


let viewParamName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  viewText ParamName vs ((enterable :: idConfigs) @ c) v


let viewParamTipe (vs : viewState) (c : htmlConfig list) (v : tipe blankOr) :
    msg Html.html =
  ViewBlankOr.viewTipe ParamTipe vs ((enterable :: idConfigs) @ c) v


let jsDragStart : Web.Node.event -> unit =
  [%raw
    "function(e){ e.dataTransfer.setData('text/plain', e.target.innerHTML); e.dataTransfer.effectAllowed = 'move'; }"]


let jsDragOver : Web.Node.event -> unit =
  [%raw "function(e){e.dataTransfer.dropEffect = 'move';}"]


let viewParamSpace (index : int) (fs : fnProps) : msg Html.html =
  let dragOver e =
    jsDragOver e ;
    IgnoreMsg
  in
  let dragEnter _ = FnParamMsg (ParamEntersSpace index) in
  let dragLeave _ = FnParamMsg ParamLeavesSpace in
  let drop e =
    e##stopPropagation () ;
    FnParamMsg (ParamDropIntoSpace index)
  in
  let keyId = string_of_int index in
  let overClass =
    match (fs.draggingParamIndex, fs.dragOverSpaceIndex) with
    | Some draggingIndex, Some spaceIndex when spaceIndex = index ->
        if draggingIndex != spaceIndex && draggingIndex + 1 != spaceIndex
        then " over"
        else ""
    | _ ->
        ""
  in
  Html.div
    [ Html.class' ("col space" ^ overClass)
    ; Vdom.attribute "" "data-pos" (string_of_int index)
    ; onEvent ~event:"dragover" ~key:("fpsdo-" ^ keyId) dragOver
    ; onEvent ~event:"dragenter" ~key:("fpsde-" ^ keyId) dragEnter
    ; onEvent ~event:"dragleave" ~key:("fpsdl-" ^ keyId) dragLeave
    ; onEvent ~event:"drop" ~key:("fpsdrop-" ^ keyId) drop ]
    []


let viewParam
    (fn : userFunction)
    (vs : viewState)
    (index : int)
    (p : userFunctionParameter) : msg Html.html list =
  let nameId = p.ufpName |> B.toID in
  let strId = showID nameId in
  let dragStart evt =
    jsDragStart evt ;
    FnParamMsg (ParamDragStart index)
  in
  let dragEnd _ = FnParamMsg ParamDragDone in
  let flashFade str =
    if str = "blinkGlow" then FnParamMsg Reset else IgnoreMsg
  in
  let conditionalClasses =
    [ ( "dragging"
      , vs.fnProps.draggingParamIndex |> Option.isSomeEqualTo ~value:index )
    ; ( "just-moved"
      , vs.fnProps.justMovedParam |> Option.isSomeEqualTo ~value:nameId ) ]
  in
  let param =
    Html.div
      [ Html.classList (("col param", true) :: conditionalClasses)
      ; Tea.Html2.Attributes.draggable "true"
      ; Vdom.attribute "" "data-pos" (string_of_int index)
      ; onEvent
          ~event:"dragstart"
          ~key:("fpds-" ^ strId)
          ~preventDefault:false
          dragStart
      ; onEvent ~event:"dragend" ~key:("fpde-" ^ strId) dragEnd
      ; ViewUtils.onAnimationEnd ~key:("fpdfaded-" ^ strId) ~listener:flashFade
      ]
      [ ( if vs.permission = Some ReadWrite
        then viewKillParameterBtn fn p
        else Vdom.noNode )
      ; viewParamName vs [wc "name"] p.ufpName
      ; viewParamTipe vs [wc "type"] p.ufpTipe
      ; fontAwesome "grip-lines" ]
  in
  let space = viewParamSpace index vs.fnProps in
  [space; param]


let view (fn : userFunction) (vs : viewState) : msg Html.html list =
  let params =
    fn.ufMetadata.ufmParameters
    |> List.indexedMap ~f:(viewParam fn vs)
    |> List.flatten
  in
  let lastSpace =
    viewParamSpace (List.length fn.ufMetadata.ufmParameters) vs.fnProps
  in
  params @ [lastSpace]
