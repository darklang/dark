open Prelude
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

let onEvent = ViewUtils.onEvent

type viewState = ViewUtils.viewState

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
  then
    ReplaceAllModificationsWithThisOne
      (fun m -> ({m with currentUserFn}, Tea.Cmd.none))
  else
    Many
      ( mods
      @ [ ReplaceAllModificationsWithThisOne
            (fun m -> ({m with currentUserFn}, Tea.Cmd.none)) ] )


let viewKillParameterBtn (uf : userFunction) (p : userFunctionParameter) :
    msg Html.html =
  let freeVariables =
    uf.ufAST
    |> FluidAST.toExpr
    |> AST.freeVariables
    |> List.map ~f:Tuple2.second
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
              ^ TLID.toString uf.ufTLID
              ^ "-"
              ^ (p.ufpName |> B.toID |> ID.toString) )
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


let viewParamName ~(classes : string list) (vs : viewState) (v : string blankOr)
    : msg Html.html =
  ViewBlankOr.viewText ~enterable:true ~classes ParamName vs v


let viewParamTipe ~(classes : string list) (vs : viewState) (v : tipe blankOr) :
    msg Html.html =
  ViewBlankOr.viewTipe ~classes ~enterable:true ParamTipe vs v


let jsDragStart : Web.Node.event -> unit =
  [%raw
    "function(e){ e.dataTransfer.setData('text/plain', e.target.innerHTML); e.dataTransfer.effectAllowed = 'move'; }"]


let jsDragOver : Web.Node.event -> unit =
  [%raw "function(e){e.dataTransfer.dropEffect = 'move';}"]


let viewParamSpace (index : int) (fs : fnProps) : msg Html.html =
  let dragOver e =
    jsDragOver e ;
    IgnoreMsg "view-param-space"
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
    (fn : functionTypes)
    (vs : viewState)
    (index : int)
    (p : userFunctionParameter) : msg Html.html list =
  let nameId = p.ufpName |> B.toID in
  let strId = ID.toString nameId in
  let dragStart evt =
    jsDragStart evt ;
    FnParamMsg (ParamDragStart index)
  in
  let dragEnd _ = FnParamMsg ParamDragDone in
  let flashFade str =
    if str = "blinkGlow"
    then FnParamMsg Reset
    else IgnoreMsg "viewparam-flash-fade"
  in
  let conditionalClasses =
    [ ( "dragging"
      , vs.fnProps.draggingParamIndex |> Option.isSomeEqualTo ~value:index )
    ; ( "just-moved"
      , vs.fnProps.justMovedParam |> Option.isSomeEqualTo ~value:nameId ) ]
  in
  let param =
    let events =
      match fn with
      | UserFunction _ ->
          [ Tea.Html2.Attributes.draggable "true"
          ; onEvent
              ~event:"dragstart"
              ~key:("fpds-" ^ strId)
              ~preventDefault:false
              dragStart
          ; onEvent ~event:"dragend" ~key:("fpde-" ^ strId) dragEnd
          ; ViewUtils.onAnimationEnd
              ~key:("fpdfaded-" ^ strId)
              ~listener:flashFade ]
      | PackageFn _ ->
          []
    in
    let killParamBtn =
      match fn with
      | UserFunction fn when vs.permission = Some ReadWrite ->
          viewKillParameterBtn fn p
      | _ ->
          Vdom.noNode
    in
    let dragIcon =
      match fn with
      | UserFunction _ ->
          fontAwesome "grip-lines"
      | PackageFn _ ->
          Vdom.noNode
    in
    Html.div
      ~unique:strId
      ( [ Html.classList (("col param", true) :: conditionalClasses)
        ; Vdom.attribute "" "data-pos" (string_of_int index) ]
      @ events )
      [ killParamBtn
      ; viewParamName vs ~classes:["name"] p.ufpName
      ; viewParamTipe vs ~classes:["type"] p.ufpTipe
      ; dragIcon ]
  in
  let space = viewParamSpace index vs.fnProps in
  [space; param]


let view (fn : functionTypes) (vs : viewState) : msg Html.html list =
  let params =
    match fn with
    | UserFunction f ->
        f.ufMetadata.ufmParameters
        |> List.indexedMap ~f:(viewParam fn vs)
        |> List.flatten
    | PackageFn f ->
        f.parameters
        |> List.map ~f:PackageManager.pmParamsToUserFnParams
        |> List.indexedMap ~f:(viewParam fn vs)
        |> List.flatten
  in
  let lastSpace = viewParamSpace (List.length params) vs.fnProps in
  params @ [lastSpace]
