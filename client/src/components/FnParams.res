open Prelude
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

let onEvent = ViewUtils.onEvent

type viewProps = ViewUtils.viewProps

let moveParams = (fn: userFunction, oldPos: int, newPos: int): userFunction => {
  let ufmParameters = fn.ufMetadata.ufmParameters |> List.moveInto(~oldPos, ~newPos)

  {...fn, ufMetadata: {...fn.ufMetadata, ufmParameters: ufmParameters}}
}

let update = (m: model, msg: fnpMsg): modification => {
  let (currentUserFn, mods) = switch msg {
  | ParamDragStart(index) => ({...m.currentUserFn, draggingParamIndex: Some(index)}, list{})
  | ParamDragDone => ({...m.currentUserFn, draggingParamIndex: None}, list{})
  | ParamEntersSpace(index) => ({...m.currentUserFn, dragOverSpaceIndex: Some(index)}, list{})
  | ParamLeavesSpace => ({...m.currentUserFn, dragOverSpaceIndex: None}, list{})
  | Reset => (Defaults.defaultFnSpace, list{})
  | ParamDropIntoSpace(newPos) =>
    Page.tlidOf(m.currentPage)
    |> Option.andThen(~f=tlid => Map.get(~key=tlid, m.userFunctions))
    |> Option.pair(m.currentUserFn.draggingParamIndex)
    |> Option.map(~f=((oldPos, fn)) => {
      let newFn = moveParams(fn, oldPos, newPos)
      let updateArgs = switch fn.ufMetadata.ufmName {
      | F(_, name) => Refactor.reorderFnCallArgs(m, fn.ufTLID, name, oldPos, newPos)
      | Blank(_) => list{}
      }

      let justMovedParam =
        List.getAt(~index=newPos, newFn.ufMetadata.ufmParameters) |> Option.map(~f=p =>
          B.toID(p.ufpName)
        )

      let fnM = {
        justMovedParam: justMovedParam,
        draggingParamIndex: None,
        dragOverSpaceIndex: None,
      }

      (fnM, list{AddOps(list{SetFunction(newFn)}, FocusNoChange), ...updateArgs})
    })
    |> Option.unwrap(~default=(m.currentUserFn, list{}))
  }

  if List.isEmpty(mods) {
    ReplaceAllModificationsWithThisOne(m => ({...m, currentUserFn: currentUserFn}, Tea.Cmd.none))
  } else {
    Many(
      Belt.List.concat(
        mods,
        list{
          ReplaceAllModificationsWithThisOne(
            m => ({...m, currentUserFn: currentUserFn}, Tea.Cmd.none),
          ),
        },
      ),
    )
  }
}

let viewKillParameterBtn = (uf: userFunction, p: userFunctionParameter): Html.html<msg> => {
  let freeVariables = uf.ufAST |> FluidAST.toExpr |> AST.freeVariables |> List.map(~f=Tuple2.second)

  let canDeleteParameter = pname => List.member(~value=pname, freeVariables) |> not

  let buttonContent = allowed =>
    if allowed {
      Html.div(
        list{
          Html.class'("parameter-btn allowed"),
          ViewUtils.eventNoPropagation(
            ~key="dufp-" ++
            (TLID.toString(uf.ufTLID) ++
            ("-" ++ (p.ufpName |> B.toID |> ID.toString))),
            "click",
            _ => DeleteUserFunctionParameter(uf.ufTLID, p),
          ),
        },
        list{fontAwesome("times-circle")},
      )
    } else {
      Html.div(
        list{
          Html.class'("parameter-btn disallowed"),
          Html.title("Can't delete parameter because it is used in the function body"),
        },
        list{fontAwesome("times-circle")},
      )
    }

  switch p.ufpName {
  | F(_, pname) => buttonContent(canDeleteParameter(pname))
  | _ => buttonContent(true)
  }
}

let viewParamName = (~classes: list<string>, vp: viewProps, v: blankOr<string>): Html.html<msg> =>
  ViewBlankOr.viewText(~enterable=true, ~classes, ParamName, vp, v)

let viewParamTipe = (~classes: list<string>, vp: viewProps, v: blankOr<tipe>): Html.html<msg> =>
  ViewBlankOr.viewTipe(~classes, ~enterable=true, ParamTipe, vp, v)

let jsDragStart: Web.Node.event => unit = %raw(
  "function(e){ e.dataTransfer.setData('text/plain', e.target.innerHTML); e.dataTransfer.effectAllowed = 'move'; }"
)

let jsDragOver: Web.Node.event => unit = %raw("function(e){e.dataTransfer.dropEffect = 'move';}")

let viewParamSpace = (index: int, fs: fnProps): Html.html<msg> => {
  let dragOver = e => {
    jsDragOver(e)
    IgnoreMsg("view-param-space")
  }

  let dragEnter = _ => FnParamMsg(ParamEntersSpace(index))
  let dragLeave = _ => FnParamMsg(ParamLeavesSpace)
  let drop = e => {
    e["stopPropagation"]()
    FnParamMsg(ParamDropIntoSpace(index))
  }

  let keyId = string_of_int(index)
  let overClass = switch (fs.draggingParamIndex, fs.dragOverSpaceIndex) {
  | (Some(draggingIndex), Some(spaceIndex)) if spaceIndex == index =>
    if draggingIndex !== spaceIndex && draggingIndex + 1 !== spaceIndex {
      " over"
    } else {
      ""
    }
  | _ => ""
  }

  Html.div(
    list{
      Html.class'("col space" ++ overClass),
      Vdom.attribute("", "data-pos", string_of_int(index)),
      onEvent(~event="dragover", ~key="fpsdo-" ++ keyId, dragOver),
      onEvent(~event="dragenter", ~key="fpsde-" ++ keyId, dragEnter),
      onEvent(~event="dragleave", ~key="fpsdl-" ++ keyId, dragLeave),
      onEvent(~event="drop", ~key="fpsdrop-" ++ keyId, drop),
    },
    list{},
  )
}

let viewParam = (fn: functionTypes, vp: viewProps, index: int, p: userFunctionParameter): list<
  Html.html<msg>,
> => {
  let nameId = p.ufpName |> B.toID
  let strId = ID.toString(nameId)
  let dragStart = evt => {
    jsDragStart(evt)
    FnParamMsg(ParamDragStart(index))
  }

  let dragEnd = _ => FnParamMsg(ParamDragDone)
  let flashFade = str =>
    if str == "blinkGlow" {
      FnParamMsg(Reset)
    } else {
      IgnoreMsg("viewparam-flash-fade")
    }

  let conditionalClasses = list{
    ("dragging", vp.fnProps.draggingParamIndex |> Option.isSomeEqualTo(~value=index)),
    ("just-moved", vp.fnProps.justMovedParam |> Option.isSomeEqualTo(~value=nameId)),
  }

  let param = {
    let events = switch fn {
    | UserFunction(_) => list{
        Tea.Html2.Attributes.draggable("true"),
        onEvent(~event="dragstart", ~key="fpds-" ++ strId, ~preventDefault=false, dragStart),
        onEvent(~event="dragend", ~key="fpde-" ++ strId, dragEnd),
        ViewUtils.onAnimationEnd(~key="fpdfaded-" ++ strId, ~listener=flashFade),
      }
    | PackageFn(_) => list{}
    }

    let killParamBtn = switch fn {
    | UserFunction(fn) if vp.permission == Some(ReadWrite) => viewKillParameterBtn(fn, p)
    | _ => Vdom.noNode
    }

    let dragIcon = switch fn {
    | UserFunction(_) => fontAwesome("grip-lines")
    | PackageFn(_) => Vdom.noNode
    }

    Html.div(
      ~unique=strId,
      list{
        Html.classList(list{("col param", true), ...conditionalClasses}),
        Vdom.attribute("", "data-pos", string_of_int(index)),
        ...events,
      },
      list{
        killParamBtn,
        viewParamName(vp, ~classes=list{"name"}, p.ufpName),
        viewParamTipe(vp, ~classes=list{"type"}, p.ufpTipe),
        dragIcon,
      },
    )
  }

  let space = viewParamSpace(index, vp.fnProps)
  list{space, param}
}

let view = (fn: functionTypes, vp: viewProps): list<Html.html<msg>> => {
  let params = switch fn {
  | UserFunction(f) =>
    f.ufMetadata.ufmParameters |> List.mapWithIndex(~f=viewParam(fn, vp)) |> List.flatten
  | PackageFn(f) =>
    f.parameters
    |> List.map(~f=PackageManager.pmParamsToUserFnParams)
    |> List.mapWithIndex(~f=viewParam(fn, vp))
    |> List.flatten
  }

  let lastSpace = viewParamSpace(List.length(params), vp.fnProps)
  Belt.List.concat(params, list{lastSpace})
}
