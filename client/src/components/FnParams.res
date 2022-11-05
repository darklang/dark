open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

module B = BlankOr

module FnParams = AppTypes.FunctionParams

type modification = AppTypes.modification
type model = AppTypes.model
type msg = AppTypes.msg
module Mod = AppTypes.Modification
module Msg = AppTypes.Msg

let onEvent = EventListeners.onEvent

type viewProps = ViewUtils.viewProps

let moveParams = (fn: PT.UserFunction.t, oldPos: int, newPos: int): PT.UserFunction.t => {
  let parameters = fn.parameters |> List.moveInto(~oldPos, ~newPos)

  {...fn, parameters: parameters}
}

let update = (m: AppTypes.model, msg: FnParams.msg): modification => {
  let (currentUserFn, mods) = switch msg {
  | ParamDragStart(index) => ({...m.currentUserFn, draggingParamIndex: Some(index)}, list{})
  | ParamDragDone => ({...m.currentUserFn, draggingParamIndex: None}, list{})
  | ParamEntersSpace(index) => ({...m.currentUserFn, dragOverSpaceIndex: Some(index)}, list{})
  | ParamLeavesSpace => ({...m.currentUserFn, dragOverSpaceIndex: None}, list{})
  | Reset => (FnParams.default, list{})
  | ParamDropIntoSpace(newPos) =>
    Page.tlidOf(m.currentPage)
    |> Option.andThen(~f=tlid => Map.get(~key=tlid, m.userFunctions))
    |> Option.pair(m.currentUserFn.draggingParamIndex)
    |> Option.map(~f=((oldPos, fn)) => {
      let newFn = moveParams(fn, oldPos, newPos)
      let updateArgs = if fn.name != "" {
        Refactor.reorderFnCallArgs(m, fn.tlid, User(fn.name), oldPos, newPos)
      } else {
        list{}
      }

      let justMovedParam =
        List.getAt(
          ~index=newPos,
          newFn.parameters,
        ) |> Option.map(~f=(p: PT.UserFunction.Parameter.t) => p.nameID)

      let fnM: FnParams.t = {
        justMovedParam: justMovedParam,
        draggingParamIndex: None,
        dragOverSpaceIndex: None,
      }

      (fnM, list{Mod.AddOps(list{SetFunction(newFn)}, FocusNoChange), ...updateArgs})
    })
    |> Option.unwrap(~default=(m.currentUserFn, list{}))
  }

  if List.isEmpty(mods) {
    ReplaceAllModificationsWithThisOne(
      "FnParams.update-empty",
      m => ({...m, currentUserFn: currentUserFn}, Tea.Cmd.none),
    )
  } else {
    Many(
      Belt.List.concat(
        mods,
        list{
          ReplaceAllModificationsWithThisOne(
            "Fnparams.update",
            m => ({...m, currentUserFn: currentUserFn}, Tea.Cmd.none),
          ),
        },
      ),
    )
  }
}

let viewKillParameterBtn = (uf: PT.UserFunction.t, p: PT.UserFunction.Parameter.t): Html.html<
  msg,
> => {
  let freeVariables = uf.body |> FluidAST.toExpr |> AST.freeVariables |> List.map(~f=Tuple2.second)

  let canDeleteParameter = pname => List.member(~value=pname, freeVariables) |> not

  let buttonContent = allowed =>
    if allowed {
      Html.div(
        list{
          Attrs.class'("parameter-btn allowed"),
          EventListeners.eventNoPropagation(
            ~key="dufp-" ++ TLID.toString(uf.tlid) ++ "-" ++ (p.nameID |> ID.toString),
            "click",
            _ => Msg.DeleteUserFunctionParameter(uf.tlid, p),
          ),
        },
        list{Icons.fontAwesome("times-circle")},
      )
    } else {
      Html.div(
        list{
          Attrs.class'("parameter-btn disallowed"),
          Attrs.title("Can't delete parameter because it is used in the function body"),
        },
        list{Icons.fontAwesome("times-circle")},
      )
    }

  if p.name != "" {
    buttonContent(canDeleteParameter(p.name))
  } else {
    buttonContent(true)
  }
}

let viewParamName = (~classes: list<string>, vp: viewProps, v: BlankOr.t<string>): Html.html<msg> =>
  ViewBlankOr.viewText(~enterable=true, ~classes, ParamName, vp, v)

let viewParamType = (~classes: list<string>, vp: viewProps, v: BlankOr.t<DType.t>): Html.html<
  msg,
> => ViewBlankOr.viewType(~classes, ~enterable=true, ParamType, vp, v)

let jsDragStart: Web.Node.event => unit = %raw(
  "function(e){ e.dataTransfer.setData('text/plain', e.target.innerHTML); e.dataTransfer.effectAllowed = 'move'; }"
)

let jsDragOver: Web.Node.event => unit = %raw("function(e){e.dataTransfer.dropEffect = 'move';}")

let viewParamSpace = (index: int, fs: FnParams.t): Html.html<msg> => {
  let dragOver = e => {
    jsDragOver(e)
    Msg.IgnoreMsg("view-param-space")
  }

  let dragEnter = _ => Msg.FnParamMsg(ParamEntersSpace(index))
  let dragLeave = _ => Msg.FnParamMsg(ParamLeavesSpace)
  let drop = e => {
    e["stopPropagation"]()
    Msg.FnParamMsg(ParamDropIntoSpace(index))
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
      Attrs.class'("col space" ++ overClass),
      Vdom.attribute("", "data-pos", string_of_int(index)),
      onEvent(~event="dragover", ~key="fpsdo-" ++ keyId, Obj.magic(dragOver)),
      onEvent(~event="dragenter", ~key="fpsde-" ++ keyId, dragEnter),
      onEvent(~event="dragleave", ~key="fpsdl-" ++ keyId, dragLeave),
      onEvent(~event="drop", ~key="fpsdrop-" ++ keyId, Obj.magic(drop)),
    },
    list{},
  )
}

let viewParam = (
  fn: functionTypes,
  vp: viewProps,
  index: int,
  p: PT.UserFunction.Parameter.t,
): list<Html.html<msg>> => {
  let strId = ID.toString(p.nameID)
  let dragStart = evt => {
    jsDragStart(evt)
    Msg.FnParamMsg(ParamDragStart(index))
  }

  let dragEnd = _ => Msg.FnParamMsg(ParamDragDone)
  let flashFade = str =>
    if str == "blinkGlow" {
      Msg.FnParamMsg(Reset)
    } else {
      Msg.IgnoreMsg("viewparam-flash-fade")
    }

  let conditionalClasses = list{
    ("dragging", vp.fnProps.draggingParamIndex |> Option.isSomeEqualTo(~value=index)),
    ("just-moved", vp.fnProps.justMovedParam |> Option.isSomeEqualTo(~value=p.nameID)),
  }

  let param = {
    let events = switch fn {
    | UserFunction(_) => list{
        Tea.Html.Attributes.draggable("true"),
        onEvent(
          ~event="dragstart",
          ~key="fpds-" ++ strId,
          ~preventDefault=false,
          Obj.magic(dragStart),
        ),
        onEvent(~event="dragend", ~key="fpde-" ++ strId, dragEnd),
        EventListeners.onAnimationEnd(~key="fpdfaded-" ++ strId, ~listener=flashFade),
      }
    | PackageFn(_) => list{}
    }

    let killParamBtn = switch fn {
    | UserFunction(fn) if vp.permission == Some(ReadWrite) => viewKillParameterBtn(fn, p)
    | _ => Vdom.noNode
    }

    let dragIcon = switch fn {
    | UserFunction(_) => Icons.fontAwesome("grip-lines")
    | PackageFn(_) => Vdom.noNode
    }

    Html.div(
      ~unique=strId,
      list{
        Attrs.classList(list{("col param", true), ...conditionalClasses}),
        Vdom.attribute("", "data-pos", string_of_int(index)),
        ...events,
      },
      list{
        killParamBtn,
        viewParamName(vp, ~classes=list{"name"}, B.fromStringID(p.name, p.nameID)),
        viewParamType(vp, ~classes=list{"type"}, B.fromOptionID(p.typ, p.typeID)),
        dragIcon,
      },
    )
  }

  let space = viewParamSpace(index, vp.fnProps)
  list{space, param}
}

let view = (fn: functionTypes, vp: viewProps): list<Html.html<msg>> => {
  let params = switch fn {
  | UserFunction(f) => f.parameters |> List.mapWithIndex(~f=viewParam(fn, vp)) |> List.flatten
  | PackageFn(f) =>
    f.parameters
    |> List.map(~f=PackageManager.pmParamsToUserFnParams)
    |> List.mapWithIndex(~f=viewParam(fn, vp))
    |> List.flatten
  }

  let lastSpace = viewParamSpace(List.length(params), vp.fnProps)
  Belt.List.concat(params, list{lastSpace})
}
