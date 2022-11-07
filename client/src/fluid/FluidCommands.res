open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

module TL = Toplevel
module K = FluidKeyboard

type modification = AppTypes.modification
type model = AppTypes.model
module Mod = AppTypes.Modification
module Msg = AppTypes.Msg
type command = AppTypes.fluidCmd
type cmdState = AppTypes.fluidCmdState

let filterInputID: string = "cmd-filter"

let fluidCommands = (_m: model) => Commands.commands

let reset = (m: model): cmdState => {
  index: 0,
  commands: fluidCommands(m),
  location: None,
  filter: None,
}

let commandsFor = (m: model, tl: toplevel, expr: fluidExpr): list<command> =>
  fluidCommands(m) |> List.filter(~f=(cmd: command) => cmd.shouldShow(m, tl, expr))

let show = (m: model, tlid: TLID.t, id: id): model => {
  let tl = TL.get(m, tlid)
  tl
  |> Option.andThen(~f=TL.getAST)
  |> Option.andThen(~f=FluidAST.findExpr(id))
  |> Option.map2(tl, ~f=(tl, expr) => {
    let cp: cmdState = {
      index: 0,
      commands: commandsFor(m, tl, expr),
      location: Some(tlid, id),
      filter: None,
    }

    {...m, fluidState: {...m.fluidState, cp: cp}}
  })
  |> Option.unwrap(~default=m)
}

let executeCommand = (m: model, tlid: TLID.t, id: id, cmd: command): modification =>
  switch TL.get(m, tlid) {
  | Some(tl) => cmd.action(m, tl, id)
  | _ => recover("No pd for the command", ~debug=(tlid, id, cmd), Mod.NoChange)
  }

let runCommand = (m: model, cmd: command): modification => {
  let cp = m.fluidState.cp
  switch cp.location {
  | Some(tlid, id) => executeCommand(m, tlid, id, cmd)
  | _ => NoChange
  }
}

let highlighted = (s: cmdState): option<command> => List.getAt(~index=s.index, s.commands)

let asName = (cmd: command): string => cmd.commandName

let moveUp = (s: cmdState): cmdState => {
  let i = s.index - 1
  {
    ...s,
    index: if i < 0 {
      0
    } else {
      i
    },
  }
}

let moveDown = (s: cmdState): cmdState => {
  let i = s.index + 1
  let max = List.length(s.commands)
  {
    ...s,
    index: if i >= max {
      max - 1
    } else {
      i
    },
  }
}

let focusItem = (i: int): AppTypes.cmd =>
  Tea_task.attempt(
    _ => Msg.IgnoreMsg("fluid-commands-focus"),
    Tea_task.nativeBinding(_ => {
      open Webapi.Dom
      open Native.Ext
      let container = Document.getElementById(document, "fluid-dropdown")
      let nthChild = querySelector(
        "#fluid-dropdown ul li:nth-child(" ++ (string_of_int(i + 1) ++ ")"),
      )

      switch (container, nthChild) {
      | (Some(el), Some(li)) =>
        let cRect = getBoundingClientRect(el)
        let cBottom = rectBottom(cRect)
        let cTop = rectTop(cRect)
        let liRect = getBoundingClientRect(li)
        let liBottom = rectBottom(liRect)
        let liTop = rectTop(liRect)
        let liHeight = rectHeight(liRect)
        if liBottom +. liHeight > cBottom {
          let offset = float_of_int(offsetTop(li))
          let padding = rectHeight(cRect) -. liHeight *. 2.0
          Element.setScrollTop(el, offset -. padding)
        } else if liTop -. liHeight < cTop {
          let offset = float_of_int(offsetTop(li))
          Element.setScrollTop(el, offset -. liHeight)
        } else {
          ()
        }
      | (_, _) => ()
      }
    }),
  )

let filter = (m: model, query: string, cp: cmdState): cmdState => {
  let allCmds = switch cp.location {
  | Some(tlid, id) =>
    let tl = TL.get(m, tlid)
    tl
    |> Option.andThen(~f=TL.getAST)
    |> Option.map2(tl, ~f=(tl, ast) =>
      ast
      |> FluidAST.findExpr(id)
      |> (
        x =>
          switch x {
          | Some(expr) => commandsFor(m, tl, expr)
          | None => list{}
          }
      )
    )
    |> recoverOpt("no tl for location", ~default=list{})
  | _ => fluidCommands(m)
  }

  let (filter, commands) = if String.length(query) > 0 {
    let isMatched = (c: command) => String.includes(~substring=query, c.commandName)
    (Some(query), List.filter(~f=isMatched, allCmds))
  } else {
    (None, fluidCommands(m))
  }

  {...cp, filter: filter, commands: commands, index: 0}
}

let isOpenOnTL = (s: cmdState, tlid: TLID.t): bool =>
  switch s.location {
  | Some(ltlid, _) if tlid == ltlid => true
  | _ => false
  }

@ocaml.doc(" onKeydown is a special keydown handler for the command palette,
  * which handles a few specific keypresses and ignores everything else.
  *
  * We can't use the generic FluidKeyboard keydown handler for this, as it's
  * too agreessive in capturing keys that we want delegated to the palette's
  * input element for default handling (like backspace and left/right arrows). ")
let onKeydown = (evt: Dom.event): option<AppTypes.msg> =>
  K.eventToKeyEvent(Obj.magic(evt)) |> Option.andThen(~f=e =>
    switch e {
    | {K.key: K.Enter, _}
    | {key: K.Up, _}
    | {key: K.Down, _}
    | {key: K.Escape, _} =>
      Some(Msg.FluidMsg(FluidInputEvent(Keypress(e))))
    | _ => None
    }
  )

let viewCommandPalette = (cp: cmdState): Html.html<AppTypes.msg> => {
  let viewCommands = (i, item) => {
    let highlighted = cp.index == i
    let name = asName(item)
    Html.li(
      list{
        Attrs.classList(list{
          ("autocomplete-item", true),
          ("fluid-selected", highlighted),
          ("valid", true),
        }),
        EventListeners.nothingMouseEvent("mouseup"),
        ViewEntry.defaultPasteHandler,
        EventListeners.eventNoPropagation(~key="cp-" ++ name, "mousedown", _ => Msg.FluidMsg(
          FluidCommandsClick(item),
        )),
        EventListeners.eventBoth(~key="-mousemove" ++ name, "mousemove", _ => Msg.FluidMsg(
          FluidUpdateDropdownIndex(i),
        )),
      },
      list{Html.text(name)},
    )
  }

  let filterInput = Html.input(
    list{
      Attrs.id(filterInputID),
      Vdom.attribute("", "spellcheck", "false"),
      Attrs.autocomplete(false),
      Events.onInput(query => Msg.FluidMsg(FluidCommandsFilter(query))),
      Events.onCB(~key="", "keydown", onKeydown),
      Events.onBlur(Msg.FluidMsg(FluidCloseCmdPalette)),
    },
    list{},
  )

  let cmdsView = Html.div(
    list{Attrs.id("fluid-dropdown")},
    list{Html.ul(list{}, List.mapWithIndex(~f=viewCommands, cp.commands))},
  )

  Html.div(list{Attrs.class("command-palette")}, list{filterInput, cmdsView})
}

let cpSetIndex = (_m: model, i: int): modification => ReplaceAllModificationsWithThisOne(
  "cpSetIndex",
  m => {
    let cp = {...m.fluidState.cp, index: i}
    let fluidState = {...m.fluidState, cp: cp, upDownCol: None}
    ({...m, fluidState: fluidState}, focusItem(i))
  },
)

let updateCmds = (m: model, keyEvt: K.keyEvent): modification => {
  let key = keyEvt.key
  switch key {
  | K.Enter =>
    switch m.fluidState.cp.location {
    | Some(tlid, id) =>
      switch highlighted(m.fluidState.cp) {
      | Some(cmd) => Many(list{executeCommand(m, tlid, id, cmd), FluidCommandsClose})
      | None => NoChange
      }
    | _ => NoChange
    }
  | K.Up =>
    ReplaceAllModificationsWithThisOne(
      "FluidCommands-up",
      m => {
        let cp = moveUp(m.fluidState.cp)
        let fluidState = {...m.fluidState, cp: cp}
        ({...m, fluidState: fluidState}, focusItem(cp.index))
      },
    )
  | K.Down =>
    ReplaceAllModificationsWithThisOne(
      "FluidCommands-down",
      m => {
        let cp = moveDown(m.fluidState.cp)
        let fluidState = {...m.fluidState, cp: cp}
        ({...m, fluidState: fluidState}, focusItem(cp.index))
      },
    )
  | K.Escape => FluidCommandsClose
  | _ => NoChange
  }
}

let isOpened = (cp: cmdState): bool => cp.location != None

let updateCommandPaletteVisibility = (m: model): model => {
  let oldTlid = switch m.fluidState.cp.location {
  | Some(tlid, _) => Some(tlid)
  | None => CursorState.tlidOf(m.cursorState)
  }

  let newTlid = CursorState.tlidOf(m.cursorState)
  if isOpened(m.fluidState.cp) && oldTlid != newTlid {
    let newCp = reset(m)
    {...m, fluidState: {...m.fluidState, cp: newCp}}
  } else {
    m
  }
}
