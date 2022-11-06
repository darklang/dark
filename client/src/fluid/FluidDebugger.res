open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

module Printer = FluidTokenizer
module Expression = FluidExpression
module Token = FluidToken

let view = (m: AppTypes.model, ast: FluidAST.t): Html.html<AppTypes.msg> => {
  let s = m.fluidState
  let tokens = FluidTokenizer.tokenizeExprForDebugger(m.fluidState.activeEditor, ast)
  let ddText = txt => Html.dd(list{}, list{Html.text(txt)})
  let dtText = txt => Html.dt(list{}, list{Html.text(txt)})
  let posData = {
    let oldGrid = Fluid.gridFor(~pos=s.oldPos, tokens)
    let newGrid = Fluid.gridFor(~pos=s.newPos, tokens)
    list{
      dtText("pos"),
      Html.dd(
        list{},
        list{
          Html.text(Int.toString(s.oldPos)),
          Html.text(" -> "),
          Html.text(Int.toString(s.newPos)),
        },
      ),
      dtText("grid"),
      Html.dd(
        list{},
        list{
          Html.text(oldGrid.col->Int.toString),
          Html.text(","),
          Html.text(oldGrid.row->Int.toString),
          Html.text(" -> "),
          Html.text(newGrid.col->Int.toString),
          Html.text(","),
          Html.text(newGrid.row->Int.toString),
        },
      ),
      dtText("TLID"),
      Html.dd(
        list{},
        list{
          Html.text(
            CursorState.tlidOf(m.cursorState)
            ->Option.map(~f=TLID.toString)
            ->Option.unwrap(~default="None"),
          ),
        },
      ),
      dtText("ast root"),
      Html.dd(list{}, list{Html.text(FluidAST.toID(ast)->ID.toString)}),
      dtText("active editor"),
      Html.dd(list{}, list{Html.text(FluidTypes.Editor.toDebugString(s.activeEditor))}),
      dtText("acIndex"),
      Html.dd(
        list{},
        list{Html.text(s.ac.index->Option.map(~f=Int.toString)->Option.unwrap(~default="None"))},
      ),
      dtText("acEntryCount"),
      Html.dd(list{}, list{Html.text(FluidAutocomplete.numCompletions(s.ac)->Int.toString)}),
      dtText("upDownCol"),
      Html.dd(
        list{},
        list{Html.text(s.upDownCol->Option.map(~f=Int.toString)->Option.unwrap(~default="None"))},
      ),
      dtText("lastInput"),
      Html.dd(list{}, list{Html.text(FluidTypes.Msg.inputEventToDebugString(s.lastInput))}),
      dtText("selection"),
      Html.dd(
        list{},
        list{
          Html.text(
            s.selectionStart
            ->Option.map(~f=selStart => Int.toString(selStart) ++ " -> " ++ Int.toString(s.newPos))
            ->Option.unwrap(~default="None"),
          ),
        },
      ),
      dtText("midClick"),
      Html.dd(list{}, list{Html.text(Bool.toString(s.midClick))}),
    }
  }

  let error = list{dtText("error"), ddText(Option.unwrap(s.error, ~default="None"))}

  let tokenData = {
    let (left, right, next) = FluidTokenizer.getNeighbours(tokens, ~pos=s.newPos)
    let ddNoProp1 = txt => Html.dd(list{Attrs.noProp}, list{Html.text(txt)})
    let tokenInfo = tkn => Html.dd(list{Attrs.class("tokenInfo")}, list{Token.show_tokenInfo(tkn)})

    let ddLeft = switch left {
    | L(_, left) => tokenInfo(left)
    | R(_, _) => ddNoProp1("Right")
    | No => ddNoProp1("None")
    }

    let ddRight = switch right {
    | L(_, _) => ddNoProp1("Left")
    | R(_, right) => tokenInfo(right)
    | No => ddNoProp1("None")
    }

    let ddNext = switch next {
    | Some(next) => tokenInfo(next)
    | None => ddNoProp1("None")
    }

    list{dtText("left"), ddLeft, dtText("right"), ddRight, dtText("next"), ddNext}
  }

  let actions = list{
    dtText("actions"),
    Html.dd(
      list{Attrs.class("actions")},
      list{Html.ul(list{}, List.map(s.actions, ~f=txt => Html.li(list{}, list{Html.text(txt)})))},
    ),
  }

  let cursorState = list{dtText("cursorState"), ddText(CursorState.toDebugString(m.cursorState))}

  let status = List.flatten(list{posData, error, tokenData, actions, cursorState})
  Html.div(list{Attrs.id("fluid-status")}, list{Html.dl(list{}, status)})
}
