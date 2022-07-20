open Prelude
module Attrs = Tea.Html2.Attributes
module Printer = FluidTokenizer
module Expression = FluidExpression
module Token = FluidToken

let view = (m: AppTypes.model, ast: FluidAST.t): Html.html<AppTypes.msg> => {
  let s = m.fluidState
  let tokens = FluidTokenizer.tokensForEditor(m.fluidState.activeEditor, ast)
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
          Html.text(string_of_int(s.oldPos)),
          Html.text(" -> "),
          Html.text(string_of_int(s.newPos)),
        },
      ),
      dtText("grid"),
      Html.dd(
        list{},
        list{
          Html.text(oldGrid.col |> string_of_int),
          Html.text(","),
          Html.text(oldGrid.row |> string_of_int),
          Html.text(" -> "),
          Html.text(newGrid.col |> string_of_int),
          Html.text(","),
          Html.text(newGrid.row |> string_of_int),
        },
      ),
      dtText("TLID"),
      Html.dd(
        list{},
        list{
          Html.text(
            CursorState.tlidOf(m.cursorState)
            |> Option.map(~f=TLID.toString)
            |> Option.unwrap(~default="None"),
          ),
        },
      ),
      dtText("ast root"),
      Html.dd(list{}, list{Html.text(FluidAST.toID(ast) |> ID.toString)}),
      dtText("active editor"),
      Html.dd(list{}, list{Html.text(show_fluidEditor(s.activeEditor))}),
      dtText("acIndex"),
      Html.dd(
        list{},
        list{
          Html.text(s.ac.index |> Option.map(~f=string_of_int) |> Option.unwrap(~default="None")),
        },
      ),
      dtText("acEntryCount"),
      Html.dd(list{}, list{Html.text(FluidAutocomplete.numCompletions(s.ac) |> string_of_int)}),
      dtText("upDownCol"),
      Html.dd(
        list{},
        list{
          Html.text(s.upDownCol |> Option.map(~f=string_of_int) |> Option.unwrap(~default="None")),
        },
      ),
      dtText("lastInput"),
      Html.dd(list{}, list{Html.text(show_fluidInputEvent(s.lastInput))}),
      dtText("selection"),
      Html.dd(
        list{},
        list{
          Html.text(
            s.selectionStart
            |> Option.map(~f=selStart =>
              string_of_int(selStart) ++ ("->" ++ string_of_int(s.newPos))
            )
            |> Option.unwrap(~default="None"),
          ),
        },
      ),
      dtText("midClick"),
      Html.dd(list{}, list{Html.text(string_of_bool(s.midClick))}),
    }
  }

  let error = list{dtText("error"), ddText(Option.unwrap(s.error, ~default="None"))}

  let tokenData = {
    let (left, right, next) = FluidTokenizer.getNeighbours(tokens, ~pos=s.newPos)
    let ddNoProp1 = txt => Html.dd(list{Html.noProp}, list{Html.text(txt)})
    let tokenInfo = tkn => Html.dd(list{Attrs.class'("tokenInfo")}, list{Token.show_tokenInfo(tkn)})

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
      list{Attrs.class'("actions")},
      list{Html.ul(list{}, List.map(s.actions, ~f=txt => Html.li(list{}, list{Html.text(txt)})))},
    ),
  }

  let cursorState = list{dtText("cursorState"), ddText(AppTypes.CursorState.show(m.cursorState))}

  let status = List.flatten(list{posData, error, tokenData, actions, cursorState})
  Html.div(list{Attrs.id("fluid-status")}, list{Html.dl(list{}, status)})
}
