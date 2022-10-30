open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module Msg = AppTypes.Msg

let flagLinkLoc = (flag: string, currentlyEnabled: bool) => {
  let loc = Tea_navigation.getLocation()
  let newSearch =
    Url.queryParams()
    |> List.filter(~f=((k, _)) => k != flag)
    |> (x =>
      if currentlyEnabled {
        x
      } else {
        list{(flag, true), ...x}
      })
    |> List.map(~f=((k, v)) =>
      k ++
      ("=" ++
      if v {
        "1"
      } else {
        "0"
      })
    )
    |> String.join(~sep="&")
    |> (
      x =>
        if x == "" {
          ""
        } else {
          "?" ++ x
        }
    )

  `${loc.protocol}//${loc.host}${loc.pathname}${newSearch}${loc.hash}`
}

let debuggerLinkLoc = (m: AppTypes.model) => flagLinkLoc("debugger", m.teaDebuggerEnabled)

let viewIntegrationTestButton = (testState: AppTypes.IntegrationTests.t<AppTypes.model>): Html.html<
  AppTypes.msg,
> => {
  let integrationTestButton = switch testState {
  | IntegrationTestExpectation(_) => list{
      Html.a(
        list{
          /* We need to block all mouse events that might change the cursorState
           * because the integration tests click this button prior to
           * (in the OCaml portion of many integration tests) checking to see
           * if the cursorState matches what we expect. */
          EventListeners.nothingMouseEvent("mousedown"),
          EventListeners.nothingMouseEvent("click"),
          EventListeners.eventNoPropagation(~key="fit", "mouseup", _ => Msg.FinishIntegrationTest),
          Attrs.src(""),
          Attrs.id("finishIntegrationTest"),
          Attrs.class'("specialButton"),
        },
        list{Html.text("Finish integration tests")},
      ),
    }
  | IntegrationTestFinished(Ok()) => list{
      Html.div(
        list{Attrs.id("integrationTestSignal"), Attrs.class'("specialButton success")},
        list{Html.text("success")},
      ),
    }
  | IntegrationTestFinished(Error(msg)) => list{
      Html.div(
        list{Attrs.id("integrationTestSignal"), Attrs.class'("specialButton failure")},
        list{\"<|"(Html.text, "failure: " ++ msg)},
      ),
    }
  | NoIntegrationTest => list{}
  }

  Html.div(list{Attrs.id("buttons")}, integrationTestButton)
}

let viewError = (message: Error.t): Html.html<AppTypes.msg> => {
  let message = Error.asOption(message)
  let viewErrorMsg = switch message {
  | None => list{Vdom.noNode}
  | Some(msg) =>
    switch Json.Decode.decodeString(Decoders.exception_, msg) {
    | Error(_) => list{Html.p(list{}, list{Html.text(msg)})}
    | Ok({result: Some(msg), _}) => list{Html.p(list{}, list{Html.text(msg)})}
    | Ok(exc) => list{Html.p(list{}, list{Html.text(exc.short)})}
    }
  }

  let viewDismissBtn = list{
    Html.p(
      list{
        Attrs.class'("dismissBtn"),
        EventListeners.eventNoPropagation("click", ~key="dismiss-error", _ => Msg.DismissErrorBar),
      },
      list{Html.text("Dismiss")},
    ),
  }

  Html.div(
    list{Attrs.classList(list{("error-panel", true), ("show", message != None)})},
    Belt.List.concat(viewErrorMsg, viewDismissBtn),
  )
}

let readOnlyMessage = (m: AppTypes.model): Html.html<AppTypes.msg> =>
  Html.div(
    list{
      Attrs.classList(list{
        ("message-panel", true),
        // Only show this on confirmed Read-only so it doesn't pop up before initial_load.
        ("show", m.permission == Some(Read)),
      }),
    },
    list{
      Html.strong(list{}, list{Html.text("Heads up:")}),
      Html.text(" this canvas is read-only; you'll be able to view and copy it but not change it."),
    },
  )
