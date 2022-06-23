open Prelude

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

  Printf.sprintf("%s//%s%s%s%s", loc.protocol, loc.host, loc.pathname, newSearch, loc.hash)
}

let debuggerLinkLoc = m => flagLinkLoc("debugger", m.teaDebuggerEnabled)

let viewIntegrationTestButton = (testState: integrationTestState): Html.html<msg> => {
  let integrationTestButton = switch testState {
  | IntegrationTestExpectation(_) => list{
      Html.a(
        list{
          /* We need to block all mouse events that might change the cursorState
           * because the integration tests click this button prior to
           * (in the OCaml portion of many integration tests) checking to see
           * if the cursorState matches what we expect. */
          ViewUtils.nothingMouseEvent("mousedown"),
          ViewUtils.nothingMouseEvent("click"),
          ViewUtils.eventNoPropagation(~key="fit", "mouseup", _ => FinishIntegrationTest),
          Html.src(""),
          Html.id("finishIntegrationTest"),
          Html.class'("specialButton"),
        },
        list{Html.text("Finish integration tests")},
      ),
    }
  | IntegrationTestFinished(Ok()) => list{
      Html.div(
        list{Html.id("integrationTestSignal"), Html.class'("specialButton success")},
        list{Html.text("success")},
      ),
    }
  | IntegrationTestFinished(Error(msg)) => list{
      Html.div(
        list{Html.id("integrationTestSignal"), Html.class'("specialButton failure")},
        list{\"<|"(Html.text, "failure: " ++ msg)},
      ),
    }
  | NoIntegrationTest => list{}
  }

  Html.div(list{Html.id("buttons")}, integrationTestButton)
}

let viewError = (message: Error.t): Html.html<msg> => {
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
        Html.class'("dismissBtn"),
        ViewUtils.eventNoPropagation("click", ~key="dismiss-error", _ => DismissErrorBar),
      },
      list{Html.text("Dismiss")},
    ),
  }

  Html.div(
    list{Html.classList(list{("error-panel", true), ("show", message != None)})},
    Belt.List.concat(viewErrorMsg, viewDismissBtn),
  )
}

let readOnlyMessage = (m: model): Html.html<msg> =>
  Html.div(
    list{
      Html.classList(list{
        ("message-panel", true),
        /* Only show this on confirmed Read-only so it doesn't pop up before initial_load. */
        ("show", m.permission == Some(Read)),
      }),
    },
    list{
      Html.strong(list{}, list{Html.text("Heads up:")}),
      Html.text(" this canvas is read-only; you'll be able to view and copy it but not change it."),
    },
  )
