// NOTE: This will change in the future to pretty tool tips, this is just an inbetween state
open Prelude

type msg = AppTypes.msg
type modification = AppTypes.modification
module Mod = AppTypes.Modification

type tutorialStep = AppTypes.Tutorial.Step.t
type tooltipState = AppTypes.Tooltip.t
type tooltipMsg = AppTypes.Tooltip.msg

type rec toolTipDirection =
  | Left
  | Right
  | Top
  | Bottom

and tooltipStyle =
  | Tutorial(tutorialStep)
  | Crud
  | Default

and tooltipContent = {
  title: string,
  details: option<list<string>>,
  action: option<(string, msg)>,
  align: toolTipDirection,
  tipAlignment: string,
  tooltipStyle: tooltipStyle,
}

@ocaml.doc(
  " [currentStepFraction currentStep] returns a tuple of the form [(currentStepNumber, totalSteps)], given the [currentStep]. "
)
let currentStepFraction = (currentStep: tutorialStep): (int, int) => {
  let currentStepNumber = switch currentStep {
  | Welcome => 1
  | VerbChange => 2
  | ReturnValue => 3
  | OpenTab => 4
  | GettingStarted => 5
  }

  let totalSteps = 5
  (currentStepNumber, totalSteps)
}

let stepNumToStep = (currentStep: int): option<tutorialStep> =>
  switch currentStep {
  | 1 => Some(Welcome)
  | 2 => Some(VerbChange)
  | 3 => Some(ReturnValue)
  | 4 => Some(OpenTab)
  | 5 => Some(GettingStarted)
  | _ => None
  }

let getPrevStep = (current: option<tutorialStep>): option<tutorialStep> =>
  switch current {
  | Some(step) =>
    let (currentStepNumber, _) = currentStepFraction(step)
    stepNumToStep(currentStepNumber - 1)
  | None => None
  }

let getNextStep = (current: option<tutorialStep>): option<tutorialStep> =>
  switch current {
  | Some(step) =>
    let (currentStepNumber, _) = currentStepFraction(step)
    stepNumToStep(currentStepNumber + 1)
  | None => None
  }

let assignTutorialToHTTPHandler = (
  tooltipState: tooltipState,
  tl: toplevel,
  tlid: TLID.t,
): tooltipState =>
  if (
    tooltipState.userTutorial.step == Some(Welcome) &&
      (tooltipState.userTutorial.tlid == None &&
      Toplevel.isHTTPHandler(tl))
  ) {
    {
      ...tooltipState,
      userTutorial: {...tooltipState.userTutorial, tlid: Some(tlid)},
    }
  } else {
    tooltipState
  }

let update = (tooltipState: tooltipState, msg: tooltipMsg): modification => {
  let (tooltipState, mods) = {
    let currentTooltip = tooltipState.tooltipSource
    switch msg {
    | OpenTooltip(tt) if !Option.isSome(currentTooltip) || Some(tt) != currentTooltip => (
        {...tooltipState, tooltipSource: Some(tt)},
        list{},
      )
    | OpenTooltip(_) | Close => ({...tooltipState, tooltipSource: None}, list{})
    | OpenLink(url) =>
      Native.Window.openUrl(url, "_blank")
      ({...tooltipState, tooltipSource: None}, list{})
    | OpenFnTooltip(fnSpace) => ({...tooltipState, fnSpace: fnSpace}, list{})
    | UpdateTutorial(tutorialMsg) =>
      let (userTutorial, mods) = switch tutorialMsg {
      | NextStep =>
        if (
          tooltipState.userTutorial.step == Some(Welcome) && tooltipState.userTutorial.tlid == None
        ) {
          (
            tooltipState.userTutorial,
            list{
              Mod.ReplaceAllModificationsWithThisOne(
                (m: AppTypes.model) => (
                  {
                    ...m,
                    toast: {
                      ...m.toast,
                      message: Some("Create a new HTTP Handler to continue tutorial"),
                    },
                  },
                  Tea.Cmd.none,
                ),
              ),
            },
          )
        } else {
          let step = getNextStep(tooltipState.userTutorial.step)
          ({...tooltipState.userTutorial, step: step}, list{})
        }
      | PrevStep =>
        let step = getPrevStep(tooltipState.userTutorial.step)
        ({...tooltipState.userTutorial, step: step}, list{})
      | CloseTutorial => (
          {step: None, tlid: None},
          list{
            ReplaceAllModificationsWithThisOne(
              m => (
                {
                  ...m,
                  firstVisitToDark: false,
                  firstVisitToThisCanvas: false,
                },
                Tea.Cmd.none,
              ),
            ),
          },
        )
      | ReopenTutorial => ({step: Some(Welcome), tlid: None}, list{})
      }

      ({...tooltipState, userTutorial: userTutorial}, mods)
    }
  }

  if List.isEmpty(mods) {
    ReplaceAllModificationsWithThisOne(m => ({...m, tooltipState: tooltipState}, Tea.Cmd.none))
  } else {
    Many(
      Belt.List.concat(
        mods,
        list{
          ReplaceAllModificationsWithThisOne(
            m => ({...m, tooltipState: tooltipState}, Tea.Cmd.none),
          ),
        },
      ),
    )
  }
}

let generateContent = (t: AppTypes.Tooltip.source): tooltipContent =>
  switch t {
  | Http => {
      title: "Click the plus sign to create a REST API endpoint.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/create-http-handler")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | Worker => {
      title: "Click the plus sign to create a worker to process asynchronous tasks.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/process-background-jobs-worker")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | Cron => {
      title: "Click the plus sign to create a scheduled job.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/create-daily-job-cron-handler")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | Repl => {
      title: "Click the plus sign to create a general purpose coding block.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/create-tool-repl")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | Datastore => {
      title: "Click to create a key-value store.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/save-data-to-datastore")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | Function => {
      title: "Click to create a reusable block of code.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/tutorials/extract-function")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | FourOhFour => {
      title: "Attempts to hit endpoints that do not yet have handlers appear here.",
      details: Some(list{
        "If you're looking for a 404 but not seeing it in this list, check the 'Deleted' section of the sidebar.",
      }),
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/discussion/trace-driven-development")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | Deleted => {
      title: "Deleted handlers appear here.",
      details: None,
      action: None,
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | PackageManager => {
      title: "A list of built-in Dark functions. Click on the name of the function to preview it.",
      details: Some(list{
        "To use the function in your canvas, start typing its name in your handler and select it from autocomplete.",
      }),
      action: None,
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | StaticAssets => {
      title: "Learn more about hosting static assets here.",
      details: None,
      action: Some(
        "Learn More",
        ToolTipMsg(OpenLink("https://docs.darklang.com/how-to/static-assets")),
      ),
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  | FnParam => {
      title: "If a function has parameters, it will need to be called once from another handler in order to assign values to the parameters and display live values. Until this happens, the function will display a warning.",
      details: None,
      action: None,
      align: Left,
      tipAlignment: "",
      tooltipStyle: Default,
    }
  | FnBackToCanvas => {
      title: "Functions live in the function space, which is separate from your main canvas. You can return to your main canvas by clicking on the name of another handler in the sidebar or the link provided here.",
      details: None,
      action: None,
      align: Top,
      tipAlignment: "",
      tooltipStyle: Default,
    }
  | Secrets => {
      title: "Store API keys, passwords or other secrets here.",
      details: Some(list{
        "Secret names will appear in your autocomplete, but their values will be redacted from live values and traces.",
      }),
      action: None,
      align: Bottom,
      tipAlignment: "align-left",
      tooltipStyle: Default,
    }
  }

let viewNavigationBtns = (tlid: option<TLID.t>, step: tutorialStep, uniqueStr: string): Html.html<
  msg,
> => {
  let prevBtn = {
    let clickEvent = switch getPrevStep(Some(step)) {
    | Some(_) =>
      let (stepNum, _) = currentStepFraction(step)
      ViewUtils.eventNoPropagation(
        ~key="prev-step-" ++ (string_of_int(stepNum) ++ ("-" ++ uniqueStr)),
        "click",
        _ => ToolTipMsg(UpdateTutorial(PrevStep)),
      )
    | None => Vdom.noProp
    }

    Html.button(
      list{
        Html.class'("page-btn"),
        ViewUtils.nothingMouseEvent("mousedown"),
        ViewUtils.nothingMouseEvent("mouseup"),
        clickEvent,
        Html.Attributes.disabled(clickEvent == Vdom.noProp),
      },
      list{Html.text("Previous")},
    )
  }

  let nextBtn = {
    let clickEvent = switch getNextStep(Some(step)) {
    | Some(_) if Option.isSome(tlid) =>
      let (stepNum, _) = currentStepFraction(step)
      ViewUtils.eventNoPropagation(
        ~key="next-step-" ++ (string_of_int(stepNum) ++ ("-" ++ uniqueStr)),
        "click",
        _ => ToolTipMsg(UpdateTutorial(NextStep)),
      )
    | Some(_) | None => Vdom.noProp
    }

    Html.button(
      list{
        Html.class'("page-btn"),
        ViewUtils.nothingMouseEvent("mousedown"),
        ViewUtils.nothingMouseEvent("mouseup"),
        clickEvent,
        Html.Attributes.disabled(clickEvent == Vdom.noProp),
      },
      list{Html.text("Next")},
    )
  }

  Html.div(list{Html.class'("btn-container")}, list{prevBtn, nextBtn})
}

let viewToolTip = (~shouldShow: bool, ~tlid: option<TLID.t>, t: tooltipContent): Html.html<
  AppTypes.msg,
> =>
  if shouldShow {
    let uniqueStr = switch tlid {
    | Some(id) => TLID.toString(id)
    | None => t.title
    }

    let viewDesc = Html.h1(list{Html.class'("description")}, list{Html.text(t.title)})
    let viewDetail = switch t.details {
    | Some(txtList) =>
      let txtview = List.map(
        ~f=txt => Html.p(list{Html.class'("details")}, list{Html.text(txt)}),
        txtList,
      )

      Html.div(list{}, txtview)
    // Html.p [Html.class' "details"] [Html.text txt]
    | None => Vdom.noNode
    }

    let viewBtn = switch t.action {
    | Some(text, action) =>
      Html.button(
        list{
          Html.class'("action-button"),
          ViewUtils.eventNoPropagation(~key="close-settings" ++ text, "click", _ => action),
        },
        list{Html.p(list{}, list{Html.text(text)})},
      )
    | None => Vdom.noNode
    }

    let closeBtn = switch t.tooltipStyle {
    | Tutorial(_) | Crud =>
      Html.button(
        list{
          Html.class'("page-btn"),
          ViewUtils.nothingMouseEvent("mousedown"),
          ViewUtils.nothingMouseEvent("mouseup"),
          ViewUtils.eventNoPropagation(
            ~key="close-tutorial-" ++ uniqueStr,
            "click",
            _ => ToolTipMsg(UpdateTutorial(CloseTutorial)),
          ),
        },
        list{Html.text("End tutorial")},
      )
    | Default => Vdom.noNode
    }

    let viewStepCount = switch t.tooltipStyle {
    | Tutorial(step) =>
      let (current, total) = currentStepFraction(step)
      Html.p(
        list{Html.class'("step-title")},
        list{Html.text(Printf.sprintf("%d/%d", current, total))},
      )
    | Crud | Default => Vdom.noNode
    }

    let viewNextPrevBtns = switch t.tooltipStyle {
    | Tutorial(step) => viewNavigationBtns(tlid, step, uniqueStr)
    | Crud | Default => Vdom.noNode
    }

    let directionToClass = switch t.align {
    | Top => "above"
    | Bottom => "below"
    | Left => "left-of"
    | Right => "right-of"
    }

    Html.div(
      list{Html.class'("tooltipWrapper")},
      list{
        Html.div(
          ~unique=uniqueStr,
          list{Html.class'("tooltips " ++ directionToClass)},
          list{
            Html.div(
              list{Html.class'("content")},
              list{viewStepCount, viewDesc, viewDetail, viewBtn, viewNextPrevBtns, closeBtn},
            ),
            Html.div(list{Html.class'("tip " ++ t.tipAlignment)}, list{}),
          },
        ),
      },
    )
  } else {
    Vdom.noNode
  }
