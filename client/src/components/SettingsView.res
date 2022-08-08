open Prelude

// Dark
module Cmd = Tea.Cmd
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard
module Html = Tea_html_extended
module T = SettingsViewTypes

module Msg = AppTypes.Msg
type msg = AppTypes.msg

type settingsViewState = T.settingsViewState

type settingsTab = T.settingsTab

let fontAwesome = ViewUtils.fontAwesome

let allTabs = list{T.UserSettings, T.Privacy, T.InviteUser(T.defaultInviteFields), T.Editor}

let validateEmail = (email: T.formField): T.formField => {
  let error = {
    let emailVal = email.value
    if String.length(emailVal) == 0 {
      Some("Field Required")
    } else if !Entry.validateEmail(emailVal) {
      Some("Invalid Email")
    } else {
      None
    }
  }

  {...email, error: error}
}

let validateForm = (tab: T.settingsTab): (bool, T.settingsTab) =>
  switch tab {
  | InviteUser(form) =>
    let text = validateEmail(form.email)
    let email = {T.email: text}
    let isInvalid = Option.is_some(text.error)
    (isInvalid, InviteUser(email))
  | _ => // shouldnt get here
    (false, tab)
  }

let submitForm = (m: AppTypes.model): (AppTypes.model, AppTypes.cmd) => {
  let tab = m.settingsView.tab
  switch tab {
  | InviteUser(info) =>
    let sendInviteMsg = {
      email: info.email.value,
      T.inviterUsername: m.username,
      inviterName: m.account.name,
    }

    ({...m, settingsView: {...m.settingsView, loading: true}}, API.sendInvite(m, sendInviteMsg))
  | _ => (m, Cmd.none)
  }
}

let update = (settingsView: T.settingsViewState, msg: T.settingsMsg): T.settingsViewState =>
  switch msg {
  | SetSettingsView(canvasList, username, orgs, orgCanvasList, isContributor) => {
      ...settingsView,
      canvasList: canvasList,
      username: username,
      orgs: orgs,
      orgCanvasList: orgCanvasList,
      isContributor: isContributor
    }
  | OpenSettingsView(tab) => {...settingsView, opened: true, tab: tab, loading: false}
  | CloseSettingsView(_) => {...settingsView, opened: false, loading: false}
  | SwitchSettingsTabs(tab) => {...settingsView, tab: tab, loading: false}
  | UpdateInviteForm(value) =>
    let form = {T.email: {value: value, error: None}}
    {...settingsView, tab: InviteUser(form)}
  | TriggerSendInviteCallback(Ok(_)) => {
      ...settingsView,
      tab: InviteUser(T.defaultInviteFields),
      loading: false,
    }
  | TriggerSendInviteCallback(Error(_)) => {
      ...settingsView,
      tab: InviteUser(T.defaultInviteFields),
      loading: false,
    }
  | SubmitForm => settingsView
  | InitRecordConsent(recordConsent) => {...settingsView, privacy: {recordConsent: recordConsent}}
  | SetRecordConsent(allow) => {...settingsView, privacy: {recordConsent: Some(allow)}}
  | SetIsContributor(isContributor) => {...settingsView, isContributor: isContributor}
  }

let getModifications = (m: AppTypes.model, msg: T.settingsMsg): list<AppTypes.modification> =>
  switch msg {
  | TriggerSendInviteCallback(Error(err)) => list{
      SettingsViewUpdate(msg),
      HandleAPIError(
        APIError.make(
          ~context="TriggerSendInviteCallback",
          ~importance=ImportantError,
          ~reload=false,
          err,
        ),
      ),
    }
  | OpenSettingsView(tab) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Url.navigateTo(SettingsModal(tab))
          ({...m, cursorState: Deselected, currentPage: SettingsModal(tab)}, cmd)
        },
      ),
    }
  | TriggerSendInviteCallback(Ok(_)) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => ({...m, toast: {toastMessage: Some("Sent!"), toastPos: None}}, Cmd.none),
      ),
    }
  | CloseSettingsView(_) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => ({...m, canvasProps: {...m.canvasProps, enablePan: true}}, Cmd.none),
      ),
      Deselect,
      MakeCmd(Url.navigateTo(Architecture)),
    }
  | SwitchSettingsTabs(tab) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Url.navigateTo(SettingsModal(tab))
          ({...m, currentPage: SettingsModal(tab)}, cmd)
        },
      ),
    }
  | SubmitForm =>
    let (isInvalid, newTab) = validateForm(m.settingsView.tab)
    if isInvalid {
      list{
        SettingsViewUpdate(msg),
        ReplaceAllModificationsWithThisOne(
          m => ({...m, settingsView: {...m.settingsView, tab: newTab}}, Cmd.none),
        ),
      }
    } else {
      list{SettingsViewUpdate(msg), ReplaceAllModificationsWithThisOne(m => submitForm(m))}
    }
  | SetRecordConsent(allow) => list{
      SettingsViewUpdate(msg),
      MakeCmd(FullstoryView.FullstoryJs.setConsent(allow)),
    }
  | SetIsContributor(isContributor) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Cmd.none

          if (isContributor) {
            ({...m, editorSettings: { ...m.editorSettings, contributorSettings: Some(AppTypes.EditorSettings.ContributorSettings.default) }}, cmd)
          } else{
            ({...m, editorSettings: { ...m.editorSettings, contributorSettings: None } }, cmd)
          }
        }
      )
    }
  | _ => list{SettingsViewUpdate(msg)}
  }

// View functions

let settingsTabToText = (tab: T.settingsTab): string =>
  switch tab {
  | NewCanvas => "NewCanvas"
  | UserSettings => "Canvases"
  | InviteUser(_) => "Share"
  | Privacy => "Privacy"
  | Editor => "Editor"
  }

// View code

let viewUserCanvases = (acc: T.settingsViewState): list<Html.html<msg>> => {
  let canvasLink = c => {
    let url = "/a/" ++ c
    Html.li(~unique=c, list{}, list{Html.a(list{Html.href(url)}, list{Html.text(url)})})
  }

  let canvases = if List.length(acc.canvasList) > 0 {
    List.map(acc.canvasList, ~f=canvasLink) |> Html.ul(list{})
  } else {
    Html.p(list{}, list{Html.text("No other personal canvases")})
  }

  let canvasView = list{
    Html.p(list{Html.class'("canvas-list-title")}, list{Html.text("Personal canvases:")}),
    Html.div(list{Html.class'("canvas-list")}, list{canvases}),
    Html.p(list{}, list{Html.text("Create a new canvas by navigating to the URL")}),
  }

  let orgs = List.map(acc.orgCanvasList, ~f=canvasLink) |> Html.ul(list{})
  let orgView = if List.length(acc.orgCanvasList) > 0 {
    list{
      Html.p(list{Html.class'("canvas-list-title")}, list{Html.text("Shared canvases:")}),
      Html.div(list{Html.class'("canvas-list")}, list{orgs}),
    }
  } else {
    list{Vdom.noNode}
  }

  Belt.List.concat(orgView, canvasView)
}

let viewInviteUserToDark = (svs: T.settingsViewState): list<Html.html<msg>> => {
  let introText = list{
    Html.h2(list{}, list{Html.text("Share Dark with a friend or colleague")}),
    Html.p(
      list{},
      list{
        Html.text(
          "Share the love! Invite a friend, and we'll send them an email saying you invited them.",
        ),
      },
    ),
    Html.p(
      list{},
      list{
        Html.text(
          "Note: This will not add them to any of your existing organizations or canvases.",
        ),
      },
    ),
  }

  let (error, inputVal) = switch svs.tab {
  | InviteUser(x) => (x.email.error |> Option.unwrap(~default=""), x.email.value)
  | _ => ("", "")
  }

  let inviteform = {
    let submitBtn = {
      let btn = if svs.loading {
        list{ViewUtils.fontAwesome("spinner"), Html.h3(list{}, list{Html.text("Loading")})}
      } else {
        list{Html.h3(list{}, list{Html.text("Send invite")})}
      }

      Html.button(
        list{
          Html.class'("submit-btn"),
          Html.Attributes.disabled(svs.loading),
          ViewUtils.eventNoPropagation(
            ~key="close-settings-modal",
            "click",
            _ => Msg.SettingsViewMsg(SubmitForm),
          ),
        },
        btn,
      )
    }

    list{
      Html.div(
        list{Html.class'("invite-form")},
        list{
          Html.div(
            list{Html.class'("form-field")},
            list{
              Html.h3(list{}, list{Html.text("Email:")}),
              Html.div(
                list{},
                list{
                  Html.input'(
                    list{
                      Vdom.attribute("", "spellcheck", "false"),
                      Events.onInput(str => Msg.SettingsViewMsg(UpdateInviteForm(str))),
                      Attributes.value(inputVal),
                    },
                    list{},
                  ),
                  Html.p(list{Html.class'("error-text")}, list{Html.text(error)}),
                },
              ),
            },
          ),
          submitBtn,
        },
      ),
    }
  }

  Belt.List.concat(introText, inviteform)
}

let viewNewCanvas = (svs: settingsViewState): list<Html.html<msg>> => {
  let text = Printf.sprintf(
    "Create a new canvas (or go to it if it already exists) by visiting /a/%s-canvasname",
    svs.username,
  )

  let text = if List.isEmpty(svs.orgs) {
    text ++ "."
  } else {
    text ++
    Printf.sprintf(
      " or /a/orgname-canvasname, where orgname may be any of (%s).",
      svs.orgs |> String.join(~sep=", "),
    )
  }

  let introText = list{
    Html.h2(list{}, list{Html.text("New Canvas")}),
    Html.p(list{}, list{Html.text(text)}),
  }

  introText
}

let viewPrivacy = (s: T.privacySettings): list<Html.html<msg>> => list{
  FullstoryView.consentRow(s.recordConsent, ~longLabels=false),
}

let viewEditorSettings = (isContributor: bool): list<Html.html<msg>> => {
  let disableOmniOpen = ViewUtils.nothingMouseEvent("mousedown")

  let radio = (
    ~value: string,
    ~label: string,
    ~msg: SettingsViewTypes.settingsMsg,
    ~checked: bool,
  ): Html.html<msg> => {
    let key = "dark-contributor-" ++ value
    Html.div(
      list{Html.class'("choice"), disableOmniOpen},
      list{
        Html.input'(
          list{
            Html.type'("radio"),
            Html.id(key),
            Html.name("dark-contributor"),
            Html.value(value),
            Html.checked(checked),
            ViewUtils.eventNoPropagation(~key, "click", _ => SettingsViewMsg(msg)),
          },
          list{},
        ),
        Html.label(list{Html.for'(key)}, list{Html.text(label)}),
      },
    )
  }

  let (yes, no) = ("Yes", "No")

  list{
    Html.div(
      list{Html.class'("setting-row")},
      list{
        Html.div(
          list{Html.class'("setting-label")},
          list{
            Html.div(list{Html.class'("title")}, list{Html.text("I'm contributing to Dark's source code")}),
            Html.div(
              list{Html.class'("description")},
              list{Html.text(
                "Extra debugger tools are available to those who contribute to Dark's source code.
                When set to true, the in-Editor debugger will show whenever the sidebar is collapsed."
              )}),
          },
        ),
        Html.div(
          list{Html.class'("setting-control")},
          list{
            radio(
              ~value="yes",
              ~label=yes,
              ~msg=SetIsContributor(true),
              ~checked=isContributor == true,
            ),
            radio(
              ~value="no",
              ~label=no,
              ~msg=SetIsContributor(false),
              ~checked=isContributor == false,
            ),
          },
        ),
      },
    )
  }
}

let settingsTabToHtml = (svs: settingsViewState): list<Html.html<msg>> => {
  let tab = svs.tab
  switch tab {
  | NewCanvas => viewNewCanvas(svs)
  | UserSettings => viewUserCanvases(svs)
  | InviteUser(_) => viewInviteUserToDark(svs)
  | Privacy => viewPrivacy(svs.privacy)
  | Editor => viewEditorSettings(svs.isContributor)
  }
}

let tabTitleView = (tab: settingsTab): Html.html<msg> => {
  let tabTitle = (t: settingsTab) => {
    let isSameTab = switch (tab, t) {
    | (InviteUser(_), InviteUser(_)) => true
    | _ => tab === t
    }

    Html.h3(
      list{
        Html.classList(list{("tab-title", true), ("selected", isSameTab)}),
        ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => Msg.SettingsViewMsg(
          SwitchSettingsTabs(t),
        )),
      },
      list{Html.text(settingsTabToText(t))},
    )
  }

  Html.div(list{Html.class'("settings-tab-titles")}, List.map(allTabs, ~f=tabTitle))
}

let onKeydown = (evt: Web.Node.event): option<msg> =>
  K.eventToKeyEvent(evt) |> Option.andThen(~f=e =>
    switch e {
    | {K.key: K.Enter, _} => Some(Msg.SettingsViewMsg(SubmitForm))
    | _ => None
    }
  )

let settingViewWrapper = (acc: settingsViewState): Html.html<msg> => {
  let tabView = settingsTabToHtml(acc)
  Html.div(
    list{Html.class'("settings-tab-wrapper")},
    list{Html.h1(list{}, list{Html.text("Settings")}), tabTitleView(acc.tab), ...tabView},
  )
}

let html = (m: AppTypes.model): Html.html<msg> => {
  let svs = m.settingsView
  let closingBtn = Html.div(
    list{
      Html.class'("close-btn"),
      ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => Msg.SettingsViewMsg(
        CloseSettingsView(svs.tab),
      )),
    },
    list{fontAwesome("times")},
  )

  Html.div(
    list{
      Html.class'("settings modal-overlay"),
      ViewUtils.nothingMouseEvent("mousedown"),
      ViewUtils.nothingMouseEvent("mouseup"),
      ViewUtils.eventNoPropagation(~key="close-setting-modal", "click", _ => Msg.SettingsViewMsg(
        CloseSettingsView(svs.tab),
      )),
    },
    list{
      Html.div(
        list{
          Html.class'("modal"),
          ViewUtils.nothingMouseEvent("click"),
          ViewUtils.eventNoPropagation(~key="ept", "mouseenter", _ => EnablePanning(false)),
          ViewUtils.eventNoPropagation(~key="epf", "mouseleave", _ => EnablePanning(true)),
          Html.onCB("keydown", "keydown", onKeydown),
        },
        list{settingViewWrapper(svs), closingBtn},
      ),
    },
  )
}
