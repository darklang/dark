open Prelude

/* Dark */
module Cmd = Tea.Cmd
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard
module Html = Tea_html_extended
module T = SettingsViewTypes

type settingsViewState = T.settingsViewState

type settingsTab = T.settingsTab

let fontAwesome = ViewUtils.fontAwesome

let allTabs = list{T.UserSettings, T.Privacy, T.InviteUser(T.defaultInviteFields)}

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
  | _ => /* shouldnt get here */
    (false, tab)
  }

let submitForm = (m: Types.model): (Types.model, Cmd.t<Types.msg>) => {
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
  | SetSettingsView(canvasList, username, orgs, orgCanvasList) => {
      ...settingsView,
      canvasList: canvasList,
      username: username,
      orgs: orgs,
      orgCanvasList: orgCanvasList,
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
  }

let getModifications = (m: Types.model, msg: T.settingsMsg): list<Types.modification> =>
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
  | _ => list{SettingsViewUpdate(msg)}
  }

/* View functions */

let settingsTabToText = (tab: T.settingsTab): string =>
  switch tab {
  | NewCanvas => "NewCanvas"
  | UserSettings => "Canvases"
  | InviteUser(_) => "Share"
  | Privacy => "Privacy"
  }

/* View code */

let viewUserCanvases = (acc: T.settingsViewState): list<Html.html<Types.msg>> => {
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

let viewInviteUserToDark = (svs: T.settingsViewState): list<Html.html<Types.msg>> => {
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
            _ => Types.SettingsViewMsg(SubmitForm),
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
                      Events.onInput(str => Types.SettingsViewMsg(UpdateInviteForm(str))),
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

let viewNewCanvas = (svs: settingsViewState): list<Html.html<Types.msg>> => {
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

let viewPrivacy = (s: T.privacySettings): list<Html.html<Types.msg>> => list{
  FullstoryView.consentRow(s.recordConsent, ~longLabels=false),
}

let settingsTabToHtml = (svs: settingsViewState): list<Html.html<Types.msg>> => {
  let tab = svs.tab
  switch tab {
  | NewCanvas => viewNewCanvas(svs)
  | UserSettings => viewUserCanvases(svs)
  | InviteUser(_) => viewInviteUserToDark(svs)
  | Privacy => viewPrivacy(svs.privacy)
  }
}

let tabTitleView = (tab: settingsTab): Html.html<Types.msg> => {
  let tabTitle = (t: settingsTab) => {
    let isSameTab = switch (tab, t) {
    | (InviteUser(_), InviteUser(_)) => true
    | _ => tab === t
    }

    Html.h3(
      list{
        Html.classList(list{("tab-title", true), ("selected", isSameTab)}),
        ViewUtils.eventNoPropagation(
          ~key="close-settings-modal",
          "click",
          _ => Types.SettingsViewMsg(SwitchSettingsTabs(t)),
        ),
      },
      list{Html.text(settingsTabToText(t))},
    )
  }

  Html.div(list{Html.class'("settings-tab-titles")}, List.map(allTabs, ~f=tabTitle))
}

let onKeydown = (evt: Web.Node.event): option<Types.msg> =>
  K.eventToKeyEvent(evt) |> Option.andThen(~f=e =>
    switch e {
    | {K.key: K.Enter, _} => Some(Types.SettingsViewMsg(SubmitForm))
    | _ => None
    }
  )

let settingViewWrapper = (acc: settingsViewState): Html.html<Types.msg> => {
  let tabView = settingsTabToHtml(acc)
  Html.div(
    list{Html.class'("settings-tab-wrapper")},
    list{Html.h1(list{}, list{Html.text("Settings")}), tabTitleView(acc.tab), ...tabView},
  )
}

let html = (m: Types.model): Html.html<Types.msg> => {
  let svs = m.settingsView
  let closingBtn = Html.div(
    list{
      Html.class'("close-btn"),
      ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => Types.SettingsViewMsg(
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
      ViewUtils.eventNoPropagation(~key="close-setting-modal", "click", _ => Types.SettingsViewMsg(
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
