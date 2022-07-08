open Prelude

let onClick = (key, fn) => ViewUtils.eventNoPropagation(~key, "click", fn)

let fontAwesome = ViewUtils.fontAwesome

type menuItem = {
  title: string,
  key: string,
  icon: option<string>,
  action: mouseEvent => msg,
  disableMsg: option<string>,
}

let isOpen = (m: model, tlid: TLID.t): bool =>
  m.tlMenus |> Map.get(~key=tlid) |> Option.map(~f=o => o.isOpen) |> Option.unwrap(~default=false)

let resetMenu = (tlid: TLID.t, m: model): model => {
  let tlMenus = m.tlMenus |> Map.update(~key=tlid, ~f=_ => Some(Defaults.defaultMenu))

  {...m, tlMenus: tlMenus}
}

let update = (m: model, tlid: TLID.t, msg: menuMsg): model => {
  let tlMenus = m.tlMenus |> Map.update(~key=tlid, ~f=_s => {
    let newS = switch msg {
    | OpenMenu => {isOpen: true}
    | CloseMenu => {isOpen: false}
    }

    Some(newS)
  })

  {...m, tlMenus: tlMenus}
}

let closeMenu = (m: model): model =>
  switch CursorState.tlidOf(m.cursorState) {
  | Some(tlid) => update(m, tlid, CloseMenu)
  | None => m
  }

let viewItem = (keyID: string, i: menuItem): Html.html<msg> => {
  let icon = switch i.icon {
  | Some(iconName) => fontAwesome(iconName)
  | None => Vdom.noNode
  }

  let classes = list{"item", i.key}
  let attrs = switch i.disableMsg {
  | Some(msg) =>
    let classes = list{"disable", ...classes}
    list{Html.class'(classes |> String.join(~sep=" ")), Html.title(msg)}
  | None => list{Html.class'(classes |> String.join(~sep=" ")), onClick(i.key ++ keyID, i.action)}
  }

  Html.div(attrs, list{icon, Html.text(i.title)})
}

let viewMenu = (s: menuState, tlid: TLID.t, items: list<menuItem>): Html.html<msg> => {
  let strTLID = TLID.toString(tlid)
  let showMenu = s.isOpen
  let actions = List.map(~f=viewItem(strTLID), items)
  let toggleMenu = {
    let cacheKey = "toggle-tl-menu-" ++ (strTLID ++ ("-" ++ string_of_bool(showMenu)))

    Html.div(
      list{
        Html.classList(list{("toggle-btn", true), ("active", showMenu)}),
        onClick(cacheKey, _ => TLMenuMsg(
          tlid,
          if showMenu {
            CloseMenu
          } else {
            OpenMenu
          },
        )),
      },
      list{fontAwesome("bars")},
    )
  }

  Html.div(
    list{
      Html.classList(list{("more-actions", true), ("show", showMenu)}),
      // Block opening the omnibox here by preventing canvas pan start
      ViewUtils.nothingMouseEvent("mousedown"),
      ViewUtils.eventPreventDefault(~key="hide-tl-opts" ++ strTLID, "mouseleave", _ => TLMenuMsg(
        tlid,
        CloseMenu,
      )),
    },
    list{toggleMenu, Html.div(list{Html.class'("actions")}, actions)},
  )
}
