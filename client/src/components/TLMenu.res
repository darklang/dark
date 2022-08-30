open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module Msg = AppTypes.Msg

let onClick = (key, fn) => EventListeners.eventNoPropagation(~key, "click", fn)

module M = AppTypes.Menu
type t = AppTypes.Menu.t

type menuItem = {
  title: string,
  key: string,
  icon: option<string>,
  action: MouseEvent.t => AppTypes.msg,
  disableMsg: option<string>,
}

let isOpen = (m: AppTypes.model, tlid: TLID.t): bool =>
  m.tlMenus
  |> Map.get(~key=tlid)
  |> Option.map(~f=(menu: t) => menu.isOpen)
  |> Option.unwrap(~default=false)

let resetMenu = (tlid: TLID.t, m: AppTypes.model): AppTypes.model => {
  let tlMenus = m.tlMenus |> Map.update(~key=tlid, ~f=_ => Some(M.default))

  {...m, tlMenus: tlMenus}
}

let update = (m: AppTypes.model, tlid: TLID.t, msg: M.msg): AppTypes.model => {
  let tlMenus = m.tlMenus |> Map.update(~key=tlid, ~f=_s => {
    let newS = switch msg {
    | OpenMenu => ({isOpen: true}: t)
    | CloseMenu => {isOpen: false}
    }

    Some(newS)
  })

  {...m, tlMenus: tlMenus}
}

let closeMenu = (m: AppTypes.model): AppTypes.model =>
  switch CursorState.tlidOf(m.cursorState) {
  | Some(tlid) => update(m, tlid, CloseMenu)
  | None => m
  }

let viewItem = (keyID: string, i: menuItem): Html.html<AppTypes.msg> => {
  let icon = switch i.icon {
  | Some(iconName) => Icons.fontAwesome(iconName)
  | None => Vdom.noNode
  }

  let classes = list{"item", i.key}
  let attrs = switch i.disableMsg {
  | Some(msg) =>
    let classes = list{"disable", ...classes}
    list{Attrs.class'(classes |> String.join(~sep=" ")), Attrs.title(msg)}
  | None => list{Attrs.class'(classes |> String.join(~sep=" ")), onClick(i.key ++ keyID, i.action)}
  }

  Html.div(attrs, list{icon, Html.text(i.title)})
}

let viewMenu = (s: M.t, tlid: TLID.t, items: list<menuItem>): Html.html<AppTypes.msg> => {
  let strTLID = TLID.toString(tlid)
  let showMenu = s.isOpen
  let actions = List.map(~f=viewItem(strTLID), items)
  let toggleMenu = {
    let cacheKey = "toggle-tl-menu-" ++ (strTLID ++ ("-" ++ string_of_bool(showMenu)))

    Html.div(
      list{
        Attrs.classList(list{("toggle-btn", true), ("active", showMenu)}),
        onClick(cacheKey, _ => Msg.TLMenuMsg(
          tlid,
          if showMenu {
            CloseMenu
          } else {
            OpenMenu
          },
        )),
      },
      list{Icons.fontAwesome("bars")},
    )
  }

  Html.div(
    list{
      Attrs.classList(list{("more-actions", true), ("show", showMenu)}),
      // Block opening the omnibox here by preventing canvas pan start
      EventListeners.nothingMouseEvent("mousedown"),
      EventListeners.eventPreventDefault(
        ~key="hide-tl-opts" ++ strTLID,
        "mouseleave",
        _ => Msg.TLMenuMsg(tlid, CloseMenu),
      ),
    },
    list{toggleMenu, Html.div(list{Attrs.class'("actions")}, actions)},
  )
}
