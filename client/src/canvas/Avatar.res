open Prelude

module Html = Tea.Html
module Attrs = Tea.Html.Attributes

module Avatar = AppTypes.Avatar

type msg = AppTypes.msg

let filterAvatarsByTlid = (avatars: list<Avatar.t>, tlid: TLID.t): list<Avatar.t> =>
  avatars |> List.filter(~f=(av: Avatar.t) =>
    switch av.tlid {
    | None => false
    | Some(avTlid) => avTlid === (tlid |> TLID.toString)
    }
  )

let avatarUrl = (email: string, name: option<string>): string => {
  // Digest.string is ReScript's MD5
  let digestedEmail = Digest.to_hex(Digest.string(email))
  let fallback = (name: option<string>) =>
    switch name {
    | None | Some("") => /* 'retro' is a fallback style:
       * https://en.gravatar.com/site/implement/images/ */
      "retro"
    | Some(name) =>
      let initials =
        String.split(~on=" ", name)
        |> List.map(~f=s => s |> String.slice(~from=0, ~to_=1))
        |> String.join(~sep="+")

      /* TODO: we can set bg/fg color, font size/color/weight, make it
       * circular: https://ui-avatars.com/
       * Note that since we're using this with gravatar, we want the
       * nested-dir style format, not the query param format */
      "https://ui-avatars.com/api" ++ ("/" ++ initials)
    }

  // TODO: add a s= param to set the size in pixels
  "https://www.gravatar.com/avatar/" ++
  (digestedEmail ++
  ("?d=" ++ Js_global.encodeURI(fallback(name))))
}

module Styles = {
  open Tailwind

  let main = Many([p0, w12, h12, border3, roundedFull, borderSolid, borderColorb18bba])

  let topbar = Many([py0_5, px1_25, w8, h8, roundedFull])

  let toplevel = Many([w6, h6, py0_5, px1_25, roundedFull])
}

let avatarDiv = (avatar: Avatar.t, style: Tailwind.t): Html.html<msg> => {
  open Tailwind
  let name: option<string> = avatar.fullname
  let email: string = avatar.email
  let username: string = avatar.username
  let avActiveTimestamp: float = avatar.serverTime |> Js.Date.valueOf
  let minusThreeMins: float = Js.Date.now() -. 3.0 *. 60.0 *. 1000.0
  let inactive: t = if minusThreeMins > avActiveTimestamp {
    opacity50
  } else {
    none
  }
  Html.img(
    list{twProp([style, inactive]), Attrs.src(avatarUrl(email, name)), Vdom.prop("alt", username)},
    list{},
  )
}

let viewToplevelAvatars = (avatars: list<Avatar.t>, tlid: TLID.t): Html.html<msg> => {
  let avList = filterAvatarsByTlid(avatars, tlid)
  let renderAvatar = (a: Avatar.t) => avatarDiv(a, Styles.toplevel)
  let avatars = List.map(~f=renderAvatar, avList)
  open Tailwind
  Html.div(list{twProp([flex, flexCol])}, avatars)
}

let viewAllAvatars = (avatars: list<Avatar.t>): Html.html<msg> => {
  // Sort by serverTime desc, then unique by avatar - gets us the most recent
  // avatar for a given username
  let avatars =
    avatars
    |> List.sortBy(~f=(avatar: Avatar.t) => avatar.serverTime)
    |> List.reverse
    |> List.uniqueBy(~f=(avatar: Avatar.t) => avatar.username)

  let avatarView = List.map(~f=avatar => avatarDiv(avatar, Styles.topbar), avatars)
  open Tailwind
  let outerStyle = Many([
    flex,
    flexCol,
    itemsCenter,
    top0,
    right20,
    fixed,
    bgBlack2,
    textXxs,
    textGrey3,
    overflowYScroll,
    my0,
    mx1_25,
    py1_25,
    px2_5,
    roundedBXl,
  ])

  let hide = if avatars == list{} {
    invisible
  } else {
    none
  }
  Html.div(
    list{twProp([outerStyle, hide])},
    list{Html.div(list{Attrs.class'("avatars-wrapper")}, avatarView), Html.text("Other users")},
  )
}

let myAvatar = (m: AppTypes.model): Avatar.t => {
  canvasId: m.canvasName,
  canvasName: m.canvasName,
  serverTime: Js.Date.make(),
  tlid: None,
  username: m.account.username,
  email: m.account.email,
  fullname: Some(m.account.name),
  browserId: m.browserId,
}
