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
  let main = %twc("p-0 w-12 h-12 border-3 rounded-full border-solid border-purple")

  let topbar = %twc("py-0.5 px-1.25 w-8 h-8 rounded-full")

  let toplevel = %twc("w-6 h-6 py-0.5 px-1.25 rounded-full")
}

let avatarDiv = (~style: string, avatar: Avatar.t): Html.html<msg> => {
  let name: option<string> = avatar.fullname
  let email: string = avatar.email
  let username: string = avatar.username
  let avActiveTimestamp: float = avatar.serverTime |> Js.Date.valueOf
  let minusThreeMins: float = Js.Date.now() -. 3.0 *. 60.0 *. 1000.0
  let inactive = if minusThreeMins > avActiveTimestamp {
    %twc("opacity-50")
  } else {
    ""
  }
  Html.img(
    list{
      Attrs.class(`${style} ${inactive}`),
      Attrs.src(avatarUrl(email, name)),
      Vdom.prop("alt", username),
    },
    list{},
  )
}

let viewToplevelAvatars = (avatars: list<Avatar.t>, tlid: TLID.t): Html.html<msg> => {
  let avList = filterAvatarsByTlid(avatars, tlid)
  let renderAvatar = (a: Avatar.t) => avatarDiv(a, ~style=Styles.toplevel)
  let avatars = List.map(~f=renderAvatar, avList)
  Html.div(list{Attrs.class(%twc("flex flex-col"))}, avatars)
}

let viewAllAvatars = (avatars: list<Avatar.t>): Html.html<msg> => {
  // Sort by serverTime desc, then unique by avatar - gets us the most recent
  // avatar for a given username
  let avatars =
    avatars
    |> List.sortBy(~f=(avatar: Avatar.t) => avatar.serverTime)
    |> List.reverse
    |> List.uniqueBy(~f=(avatar: Avatar.t) => avatar.username)

  let avatarView = List.map(~f=avatar => avatarDiv(avatar, ~style=Styles.topbar), avatars)

  let outerStyle = %twc(
    "flex flex-col items-center top-0 right-20 fixed bg-black2 text-xxs text-grey3 overflow-y-scroll my-0 mx-1.25 py-1.25 px-2.5 rounded-b-xl"
  )

  let hide = if avatars == list{} {
    %twc("invisible")
  } else {
    ""
  }
  Html.div(
    list{Attrs.classes([outerStyle, hide])},
    list{Html.div(list{Attrs.class("avatars-wrapper")}, avatarView), Html.text("Other users")},
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
