open Prelude
open Types
open Tc

let avatarUrl (email : string) (name : string option) : string =
  (* Digest.string is Bucklescript's MD5 *)
  let digestedEmail = Digest.to_hex (Digest.string email) in
  let fallback (name : string option) =
    match name with
    | None | Some "" ->
        (* 'retro' is a fallback style:
         * https://en.gravatar.com/site/implement/images/ *)
        "retro"
    | Some name ->
        let initials =
          String.split ~on:" " name
          |> List.map ~f:(fun s -> s |> String.slice ~from:0 ~to_:1)
          |> String.join ~sep:"+"
        in
        (* TODO: we can set bg/fg color, font size/color/weight, make it
         * circular: https://ui-avatars.com/
         * Note that since we're using this with gravatar, we want the
         * nested-dir style format, not the query param format *)
        "https://ui-avatars.com/api" ^ "/" ^ initials
  in
  (* TODO: add a s= param to set the size in pixels *)
  "https://www.gravatar.com/avatar/"
  ^ digestedEmail
  ^ "?d="
  ^ Js_global.encodeURI (fallback name)


let avatarDiv (avatar : avatar) : msg Html.html =
  let name : string option = avatar.fullname in
  let email : string = avatar.email in
  let username : string = avatar.username in
  let avActiveTimestamp : float = avatar.serverTime |> Js.Date.valueOf in
  let minusThreeMins : float = Js.Date.now () -. (3.0 *. 60.0 *. 1000.0) in
  let inactive : bool = minusThreeMins > avActiveTimestamp in
  Html.img
    [ Html.classList [("avatar", true); ("inactive", inactive)]
    ; Html.src (avatarUrl email name)
    ; Vdom.prop "alt" username ]
    []


let viewAvatars (avatars : avatarsList) : msg Html.html =
  let renderAvatar (a : avatar) = avatarDiv a in
  let avatars = List.map ~f:renderAvatar avatars in
  Html.div [Html.class' "avatars"] avatars


let viewAllAvatars (avatars : avatarsList) : msg Html.html =
  let avatars = avatars |> List.uniqueBy ~f:(fun avatar -> avatar.username) in
  let avatarView = List.map ~f:avatarDiv avatars in
  Html.div
    [Html.class' "all-avatars"]
    [Html.div [Html.class' "avatars-wrapper"] avatarView]
