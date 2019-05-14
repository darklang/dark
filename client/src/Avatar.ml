open Prelude
open Types

let avatarUrl (email : string) (name : string option) : string =
  let fallback (name : string option) =
    match name with
    | None | Some "" ->
        (* 'retro' is a fallback style:
         * https://en.gravatar.com/site/implement/images/ *)
        "retro"
    | Some name ->
        let initials =
          String.split_on_char ' ' name
          |> List.map (fun s -> String.make 1 s.[0])
          |> String.concat "+"
        in
        (* TODO: we can set bg/fg color, font size/color/weight, make it
         * circular: https://ui-avatars.com/
         * Note that since we're using this with gravatar, we want the
         * nested-dir style format, not the query param format *)
        "https://ui-avatars.com/api" ^ "/" ^ initials
  in
  (* TODO: add a s= param to set the size in pixels *)
  "https://www.gravatar.com/avatar/"
  (* Digest.string is Bucklescript's MD5 *)
  ^ Digest.string email
  ^ "?d="
  ^ Js_global.encodeURI (fallback name)


let avatarDiv (avatar : avatar) : msg Html.html =
  (* TODO name and email from state *)
  let name : string option = avatar.fullName in
  let email : string = avatar.email in
  let username : string = avatar.username in
  let class_ =
    String.concat " " ["avatar"; (if avatar.active then "" else "inactive")]
  in
  Html.div
    [Html.class' class_]
    [Html.img [Html.src (avatarUrl email name); Vdom.prop "alt" username] []]


let avatarsView (avatars : avatarsList) (tl : bool) : msg Html.html =
  let class_ = String.concat " " ["avatars"; (if tl then "active" else "")] in
  let renderAvatar (a : avatar) = avatarDiv a in
  let avatars = List.map renderAvatar avatars in
  Html.div [Html.class' class_] avatars


(* let cols = List.map ~f:(viewDBCol vs true db.dbTLID) migra.cols in *)

let allAvatarsView (avatars : avatarsList) : msg Html.html =
  let renderAvatar (a : avatar) = avatarDiv a in
  let header = Html.p [] [Html.text "Active users"] in
  let avatars = List.map renderAvatar avatars in
  Html.div [Html.class' "all-avatars"] (header :: avatars)
