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


let avatarDiv (username : string) : msg Html.html =
  (* TODO name and email from state *)
  let name : string option = None in
  let email : string = username in
  Html.div
    []
    [Html.img [Html.src (avatarUrl email name); Vdom.prop "alt" username] []]
