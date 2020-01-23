(* This is a stateless component, we modify the model state directly *)

open Prelude

let onClick key fn = ViewUtils.eventNoPropagation ~key "click" fn

let fontAwesome = ViewUtils.fontAwesome

let toggleButton = ViewUtils.toggleIconButton

type menuItem =
  { title : string
  ; key : string
  ; icon : string option
  ; action : mouseEvent -> msg
  ; condition : (bool * string) option }

(*
type state = { isOpened : bool; ofTL : Types.tlid; items : menuItem list } 

let init ?(items=[]) (ofTL : Types.tlid) : state = { isOpened = false; items; ofTL}

type msg =
  | OpenMenu
  | CloseMenu
  [@@bs.deriving {accessors}]
*)

let update (m : model) (tlid : tlid) (msg : menuMsg) : model =
  let tlMenus =
    m.tlMenus
    |> TLIDDict.update ~tlid ~f:(fun _s ->
           (* let oldS = s |> Option.withDefault ~default in *)
           let newS =
             match msg with
             | OpenMenu ->
                 {isOpen = true}
             | CloseMenu ->
                 {isOpen = false}
           in
           Some newS)
  in
  {m with tlMenus}


let closeMenu (m : model) : model =
  match Prelude.tlidOf m.cursorState with
  | Some tlid ->
      update m tlid CloseMenu
  | None ->
      m


(*
let view (s: menuState) (tl : toplevel) : msg Html.html =
*)

let viewMenu (s : menuState) (tlid : tlid) (items : menuItem list) :
    msg Html.html =
  let strTLID = showTLID tlid in
  let showMenu = s.isOpen in
  let actions =
    items
    |> List.map ~f:(fun i ->
           let icon =
             match i.icon with
             | Some iconName ->
                 fontAwesome iconName
             | None ->
                 Vdom.noNode
           in
           Html.div
             [onClick (i.key ^ strTLID) i.action]
             [icon; Html.text i.title])
  in
  let toggleMenu =
    toggleButton
      ~name:"toggle-btn"
      ~activeIcon:"bars"
      ~inactiveIcon:"bars"
      ~msg:(fun _ -> TLMenuMsg (tlid, if showMenu then CloseMenu else OpenMenu))
      ~active:showMenu
      ~key:("toggle-tl-menu-" ^ strTLID)
  in
  Html.div
    [Html.classList [("more-actions", true); ("show", showMenu)]]
    [ toggleMenu
    ; Html.div
        [ Html.class' "actions"
        ; ViewUtils.eventNoPropagation
            ~key:("hide-tl-opts" ^ strTLID)
            "mouseleave"
            (fun _ -> TLMenuMsg (tlid, CloseMenu)) ]
        actions ]
