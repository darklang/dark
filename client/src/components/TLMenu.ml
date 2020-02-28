open Prelude

let onClick key fn = ViewUtils.eventNoPropagation ~key "click" fn

let fontAwesome = ViewUtils.fontAwesome

type menuItem =
  { title : string
  ; key : string
  ; icon : string option
  ; action : mouseEvent -> msg
  ; disableMsg : string option }

let isOpen (m : model) (tlid : tlid) : bool =
  m.tlMenus
  |> TLIDDict.get ~tlid
  |> Option.map ~f:(fun o -> o.isOpen)
  |> Option.withDefault ~default:false


let update (m : model) (tlid : tlid) (msg : menuMsg) : model =
  let tlMenus =
    m.tlMenus
    |> TLIDDict.update ~tlid ~f:(fun _s ->
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


let viewItem (keyID : string) (i : menuItem) : msg Html.html =
  let icon =
    match i.icon with
    | Some iconName ->
        fontAwesome iconName
    | None ->
        Vdom.noNode
  in
  let classes = ["item"; i.key] in
  let attrs =
    match i.disableMsg with
    | Some msg ->
        let classes = "disable" :: classes in
        [Html.class' (classes |> String.join ~sep:" "); Html.title msg]
    | None ->
        [ Html.class' (classes |> String.join ~sep:" ")
        ; onClick (i.key ^ keyID) i.action ]
  in
  Html.div attrs [icon; Html.text i.title]


let viewMenu (s : menuState) (tlid : tlid) (items : menuItem list) :
    msg Html.html =
  let strTLID = showTLID tlid in
  let showMenu = s.isOpen in
  let actions = List.map ~f:(viewItem strTLID) items in
  let toggleMenu =
    let cacheKey =
      "toggle-tl-menu-" ^ strTLID ^ "-" ^ string_of_bool showMenu
    in
    Html.div
      [ Html.classList [("toggle-btn", true); ("active", showMenu)]
      ; onClick cacheKey (fun _ ->
            TLMenuMsg (tlid, if showMenu then CloseMenu else OpenMenu)) ]
      [fontAwesome "bars"]
  in
  Html.div
    [ Html.classList [("more-actions", true); ("show", showMenu)]
      (* Block opening the omnibox here by preventing canvas pan start *)
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.eventPreventDefault
        ~key:("hide-tl-opts" ^ strTLID)
        "mouseleave"
        (fun _ -> TLMenuMsg (tlid, CloseMenu)) ]
    [toggleMenu; Html.div [Html.class' "actions"] actions]
