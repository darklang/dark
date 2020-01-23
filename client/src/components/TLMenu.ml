open Prelude

let onClick key fn = ViewUtils.eventNoPropagation ~key "click" fn

let fontAwesome = ViewUtils.fontAwesome

type menuItem =
  { title : string
  ; key : string
  ; icon : string option
  ; action : mouseEvent -> msg
  ; disableMsg : string option }

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


let toggleButton
    ~(name : string)
    ~(activeIcon : string)
    ~(inactiveIcon : string)
    ~(msg : mouseEvent -> msg)
    ~(active : bool)
    ~(key : string) : msg Html.html =
  let icon = if active then activeIcon else inactiveIcon in
  let cacheKey = key ^ "-" ^ string_of_bool active in
  Html.div
    [Html.classList [(name, true); ("active", active)]; onClick cacheKey msg]
    [fontAwesome icon]


let viewItem (keyID : string) (i : menuItem) : msg Html.html =
  let icon =
    match i.icon with
    | Some iconName ->
        fontAwesome iconName
    | None ->
        Vdom.noNode
  in
  let attrs =
    match i.disableMsg with
    | Some msg ->
        [Html.class' "item disable"; Html.title msg]
    | None ->
        [Html.class' "item"; onClick (i.key ^ keyID) i.action]
  in
  Html.div attrs [icon; Html.text i.title]


let viewMenu (s : menuState) (tlid : tlid) (items : menuItem list) :
    msg Html.html =
  let strTLID = showTLID tlid in
  let showMenu = s.isOpen in
  let actions = List.map ~f:(viewItem strTLID) items in
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
