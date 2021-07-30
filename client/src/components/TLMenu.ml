open Prelude

let onClick key fn = ViewUtils.eventNoPropagation ~key "click" fn

let fontAwesome = ViewUtils.fontAwesome

type menuItem =
  { title : string
  ; key : string
  ; icon : string option
  ; action : mouseEvent -> msg
  ; disableMsg : string option }

let isOpen (m : model) (tlid : TLID.t) : bool =
  m.tlMenus
  |> Map.get ~key:tlid
  |> Option.map ~f:(fun o -> o.isOpen)
  |> Option.unwrap ~default:false


let resetMenu (tlid : TLID.t) (m : model) : model =
  let tlMenus =
    m.tlMenus |> Map.update ~key:tlid ~f:(fun _ -> Some Defaults.defaultMenu)
  in
  {m with tlMenus}


let update (m : model) (tlid : TLID.t) (msg : menuMsg) : model =
  let tlMenus =
    m.tlMenus
    |> Map.update ~key:tlid ~f:(fun _s ->
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
  match CursorState.tlidOf m.cursorState with
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


let viewMenu (s : menuState) (tlid : TLID.t) (items : menuItem list) :
    msg Html.html =
  let strTLID = TLID.toString tlid in
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
