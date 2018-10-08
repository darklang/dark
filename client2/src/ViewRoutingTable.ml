module B = Blank
module Attrs = Html.Attributes
open Prelude
module TL = Toplevel
open Types
open ViewUtils

type entry =
  { name: string option
  ; prefix: string list
  ; verbs: (string * pos) list
  ; tlid: tLID }

let missingEventSpaceDesc = "<missing event space>"

let missingEventRouteDesc = "<missing route>"

let spaceName tl =
  match tl.data with
  | TLHandler h -> h.spec.module_ |> B.toMaybe
  | _ -> Nothing

let splitBySpace tls =
  let spaceName_ tl =
    tl |> TL.asHandler
    |> Maybe.map (fun x -> x.spec)
    |> Maybe.map (fun x -> x.module_)
    |> Maybe.andThen B.toMaybe
    |> Maybe.withDefault missingEventSpaceDesc
  in
  tls |> List.sortBy spaceName_
  |> LE.groupWhile (fun a b -> spaceName_ a == spaceName_ b)
  |> List.map (fun hs ->
         let space =
           hs |> List.head |> Option.getExn "splitBySpace" |> spaceName_
         in
         (space, hs) )

let tl2entry tls =
  tls
  |> List.filterMap (fun tl ->
         match TL.asHandler tl with
         | Just h -> Just (tl.pos, tl.id, h)
         | Nothing -> Nothing )
  |> List.map (fun (pos, tlid, h) ->
         { name=
             (match h.spec.name with F (_, s) -> Just s | Blank _ -> Nothing)
         ; prefix= []
         ; verbs=
             ( match h.spec.modifier with
             | F (_, s) -> [(s, pos)]
             | Blank _ -> [("_", pos)] )
         ; tlid } )
  |> List.sortBy (fun c -> Maybe.withDefault "ZZZZZZ" c.name)

let collapseByVerb es =
  es
  |> LE.groupWhile (fun a b -> a.name == b.name)
  |> List.map
       (List.foldr
          (fun curr list ->
            if curr.name == Nothing then curr :: list
            else
              match list with
              | [] -> [curr]
              | [rest; prev] ->
                  let new_ = {prev with verbs= prev.verbs ++ curr.verbs} in
                  new_ :: rest )
          [])
  |> List.concat

let prefixify hs =
  match hs with
  | [] -> hs
  | [_] -> hs
  | [rest; h] -> (
    match h.name with
    | Nothing -> h :: prefixify rest
    | Just name -> (
        let len = String.length name in
        let _ = "type annotation" in
        let makePrefix h2 =
          match h2.name with
          | Just name2 ->
              let newName = String.dropLeft len name2 in
              {h2 with name= Just newName; prefix= h.prefix ++ [name]}
          | _ -> h2
        in
        let isPrefixOf h2 =
          match h2.name with
          | Nothing -> false
          | Just n2 -> String.startsWith name n2
        in
        match LE.splitWhen (fun h2 -> not (isPrefixOf h2)) rest with
        | Nothing -> h :: (rest |> List.map makePrefix |> prefixify)
        | Just (matched, unmatched) ->
            h :: ((prefixify <| List.map makePrefix matched) ++ unmatched) ) )

let ordering a b =
  match (a, b) with
  | "HTTP", _ -> LT
  | _, "HTTP" -> GT
  | _ ->
      if b == missingEventRouteDesc then LT
      else if a == missingEventRouteDesc then GT
      else compare a b

let undoButton tlid page =
  buttonLink (text "undo" "Restore") (RestoreToplevel tlid) (Just page)

let viewGroup m showLink showUndo (spacename, entries) =
  let def s = Maybe.withDefault missingEventRouteDesc s in
  let externalLink h =
    if showLink == ShowLink && List.member "GET" (List.map Tuple.first h.verbs)
    then
      match h.name with
      | Just n ->
          let target = String.join "" (h.prefix ++ [n]) in
          Html.a
            [ Attrs.class_ "external"
            ; Attrs.href
                ( "//"
                ++ Http.encodeUri m.canvasName
                ++ "." ++ m.userContentHost ++ target )
            ; Attrs.target "_blank" ]
            [fontAwesome "external-link-alt"]
      | Nothing -> Html.div [] []
    else Html.div [] []
  in
  let verbs e =
    e.verbs
    |> List.map (fun (verb, pos) -> tlLink pos "verb-link" verb)
    |> List.intersperse (Html.text ",")
  in
  let entryHtml e =
    let pos =
      e.verbs |> List.head
      |> Option.getExn "viewGroup/entryHtml"
      |> Tuple.second
    in
    div "handler"
      ( [ div "name"
            (List.map (text "prefix") e.prefix ++ [Html.text (def e.name)])
        ; div "extra" [span "verbs" (verbs e); externalLink e] ]
      ++
      if showUndo == ShowUndo then [undoButton e.tlid (Toplevels pos)] else []
      )
  in
  let routes = div "routes" (List.map entryHtml entries) in
  let distinctEntries =
    entries |> List.map (fun x -> x.verbs) |> List.concat
  in
  let button =
    if (spacename == "HTTP" && showUndo) /= ShowUndo then
      Just CreateRouteHandler
    else Nothing
  in
  section spacename distinctEntries button routes

type collapseVerbs = CollapseVerbs | DontCollapseVerbs

type showLink = ShowLink | DontShowLink

type showUndo = ShowUndo | DontShowUndo

let viewRoutes m collapse showLink showUndo =
  let tls = m.toplevels in
  tls |> splitBySpace
  |> List.sortWith (fun (a, _) (b, _) -> ordering a b)
  |> List.map (Tuple.mapSecond tl2entry)
  |> (fun entries ->
       if collapse == CollapseVerbs then
         List.map (Tuple.mapSecond collapseByVerb) entries
       else entries )
  |> List.map (Tuple.mapSecond prefixify)
  |> List.map (viewGroup m showLink showUndo)

let viewDeletedTLs m =
  let tls = m.deletedToplevels in
  let routes = viewRoutes m DontCollapseVerbs DontShowLink ShowUndo in
  let dbs = viewRestorableDBs tls in
  let h = header "Deleted" tls Nothing in
  Html.details [Attrs.class_ "routing-section deleted"] ([h] ++ routes ++ [dbs])

let span class_ subs = Html.span [Attrs.class_ class_] subs

let text class_ msg = span class_ [Html.text msg]

let div class_ subs = Html.div [Attrs.class_ class_] subs

let header name list addHandler =
  Html.summary [Attrs.class_ "header"]
    [ text "title" name
    ; text "parens" "("
    ; text "count" (list |> List.length |> string_of_int)
    ; text "parens" ")"
    ; ( match addHandler with
      | Just msg -> buttonLink (fontAwesome "plus-circle") msg Nothing
      | Nothing -> text "" "" ) ]

let section name entries addHandler routes =
  if List.length entries == 0 then
    Html.div
      [Attrs.class_ "routing-section empty"]
      [header name entries addHandler; routes]
  else
    Html.details
      [Attrs.class_ "routing-section"]
      [header name entries addHandler; routes]

let buttonLink content handler page =
  let href =
    page |> Maybe.map (fun p -> Attrs.href (Url.urlFor p)) |> ME.toList
  in
  let event =
    match page with
    | Nothing -> eventNoDefault "click" (fun _ -> handler)
    | Just _ -> eventNoPropagation "click" (fun _ -> handler)
  in
  Html.a ([event; Attrs.class_ "button-link"] ++ href) [content]

let tlLink pos class_ name = Url.linkFor (Toplevels pos) class_ [Html.text name]

let fnLink fn isUsed text_ =
  Url.linkFor
    (Fn (fn.tlid, Defaults.centerPos))
    (if isUsed then "default-link" else "default-link unused")
    [Html.text text_]

let view404s f404s =
  let theCreateLink fof =
    buttonLink (fontAwesome "plus-circle") (CreateHandlerFrom404 fof) Nothing
  in
  let theDeleteLink fof =
    buttonLink (fontAwesome "minus-circle") (Delete404 fof) Nothing
  in
  let fofHtml ({space; path; modifier} as fof) =
    div "fof"
      [ text "path" path
      ; (if space == "HTTP" then text "" "" else text "space" space)
      ; text "modifier" modifier
      ; theCreateLink fof
      ; theDeleteLink fof ]
  in
  let routes = div "404s" (List.map fofHtml f404s) in
  section "404s" f404s Nothing routes

let viewDBs tls =
  let dbs =
    tls
    |> List.filter (fun tl -> TL.asDB tl /= Nothing)
    |> List.map (fun tl -> (tl.pos, TL.asDB tl |> Option.getExn "asDB"))
    |> List.sortBy (fun (_, db) -> db.name)
  in
  let dbHtml (pos, db) =
    div "simple-route" [span "name" [tlLink pos "default-link" db.name]]
  in
  let routes = div "dbs" (List.map dbHtml dbs) in
  section "DBs" dbs Nothing routes

let viewRestorableDBs tls =
  let dbs =
    tls
    |> List.filter (fun tl -> TL.asDB tl /= Nothing)
    |> List.map (fun tl -> (tl.pos, tl.id, TL.asDB tl |> Option.getExn "asDB"))
    |> List.sortBy (fun (_, _, db) -> db.name)
  in
  let dbHtml (pos, tlid, db) =
    div "simple-route" [text "name" db.name; undoButton tlid (Toplevels pos)]
  in
  let routes = div "dbs" (List.map dbHtml dbs) in
  section "DBs" dbs Nothing routes

let viewUserFunctions m =
  let fns =
    m.userFunctions |> List.filter (fun fn -> B.isF fn.metadata.name)
  in
  let fnNamedLink fn name =
    let useCount = countFnUsage m name in
    if useCount == 0 then
      [ span "name" [fnLink fn false name]
      ; buttonLink
          (fontAwesome "minus-circle")
          (DeleteUserFunction fn.tlid) Nothing ]
    else
      let countedName = name ++ " (" ++ string_of_int useCount ++ ")" in
      [span "name" [fnLink fn true countedName]]
  in
  let fnHtml fn =
    div "simple-route"
      (let fnName = B.asF fn.metadata.name in
       match fnName with
       | Just name -> fnNamedLink fn name
       | Nothing -> [span "name" [fnLink fn true "should be filtered by here"]])
  in
  let routes = div "fns" (List.map fnHtml fns) in
  section "Functions" fns (Just CreateFunction) routes

let viewRoutingTable m =
  let sections =
    viewRoutes m CollapseVerbs ShowLink DontShowUndo
    ++ [viewDBs m.toplevels] ++ [view404s m.f404s] ++ [viewUserFunctions m]
    ++ [viewDeletedTLs m]
  in
  let html =
    Html.div
      [ Attrs.class_ "viewing-table"
      ; nothingMouseEvent "mouseup"
      ; eventNoPropagation "mouseenter" (fun _ -> EnablePanning false)
      ; eventNoPropagation "mouseleave" (fun _ -> EnablePanning true) ]
      sections
  in
  Html.div [Attrs.id "sidebar-left"] [html]
