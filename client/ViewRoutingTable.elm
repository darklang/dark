module ViewRoutingTable exposing (viewRoutingTable)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import List.Extra as LE
import Maybe.Extra as ME
import Tuple2 as T2

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Viewport
import Toplevel as TL
import Blank as B
import Toplevel
import ViewUtils exposing (..)
import Url
import Defaults
import Refactor exposing (countFnUsage)


type alias Entry = { name: Maybe String
                   , prefix: List String
                   , verbs: List (String, Pos)
                   , tlid: TLID
                   }

missingEventSpaceDesc : String
missingEventSpaceDesc = "<missing event space>"
missingEventRouteDesc : String
missingEventRouteDesc = "<missing route>"

spaceName : Toplevel -> Maybe String
spaceName tl =
  case tl.data of
    TLHandler h -> h.spec.module_ |> B.toMaybe
    _ -> Nothing

splitBySpace : List Toplevel -> List (String, List Toplevel)
splitBySpace tls =
  let spaceName tl = tl
                     |> TL.asHandler
                     |> Maybe.map .spec
                     |> Maybe.map .module_
                     |> Maybe.andThen B.toMaybe
                     |> Maybe.withDefault missingEventSpaceDesc
  in
  tls
  |> List.sortBy spaceName
  |> LE.groupWhile (\a b -> spaceName a == spaceName b)
  |> List.map (\hs ->
                 let space = hs
                             |> List.head
                             |> deMaybe "splitBySpace"
                             |> spaceName
                in
                 (space, hs))



tl2entry : List Toplevel -> List Entry
tl2entry tls =
  tls
  |> List.filterMap
      (\tl ->
        case TL.asHandler tl of
          Just h -> Just (tl.pos, tl.id, h)
          Nothing -> Nothing)
  |> List.map
       (\(pos, tlid, h) ->
         { name =
             case h.spec.name of
               F _ s -> Just s
               Blank _ -> Nothing
         , prefix = []
         , verbs =
             case h.spec.modifier of
               F _ s -> [(s, pos)]
               Blank _ -> [("_", pos)]
         , tlid = tlid
         })
  |> List.sortBy (\c -> Maybe.withDefault "ZZZZZZ" c.name)

collapseByVerb : List Entry -> List Entry
collapseByVerb es =
  es
  |> LE.groupWhile (\a b -> a.name == b.name)
  |> List.map (List.foldr (\curr list ->
                   if curr.name == Nothing
                   then curr :: list
                   else
                     case list of
                       [] -> [curr]
                       prev :: rest ->
                         let new =
                           { prev | verbs = prev.verbs ++ curr.verbs }
                         in new :: rest
                ) [])
  |> List.concat


prefixify : List Entry -> List Entry
prefixify hs =
  case hs of
    [] -> hs
    [_] -> hs
    h :: rest ->
      case h.name of
        Nothing -> h :: prefixify rest
        Just name ->
          let len = String.length name
              makePrefix : Entry -> Entry
              makePrefix h2 =
                case h2.name of
                  Just name2 ->
                    let newName = String.dropLeft len name2 in
                    { h2 | name = Just newName
                         , prefix = h.prefix ++ [name]
                    }
                  _ -> h2
              isPrefixOf h2 =
                case h2.name of
                  Nothing -> False
                  Just n2 -> String.startsWith name n2
          in
          -- this should short circuit immediately when not matching, as
          -- first handler will make the fn succeed
          case LE.splitWhen (\h2 -> not (isPrefixOf h2)) rest of
            Nothing ->
              -- never hits, so everything is prefixed
              h :: (rest |> List.map makePrefix |> prefixify)
            Just (matched, unmatched) ->
              h :: (prefixify <| (List.map makePrefix matched) ++ unmatched)

ordering : String -> String -> Order
ordering a b =
  case (a, b) of
    -- to the start
    ("HTTP", _) -> LT
    (_, "HTTP") -> GT
    -- to the end
    (a, b) ->
      if a == missingEventRouteDesc
      then GT
      else if b == missingEventRouteDesc
      then LT
      else compare a b

undoButton : TLID -> Page -> Html.Html Msg
undoButton tlid page =
  buttonLink
    (text "undo" "Restore")
    (RestoreToplevel tlid)
    (Just page)

viewGroup : ShowLink -> ShowUndo -> (String, List Entry) -> Html.Html Msg
viewGroup showLink showUndo (spacename, entries) =
  let def s = Maybe.withDefault missingEventRouteDesc s
      externalLink h =
        if showLink == ShowLink
           && List.member "GET" (List.map Tuple.first h.verbs)
        then
          case h.name of
            Just n ->
              let source = String.join "" (h.prefix ++ [n]) in
              Html.a [ Attrs.class "external"
                     , Attrs.href source
                     , Attrs.target "_blank"
                     ]
                     [fontAwesome "external-link-alt"]
            Nothing ->
              Html.div [] []
        else
          Html.div [] []

      verbs e =
        e.verbs
        |> List.map
          (\(verb, pos) -> tlLink pos "verb-link" verb)
        |> List.intersperse (Html.text ",")
      entryHtml e =
        let pos = e.verbs
                  |> List.head
                  |> deMaybe "viewGroup/entryHtml"
                  |> Tuple.second
        in
        div "handler" ([ div "name"
                          (  List.map (text "prefix") e.prefix
                          ++ [Html.text (def e.name)])
                       , div "extra"
                         [ span "verbs" (verbs e)
                         , externalLink e
                         ]
                       ]
                       ++ (if showUndo == ShowUndo
                           then [undoButton e.tlid (Toplevels pos)]
                           else []))
      routes = div "routes" (List.map entryHtml entries)
      distinctEntries = entries
                        |> List.map .verbs
                        |> List.concat
      button = if spacename == "HTTP"
                  && showUndo /= ShowUndo
               then (Just CreateRouteHandler)
               else Nothing
  in
  section spacename distinctEntries button routes

type CollapseVerbs = CollapseVerbs | DontCollapseVerbs
type ShowLink = ShowLink | DontShowLink
type ShowUndo = ShowUndo | DontShowUndo

viewRoutes : List Toplevel -> CollapseVerbs -> ShowLink -> ShowUndo -> List (Html.Html Msg)
viewRoutes tls collapse showLink showUndo =
  tls
  |> splitBySpace
  |> List.sortWith (\(a,_) (b,_) -> ordering a b)
  |> List.map (T2.map tl2entry)
  |> (\entries ->
       if collapse == CollapseVerbs
       then List.map (T2.map collapseByVerb) entries
       else entries)
  |> List.map (T2.map prefixify)
  |> List.map (viewGroup showLink showUndo)

viewDeletedTLs : List Toplevel -> Html.Html Msg
viewDeletedTLs tls =
  let routes = viewRoutes tls DontCollapseVerbs DontShowLink ShowUndo
      dbs = viewRestorableDBs tls
      h = header "Deleted" tls Nothing
  in
  Html.details
    [ Attrs.class "routing-section deleted" ]
    ([ h ] ++ routes ++ [dbs])




----------------------------------
-- Html
----------------------------------
span : String -> List (Html.Html Msg) -> Html.Html Msg
span class subs = Html.span [Attrs.class class] subs

text : String -> String  -> Html.Html Msg
text class msg = span class [Html.text msg]

div : String -> List (Html.Html Msg) -> Html.Html Msg
div class subs = Html.div [Attrs.class class] subs

header : String -> List a -> Maybe Msg -> Html.Html Msg
header name list addHandler =
  Html.summary
    [ Attrs.class "header" ]
    [ text "title" name
    , text "parens" "("
    , text "count" (list |> List.length |> toString)
    , text "parens" ")",
    case addHandler of
        Just msg ->
            buttonLink (fontAwesome "plus-circle") msg Nothing
        Nothing ->
            text "" ""
    ]

section : String -> List a -> Maybe Msg -> Html.Html Msg -> Html.Html Msg
section name entries addHandler routes =
  if List.length entries == 0
  then
    Html.div
      [ Attrs.class "routing-section empty"]
      [ header name entries addHandler, routes]
  else
    Html.details
      [ Attrs.class "routing-section"]
      [ header name entries addHandler, routes]

buttonLink : Html.Html Msg -> Msg -> Maybe Page -> Html.Html Msg
buttonLink content handler page =
  let href = page
             |> Maybe.map (\p -> Attrs.href (Url.urlFor p))
             |> ME.toList
      event = case page of
                Nothing -> eventNoDefault "click" (\_ -> handler)
                Just _ -> eventNoPropagation "click" (\_ -> handler)
  in
  Html.a
    ([ event
     , Attrs.class "button-link"
     ] ++ href)
    [ content ]

tlLink : Pos -> String -> String -> Html.Html Msg
tlLink pos class name =
  Url.linkFor
    (Toplevels pos)
    class
    [Html.text name]

fnLink : UserFunction -> Bool -> String -> Html.Html Msg
fnLink fn isUsed text =
  Url.linkFor
    (Fn fn.tlid Defaults.fnPos)
    (if isUsed then "default-link" else "default-link unused")
    [Html.text text]



view404s : List FourOhFour -> Html.Html Msg
view404s f404s  =
  let thelink fof =
        buttonLink
          (fontAwesome "plus-circle")
          (CreateHandlerFrom404 fof)
          Nothing

      fofHtml (space, path, modifier, values) =
        div "fof"
          [ text "path" path
          , (if space == "HTTP" then text "" "" else text "space" space)
          , text "modifier" modifier
          , thelink (space, path, modifier, values)
          ]
      routes = div "404s" (List.map fofHtml f404s)
  in section "404s" f404s Nothing routes

viewDBs : List Toplevel -> Html.Html Msg
viewDBs tls =
  let dbs = tls
            |> List.filter (\tl -> TL.asDB tl /= Nothing)
            |> List.map (\tl -> (tl.pos, TL.asDB tl |> deMaybe "asDB"))
            |> List.sortBy (\(_, db) -> db.name)

      dbHtml (pos, db) =
        div "simple-route"
          [ span "name" [tlLink pos "default-link" db.name]]

      routes = div "dbs" (List.map dbHtml dbs)
  in section "DBs" dbs Nothing routes

viewRestorableDBs : List Toplevel -> Html.Html Msg
viewRestorableDBs tls =
  let dbs = tls
            |> List.filter (\tl -> TL.asDB tl /= Nothing)
            |> List.map (\tl -> (tl.pos, tl.id, TL.asDB tl |> deMaybe "asDB"))
            |> List.sortBy (\(_, _, db) -> db.name)

      dbHtml (pos, tlid, db) =
        div "simple-route"
          [ text "name" db.name, undoButton tlid (Toplevels pos)]

      routes = div "dbs" (List.map dbHtml dbs)
  in section "DBs" dbs Nothing routes


viewUserFunctions : Model -> Html.Html Msg
viewUserFunctions m =
  let fns = m.userFunctions
            |> List.filter
              (\fn -> B.isF fn.metadata.name)

      fnNamedLink fn name =
        let useCount = countFnUsage m name
        in if useCount == 0
          then
            [ span "name" [ fnLink fn False name ]
              , buttonLink
                  (fontAwesome "minus-circle")
                  (DeleteUserFunction fn.tlid)
                  Nothing
            ]
          else
            let countedName = name ++ " (" ++ (toString useCount) ++ ")"
            in [ span "name" [fnLink fn True countedName] ]

      fnHtml fn =
        div "simple-route" (
          let fnName = B.asF fn.metadata.name
          in case fnName of
            Just name -> fnNamedLink fn name
            Nothing ->
              [ span "name"
                [ fnLink fn True "should be filtered by here" ]
              ]
          )

      routes = div "fns" (List.map fnHtml fns)
  in section "Functions" fns (Just CreateFunction) routes



viewRoutingTable : Model -> Html.Html Msg
viewRoutingTable m =
  let sections = viewRoutes m.toplevels CollapseVerbs ShowLink DontShowUndo
                 ++ [viewDBs m.toplevels]
                 ++ [view404s m.f404s]
                 ++ [viewUserFunctions m]
                 ++ [viewDeletedTLs m.deletedToplevels]
      html = Html.div
               [ Attrs.class "viewing-table"
               ]
               sections

  in Html.div [ Attrs.id "sidebar-left" ] [html]
