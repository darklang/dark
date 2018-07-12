module ViewRoutingTable exposing (viewRoutingTable)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import List.Extra as LE
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





collapseHandlers : List Toplevel -> List Entry
collapseHandlers tls =
  tls
  |> List.filterMap
      (\tl ->
        case TL.asHandler tl of
          Just h -> Just (tl.pos, h)
          Nothing -> Nothing)
  |> List.map
       (\(pos, h) ->
         { name =
             case h.spec.name of
               F _ s -> Just s
               Blank _ -> Nothing
         , prefix = []
         , verbs =
             case h.spec.modifier of
               F _ s -> [(s, pos)]
               Blank _ -> [("_", pos)]
         })
  |> List.sortBy (\c -> Maybe.withDefault "ZZZZZZ" c.name)
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

viewGroup : Model -> (String, List Entry) -> Html.Html Msg
viewGroup m (spacename, entries) =
  let def s = Maybe.withDefault missingEventRouteDesc s
      externalLink h =
        if List.member "GET" (List.map Tuple.first h.verbs)
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
          (\(verb, pos) -> newLink pos "verb-link" verb)
        |> List.intersperse (Html.text ",")
      entryHtml e =
        div "handler" [ div "name"
                          (  List.map (text "prefix") e.prefix
                          ++ [Html.text (def e.name)])
                      , externalLink e
                      , span "verbs" (verbs e)
                      ]
      routes = div "routes" (List.map entryHtml entries)
  in
  section spacename entries (if spacename == "HTTP" then (Just CreateRouteHandler) else Nothing) routes

viewRoutes : Model -> List (Html.Html Msg)
viewRoutes m =
  m.toplevels
  |> splitBySpace
  |> List.sortWith (\(a,_) (b,_) -> ordering a b)
  |> List.map (T2.map collapseHandlers)
  |> List.map (T2.map prefixify)
  |> List.map (viewGroup m)


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
            link (fontAwesome "plus-circle") (msg)
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

link : Html.Html Msg -> Msg -> Html.Html Msg
link content handler =
  Html.a
    [ eventNoPropagation "mouseup" (\_ -> handler)
    , Attrs.href ""
    , Attrs.class "verb-link"
    ]
    [ content ]

newLink : Pos -> String -> String -> Html.Html Msg
newLink pos classes name =
  Url.linkFor
    (Toplevels pos)
    classes
    [Html.text name]

view404s : Model -> Html.Html Msg
view404s m =
  let thelink fof =
      link (fontAwesome "plus-circle") (CreateHandlerFrom404 fof)

      fofHtml (space, path, modifier, values) =
        div "fof"
          [ text "path" path
          , thelink (space, path, modifier, values)
          , text "space" space
          , text "modifier" modifier
          ]
      routes = div "404s" (List.map fofHtml m.f404s)
  in section "404s" m.f404s Nothing routes

viewDBs : Model -> Html.Html Msg
viewDBs m =
  let dbs = m.toplevels
            |> List.filter (\tl -> TL.asDB tl /= Nothing)
            |> List.map (\tl -> (tl.pos, TL.asDB tl |> deMaybe "asDB"))

      dbHtml (pos, db) =
        div "simple-route"
          [ span "name" [newLink pos "default-link" db.name]]

      routes = div "dbs" (List.map dbHtml dbs)
  in section "DBs" dbs Nothing routes


viewUserFunctions : Model -> Html.Html Msg
viewUserFunctions m =
  let fns = m.userFunctions
            |> List.filter
              (\fn -> B.isF fn.metadata.name)

      fnLink fn =
        let name = B.asF fn.metadata.name
        in case name of
          Just fnName ->
            let useCount = countFnUsage m fnName
            in Url.linkFor
              (Fn fn.tlid Defaults.fnPos)
              ("default-link" ++ (if useCount==0 then " unused" else ""))
              [Html.text (fnName ++ "(" ++ (toString useCount) ++ ")")]
          Nothing ->
            Url.linkFor (Fn fn.tlid Defaults.fnPos) "default-link" [Html.text "should be filtered by here"]

      fnHtml fn =
        div "simple-route"
          [ span "name"
            [ fnLink fn]
          ]

      routes = div "fns" (List.map fnHtml fns)
  in
      section "Functions" fns (Just CreateFunction) routes



viewRoutingTable : Model -> Html.Html Msg
viewRoutingTable m =
  let sections = viewRoutes m
                 ++ [viewDBs m]
                 ++ [view404s m]
                 ++ [viewUserFunctions m]
      html = Html.div
               [ Attrs.class "viewing-table"
               , nothingMouseEvent "mouseup"
               ]
               sections

  in placeHtml m (Viewport.toAbsolute m {vx=0, vy=0}) html
