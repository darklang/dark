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
import SpecHeaders



type alias Entry = { name: Maybe String
                   , prefix: List String
                   , verbs: List (String, Pos)
                   , isHttp: Bool }

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
                     |> Maybe.withDefault "<no space yet>"
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
               Flagged _ _ _ _ _ ->
                 impossible ("FF in spec name", h.spec.name)
         , prefix = []
         , isHttp = SpecHeaders.isHTTP h.spec
         , verbs =
             case h.spec.modifier of
               F _ s -> [(s, pos)]
               Blank _ -> [("_", pos)]
               Flagged _ _ _ _ _ ->
                 impossible ("FF in spec modifier", h.spec.modifier)
         })
  |> List.sortBy (\c -> Maybe.withDefault "ZZZZZZ" c.name)
  |> LE.groupWhile (\a b -> a.name == b.name)
  |> List.map (List.foldr (\curr list ->
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


----------------------------------
-- Html
----------------------------------
span : String -> List (Html.Html Msg) -> Html.Html Msg
span class subs = Html.span [Attrs.class class] subs

text : String -> String  -> Html.Html Msg
text class msg = span class [Html.text msg]

div : String -> List (Html.Html Msg) -> Html.Html Msg
div class subs = Html.div [Attrs.class class] subs

header : String -> List a -> Html.Html Msg
header name list =
  Html.summary
    [ Attrs.class "header" ]
    [ text "title" name
    , text "parens" "("
    , text "count" (list |> List.length |> toString)
    , text "parens" ")"
    ]

link : Html.Html Msg -> Msg -> Html.Html Msg
link content handler =
  Html.a
    [ eventNoPropagation "mouseup" (\_ -> handler)
    , Attrs.src ""
    , Attrs.class "verb-link as-pointer"
    ]
    [ content ]


viewGroup : Model -> (String, List Entry) -> Html.Html Msg
viewGroup m (spacename, entries) =
  let def s = Maybe.withDefault "<no route yet>" s
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
          (\(verb, pos) ->
            link (Html.text verb) (NavigateTo (Viewport.urlForPos pos)))
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
  Html.details
    [ Attrs.class "routing-section"]
    [ header spacename entries, routes]

viewRoutes : Model -> List (Html.Html Msg)
viewRoutes m =
  m.toplevels
  |> splitBySpace
  |> List.map (T2.map collapseHandlers)
  |> List.map (T2.map prefixify)
  |> List.map (viewGroup m)



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
  in Html.details
       [Attrs.class "routing-section"]
       [header "404s" m.f404s, routes]

viewDBs : Model -> Html.Html Msg
viewDBs m =
  let dbs = m.toplevels
            |> List.filter (\tl -> TL.asDB tl /= Nothing)
            |> List.map (\tl -> (tl.pos, TL.asDB tl |> deMaybe "asDB"))
      thelink (pos, db) =
        link (Html.text db.name) (NavigateTo (Viewport.urlForPos pos))

      dbHtml (pos, db) =
        div "db-route"
          [ span "name" [thelink (pos, db)] ]

      routes = div "dbs" (List.map dbHtml dbs)
  in Html.details
       [Attrs.class "routing-section"]
       [header "DBs" dbs, routes]





viewRoutingTable : Model -> Html.Html Msg
viewRoutingTable m =
  let sections = viewRoutes m
                 ++ [viewDBs m]
                 ++ [view404s m]
      html = Html.div
               [Attrs.class "viewing-table"]
               sections

  in placeHtml m (Viewport.toAbsolute m {vx=0, vy=0}) html



