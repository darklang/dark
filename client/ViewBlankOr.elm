module ViewBlankOr exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Attributes as Attrs
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Autocomplete
import Toplevel as TL
import AST
import Runtime as RT
import Blank as B
import Runtime
import Toplevel
import ViewEntry
import ViewUtils exposing (..)
import Util



type HtmlConfig =
                -- Add this class (can be done multiple times)
                  WithClass String
                -- when you click this node, select this pointer
                | ClickSelectAs ID
                | ClickSelect -- use withID
                -- highlight this node as if it were ID
                | MouseoverAs ID
                | Mouseover
                -- display the value from this ID
                | DisplayValueOf ID
                | DisplayValue
                -- use this as ID for Mouseover, ClickSelect and
                -- DisplayValue
                | WithID ID
                -- show a featureflag
                | WithFF

wc : String -> HtmlConfig
wc = WithClass

idConfigs : List HtmlConfig
idConfigs =
  [ClickSelect, DisplayValue, Mouseover]

atom : HtmlConfig
atom = wc "atom"

nested : HtmlConfig
nested = wc "nested"

text : ViewState -> List HtmlConfig -> String -> Html.Html Msg
text vs c str =
  div vs c [Html.text str]

keyword : ViewState -> List HtmlConfig -> String -> Html.Html Msg
keyword vs c name =
  text vs (atom :: wc "keyword" :: wc name :: c) name



-- Create a Html.div for this ID, incorporating all ID-related data,
-- such as whether it's selected, appropriate events, mouseover, etc.
div : ViewState -> List HtmlConfig -> List (Html.Html Msg) -> Html.Html Msg
div vs configs content =
  let selectedID = case vs.state of
                     Selecting _ (Just id) -> Just id
                     _ -> Nothing

      getFirst fn = configs |> List.filterMap fn |> List.head

      -- Extract config
      thisID = getFirst (\a -> case a of
                                 WithID id -> Just id
                                 _ -> Nothing)

      clickAs = getFirst (\a -> case a of
                                  ClickSelectAs id -> Just id
                                  ClickSelect -> thisID
                                  _ -> Nothing)
      hoverAs = getFirst (\a -> case a of
                                  DisplayValueOf id -> Just id
                                  DisplayValue -> thisID
                                  _ -> Nothing)
      mouseoverAs = getFirst (\a -> case a of
                                      MouseoverAs id -> Just id
                                      Mouseover -> thisID
                                      _ -> Nothing)
      classes = configs
                |> List.filterMap (\a -> case a of
                                           WithClass c -> Just c
                                           _ -> Nothing)
      showFeatureFlag = List.member WithFF configs



      -- Start using the config
      hoverdata =
        case hoverAs of
          Just (ID id) ->
            Dict.get id vs.lvs
            |> Maybe.map .value
            |> Maybe.map (\v -> if Runtime.isError v
                                then Err (Runtime.extractErrorMessage v)
                                else Ok v)
          _ -> Nothing

      (valClasses, title) =
        case hoverdata of
          Nothing -> ([], [])
          Just (Ok msg) -> ([], [Attrs.title msg])
          Just (Err err) -> (["value-error"], [Attrs.title err])

      selected = thisID == selectedID
                 && ME.isJust thisID
      mouseover = mouseoverAs == vs.hovering
                                 && ME.isJust mouseoverAs

      idAttr = case thisID of
                 Just id -> ["id-" ++ toString (deID id)]
                 _ -> []
      allClasses = classes
                  ++ idAttr
                  ++ valClasses
                  ++ (if selected then ["selected"] else [])
                  ++ (if mouseover then ["mouseovered"] else [])
      classAttr = Attrs.class (String.join " " allClasses)
      events =
        case clickAs of
          Just id ->
            [ eventNoPropagation "click" (BlankOrClick vs.tl.id id)
            , eventNoPropagation "dblclick" (BlankOrDoubleClick vs.tl.id id)
            , eventNoPropagation "mouseenter" (BlankOrMouseEnter vs.tl.id id)
            , eventNoPropagation "mouseleave" (BlankOrMouseLeave vs.tl.id id)
            ]
          _ -> []

      attrs = events ++ title ++ [classAttr]
      featureFlag = if showFeatureFlag
                    then [viewFeatureFlag]
                    else []
  in
    Html.div attrs (content ++ featureFlag)

type alias Viewer a = ViewState -> List HtmlConfig -> a -> Html.Html Msg
type alias BlankViewer a = Viewer (BlankOr a)

viewText : PointerType -> ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewText pt vs c str =
  viewBlankOr (text vs) pt vs c str

placeHolderFor : ViewState -> ID -> PointerType -> String
placeHolderFor vs id pt =
  let paramPlaceholder =
        vs.tl
        |> TL.asHandler
        |> Maybe.map .ast
        |> Maybe.andThen
            (\ast ->
              case AST.getParamIndex ast id of
                Just (name, index) ->
                  case Autocomplete.findFunction vs.ac name of
                    Just {parameters} ->
                      LE.getAt index parameters
                    Nothing -> Nothing
                _ -> Nothing)
        |> Maybe.map (\p -> p.name ++ ": " ++ RT.tipe2str p.tipe ++ "")
        |> Maybe.withDefault ""
  in
  case pt of
    VarBind -> "varname"
    EventName ->
      if vs.isHTTP
      then "route"
      else "event name"
    EventModifier ->
      if vs.isHTTP
      then "verb"
      else "event modifier"
    EventSpace -> "event space"
    Expr -> paramPlaceholder
    Field -> "fieldname"
    DBColName -> "db field name"
    DBColType -> "db type"
    DarkType -> "type"
    DarkTypeField -> "fieldname"
    FFMsg -> "Flag name"



viewBlankOr : (List HtmlConfig -> a -> Html.Html Msg) -> PointerType ->
  ViewState -> List HtmlConfig -> BlankOr a -> Html.Html Msg
viewBlankOr htmlFn pt vs c bo =
  let
      _ = case bo of
            Flagged _ _ _ _ _ ->
              let _ = Debug.log "state " vs.state in
              let _ = Debug.log "bo" bo in
              bo
            _ -> bo

      isSelected id =
        case vs.state of
          Selecting _ (Just sId) -> sId == id
          _ -> False

      wID id = [WithID id]
      wFF id =
        if isSelected id then [WithFF] else []

      drawBlank id =
        div vs
          ([WithClass "blank"] ++ idConfigs ++ c ++ wID id ++ wFF id)
          [Html.text (placeHolderFor vs id pt)]

      drawFilled id fill =
        let configs =
          wID id
          ++ c
          ++ wFF id
          ++ (if pt == Expr then idConfigs else [])
        in htmlFn configs fill

      drawFilledInFlag id fill =
        htmlFn [] fill

      drawBlankInFlag id =
        div vs
          ([WithClass "blank"])
          [Html.text (placeHolderFor vs id pt)]

      drawInFlag id bo =
        case bo of
          F fid fill  ->
            [div vs (WithID id :: idConfigs) [drawFilledInFlag id fill]]
          Blank id ->
            [drawBlankInFlag id]
          _ -> Util.impossible "nested flagging not allowed for now" []


      drawFlagged id msg setting l r =
         if isSelected id
         then
           div vs
             [ wc "flagged shown"]
             (drawInFlag id (B.flattenFF bo) ++
              [ fontAwesome "flag"
              , viewText FFMsg vs [wc "flag-message"] msg
              , text vs [wc "flag-setting"] (toString setting)
              , viewBlankOr htmlFn pt vs [wc "flag-left nested-flag"] l
              , viewBlankOr htmlFn pt vs [wc "flag-right nested-flag"] r
              ])
        else
          Html.div
            [Attrs.class "flagged hidden"]
            (drawInFlag id (B.flattenFF bo) ++ [fontAwesome "flag"])



      -- the desired css layouts are:
      -- no ff:
      --   .blank/expr
      --     .feature-flag (only if selected)
      -- after click
      --   .flagged
      --     .message
      --     .setting
      --     .flag-left
      --       etc
      --     .flag-right
      --       etc
      thisText =
        case bo of
          Flagged fid msg setting l r ->
            drawFlagged fid msg setting l r
          F id fill -> drawFilled id fill
          Blank id -> drawBlank id

  in
  case vs.state of
    Entering (Filling _ thisID) ->
      let id = B.toID bo in
      if id == thisID
      then
        let allowStringEntry = pt == Expr
            placeholder = placeHolderFor vs id pt
        in
        ViewEntry.entryHtml allowStringEntry placeholder vs.ac
      else thisText
    _ -> thisText



viewFeatureFlag : Html.Html Msg
viewFeatureFlag =
  Html.div
    [ Attrs.class "feature-flag"
    , eventNoPropagation "click" (\_ -> StartFeatureFlag)]
    [ fontAwesome "flag"]
