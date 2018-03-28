module ViewBlankOr exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
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

viewBlankOr : (List HtmlConfig -> a -> Html.Html Msg) -> PointerType ->
  ViewState -> List HtmlConfig -> BlankOr a -> Html.Html Msg
viewBlankOr htmlFn pt vs c bo =
  let id = B.toID bo
      paramPlaceholder =
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

      placeholder =
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

      selected = case vs.state of
                   Selecting _ (Just sId) -> sId == id
                   _ -> False

      thisTextFn flagClass bo =
        let std = c ++ [WithID id]
            ++ (if selected then [WithFF] else [])
        in
        case bo of
          Blank _ ->
            div vs
              ([WithClass "blank"] ++ idConfigs ++ std)
              [Html.text placeholder]
          F _ fill ->
            let configs =
              std
              ++ (if pt == Expr then idConfigs else [])
            in htmlFn configs fill
          Flagged _ _ _ _ _ -> Debug.crash "vbo"

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
      thisText = case bo of
                   Flagged fid msg setting l r ->
                     if selected
                     then
                       div vs
                         [wc "flagged shown", WithID fid]
                         [ text vs [wc "flag-message"] msg
                         , text vs [wc "flag-setting"] (toString setting)
                         , thisTextFn [] (B.flattenFF bo)
                         , Html.div [Attrs.class "flag-left"]
                             [thisTextFn [] l]
                         , Html.div [Attrs.class "flag-right"]
                             [thisTextFn [] r]]
                    else
                      Html.div
                        [Attrs.class "flagged hidden"]
                        [thisTextFn [] (B.flattenFF bo)]

                   _ -> thisTextFn [] bo

      allowStringEntry = pt == Expr
  in
  case vs.state of
    Entering (Filling _ thisID) ->
      if id == thisID
      then ViewEntry.entryHtml allowStringEntry placeholder vs.ac
      else thisText
    _ -> thisText



viewFeatureFlag : Html.Html Msg
viewFeatureFlag =
  Html.div
    [ Attrs.class "feature-flag"
    , Events.onMouseDown StartFeatureFlag]
    [ fontAwesome "flag"]
