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
import Pointer as P
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
                | ClickSelectAs Pointer
                | ClickSelect -- use withID
                -- highlight this node as if it were ID
                | MouseoverAs ID
                | Mouseover
                -- display the value from this ID
                | DisplayValueOf ID
                | DisplayValue
                -- use this as ID for Mouseover, ClickSelect and
                -- DisplayValue
                | WithID Pointer

wc : String -> HtmlConfig
wc = WithClass

idConfigs : List HtmlConfig
idConfigs =
  [ClickSelect, DisplayValue, Mouseover]

atom : HtmlConfig
atom = wc "atom"

text_ : ViewState -> List HtmlConfig -> String -> Html.Html Msg
text_ vs c str =
  div vs c [Html.text str]

keyword_ : ViewState -> List HtmlConfig -> String -> Html.Html Msg
keyword_ vs c name =
  text_ vs (atom :: wc "keyword" :: wc name :: c) name

nested_ : ViewState -> List HtmlConfig -> List (Html.Html Msg) -> Html.Html Msg
nested_ vs c items =
  div vs (WithClass "nested" :: c) items

selectable_ : ViewState -> List HtmlConfig -> Html.Html Msg -> Html.Html Msg
selectable_ vs c item =
  div vs (atom :: idConfigs ++ c) [item]



-- Create a Html.div for this ID, incorporating all ID-related data,
-- such as whether it's selected, appropriate events, mouseover, etc.
div : ViewState -> List HtmlConfig -> List (Html.Html Msg) -> Html.Html Msg
div vs configs content =
  let selectedID = case vs.state of
                     Selecting _ (Just p) -> Just (P.toID p)
                     _ -> Nothing

      getFirst fn = configs |> List.filterMap fn |> List.head

      -- Extract config
      thisPointer = getFirst (\a -> case a of
                                      WithID p -> Just p
                                      _ -> Nothing)
      thisID = thisPointer |> Maybe.map P.toID

      clickAs = getFirst (\a -> case a of
                                  ClickSelectAs p -> Just p
                                  ClickSelect -> thisPointer
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
        if selected
        then []
        else
          case clickAs of
            Just p ->
              let id = P.toID p in
              [ eventNoPropagation "mouseup"
                  (ToplevelClickUp vs.tlid (Just p))
              , eventNoPropagation "mouseenter" (MouseEnter id)
              , eventNoPropagation "mouseleave" (MouseLeave id)
              ]
            _ -> []
      attrs = events ++ title ++ [classAttr]

  in
    Html.div attrs content

type alias Viewer a = ViewState -> List HtmlConfig -> a -> Html.Html Msg
type alias BlankViewer a = Viewer (BlankOr a)

viewText : PointerType -> ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewText pt vs c str =
  viewBlankOr (text_ vs) pt vs c str

viewBlankOr : (List HtmlConfig -> a -> Html.Html Msg) -> PointerType ->
  ViewState -> List HtmlConfig -> BlankOr a -> Html.Html Msg
viewBlankOr htmlFn pt vs c bo =
  let p = B.toP pt bo
      id = P.toID p
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
                   Selecting _ (Just p) -> P.toID p == id
                   _ -> False

      featureFlag = if selected
                    then [viewFeatureFlag]
                    else []
      thisTextFn bo =
        case bo of
          Blank _ ->
            div vs
              ([WithClass "blank", WithID p] ++ idConfigs ++ c)
              ([Html.text placeholder] ++ featureFlag)
          F _ fill ->
            -- to add FeatureFlag here, we need to pass it
            -- to the htmlFn maybe?
            if pt == Expr
            then htmlFn ([WithID p] ++ c) fill
            else htmlFn ([WithID p] ++ idConfigs ++ c) fill
          Flagged _ _ _ _ -> Debug.crash "vbo"

      thisText = case bo of
                   Flagged msg setting l r ->
                     Html.div
                       [Attrs.class "flagged"]
                       [ Html.text msg
                       , Html.text (toString setting)
                       , thisTextFn l
                       , thisTextFn r]
                   _ -> thisTextFn bo

      allowStringEntry = pt == Expr

      text = case vs.state of
               Entering (Filling _ thisP) ->
                 if p == thisP
                 then ViewEntry.entryHtml allowStringEntry placeholder vs.ac
                 else thisText
               _ -> thisText
  in
  text


viewFeatureFlag : Html.Html Msg
viewFeatureFlag =
  Html.div
    [ Attrs.class "feature-flag"
    , Events.onMouseDown StartFeatureFlag]
    [ fontAwesome "flag"]



