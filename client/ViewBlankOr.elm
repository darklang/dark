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
import Analysis
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

text_ : Viewer String
text_ m tl c str =
  div m tl c [Html.text str]

keyword_ : Viewer String
keyword_ m tl c name =
  text_ m tl (atom :: wc "keyword" :: wc name :: c) name

nesteds_ : Viewer (List (Html.Html Msg))
nesteds_ m tl c items =
  div m tl (WithClass "nested" :: c) items

nested_ : Viewer (Html.Html Msg)
nested_ m tl c item =
  nesteds_ m tl c [item]

selectable_ : Viewer (Html.Html Msg)
selectable_ m tl c item =
  div m tl (atom :: idConfigs ++ c) [item]



-- Create a Html.div for this ID, incorporating all ID-related data,
-- such as whether it's selected, appropriate events, mouseover, etc.
div : Viewer (List (Html.Html Msg))
div m tl configs content =
  let selectedID = case unwrapState m.state of
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
      lvs = Analysis.getLiveValuesDict m tl.id
      hoverdata =
        case hoverAs of
          Just (ID id) ->
            Dict.get id lvs
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
      mouseover = mouseoverAs == (m.hovering |> List.head)
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
                  (ToplevelClickUp tl.id (Just p))
              , eventNoPropagation "mouseenter" (MouseEnter id)
              , eventNoPropagation "mouseleave" (MouseLeave id)
              ]
            _ -> []
      attrs = events ++ title ++ [classAttr]

  in
    Html.div attrs content


type alias Viewer a = Model -> Toplevel -> List HtmlConfig -> a -> Html.Html Msg
type alias BlankViewer a = Viewer (BlankOr a)

viewBlankOrText : PointerType -> BlankViewer String
viewBlankOrText pt m tl c str =
  viewBlankOr (text_ m tl) pt m tl c str

viewBlankOr : (List HtmlConfig -> a -> Html.Html Msg) -> PointerType ->
  BlankViewer a
viewBlankOr htmlFn pt m tl c bo =
  let p = B.toP pt bo
      id = P.toID p
      paramPlaceholder =
        tl
        |> TL.asHandler
        |> Maybe.map .ast
        |> Maybe.andThen
            (\ast ->
              case AST.getParamIndex ast id of
                Just (name, index) ->
                  case Autocomplete.findFunction m.complete name of
                    Just {parameters} ->
                      LE.getAt index parameters
                    Nothing -> Nothing
                _ -> Nothing)
        |> Maybe.map (\p -> p.name ++ ": " ++ RT.tipe2str p.tipe ++ "")
        |> Maybe.withDefault ""

      isHTTP = TL.isHTTPHandler tl
      placeholder =
        case pt of
          VarBind -> "varname"
          EventName ->
            if isHTTP
            then "route"
            else "event name"
          EventModifier ->
            if isHTTP
            then "verb"
            else "event modifier"
          EventSpace -> "event space"
          Expr -> paramPlaceholder
          Field -> "fieldname"
          DBColName -> "db field name"
          DBColType -> "db type"
          DarkType -> "type"
          DarkTypeField -> "fieldname"

      selected = case unwrapState m.state of
                     Selecting _ (Just p) -> P.toID p == id
                     _ -> False

      featureFlag = if selected
                    then [viewFeatureFlag]
                    else []
      thisTextFn bo = case bo of
                        Blank _ ->
                          div m tl
                            ([WithClass "blank", WithID p]
                             ++ idConfigs ++ c)
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

      text = case unwrapState m.state of
               Entering (Filling _ thisP) ->
                 if p == thisP
                 then ViewEntry.entryHtml allowStringEntry placeholder m.complete
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



