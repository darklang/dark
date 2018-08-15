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
import Prelude exposing (..)
import Functions
import Autocomplete
import Toplevel as TL
import AST
import Runtime as RT
import Blank as B
import Runtime
import Toplevel
import Url
import ViewEntry
import ViewUtils exposing (..)
import Defaults


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
                -- display computed value from this ID
                | ComputedValueAs ID
                | ComputedValue
                -- show an 'edit function' link
                | WithEditFn TLID


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
  div vs c <| [Html.div [Attrs.class "quote quote-start"] []] ++ [Html.text str] ++ [Html.div [Attrs.class "quote quote-end"] []]

keyword : ViewState -> List HtmlConfig -> String -> Html.Html Msg
keyword vs c name =
  text vs (atom :: wc "keyword" :: wc name :: c) name

tipe : ViewState -> List HtmlConfig -> Tipe -> Html.Html Msg
tipe vs c t =
  text vs c (Runtime.tipe2str t)

withFeatureFlag : ViewState -> BlankOr a -> List HtmlConfig
withFeatureFlag vs v =
  if idOf vs.cursorState == Just (B.toID v)
  then [WithFF]
  else []

withEditFn : ViewState -> BlankOr NExpr -> List HtmlConfig
withEditFn vs v =
  if idOf vs.cursorState == Just (B.toID v)
  then
    case v of
      F _ (FnCall name _ _) ->
        case LE.find (Functions.sameName name) vs.ufns of
          Just fn -> [WithEditFn fn.tlid]
          _ -> []
      _ -> []
  else []

getLiveValue : LVDict -> ID -> Maybe (Result String LiveValue)
getLiveValue lvs (ID id) =
  lvs
  |> Dict.get id
  |> Maybe.map (\lv -> if Runtime.isError lv
                       then Err (Runtime.extractErrorMessage lv)
                       else Ok lv)



viewLiveValue : ViewState -> List (Html.Html Msg)
viewLiveValue vs =
  let cursorLiveValue =
        case vs.cursorState of
          Selecting tlid (Just (ID id)) ->
            Dict.get id vs.lvs
          Entering (Filling tlid (ID id)) ->
            Dict.get id vs.lvs
          _ -> Nothing
  in
      case cursorLiveValue of
        Just sv->
          [
            Html.div
              [Attrs.class "computed-value"]
              [Html.div
                [Attrs.class "computed-value-value"]
                [Html.text sv.value]
              ]
          ]
        _ -> []

-- Create a Html.div for this ID, incorporating all ID-related data,
-- such as whether it's selected, appropriate events, mouseover, etc.
div : ViewState -> List HtmlConfig -> List (Html.Html Msg) -> Html.Html Msg
div vs configs content =
  let getFirst fn = configs |> List.filterMap fn |> List.head

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
      computedValueAs = getFirst (\a -> case a of
                                          ComputedValueAs id -> Just id
                                          ComputedValue -> thisID
                                          _ -> Nothing)
      showFeatureFlag = List.member WithFF configs
      editFn = getFirst (\a -> case a of
                                 WithEditFn id -> Just id
                                 _ -> Nothing)


      value = getLiveValue vs.lvs

      selectedID =
        case vs.cursorState of
          Selecting _ (Just id) -> Just id
          _ -> Nothing

      selected =
        thisID == selectedID && ME.isJust thisID

      liveValueHtml =
        if selected then viewLiveValue vs else []

      computedValueData = Maybe.andThen value computedValueAs
      hoverdata = Maybe.andThen value hoverAs

      mouseover =
        mouseoverAs == vs.hovering && ME.isJust mouseoverAs

      incomplete =
        case computedValueData of
          Nothing -> False
          Just (Err _) -> False
          Just (Ok lv) -> Runtime.isIncomplete lv

      idAttr = case thisID of
                 Just id -> ["blankOr", "id-" ++ toString (deID id)]
                 _ -> []
      allClasses = classes
                  ++ idAttr
                  ++ (if selected then ["selected"] else [])
                  ++ (if mouseover then ["mouseovered"] else [])
                  ++ (if incomplete then ["incomplete"] else [])
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

      attrs = events ++ [classAttr]
      featureFlagHtml = if showFeatureFlag
                        then [viewFeatureFlag]
                        else []
      editFnHtml = case editFn of
                     Just editFn ->
                       [viewEditFn editFn showFeatureFlag]
                     Nothing -> if showFeatureFlag then [viewCreateFn] else []
      rightSideHtml =
        Html.div
        [Attrs.class "expr-actions"]
        (featureFlagHtml ++ editFnHtml)
  in
    Html.div attrs (content ++ [rightSideHtml]  ++ liveValueHtml)


type alias Viewer a = ViewState -> List HtmlConfig -> a -> Html.Html Msg
type alias BlankViewer a = Viewer (BlankOr a)

viewText : PointerType -> ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewText pt vs c str =
  viewBlankOr text pt vs c str

viewTipe : PointerType -> ViewState -> List HtmlConfig -> BlankOr Tipe -> Html.Html Msg
viewTipe pt vs c str =
  viewBlankOr tipe pt vs c str

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
      case vs.handlerSpace of
        HSHTTP -> "route"
        HSCron -> "event name"
        HSOther -> "event name"
        HSEmpty -> "event name"
    EventModifier ->
      case vs.handlerSpace of
        HSHTTP -> "verb"
        HSCron -> "event interval"
        HSOther -> "event modifier"
        HSEmpty -> "event modifier"
    EventSpace -> "event space"
    Expr -> paramPlaceholder
    Field -> "fieldname"
    Key -> "keyname"
    DBColName -> "db field name"
    DBColType -> "db type"
    DarkType -> "type"
    DarkTypeField -> "fieldname"
    FFMsg -> "flag name"
    FnName -> "function name"
    ParamName -> "param name"
    ParamTipe -> "param type"

viewBlankOr :
    (ViewState -> List HtmlConfig -> a -> Html.Html Msg) ->
    PointerType ->
    ViewState ->
    List HtmlConfig ->
    BlankOr a ->
      Html.Html Msg
viewBlankOr htmlFn pt vs c bo =
  let
      wID id = [WithID id]
      drawBlank id =
        div vs
          ([WithClass "blank"] ++ c ++ wID id)
          [Html.text (placeHolderFor vs id pt)]

      drawFilled id fill =
        let configs = wID id ++ c
        in htmlFn vs configs fill

      thisText =
        case bo of
          F id fill -> drawFilled id fill
          Blank id -> drawBlank id

  in
  case vs.cursorState of
    Entering (Filling _ thisID) ->
      let id = B.toID bo in
      if id == thisID && vs.showEntry
      then
        let allowStringEntry =
              if pt == Expr
              then StringEntryAllowed
              else StringEntryNotAllowed
            stringEntryWidth =
              if vs.tooWide
              then StringEntryShortWidth
              else StringEntryNormalWidth
            placeholder = placeHolderFor vs id pt
        in
            div vs c
              ([ ViewEntry.entryHtml
                  allowStringEntry stringEntryWidth placeholder vs.ac] ++ (viewLiveValue vs))
      else thisText

    SelectingCommand tlid id ->
      if id == B.toID bo
      then
        Html.div
          [Attrs.class "selecting-command"]
          [ thisText
          , ViewEntry.entryHtml StringEntryNotAllowed StringEntryNormalWidth "command" vs.ac]
      else
        thisText

    _ -> thisText


viewFeatureFlag : Html.Html Msg
viewFeatureFlag =
  Html.div
    [ Attrs.class "flag"
    , eventNoPropagation "click" (\_ -> StartFeatureFlag) ]
    [ fontAwesome "flag"]

viewCreateFn : Html.Html Msg
viewCreateFn =
  Html.div
    [ Attrs.class "exfun"
    , eventNoPropagation "click" (\_ -> ExtractFunction) ]
    [ fontAwesome "share-square" ]

viewEditFn : TLID -> Bool -> Html.Html Msg
viewEditFn tlid hasFlagAlso =
  let rightOffset =
        if hasFlagAlso
        then "-34px"
        else "-16px"
  in
  Html.a
    [ Attrs.class "edit-fn"
    , Attrs.style [("right", rightOffset)]
    , Attrs.href (Url.urlFor (Fn tlid Defaults.fnPos))
    ]
    [ fontAwesome "edit"]
