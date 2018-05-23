module ViewBlankOr exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Events as Events
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
import ViewScaffold


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
      F _ (FnCall name _) ->
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




-- Create a Html.div for this ID, incorporating all ID-related data,
-- such as whether it's selected, appropriate events, mouseover, etc.
div : ViewState -> List HtmlConfig -> List (Html.Html Msg) -> Html.Html Msg
div vs configs content =
  let selectedID = case vs.cursorState of
                     Selecting _ (Just id) -> Just id
                     _ -> Nothing

      getFirst fn = configs |> List.filterMap fn |> List.head

      -- Extract config
      thisID = getFirst (\a -> case a of
                                 WithID id -> Just id
                                 _ -> Nothing)
      documentation = case thisID of
        Just id -> Just <| placeHolderFor vs id Expr
        Nothing -> Nothing
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

      computedValueData = Maybe.andThen value computedValueAs
      hoverdata = Maybe.andThen value hoverAs

      (computedValueClasses, computedValue) =
        if incomplete
          || (Just vs.tlid) /= tlidOf vs.cursorState
        then
          ([], [])
        else
          case computedValueData of
            Nothing -> ([], [])
            Just (Ok lv) ->
              ( ["computed-value"]
              , [ Html.div
                    [Attrs.class "computed-value-value"]
                    [Html.text lv.value]
                ])
            Just (Err err) ->
              ( ["computed-value computed-value-error"]
              , [ Html.div
                    [Attrs.class "computed-value-value"]
                    [ViewScaffold.viewError (Just err)]
                ])

      selected = thisID == selectedID
                 && ME.isJust thisID
      mouseover = mouseoverAs == vs.hovering
                                 && ME.isJust mouseoverAs
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
                  ++ computedValueClasses
                  ++ (if selected then ["selected"] else [])
                  ++ (if mouseover then ["mouseovered"] else [])
                  ++ (if incomplete then ["incomplete"] else [])
      classAttr = Attrs.class (String.join " " allClasses)
      docsAttr = case documentation of
        Just docs -> [Attrs.attribute "data-docs" docs]
        Nothing -> []
      events =
        case clickAs of
          Just id ->
            [ eventNoPropagation "click" (BlankOrClick vs.tl.id id)
            , eventNoPropagation "dblclick" (BlankOrDoubleClick vs.tl.id id)
            , eventNoPropagation "mouseenter" (BlankOrMouseEnter vs.tl.id id)
            , eventNoPropagation "mouseleave" (BlankOrMouseLeave vs.tl.id id)
            ]
          _ -> []

      attrs = events ++ docsAttr ++ [classAttr]
      featureFlagHtml = if showFeatureFlag
                        then [viewFeatureFlag]
                        else []
      editFnHtml = case editFn of
                     Just editFn ->
                       [viewEditFn editFn showFeatureFlag]
                     Nothing -> []
  in
    Html.div attrs (content ++ featureFlagHtml ++ editFnHtml ++ computedValue)

type alias Viewer a = ViewState -> List HtmlConfig -> a -> Html.Html Msg
type alias BlankViewer a = Viewer (BlankOr a)

viewText : PointerType -> ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewText pt vs c str =
  viewBlankOr text B.shallowWithinFn pt vs c str

viewTipe : PointerType -> ViewState -> List HtmlConfig -> BlankOr Tipe -> Html.Html Msg
viewTipe pt vs c str =
  viewBlankOr tipe B.shallowWithinFn pt vs c str

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
    (a -> ID -> Bool) ->
    PointerType ->
    ViewState ->
    List HtmlConfig ->
    BlankOr a ->
      Html.Html Msg
viewBlankOr htmlFn isWithinFn pt vs c bo =
  let
      isSelectionWithin bo =
        idOf vs.cursorState
        |> Maybe.map (B.within bo isWithinFn)
        |> Maybe.withDefault False

      wID id = [WithID id]
      drawBlank id =
        div vs
          ([WithClass "blank"] ++ c ++ wID id)
          [Html.text (placeHolderFor vs id pt)]

      drawFilled id fill =
        let configs = wID id ++ c
        in htmlFn vs configs fill

      drawFilledInsideFlag id fill =
        -- This may be complex and have nested blanks which are
        -- selected, even though this is not a blank, so showEntry is
        -- important.
        let vs2 = { vs | showEntry = False } in
        htmlFn vs2 [] fill

      drawBlankInsideFlag id =
        let vs2 = { vs | showEntry = False } in
        div vs2
          ([WithClass "blank"])
          [Html.text (placeHolderFor vs id pt)]

      drawInFlag id bo =
        let vs2 = { vs | showEntry = False } in
        case bo of
          F fid fill  ->
            [div vs2 [ DisplayValueOf fid
                    , ClickSelectAs id
                    , WithID id]
               [drawFilledInsideFlag id fill]]
          Blank id ->
            [drawBlankInsideFlag id]
          _ -> recoverable ("nested flagging not allowed for now", bo) []

      drawSetting ffID setting =
        Html.div
          [Attrs.class "setting-slider" ]
          [ Html.input
              [ Attrs.type_ "range"
              , Attrs.min "0"
              , Attrs.max "100"
              , Attrs.step "0.5"
              , Attrs.value (toString setting)
              , eventNoPropagation "click" NothingClick
              , eventNoPropagation "mousedown" NothingClick
              , Events.onWithOptions
                  "mouseup"
                  { stopPropagation = False, preventDefault = False }
                  (decodeSliderInputEvent (\_ -> SliderChange ffID))
              , Events.onWithOptions
                  "input"
                  { stopPropagation = False, preventDefault = False }
                  (decodeSliderInputEvent (SliderMoving ffID))
              ]
              []
          ]

      drawEndFeatureFlag setting ffID =
        let (actionClass, icon) =
              if setting == 0 || setting == 100
              then ("valid-action", "check")
              else ("invalid-action", "times")
        in
        Html.div
        [ Attrs.class (String.join " " ["end-ff", actionClass])
        , Attrs.attribute "data-content" "Click to finalize and remove flag"
        , eventNoPropagation "click" (\_ -> EndFeatureFlag ffID)]
        [ fontAwesome icon ]

      redFlag id =
        Html.div
          [ eventNoPropagation "click" (BlankOrClick vs.tlid id)
          , eventNoPropagation "mousedown" (BlankOrClick vs.tlid id)
          , eventNoPropagation "mouseup" (BlankOrClick vs.tlid id)
          ]
          [fontAwesome "flag"]


      drawFlagged id msg setting l r =
         if isSelectionWithin (Flagged id msg setting l r)
         then
           div vs
             [ wc "flagged shown"]
             (drawInFlag id (B.flattenFF bo) ++
              [ fontAwesome "flag"
              , viewText FFMsg vs (wc "flag-message" :: idConfigs) msg
              , drawSetting id setting
              , drawEndFeatureFlag setting id
              , div vs [wc "flag-left nested-flag"]
                  [viewBlankOr htmlFn isWithinFn pt vs idConfigs l]
              , div vs [wc "flag-right nested-flag"]
                  [viewBlankOr htmlFn isWithinFn pt vs idConfigs r]
              ])
        else
          Html.div
            [Attrs.class "flagged hidden"]
            (drawInFlag id (B.flattenFF bo) ++ [redFlag id])



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
  case vs.cursorState of
    Entering (Filling _ thisID) ->
      let id = B.toID bo in
      if id == thisID && vs.showEntry
      then
        let allowStringEntry = if pt == Expr then StringEntryAllowed else StringEntryNotAllowed
            stringEntryWidth = if vs.tooWide then StringEntryShortWidth else StringEntryNormalWidth
            placeholder = placeHolderFor vs id pt
        in
            div vs c [ViewEntry.entryHtml allowStringEntry stringEntryWidth placeholder vs.ac]
      else thisText
    _ -> thisText


viewFeatureFlag : Html.Html Msg
viewFeatureFlag =
  Html.div
    [ Attrs.class "feature-flag"
    , eventNoPropagation "click" (\_ -> StartFeatureFlag)]
    [ fontAwesome "flag"]


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
    , Attrs.href (Url.urlFor (Fn tlid))
    ]
    [ fontAwesome "edit"]
