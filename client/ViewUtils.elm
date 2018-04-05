module ViewUtils exposing (..)

-- builtin
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
-- import String.Extra as SE
-- import List.Extra as LE
-- import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Viewport
import Toplevel as TL
import Analysis
import AST
import Blank as B

type alias ViewState =
  { tl: Toplevel
  , cursorState: CursorState
  , tlid: TLID
  , hovering: Maybe ID
  , ac: Autocomplete
  , isHTTP: Bool
  , lvs: LVDict
  , ffDisabled : Bool
  , ufns: List UserFunction
  , results: List AResult
  , relatedBlankOrs: List ID
  }

createVS : Model -> Toplevel -> ViewState
createVS m tl = { tl = tl
                , cursorState = unwrapCursorState m.cursorState
                , tlid = tl.id
                , hovering = m.hovering |> List.head
                , ac = m.complete
                , isHTTP = TL.isHTTPHandler tl
                , lvs = Analysis.getLiveValuesDict m tl.id
                , ffDisabled =
                  case tl.data of
                    TLHandler _ -> False
                    _ -> True
                , ufns = m.userFunctions
                , results = Analysis.getAnalysisResults m tl.id
                , relatedBlankOrs =
                  case (unwrapCursorState m.cursorState, TL.asHandler tl) of
                    (Entering (Filling _ id), Just h) ->
                      case TL.find tl id of
                        Just pd ->
                          case TL.getParentOf tl pd of
                            Just (PExpr e) -> e |> AST.usesOf |> List.map B.toID
                            _ -> []
                        _ -> []
                    _ -> []
                }

fontAwesome : String -> Html.Html Msg
fontAwesome name =
  Html.i [Attrs.class ("fa fa-" ++ name)] []

eventNoPropagation : String -> (MouseEvent -> Msg) -> Html.Attribute Msg
eventNoPropagation event constructor =
  Events.onWithOptions
    event
    { stopPropagation = True, preventDefault = False}
    (decodeClickEvent constructor)

nothingMouseEvent : String -> Html.Attribute Msg
nothingMouseEvent name = eventNoPropagation name NothingClick

decodeClickEvent : (MouseEvent -> a) -> JSD.Decoder a
decodeClickEvent fn =
  let toA : Int -> Int -> Int -> a
      toA px py button =
        fn {pos= {vx=px, vy=py}, button = button}
  in JSDP.decode toA
      |> JSDP.required "pageX" JSD.int
      |> JSDP.required "pageY" JSD.int
      |> JSDP.required "button" JSD.int

-- input fires all along the drag, while
decodeSliderInputEvent : (String -> a) -> JSD.Decoder a
decodeSliderInputEvent fn =
  JSDP.decode fn
  |> JSDP.requiredAt ["target", "value"] JSD.string



placeHtml : Model -> Pos -> Html.Html Msg -> Html.Html Msg
placeHtml m pos html =
  let rcpos = Viewport.toViewport m pos
      div class subs = Html.div [Attrs.class class] subs
  in
  Html.div [ Attrs.class "node"
           , Attrs.style [ ("left", (toString rcpos.vx) ++ "px")
                         , ("top", (toString rcpos.vy) ++ "px")
                         ]
           ]
           [ html ]

axisLine : Model -> Pos -> Html.Html Msg
axisLine m p =
  let px = Viewport.toViewport m p
  in
  Html.div [ Attrs.classList [ ("axis", True)
                             , ("horizontal", p.x > p.y)
                             ]
           , Attrs.style [ ("left", (toString px.vx) ++ "px")
                         , ("top", (toString px.vy) ++ "px")
                         ]
           ] []


inCh : Int -> String
inCh w =
  w
  |> toString
  |> \s -> s ++ "ch"

widthInCh : Int -> Html.Attribute Msg
widthInCh w =
  w
  |> inCh
  |> \w -> Attrs.style [("width", w)]



