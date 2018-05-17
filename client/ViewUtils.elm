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
import Prelude exposing (..)
import Viewport
import Toplevel as TL
import Analysis
import DB
import AST
import Blank as B
import Pointer as P

type alias ViewState =
  { tl: Toplevel
  , cursorState: CursorState
  , tlid: TLID
  , hovering: Maybe ID
  , ac: Autocomplete
  , isHTTP: Bool
  , showEntry : Bool
  , lvs: LVDict
  , dbLocked : Bool
  , ufns: List UserFunction
  , results: List AResult
  , relatedBlankOrs: List ID
  , tooWide: Bool
  , computedValuesDisabled: Bool
  , executingFunctions: List ID
  }

createVS : Model -> Toplevel -> ViewState
createVS m tl = { tl = tl
                , cursorState = unwrapCursorState m.cursorState
                , tlid = tl.id
                , hovering =
                  m.hovering
                  |> List.head
                  |> Maybe.andThen
                    (\i ->
                      case idOf m.cursorState of
                        Just cur ->
                          case TL.find tl i of
                            Just (PExpr exp) ->
                              let cursorSubsumedByHover =
                                exp
                                |> AST.allData
                                |> List.map P.toID
                                |> List.member cur
                              in
                                  if cursorSubsumedByHover
                                  then Nothing
                                  else Just i
                            _ -> if cur == i then Nothing else Just i
                        _ -> Just i)
                , ac = m.complete
                , showEntry = True
                , isHTTP = TL.isHTTPHandler tl
                , lvs = Analysis.getLiveValuesDict m tl.id
                , dbLocked = DB.isLocked m tl.id
                , ufns = m.userFunctions
                , results = Analysis.getAnalysisResults m tl.id
                , relatedBlankOrs =
                    case unwrapCursorState m.cursorState of
                      Entering (Filling _ id) ->
                        case TL.find tl id of
                          Just (PVarBind (F _ var)) as pd ->
                            case TL.getParentOf tl (deMaybe "impossible" pd)  of
                              Just (PExpr e) ->
                                case e of
                                  F _ (Let _ _ body) ->
                                    AST.uses var body |> List.map B.toID
                                  F _ (Lambda _ body) ->
                                    AST.uses var body |> List.map B.toID
                                  _ -> []

                              _ -> []
                          _ -> []
                      _ -> []
                , tooWide = False
                , computedValuesDisabled =
                  m.computedValuesDisabled
                , executingFunctions = List.filter (\(tlid,id) -> tlid == tl.id) m.executingFunctions
                                       |> List.map (\(tlid,id) -> id)
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



