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
import Util

type alias ViewState =
  { tl: Toplevel
  , cursorState: CursorState
  , tlid: TLID
  , hovering: Maybe ID
  , ac: Autocomplete
  , handlerSpace: HandlerSpace
  , showEntry : Bool
  , lvs: LVDict
  , dbLocked : Bool
  , ufns: List UserFunction
  , results: List AResult
  , relatedBlankOrs: List ID
  , tooWide: Bool
  , executingFunctions: List ID
  , tlCursors: TLCursors
  , testVariants: List VariantTest
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
                , handlerSpace = TL.spaceOf tl
                               |> Maybe.withDefault HSOther
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
                , executingFunctions = List.filter (\(tlid,id) -> tlid == tl.id) m.executingFunctions
                                       |> List.map (\(tlid,id) -> id)
                , tlCursors = m.tlCursors
                , testVariants = m.tests
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


blankOrLength : BlankOr String -> Int
blankOrLength b =
  case B.flattenFF b of
    Blank _ -> 6
    F _ str -> String.length str
    _ -> impossible "flat"

visualStringLength : String -> Int
visualStringLength string =
  string
  |> Util.replace "\t" "        " -- replace tabs with 8 ch for ch counting
  |> String.length

approxWidth : Expr -> Int
approxWidth e =
  case B.flattenFF e of
    Blank _ -> 6
    Flagged _ _ _ _ _ -> impossible "flat"
    F _ ne -> approxNWidth ne

approxNWidth : NExpr -> Int
approxNWidth ne =
  case ne of
    Value v ->
      -- TODO: calculate visual width here
      toString v |> String.length

    Variable name ->
      String.length name

    Let lhs rhs body ->
      max
        (blankOrLength lhs
         + approxWidth rhs
         + 4 -- "let "
         + 3) -- " = "
        (approxWidth body)

    If cond ifbody elsebody ->
      3 -- "if "
      |> (+) (approxWidth cond)
      |> max (approxWidth ifbody + 2) -- indent
      |> max (approxWidth elsebody + 2) -- indent

    FnCall name exprs ->
      let sizes = exprs
                  |> List.map approxWidth
                  |> List.map ((+) 1) -- the space in between
                  |> List.sum in

      String.length name + sizes

    Lambda vars expr ->
      max
        (approxWidth expr)
        7 -- "| var -->" (only one var for now)

    Thread exprs ->
      exprs
      |> List.map approxWidth
      |> List.maximum
      |> Maybe.withDefault 2
      |> (+) 1 -- the pipe

    FieldAccess obj field ->
      approxWidth obj
      + 1 -- '.'
      + (blankOrLength field)

    ListLiteral exprs ->
      exprs
      |> List.map approxWidth
      |> List.map ((+) 2) -- ', '
      |> List.sum
      |> (+) 4 -- [  ]

    ObjectLiteral pairs ->
      pairs
      |> List.map (\(k,v) -> blankOrLength k + approxWidth v)
      |> List.map ((+) 2) -- ': '
      |> List.map ((+) 2) -- ', '
      |> List.maximum
      |> Maybe.withDefault 0
      |> (+) 4 -- {  }
