module ViewCode exposing
    ( viewDarkType
    , viewExpr
    , viewHandler
    )

import AST
import Blank as B
import Dict
import Html
import Html.Attributes as Attrs
import List.Extra as LE
import Maybe.Extra as ME
import Nineteen.String
import Prelude exposing (..)
import Runtime as RT
import SpecHeaders
import Svg
import Svg.Attributes as SA
import Types exposing (..)
import Util exposing (transformToStringEntry)
import ViewBlankOr exposing (..)
import ViewUtils exposing (..)
import VirtualDom


viewFieldName : BlankViewer String
viewFieldName vs c f =
    let
        configs =
            c ++ [ ClickSelectAs (B.toID f) ] ++ withFeatureFlag vs f
    in
    viewBlankOr viewNFieldName Field vs configs f


viewVarBind : BlankViewer String
viewVarBind vs c v =
    let
        configs =
            idConfigs ++ c
    in
    viewBlankOr viewNVarBind VarBind vs configs v


viewKey : BlankViewer String
viewKey vs c k =
    let
        configs =
            idConfigs ++ c
    in
    viewBlankOr viewNVarBind Key vs configs k


viewDarkType : BlankViewer NDarkType
viewDarkType vs c dt =
    let
        configs =
            idConfigs ++ c
    in
    viewBlankOr viewNDarkType DarkType vs configs dt


viewExpr : Int -> BlankViewer NExpr
viewExpr depth vs c e =
    let
        width =
            approxWidth e

        widthClass =
            [ wc ("width-" ++ toString width) ]
                ++ (if width > 120 then
                        [ wc "too-wide" ]

                    else
                        []
                   )

        configs =
            idConfigs
                ++ c
                ++ withFeatureFlag vs e
                ++ withEditFn vs e
                ++ widthClass

        id =
            B.toID e
    in
    viewBlankOr (viewNExpr depth id) Expr vs configs e


viewEventName : BlankViewer String
viewEventName vs c v =
    let
        configs =
            idConfigs ++ c
    in
    viewText EventName vs configs v


viewEventSpace : BlankViewer String
viewEventSpace vs c v =
    let
        configs =
            idConfigs ++ c
    in
    viewText EventSpace vs configs v


viewEventModifier : BlankViewer String
viewEventModifier vs c v =
    let
        configs =
            idConfigs ++ c
    in
    viewText EventModifier vs configs v


viewNDarkType : Viewer NDarkType
viewNDarkType vs c d =
    case d of
        DTEmpty ->
            text vs c "Empty"

        DTString ->
            text vs c "String"

        DTAny ->
            text vs c "Any"

        DTInt ->
            text vs c "Int"

        DTObj ts ->
            let
                nested =
                    ts
                        |> List.map
                            (\( n, dt ) ->
                                [ viewText DarkTypeField vs [ wc "fieldname" ] n
                                , text vs [ wc "colon" ] ":"
                                , viewDarkType vs [ wc "fieldvalue" ] dt
                                ]
                            )
                        |> List.intersperse
                            [ text vs [ wc "separator" ] "," ]
                        |> List.concat

                open =
                    text vs [ wc "open" ] "{"

                close =
                    text vs [ wc "close" ] "}"
            in
            Html.div
                [ Attrs.class "type-object" ]
                ([ open ] ++ nested ++ [ close ])


viewNVarBind : Viewer VarName
viewNVarBind vs config f =
    text vs config f


viewNFieldName : Viewer FieldName
viewNFieldName vs config f =
    text vs config f


depthString : Int -> String
depthString n =
    "precedence-" ++ toString n


viewRopArrow : ViewState -> Html.Html Msg
viewRopArrow vs =
    let
        line =
            Svg.path
                [ SA.stroke "red"
                , SA.strokeWidth "1.5px"
                , SA.d "M 0,0 z"
                , VirtualDom.attribute "opacity" "0.3"
                , SA.markerEnd "url(#arrow)"
                ]
                []

        head =
            Svg.defs
                []
                [ Svg.marker
                    [ SA.id "arrow"
                    , SA.markerWidth "10"
                    , SA.markerHeight "10"
                    , SA.refX "0"
                    , SA.refY "3"
                    , SA.orient "auto"
                    , SA.markerUnits "strokeWidth"
                    ]
                    [ Svg.path
                        [ SA.d "M0,0 L0,6 L9,3 z"
                        , SA.fill "#f00"
                        ]
                        []
                    ]
                ]

        svg =
            Svg.svg
                [ Attrs.style
                    [ ( "position", "absolute" )
                    , ( "pointer-events", "none" ) -- don't eat clicks
                    , ( "margin-top", "-10px" )
                    , ( "fill", "none" )
                    ]
                ]
                [ line, head ]
    in
    Html.node
        "rop-arrow"
        -- Force the rop-webcomponent to update to fix the size
        -- TODO: fix this in the component, as it's too expensive to generate a
        -- new random number for every view and pass it in if we call this function.
        [ VirtualDom.attribute "update" "true"
        , deTLID vs.tl.id
            |> Nineteen.String.fromInt
            |> VirtualDom.attribute "tlid"
        ]
        [ svg ]


viewNExpr : Int -> ID -> Viewer NExpr
viewNExpr d id vs config e =
    let
        vExpr d =
            viewExpr d vs []

        vExprTw d =
            let
                vs2 =
                    { vs | tooWide = True }
            in
            viewExpr d vs2 []

        t =
            text vs

        n c =
            div vs (nested :: c)

        a c =
            text vs (atom :: c)

        kw =
            keyword vs

        all =
            idConfigs ++ config

        dv =
            DisplayValue

        cs =
            ClickSelect

        mo =
            Mouseover

        incD =
            d + 1
    in
    case e of
        Value v ->
            let
                cssClass =
                    v |> RT.tipeOf |> toString |> String.toLower

                valu =
                    -- TODO: remove
                    if RT.isString v then
                        transformToStringEntry v

                    else
                        v

                computedValue =
                    if d == 0 then
                        [ ComputedValue ]

                    else
                        []

                tooWide =
                    if vs.tooWide then
                        [ wc "short-strings" ]

                    else
                        []
            in
            a (wc cssClass :: wc "value" :: all ++ tooWide ++ computedValue) valu

        Variable name ->
            if List.member id vs.relatedBlankOrs then
                a (wc "variable" :: wc "related-change" :: all) vs.ac.value

            else
                a (wc "variable" :: all) name

        Let lhs rhs body ->
            let
                lhsID =
                    B.toID lhs

                bodyID =
                    B.toID body
            in
            n (wc "letexpr" :: all)
                [ kw [] "let"
                , viewVarBind vs [ wc "letvarname" ] lhs
                , a [ wc "letbind", ComputedValueAs lhsID ] "="
                , n [ wc "letrhs", dv, cs ] [ vExpr d rhs ]
                , n [ wc "letbody" ] [ vExpr d body ]
                ]

        If cond ifbody elsebody ->
            n (wc "ifexpr" :: all)
                [ kw [] "if"
                , n [ wc "cond" ] [ vExpr d cond ]
                , n [ wc "ifbody" ] [ vExpr 0 ifbody ]
                , kw [] "else"
                , n [ wc "elsebody" ] [ vExpr 0 elsebody ]
                ]

        FnCall name exprs sendToRail ->
            let
                width =
                    approxNWidth e

                viewTooWideArg name d e =
                    Html.div
                        [ Attrs.attribute "param-name" name
                        , Attrs.class "arg-on-new-line"
                        ]
                        [ vExprTw d e ]

                ve name =
                    if width > 120 then
                        viewTooWideArg name

                    else
                        vExpr

                fnname parens =
                    let
                        withP name =
                            if parens then
                                "(" ++ name ++ ")"

                            else
                                name
                    in
                    case String.split "::" name of
                        [ mod, justname ] ->
                            n [ wc "namegroup", atom ]
                                [ t [ wc "module" ] mod
                                , t [ wc "moduleseparator" ] "::"
                                , t [ wc "fnname" ] (withP justname)
                                ]

                        _ ->
                            a [ wc "fnname" ] (withP name)

                fn =
                    vs.ac.functions
                        |> LE.find (\f -> f.name == name)
                        |> Maybe.withDefault
                            { name = "fnLookupError"
                            , parameters = []
                            , description = "default, fn error"
                            , returnTipe = TError
                            , previewExecutionSafe = True
                            , infix = False
                            }

                previous =
                    case vs.tl.data of
                        TLHandler h ->
                            h.ast
                                |> AST.threadPrevious id
                                |> ME.toList

                        TLFunc f ->
                            f.ast
                                |> AST.threadPrevious id
                                |> ME.toList

                        TLDB db ->
                            impossible db

                -- buttons
                allExprs =
                    previous ++ exprs

                isComplete v =
                    v
                        |> getLiveValue vs.lvs
                        |> (\v ->
                                case v of
                                    Nothing ->
                                        False

                                    Just (Err _) ->
                                        True

                                    Just (Ok val) ->
                                        not
                                            (RT.isIncomplete val
                                                || RT.isError val
                                            )
                           )

                ropArrow =
                    if sendToRail == NoRail then
                        Html.div [] []

                    else
                        viewRopArrow vs

                paramsComplete =
                    List.all (isComplete << B.toID) allExprs

                resultHasValue =
                    isComplete id

                buttonUnavailable =
                    not paramsComplete

                showButton =
                    not fn.previewExecutionSafe

                buttonNeeded =
                    not resultHasValue

                showExecuting =
                    isExecuting vs id

                exeIcon =
                    "play"

                events =
                    [ eventNoPropagation "click"
                        (\_ -> ExecuteFunctionButton vs.tl.id id)
                    , nothingMouseEvent "mouseup"
                    , nothingMouseEvent "mousedown"
                    , nothingMouseEvent "dblclick"
                    ]

                ( bClass, bEvent, bTitle, bIcon ) =
                    if buttonUnavailable then
                        ( "execution-button-unavailable"
                        , []
                        , "Cannot run: some parameters are incomplete"
                        , exeIcon
                        )

                    else if buttonNeeded then
                        ( "execution-button-needed"
                        , events
                        , "Click to execute function"
                        , exeIcon
                        )

                    else
                        ( "execution-button-repeat"
                        , events
                        , "Click to execute function again"
                        , "redo"
                        )

                executingClass =
                    if showExecuting then
                        " is-executing"

                    else
                        ""

                button =
                    if not showButton then
                        []

                    else
                        [ Html.div
                            ([ Attrs.class ("execution-button " ++ bClass ++ executingClass)
                             , Attrs.title bTitle
                             ]
                                ++ bEvent
                            )
                            [ fontAwesome bIcon ]
                        ]

                fnDiv parens =
                    n
                        [ wc "op", wc name, ComputedValueAs id ]
                        (fnname parens :: ropArrow :: button)
            in
            case ( fn.infix, exprs, fn.parameters ) of
                ( True, [ first, second ], [ p1, p2 ] ) ->
                    n (wc "fncall infix" :: wc (depthString d) :: all)
                        [ n [ wc "lhs" ] [ ve p1.name incD first ]
                        , fnDiv False
                        , n [ wc "rhs" ] [ ve p2.name incD second ]
                        ]

                _ ->
                    let
                        args =
                            List.map2
                                (\p e -> ve p.name incD e)
                                fn.parameters
                                exprs
                    in
                    n (wc "fncall prefix" :: wc (depthString d) :: all)
                        (fnDiv fn.infix :: args)

        Lambda vars expr ->
            let
                varname v =
                    t [ wc "lambdavarname", atom ] v
            in
            n (wc "lambdaexpr" :: all)
                [ n [ wc "lambdabinding" ] (List.map (viewVarBind vs [ atom ]) vars)
                , a [ wc "arrow" ] "->"
                , n [ wc "lambdabody" ] [ vExpr 0 expr ]
                ]

        Thread exprs ->
            let
                pipe =
                    a [ wc "thread pipe" ] "|>"

                texpr e =
                    let
                        id =
                            B.toID e

                        dopts =
                            if d == 0 then
                                [ DisplayValueOf id, ClickSelectAs id, ComputedValueAs id ]

                            else
                                [ DisplayValueOf id, ClickSelectAs id ]
                    in
                    n ([ wc "threadmember" ] ++ dopts)
                        [ pipe, vExpr 0 e ]
            in
            n (wc "threadexpr" :: mo :: dv :: config)
                (List.map texpr exprs)

        FieldAccess obj field ->
            n (wc "fieldaccessexpr" :: all)
                [ n [ wc "fieldobject" ] [ vExpr 0 obj ]
                , a [ wc "fieldaccessop operator" ] "."
                , viewFieldName vs
                    [ wc "fieldname"
                    , atom
                    , DisplayValueOf id
                    , ComputedValueAs id
                    ]
                    field
                ]

        ListLiteral exprs ->
            let
                open =
                    a [ wc "openbracket" ] "["

                close =
                    a [ wc "closebracket" ] "]"

                comma =
                    a [ wc "comma" ] ","

                lexpr e =
                    let
                        id =
                            B.toID e

                        dopts =
                            if d == 0 then
                                [ DisplayValueOf id, ClickSelectAs id, ComputedValueAs id ]

                            else
                                [ DisplayValueOf id, ClickSelectAs id ]
                    in
                    n ([ wc "listelem" ] ++ dopts)
                        [ vExpr 0 e ]

                new =
                    List.map lexpr exprs
                        |> List.intersperse comma
            in
            n (wc "list" :: mo :: dv :: config)
                ([ open ] ++ new ++ [ close ])

        ObjectLiteral pairs ->
            let
                colon =
                    a [ wc "colon" ] ":"

                open =
                    a [ wc "openbrace" ] "{"

                close =
                    a [ wc "closebrace" ] "}"

                pexpr ( k, v ) =
                    n [ wc "objectpair" ]
                        [ viewKey vs [] k, colon, vExpr 0 v ]
            in
            n (wc "object" :: mo :: dv :: config)
                ([ open ] ++ List.map pexpr pairs ++ [ close ])

        FeatureFlag msg cond a b ->
            let
                exprLabel msg =
                    Html.label [ Attrs.class "expr-label" ] [ Html.text msg ]

                isExpanded =
                    let
                        mv =
                            Dict.get (deID id) vs.featureFlags
                    in
                    case mv of
                        Just b ->
                            b

                        Nothing ->
                            True

                pickA =
                    Html.div
                        [ Attrs.class "icon pick-a parameter-btn info"
                        , Attrs.attribute "data-content" "Use Case A"
                        , Attrs.title "delete Feature Flag & use Case A"
                        , eventNoPropagation "click"
                            (\_ -> EndFeatureFlag id PickA)
                        ]
                        [ fontAwesome "check" ]

                pickB =
                    Html.div
                        [ Attrs.class "icon pick-b parameter-btn info"
                        , Attrs.attribute "data-content" "Use Case B"
                        , Attrs.title "delete Feature Flag & use Case B"
                        , eventNoPropagation "click"
                            (\_ -> EndFeatureFlag id PickB)
                        ]
                        [ fontAwesome "check" ]

                hideModal =
                    Html.div
                        [ Attrs.attribute "data-content" "Hide ff details"
                        , eventNoPropagation "click" (\_ -> ToggleFeatureFlag id False)
                        ]
                        [ fontAwesome "minus" ]

                expandModal =
                    Html.div
                        [ Attrs.attribute "data-content" "Show ff details"
                        , eventNoPropagation "click" (\_ -> ToggleFeatureFlag id True)
                        ]
                        [ fontAwesome "flag" ]

                titleBar =
                    Html.div [ Attrs.class "row title-bar" ]
                        [ viewText FFMsg vs (wc "flag-name" :: idConfigs) msg
                        , Html.div [ Attrs.class "actions" ]
                            [ if isExpanded then
                                hideModal

                              else
                                expandModal
                            ]
                        ]

                condValue =
                    ViewBlankOr.getLiveValue vs.lvs (B.toID cond)

                condResult =
                    case condValue of
                        Just (Ok lv) ->
                            RT.isTrue lv.value

                        _ ->
                            False

                blockCondition =
                    Html.div
                        [ Attrs.class "row condition" ]
                        [ exprLabel "Condition (run Case B if...)"
                        , vExpr 0 cond
                        ]

                exprBlock lbl act exp =
                    Html.div
                        [ Attrs.class "cond-expr" ]
                        [ exprLabel lbl
                        , act
                        , div vs [ wc "expr-block" ] [ vExpr 0 exp ]
                        ]

                expressions =
                    Html.div
                        [ Attrs.class "row expressions" ]
                        [ exprBlock "Case A" pickA a
                        , exprBlock "Case B" pickB b
                        ]
            in
            div vs
                [ wc "flagged shown" ]
                [ viewExpr 0
                    { vs | showEntry = False }
                    []
                    (if condResult then
                        b

                     else
                        a
                    )
                , fontAwesome "flag"
                , Html.div
                    [ Attrs.class
                        ("feature-flag"
                            ++ (if isExpanded then
                                    " expand"

                                else
                                    ""
                               )
                        )
                    ]
                    [ titleBar
                    , blockCondition
                    , expressions
                    ]
                ]


isExecuting : ViewState -> ID -> Bool
isExecuting vs id =
    List.member id vs.executingFunctions


viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
    let
        ast =
            Html.div
                [ Attrs.class "ast" ]
                [ Html.div
                    [ Attrs.class "rop-rail" ]
                    [ viewExpr 0 vs [] h.ast ]
                ]

        externalLink =
            case ( h.spec.modifier, h.spec.name ) of
                ( F _ "GET", F _ name ) ->
                    [ Html.a
                        [ Attrs.class "external"
                        , Attrs.href name
                        , Attrs.target "_blank"
                        ]
                        [ fontAwesome "external-link-alt" ]
                    ]

                _ ->
                    []

        input =
            Html.div
                [ Attrs.class "spec-type input-type" ]
                [ Html.span [ Attrs.class "header" ] [ Html.text "Input:" ]
                , viewDarkType vs [] h.spec.types.input
                ]

        output =
            Html.div
                [ Attrs.class "spec-type output-type" ]
                [ Html.span [ Attrs.class "header" ] [ Html.text "Output:" ]
                , viewDarkType vs [] h.spec.types.output
                ]

        modifier =
            if SpecHeaders.visibleModifier h.spec then
                viewEventModifier vs [ wc "modifier" ] h.spec.modifier

            else
                Html.div [] []

        lock =
            Html.div
                [ Attrs.classList
                    [ ( "handler-lock", True )
                    , ( "is-locked", vs.handlerLocked )
                    ]
                , eventNoPropagation "click"
                    (\_ -> LockHandler vs.tlid (not vs.handlerLocked))
                ]
                [ fontAwesome
                    (if vs.handlerLocked then
                        "lock"

                     else
                        "lock-open"
                    )
                ]

        header =
            Html.div
                [ Attrs.class "spec-header" ]
                [ viewEventName vs [ wc "name" ] h.spec.name
                , Html.div [] externalLink
                , viewEventSpace vs [ wc "module" ] h.spec.module_
                , modifier
                , lock
                ]
    in
    [ header, ast ]
