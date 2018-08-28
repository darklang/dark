module JSON exposing
    ( decodeBlankOr
    , decodeException
    , decodeID
    , decodePair
    , decodePos
    , decodeTLID
    , decodeTriple
    , decodeVPos
    , decodeVariant0
    , decodeVariant1
    , decodeVariant2
    , decodeVariant3
    , decodeVariant4
    , decodeVariant5
    , decodeVariants
    , encodeBlankOr
    , encodeHttpError
    , encodeID
    , encodePair
    , encodePos
    , encodeTLID
    , encodeTriple
    , encodeVPos
    , encodeVariant
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Json.Encode as JSE
import Json.Encode.Extra as JSEE
import Types exposing (..)



------------------------------------
-- Variants
------------------------------------


encodeVariant : String -> List JSE.Value -> JSE.Value
encodeVariant name vals =
    JSE.list (JSE.string name :: vals)


decodeVariant5 :
    (b -> c -> d -> e -> f -> a)
    -> JSD.Decoder b
    -> JSD.Decoder c
    -> JSD.Decoder d
    -> JSD.Decoder e
    -> JSD.Decoder f
    -> JSD.Decoder a
decodeVariant5 const d1 d2 d3 d4 d5 =
    JSD.map5 const
        (JSD.index 1 d1)
        (JSD.index 2 d2)
        (JSD.index 3 d3)
        (JSD.index 4 d4)
        (JSD.index 5 d5)


decodeVariant4 :
    (b -> c -> d -> e -> a)
    -> JSD.Decoder b
    -> JSD.Decoder c
    -> JSD.Decoder d
    -> JSD.Decoder e
    -> JSD.Decoder a
decodeVariant4 const d1 d2 d3 d4 =
    JSD.map4 const
        (JSD.index 1 d1)
        (JSD.index 2 d2)
        (JSD.index 3 d3)
        (JSD.index 4 d4)


decodeVariant3 :
    (b -> c -> d -> a)
    -> JSD.Decoder b
    -> JSD.Decoder c
    -> JSD.Decoder d
    -> JSD.Decoder a
decodeVariant3 const d1 d2 d3 =
    JSD.map3 const
        (JSD.index 1 d1)
        (JSD.index 2 d2)
        (JSD.index 3 d3)


decodeVariant2 :
    (b -> c -> a)
    -> JSD.Decoder b
    -> JSD.Decoder c
    -> JSD.Decoder a
decodeVariant2 const d1 d2 =
    JSD.map2 const
        (JSD.index 1 d1)
        (JSD.index 2 d2)


decodeVariant1 :
    (b -> a)
    -> JSD.Decoder b
    -> JSD.Decoder a
decodeVariant1 const d1 =
    JSD.map const
        (JSD.index 1 d1)


decodeVariant0 :
    a
    -> JSD.Decoder a
decodeVariant0 const =
    JSD.succeed const


decodeVariants : List ( String, JSD.Decoder a ) -> JSD.Decoder a
decodeVariants decoders =
    let
        map : Dict.Dict String (JSD.Decoder a)
        map =
            Dict.fromList decoders

        names =
            List.map Tuple.first decoders

        nameStr =
            String.join ", " names
    in
    JSD.index 0 JSD.string
        |> JSD.andThen
            (\str ->
                case Dict.get str map of
                    Just decoder ->
                        decoder

                    Nothing ->
                        JSD.fail <| "Got " ++ str ++ ", expected one of " ++ nameStr
            )



------------------------------------
-- IDs
------------------------------------


encodeID : ID -> JSE.Value
encodeID (ID id) =
    JSE.int id


encodeTLID : TLID -> JSE.Value
encodeTLID (TLID id) =
    JSE.int id


decodeID : JSD.Decoder ID
decodeID =
    JSD.map ID JSD.int


decodeTLID : JSD.Decoder TLID
decodeTLID =
    JSD.map TLID JSD.int



------------------------------------
-- Blanks
------------------------------------


encodeBlankOr : (a -> JSE.Value) -> BlankOr a -> JSE.Value
encodeBlankOr encoder v =
    case v of
        F (ID id) s ->
            encodeVariant "Filled" [ JSE.int id, encoder s ]

        Blank (ID id) ->
            encodeVariant "Blank" [ JSE.int id ]


decodeBlankOr : JSD.Decoder a -> JSD.Decoder (BlankOr a)
decodeBlankOr d =
    decodeVariants
        [ ( "Filled", decodeVariant2 F decodeID d )
        , ( "Blank", decodeVariant1 Blank decodeID )
        ]



------------------------------------
-- Misc
------------------------------------


decodePair : JSD.Decoder a -> JSD.Decoder b -> JSD.Decoder ( a, b )
decodePair d1 d2 =
    JSD.map2 (,)
        (JSD.index 0 d1)
        (JSD.index 1 d2)


encodePair : (a -> JSE.Value) -> (b -> JSE.Value) -> ( a, b ) -> JSE.Value
encodePair encA encB ( a, b ) =
    JSE.list [ encA a, encB b ]


decodeTriple :
    JSD.Decoder a
    -> JSD.Decoder b
    -> JSD.Decoder c
    -> JSD.Decoder ( a, b, c )
decodeTriple d1 d2 d3 =
    JSD.map3 (,,)
        (JSD.index 0 d1)
        (JSD.index 1 d2)
        (JSD.index 2 d3)


encodeTriple : (a -> JSE.Value) -> (b -> JSE.Value) -> (c -> JSE.Value) -> ( a, b, c ) -> JSE.Value
encodeTriple encA encB encC ( a, b, c ) =
    JSE.list [ encA a, encB b, encC c ]


encodePos : Pos -> JSE.Value
encodePos { x, y } =
    JSE.object
        [ ( "x", JSE.int x )
        , ( "y", JSE.int y )
        ]


encodeVPos : VPos -> JSE.Value
encodeVPos { vx, vy } =
    JSE.object
        [ ( "vx", JSE.int vx )
        , ( "vy", JSE.int vy )
        ]


decodePos : JSD.Decoder Pos
decodePos =
    JSDP.decode Pos
        |> JSDP.required "x" JSD.int
        |> JSDP.required "y" JSD.int


decodeVPos : JSD.Decoder VPos
decodeVPos =
    JSDP.decode VPos
        |> JSDP.required "vx" JSD.int
        |> JSDP.required "vy" JSD.int


decodeException : JSD.Decoder Exception
decodeException =
    let
        toExc short long tipe actual actualType result resultType expected info workarounds =
            { short = short
            , long = long
            , tipe = tipe
            , actual = actual
            , actualType = actualType
            , result = result
            , resultType = resultType
            , expected = expected
            , info = info
            , workarounds = workarounds
            }
    in
    JSDP.decode toExc
        |> JSDP.required "short" JSD.string
        |> JSDP.required "long" (JSD.maybe JSD.string)
        |> JSDP.required "tipe" JSD.string
        |> JSDP.required "actual" (JSD.maybe JSD.string)
        |> JSDP.required "actual_tipe" (JSD.maybe JSD.string)
        |> JSDP.required "result" (JSD.maybe JSD.string)
        |> JSDP.required "result_tipe" (JSD.maybe JSD.string)
        |> JSDP.required "expected" (JSD.maybe JSD.string)
        |> JSDP.required "info" (JSD.dict JSD.string)
        |> JSDP.required "workarounds" (JSD.list JSD.string)


encodeHttpError : Http.Error -> JSE.Value
encodeHttpError e =
    let
        encodeResponse r =
            JSE.object
                [ ( "url", JSE.string r.url )
                , ( "status"
                  , JSE.object
                        [ ( "code", JSE.int r.status.code )
                        , ( "message", JSE.string r.status.message )
                        ]
                  )
                , ( "headers", JSEE.dict identity JSE.string r.headers )
                , ( "body", JSE.string r.body )
                ]
    in
    case e of
        Http.BadUrl url ->
            JSE.object
                [ ( "type", JSE.string "BadUrl" )
                , ( "url", JSE.string url )
                ]

        Http.Timeout ->
            JSE.object [ ( "type", JSE.string "Timeout" ) ]

        Http.NetworkError ->
            JSE.object [ ( "type", JSE.string "NetworkError" ) ]

        Http.BadStatus response ->
            JSE.object
                [ ( "type", JSE.string "BadStatus" )
                , ( "response", encodeResponse response )
                ]

        Http.BadPayload msg response ->
            JSE.object
                [ ( "type", JSE.string "BadPayload" )
                , ( "message", JSE.string msg )
                , ( "response", encodeResponse response )
                ]
