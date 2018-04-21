module JSON exposing (..)

-- builtin
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Dict exposing (Dict)

-- lib

-- dark
import Types exposing (..)


------------------------------------
-- Variants
------------------------------------
encodeVariant : String -> List JSE.Value -> JSE.Value
encodeVariant name vals =
  JSE.list (JSE.string name :: vals)

decodeVariant5 : (b -> c -> d -> e -> f -> a) ->
                 JSD.Decoder b ->
                 JSD.Decoder c ->
                 JSD.Decoder d ->
                 JSD.Decoder e ->
                 JSD.Decoder f ->
                 JSD.Decoder a
decodeVariant5 const d1 d2 d3 d4 d5 =
  JSD.map5 const
    (JSD.index 1 d1)
    (JSD.index 2 d2)
    (JSD.index 3 d3)
    (JSD.index 4 d4)
    (JSD.index 5 d5)


decodeVariant4 : (b -> c -> d -> e -> a) ->
                 JSD.Decoder b ->
                 JSD.Decoder c ->
                 JSD.Decoder d ->
                 JSD.Decoder e ->
                 JSD.Decoder a
decodeVariant4 const d1 d2 d3 d4 =
  JSD.map4 const
    (JSD.index 1 d1)
    (JSD.index 2 d2)
    (JSD.index 3 d3)
    (JSD.index 4 d4)


decodeVariant3 : (b -> c -> d -> a) ->
                 JSD.Decoder b ->
                 JSD.Decoder c ->
                 JSD.Decoder d ->
                 JSD.Decoder a
decodeVariant3 const d1 d2 d3 =
  JSD.map3 const
    (JSD.index 1 d1)
    (JSD.index 2 d2)
    (JSD.index 3 d3)


decodeVariant2 : (b -> c -> a) ->
                 JSD.Decoder b ->
                 JSD.Decoder c ->
                 JSD.Decoder a
decodeVariant2 const d1 d2 =
  JSD.map2 const
    (JSD.index 1 d1)
    (JSD.index 2 d2)


decodeVariant1 : (b -> a) ->
                 JSD.Decoder b ->
                 JSD.Decoder a
decodeVariant1 const d1 =
  JSD.map const
    (JSD.index 1 d1)

decodeVariant0 : a ->
                 JSD.Decoder a
decodeVariant0 const =
  JSD.succeed const


decodeVariants : List (String, JSD.Decoder a) -> JSD.Decoder a
decodeVariants decoders =
  let map : Dict.Dict String (JSD.Decoder a)
      map = Dict.fromList decoders
      names = List.map Tuple.first decoders
      nameStr = String.join ", " names
  in
  JSD.index 0 JSD.string
  |> JSD.andThen (\str ->
    case Dict.get str map of
      Just decoder ->
        decoder
      Nothing ->
        JSD.fail <| "Got " ++ str ++ ", expected one of " ++ nameStr)



------------------------------------
-- Blanks
------------------------------------

encodeBlankOr : (a -> JSE.Value) -> (BlankOr a) -> JSE.Value
encodeBlankOr encoder v =
  case v of
    F (ID id) s ->
      encodeVariant "Filled" [JSE.int id, encoder s]
    Blank (ID id) ->
      encodeVariant "Blank" [JSE.int id]
    Flagged id msg s a b ->
      encodeVariant
        "Flagged"
        [ encodeID id
        , encodeBlankOr JSE.string msg
        , JSE.int s
        , encodeBlankOr encoder a
        , encodeBlankOr encoder b
        ]

decodeBlankOr : JSD.Decoder a -> JSD.Decoder (BlankOr a)
decodeBlankOr d =
  let db = JSD.lazy (\_ -> decodeBlankOr d)
      ds = JSD.lazy (\_ -> decodeBlankOr JSD.string) in
  decodeVariants
  [ ("Filled", decodeVariant2 F decodeID d)
  , ("Blank", decodeVariant1 Blank decodeID)
  , ("Flagged", decodeVariant5 Flagged decodeID ds JSD.int db db)
  ]

------------------------------------
-- IDs
------------------------------------
encodeID : ID -> JSE.Value
encodeID (ID id) = JSE.int id

encodeTLID : TLID -> JSE.Value
encodeTLID (TLID id) = JSE.int id

decodeID : JSD.Decoder ID
decodeID = JSD.map ID JSD.int

decodeTLID : JSD.Decoder TLID
decodeTLID = JSD.map TLID JSD.int
------------------------------------
-- Misc
------------------------------------
decodePair : JSD.Decoder a -> JSD.Decoder b -> JSD.Decoder (a,b)
decodePair d1 d2 =
  JSD.map2 (,)
    (JSD.index 0 d1)
    (JSD.index 1 d2)

encodePair : (a -> JSE.Value) -> (b -> JSE.Value) -> (a, b) -> JSE.Value
encodePair encA encB (a, b) =
  JSE.list [encA a, encB b]

decodeTriple : JSD.Decoder a -> JSD.Decoder b -> JSD.Decoder c ->
  JSD.Decoder (a,b,c)
decodeTriple d1 d2 d3 =
  JSD.map3 (,,)
    (JSD.index 0 d1)
    (JSD.index 1 d2)
    (JSD.index 2 d3)

encodeTriple : (a -> JSE.Value) -> (b -> JSE.Value) -> (c -> JSE.Value) -> (a, b, c) -> JSE.Value
encodeTriple encA encB encC (a, b, c) =
  JSE.list [encA a, encB b, encC c]


encodePos : Pos -> JSE.Value
encodePos {x,y} =
  JSE.object [ ("x", JSE.int x)
             , ("y", JSE.int y)]

encodeVPos : VPos -> JSE.Value
encodeVPos {vx,vy} =
  JSE.object [ ("vx", JSE.int vx)
             , ("vy", JSE.int vy)]

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
  let toExc : String -> String -> String -> String -> String ->
              String -> String -> String -> Dict String String ->
              List String -> Exception
      toExc short long tipe actual actualType result resultType expected info workarounds =
        { short=short
        , long=long
        , tipe=tipe
        , actual=actual
        , actualType=actualType
        , result=result
        , resultType=resultType
        , expected=expected
        , info=info
        , workarounds=workarounds }
  in
  JSDP.decode toExc
    |> JSDP.required "short" JSD.string
    |> JSDP.required "long" JSD.string
    |> JSDP.required "tipe" JSD.string
    |> JSDP.required "actual" JSD.string
    |> JSDP.required "actual_tipe" JSD.string
    |> JSDP.required "result" JSD.string
    |> JSDP.required "result_tipe" JSD.string
    |> JSDP.required "expected" JSD.string
    |> JSDP.required "info" (JSD.dict JSD.string)
    |> JSDP.required "workarounds" (JSD.list JSD.string)

