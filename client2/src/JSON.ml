open Belt
open Tea
open! Porting
module JSD = Json.Decode
module JSDP = Json.Decode.Pipeline
module JSE = Json.Encode
module JSEE = Json.Encode.Extra
open Types

let encodeVariant name vals = JSE.list (JSE.string name :: vals)

let decodeVariant5 const d1 d2 d3 d4 d5 =
  JSD.map5 const (JSD.index 1 d1) (JSD.index 2 d2) (JSD.index 3 d3)
    (JSD.index 4 d4) (JSD.index 5 d5)

let decodeVariant4 const d1 d2 d3 d4 =
  JSD.map4 const (JSD.index 1 d1) (JSD.index 2 d2) (JSD.index 3 d3)
    (JSD.index 4 d4)

let decodeVariant3 const d1 d2 d3 =
  JSD.map3 const (JSD.index 1 d1) (JSD.index 2 d2) (JSD.index 3 d3)

let decodeVariant2 const d1 d2 =
  JSD.map2 const (JSD.index 1 d1) (JSD.index 2 d2)

let decodeVariant1 const d1 = JSD.map const (JSD.index 1 d1)

let decodeVariant0 const = JSD.succeed const

let decodeVariants decoders =
  let _ = "type annotation" in
  let map = Dict.fromList decoders in
  let names = List.map Tuple.first decoders in
  let nameStr = String.join ", " names in
  JSD.index 0 JSD.string
  |> JSD.andThen (fun str ->
         match Dict.get str map with
         | Some decoder -> decoder
         | None ->
             (((JSD.fail <| "Got ") ^ str) ^ ", expected one of ") ^ nameStr )

let encodeID (ID id) = JSE.int id

let encodeTLID (TLID id) = JSE.int id

let decodeID = JSD.map ID JSD.int

let decodeTLID = JSD.map TLID JSD.int

let encodeBlankOr encoder v =
  match v with
  | F (ID id, s) -> encodeVariant "Filled" [JSE.int id; encoder s]
  | Blank (ID id) -> encodeVariant "Blank" [JSE.int id]

let decodeBlankOr d =
  decodeVariants
    [ ("Filled", decodeVariant2 F decodeID d)
    ; ("Blank", decodeVariant1 Blank decodeID) ]

let decodePair d1 d2 = JSD.map2 Tuple2.create (JSD.index 0 d1) (JSD.index 1 d2)

let encodePair encA encB (a, b) = JSE.list [encA a; encB b]

let decodeTriple d1 d2 d3 =
  JSD.map3 Tuple3.create (JSD.index 0 d1) (JSD.index 1 d2) (JSD.index 2 d3)

let encodeTriple encA encB encC (a, b, c) = JSE.list [encA a; encB b; encC c]

let decodeQuadriple d1 d2 d3 d4 =
  JSD.map4 Tuple4.create (JSD.index 0 d1) (JSD.index 1 d2) (JSD.index 2 d3)
    (JSD.index 3 d4)

let encodeQuadriple encA encB encC encD (a, b, c, d) =
  JSE.list [encA a; encB b; encC c; encD d]

let encodePos {x; y} = JSE.object_ [("x", JSE.int x); ("y", JSE.int y)]

let encodeVPos {vx; vy} = JSE.object_ [("vx", JSE.int vx); ("vy", JSE.int vy)]

let decodePos =
  JSDP.decode Pos |> JSDP.required "x" JSD.int |> JSDP.required "y" JSD.int

let decodeVPos =
  JSDP.decode VPos |> JSDP.required "vx" JSD.int |> JSDP.required "vy" JSD.int

let decodeException =
  let toExc short long tipe actual actualType result resultType expected info
      workarounds =
    { short
    ; long
    ; tipe
    ; actual
    ; actualType
    ; result
    ; resultType
    ; expected
    ; info
    ; workarounds }
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

let encodeException e =
  JSE.object_
    [ ("short", JSE.string e.short)
    ; ("long", JSEE.maybe JSE.string e.long)
    ; ("actual", JSEE.maybe JSE.string e.actual)
    ; ("actual_tipe", JSEE.maybe JSE.string e.actual)
    ; ("result", JSEE.maybe JSE.string e.actual)
    ; ("result_tipe", JSEE.maybe JSE.string e.actual)
    ; ("expected", JSEE.maybe JSE.string e.actual)
    ; ("info", JSEE.dict identity JSE.string e.info)
    ; ("workarounds", JSE.list (List.map JSE.string e.workarounds)) ]

let encodeList enc l = List.map enc l |> JSE.list

let encodeHttpError e =
  let encodeResponse r =
    JSE.object_
      [ ("url", JSE.string r.url)
      ; ( "status"
        , JSE.object_
            [ ("code", JSE.int r.status.code)
            ; ("message", JSE.string r.status.message) ] )
      ; ("headers", JSEE.dict identity JSE.string r.headers)
      ; ("body", JSE.string r.body) ]
  in
  match e with
  | Http.BadUrl url ->
      JSE.object_ [("type", JSE.string "BadUrl"); ("url", JSE.string url)]
  | Http.Timeout -> JSE.object_ [("type", JSE.string "Timeout")]
  | Http.NetworkErroror -> JSE.object_ [("type", JSE.string "NetworkError")]
  | Http.BadStatus response ->
      JSE.object_
        [ ("type", JSE.string "BadStatus")
        ; ("response", encodeResponse response) ]
  | Http.BadPayload (msg, response) ->
      JSE.object_
        [ ("type", JSE.string "BadPayload")
        ; ("message", JSE.string msg)
        ; ("response", encodeResponse response) ]
