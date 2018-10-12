open! Porting

module D = Json_decode_extended
module E = Json_encode_extended

module Decoders = struct
  let rec tipe json =
    let open D in
    let dv0 = decodeVariant0 in
    let dv1 = decodeVariant1 in
    json
    |> decodeVariants
      [ ("TInt", dv0 Types_copy.tInt)
      ; ("TStr", dv0 Types_copy.tStr)
      ; ("TChar", dv0 Types_copy.tChar)
      ; ("TBool", dv0 Types_copy.tBool)
      ; ("TFloat", dv0 Types_copy.tFloat)
      ; ("TObj", dv0 Types_copy.tObj)
      ; ("TList", dv0 Types_copy.tList)
      ; ("TAny", dv0 Types_copy.tAny)
      ; ("TNull", dv0 Types_copy.tNull)
      ; ("TBlock", dv0 Types_copy.tBlock)
      ; ("TIncomplete", dv0 Types_copy.tIncomplete)
      ; ("TError", dv0 Types_copy.tError)
      ; ("TResp", dv0 Types_copy.tResp)
      ; ("TDB", dv0 Types_copy.tDB)
      ; ("TID", dv0 Types_copy.tID)
      ; ("TDate", dv0 Types_copy.tDate)
      ; ("TTitle", dv0 Types_copy.tTitle)
      ; ("TUrl", dv0 Types_copy.tUrl)
      ; ("TPassword", dv0 Types_copy.tPassword)
      ; ("TUuid", dv0 Types_copy.tUuid)
      ; ("TOption", dv0 Types_copy.tOption)
      ; ("TErrorRail", dv0 Types_copy.tErrorRail)
      ; ("TBelongsTo", dv1 Types_copy.tBelongsTo string)
      ; ("THasMany", dv1 Types_copy.tHasMany string)
      ; ("TDbList", dv1 Types_copy.tDbList tipe)
      ]

  let pos json : Types_copy.pos =
    {
      x = json |> D.field "x" D.int;
      y = json |> D.field "y" D.int;
    }

  let vPos json : Types_copy.vPos =
    {
      vx = json |> D.field "vx" D.int;
      vy = json |> D.field "vy" D.int;
    }

  let tlid json =
    Types_copy.TLID (D.int json)

  let id json =
    Types_copy.ID (D.int json)

  let entering json =
    let open D in
    let dv1 = decodeVariant1 in
    let dv2 = decodeVariant2 in
    json
    |> decodeVariants
      [ ("Creating", dv1 Types_copy.creating pos)
      ; ("Filling", dv2 Types_copy.filling tlid id)
      ]

  let rec cursorState json =
    let open D in
    let dv0 = decodeVariant0 in
    let dv1 = decodeVariant1 in
    let dv2 = decodeVariant2 in
    let dv4 = decodeVariant4 in
    json
    |> decodeVariants
      [ ("Selecting", dv2 Types_copy.selecting tlid (optional (id)))
      ; ("Entering", dv1 Types_copy.entering entering)
      ; ("Dragging", dv4 Types_copy.dragging tlid vPos bool cursorState)
      ; ("Deselected", dv0 Types_copy.deselected)
      ; ("SelectingCommand", dv2 Types_copy.selectingCommand tlid id)
      ]

end

module Encoders = struct
  let id (Types_copy.ID inner_id) = E.int inner_id
  let tlid (Types_copy.TLID inner_tlid) = E.int inner_tlid
  let pos (p: Types_copy.pos) =
    E.object_ [
      ("x", E.int p.x)
    ; ("y", E.int p.y)
    ]
  let vPos (vp: Types_copy.vPos) =
    E.object_ [
      ("vx", E.int vp.vx)
    ; ("vy", E.int vp.vy)
    ]
  let entryCursor ec =
    let open Types_copy in
    match ec with
    | Creating p ->
      E.encodeVariant "Creating" [pos p]
    | Filling (tlid_, id_) ->
      E.encodeVariant "Filling" [tlid tlid_; id id_]
  let rec cursorState cs =
    let open Types_copy in
    match cs with
    | Selecting (tlid_, id_) ->
      E.encodeVariant "Selecting" [tlid tlid_; E.nullable id id_]
    | Entering ec ->
      E.encodeVariant "Entering" [entryCursor ec]
    | Dragging (tlid_, vp, hasMoved, wrappedCs) ->
      E.encodeVariant
        "Dragging"
        [tlid tlid_; vPos vp; E.bool hasMoved; cursorState wrappedCs]
    | SelectingCommand (tlid_, id_) ->
      E.encodeVariant "SelectingCommand" [tlid tlid_; id id_]
    | Deselected ->
      E.encodeVariant "Deselected" []
  let rec tipe t =
    let open Types_copy in
    match t with
    | TInt -> E.encodeVariant "TInt" []
    | TStr -> E.encodeVariant "TStr" []
    | TChar -> E.encodeVariant "TChar" []
    | TBool -> E.encodeVariant "TBool" []
    | TFloat -> E.encodeVariant "TFloat" []
    | TObj -> E.encodeVariant "TObj" []
    | TList -> E.encodeVariant "TList" []
    | TAny -> E.encodeVariant "TAny" []
    | TNull -> E.encodeVariant "TNull" []
    | TBlock -> E.encodeVariant "TBlock" []
    | TIncomplete -> E.encodeVariant "TIncomplete" []
    | TError -> E.encodeVariant "TError" []
    | TResp -> E.encodeVariant "TResp" []
    | TDB -> E.encodeVariant "TDB" []
    | TID -> E.encodeVariant "TID" []
    | TDate -> E.encodeVariant "TDate" []
    | TTitle -> E.encodeVariant "TTitle" []
    | TUrl -> E.encodeVariant "TUrl" []
    | TPassword -> E.encodeVariant "TPassword" []
    | TUuid -> E.encodeVariant "TUuid" []
    | TOption -> E.encodeVariant "TOption" []
    | TErrorRail -> E.encodeVariant "TErrorRail" []
    | TBelongsTo s -> E.encodeVariant "TBelongsTo" [E.string s]
    | THasMany s -> E.encodeVariant "THasMany" [E.string s]
    | TDbList dbt -> E.encodeVariant "TDbList" [tipe dbt]
end
