open! Porting

module JDE = Json_decode_extended

module Decoders = struct
  let rec tipe json =
    let open JDE in
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
      x = json |> JDE.field "x" JDE.int;
      y = json |> JDE.field "y" JDE.int;
    }

  let vpos json : Types_copy.vPos =
    {
      vx = json |> JDE.field "vx" JDE.int;
      vy = json |> JDE.field "vy" JDE.int;
    }

  let tlid json =
    Types_copy.TLID (JDE.int json)

  let id json =
    Types_copy.ID (JDE.int json)

  let entering json =
    let open JDE in
    let dv1 = decodeVariant1 in
    let dv2 = decodeVariant2 in
    json
    |> decodeVariants
      [ ("Creating", dv1 Types_copy.creating pos)
      ; ("Filling", dv2 Types_copy.filling tlid id)
      ]

  let rec cursorState json =
    let open JDE in
    let dv0 = decodeVariant0 in
    let dv1 = decodeVariant1 in
    let dv2 = decodeVariant2 in
    let dv4 = decodeVariant4 in
    json
    |> decodeVariants
      [ ("Selecting", dv2 Types_copy.selecting tlid (optional (id)))
      ; ("Entering", dv1 Types_copy.entering entering)
      ; ("Dragging", dv4 Types_copy.dragging tlid vpos bool cursorState)
      ; ("Deselected", dv0 Types_copy.deselected)
      ; ("SelectingCommand", dv2 Types_copy.selectingCommand tlid id)
      ]

end

module Encoders = struct
end
