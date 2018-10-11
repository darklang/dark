open! Porting

module Decoders = struct
  let rec tipe json =
    let open Json_decode_extended in
    let dv0 = decodeVariant0 in
    let dv1 = decodeVariant1 in
    decodeVariants
      [ ("TInt", dv0 Types_copy.tint)
      ; ("TStr", dv0 Types_copy.tstr)
      ; ("TChar", dv0 Types_copy.tchar)
      ; ("TBool", dv0 Types_copy.tbool)
      ; ("TFloat", dv0 Types_copy.tfloat)
      ; ("TObj", dv0 Types_copy.tobj)
      ; ("TList", dv0 Types_copy.tlist)
      ; ("TAny", dv0 Types_copy.tany)
      ; ("TNull", dv0 Types_copy.tnull)
      ; ("TBlock", dv0 Types_copy.tblock)
      ; ("TIncomplete", dv0 Types_copy.tincomplete)
      ; ("TError", dv0 Types_copy.terror)
      ; ("TResp", dv0 Types_copy.tresp)
      ; ("TDB", dv0 Types_copy.tdb)
      ; ("TID", dv0 Types_copy.tid)
      ; ("TDate", dv0 Types_copy.tdate)
      ; ("TTitle", dv0 Types_copy.ttitle)
      ; ("TUrl", dv0 Types_copy.turl)
      ; ("TPassword", dv0 Types_copy.tpassword)
      ; ("TUuid", dv0 Types_copy.tuuid)
      ; ("TOption", dv0 Types_copy.toption)
      ; ("TErrorRail", dv0 Types_copy.terrorrail)
      ; ("TBelongsTo", dv1 Types_copy.tbelongsto string)
      ; ("THasMany", dv1 Types_copy.thasmany string)
      ; ("TDbList", dv1 Types_copy.tdblist tipe)
      ]

  let pos json =
    let open Json_decode_extended in
    {
      x = json |> field "x" float;
      y = json |> field "y" float;
    }

  let vpos json =
    let open Json_decode_extended in
    {
      vx = json |> field "vx" float;
      vy = json |> field "vy" float;
    }

  let tlid json =
    let open Json_decode_extended in
    json |> int |> map Types_copy.tlid

  let id json =
    let open Json_decode_extended in
    json |> int |> map Types_copy.id

  let entering json =
    let open Json_decode_extended in
    decodeVariants [
      ("Creating", dv1 Types_copy.creating pos)
    ; ("Filling", dv2 Types_copy.filling tlid id)
    ]

  let rec cursorState json =
    let open Json_decode_extended in
    decodeVariants [
      ("Selecting", dv2 Types_copy.selecting tlid (optional (decodeID)))
    ; ("Entering", dv1 Types_copy.entering entering)
    ; ("Dragging", dv4 Types_copy.dragging tlid vpos bool cursorState)
    ; ("Deselected", dv0 Types_copy.deselected)
    ; ("SelectingCommand", dv2 Types_copy.selectingcommand tlid id)
    ]

end

module Encoders = struct
end
