open! Porting

module Decoders = struct
  let rec tipe json =
    let open Json_decode_extended in
    let dv0 = decodeVariant0 in
    let dv1 = decodeVariant1 in
    let dv2 = decodeVariant2 in
    decodeVariants [
      ("TInt", dv0 Types_copy.tint)
      ("TStr", dv0 Types_copy.tstr)
      ("TChar", dv0 Types_copy.tchar)
      ("TBool", dv0 Types_copy.tbool)
      ("TFloat", dv0 Types_copy.tfloat)
      ("TObj", dv0 Types_copy.tobj)
      ("TList", dv0 Types_copy.tlist)
      ("TAny", dv0 Types_copy.tany)
      ("TNull", dv0 Types_copy.tnull)
      ("TBlock", dv0 Types_copy.tblock)
      ("TIncomplete", dv0 Types_copy.tincomplete)
      ("TError", dv0 Types_copy.terror)
      ("TResp", dv0 Types_copy.tresp)
      ("TDB", dv0 Types_copy.tdb)
      ("TID", dv0 Types_copy.tid)
      ("TDate", dv0 Types_copy.tdate)
      ("TTitle", dv0 Types_copy.ttitle)
      ("TUrl", dv0 Types_copy.turl)
      ("TPassword", dv0 Types_copy.tpassword)
      ("TUuid", dv0 Types_copy.tuuid)
      ("TOption", dv0 Types_copy.toption)
      ("TErrorRail", dv0 Types_copy.terrorrail)
      ("TBelongsTo", dv1 Types_copy.tbelongsto string)
      ("THasMany", dv1 Types_copy.thasmany string)
      ("TDbList", dv1 Types_copy.tdblist tipe)
    ]
end

module Encoders = struct
end
