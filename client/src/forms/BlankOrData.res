type rec t =
  | PEventName(BlankOr.t<string>)
  | PEventModifier(BlankOr.t<string>)
  | PEventSpace(BlankOr.t<string>)
  | PDBName(BlankOr.t<string>)
  | PDBColName(BlankOr.t<string>)
  | PDBColType(BlankOr.t<string>)
  | PFnName(BlankOr.t<string>)
  | PFnReturnType(BlankOr.t<DType.t>)
  | PParamName(BlankOr.t<string>)
  | PParamType(BlankOr.t<DType.t>)
  | PTypeName(BlankOr.t<string>)
  | PTypeFieldName(BlankOr.t<string>)
  | PTypeFieldType(BlankOr.t<DType.t>)

let decode = (j: Js.Json.t): t => {
  open Json_decode_extended
  let dv1 = variant1
  variants(
    list{
      ("PEventName", dv1(x => PEventName(x), BlankOr.decode(string))),
      ("PEventSpace", dv1(x => PEventSpace(x), BlankOr.decode(string))),
      ("PEventModifier", dv1(x => PEventModifier(x), BlankOr.decode(string))),
      ("PDBName", dv1(x => PDBName(x), BlankOr.decode(string))),
      ("PDBColName", dv1(x => PDBColName(x), BlankOr.decode(string))),
      ("PDBColType", dv1(x => PDBColType(x), BlankOr.decode(string))),
      ("PFnName", dv1(x => PFnName(x), BlankOr.decode(string))),
      ("PParamName", dv1(x => PParamName(x), BlankOr.decode(string))),
      ("PParamTipe", dv1(x => PParamType(x), BlankOr.decode(DType.decode))),
      ("PTypeFieldName", dv1(x => PTypeFieldName(x), BlankOr.decode(string))),
      ("PTypeFieldTipe", dv1(x => PTypeFieldType(x), BlankOr.decode(DType.decode))),
    },
    j,
  )
}

let encode = (bd: t): Js.Json.t => {
  open Json_encode_extended
  let ev = variant
  switch bd {
  | PEventName(name) => ev("PEventName", list{BlankOr.encode(string, name)})
  | PEventModifier(modifier) => ev("PEventModifier", list{BlankOr.encode(string, modifier)})
  | PEventSpace(space) => ev("PEventSpace", list{BlankOr.encode(string, space)})
  | PDBName(name) => ev("PDBName", list{BlankOr.encode(string, name)})
  | PDBColName(colname) => ev("PDBColName", list{BlankOr.encode(string, colname)})
  | PDBColType(coltype) => ev("PDBColType", list{BlankOr.encode(string, coltype)})
  | PFnName(msg) => ev("PFnName", list{BlankOr.encode(string, msg)})
  | PFnReturnType(msg) => ev("PFnReturnTipe", list{BlankOr.encode(DType.encode, msg)})
  | PParamName(msg) => ev("PParamName", list{BlankOr.encode(string, msg)})
  | PParamType(msg) => ev("PParamTipe", list{BlankOr.encode(DType.encode, msg)})
  | PTypeName(n) => ev("PTypeName", list{BlankOr.encode(string, n)})
  | PTypeFieldName(n) => ev("PTypeFieldName", list{BlankOr.encode(string, n)})
  | PTypeFieldType(t) => ev("PTypeFieldTipe", list{BlankOr.encode(DType.encode, t)})
  }
}
