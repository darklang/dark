open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLID.Dict

type model = AppTypes.model

let blankOrData = (t: PT.UserType.t): list<blankOrData> => {
  let namePointer = PTypeName(t.name)
  let definitionPointers = switch t.definition {
  | UTRecord(fields) =>
    List.fold(
      ~initial=list{},
      ~f=(acc, f) =>
        Belt.List.concat(
          acc,
          list{
            PTypeFieldName(BlankOr.fromStringID(f.name, f.nameID)),
            PTypeFieldTipe(BlankOr.fromOptionID(f.typ, f.typeID)),
          },
        ),
      fields,
    )
  }

  list{namePointer, ...definitionPointers}
}

let toID = (ut: PT.UserType.t): TLID.t => ut.tlid

let upsert = (m: model, ut: PT.UserType.t): model => {
  ...m,
  userTipes: Map.add(~key=ut.tlid, ~value=ut, m.userTipes),
}

let update = (m: model, ~tlid: TLID.t, ~f: PT.UserType.t => PT.UserType.t): model => {
  ...m,
  userTipes: Map.updateIfPresent(~key=tlid, ~f, m.userTipes),
}

let remove = (m: model, ut: PT.UserType.t): model => {
  ...m,
  userTipes: Map.remove(~key=ut.tlid, m.userTipes),
}

let fromList = (uts: list<PT.UserType.t>): TLID.Dict.t<PT.UserType.t> =>
  uts |> List.map(~f=(ut: PT.UserType.t) => (ut.tlid, ut)) |> TLID.Dict.fromList

let allNames = (tipes: TLID.Dict.t<PT.UserType.t>): list<string> =>
  tipes |> Map.filterMapValues(~f=(ut: PT.UserType.t) => B.toOption(ut.name))

let toTUserType = (tipe: PT.UserType.t): option<DType.t> =>
  tipe.name |> B.toOption |> Option.map(~f=n => DType.TUserType(n, tipe.version))

let replaceDefinitionElement = (
  old: blankOrData,
  new_: blankOrData,
  tipe: PT.UserType.t,
): PT.UserType.t => {
  let sId = P.toID(old)
  switch tipe.definition {
  | UTRecord(fields) =>
    let newFields = fields->List.map(~f=f =>
      if f.nameID == sId {
        switch new_ {
        | PTypeFieldName(new) =>
          let (name, nameID) = B.toStringID(new)
          {...f, name: name, nameID: nameID}
        | _ => f
        }
      } else if f.typeID == sId {
        switch new_ {
        | PTypeFieldTipe(new) =>
          let (typ, typeID) = B.toOptionID(new)
          {...f, typ: typ, typeID: typeID}
        | _ => f
        }
      } else {
        f
      }
    )

    {...tipe, definition: UTRecord(newFields)}
  }
}

let replaceTypeName = (old: blankOrData, new_: blankOrData, tipe: PT.UserType.t): PT.UserType.t => {
  let sId = P.toID(old)
  if B.toID(tipe.name) == sId {
    switch new_ {
    | PTypeName(new_) => {...tipe, name: B.replace(sId, new_, tipe.name)}
    | _ => tipe
    }
  } else {
    tipe
  }
}

let replace = (old: blankOrData, new_: blankOrData, tipe: PT.UserType.t): PT.UserType.t =>
  tipe |> replaceTypeName(old, new_) |> replaceDefinitionElement(old, new_)

let extend = (tipe: PT.UserType.t): PT.UserType.t =>
  switch tipe.definition {
  | UTRecord(fields) =>
    let newFields = Belt.List.concat(
      fields,
      list{{name: "", nameID: gid(), typ: None, typeID: gid()}},
    )
    {...tipe, definition: UTRecord(newFields)}
  }

let removeField = (tipe: PT.UserType.t, field: PT.UserType.RecordField.t): PT.UserType.t =>
  switch tipe.definition {
  | UTRecord(fields) =>
    let newFields = List.filter(~f=f => field != f, fields)
    {...tipe, definition: UTRecord(newFields)}
  }
