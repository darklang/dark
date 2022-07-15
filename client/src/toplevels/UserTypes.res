open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLID.Dict

let blankOrData = (t: PT.UserType.t): list<blankOrData> => {
  let namePointer = PTypeName(t.utName)
  let definitionPointers = switch t.utDefinition {
  | UTRecord(fields) =>
    List.fold(
      ~initial=list{},
      ~f=(acc, f) =>
        Belt.List.concat(acc, list{PTypeFieldName(f.urfName), PTypeFieldTipe(f.urfTipe)}),
      fields,
    )
  }

  list{namePointer, ...definitionPointers}
}

let toID = (ut: PT.UserType.t): TLID.t => ut.utTLID

let upsert = (m: model, ut: PT.UserType.t): model => {
  ...m,
  userTipes: Map.add(~key=ut.utTLID, ~value=ut, m.userTipes),
}

let update = (m: model, ~tlid: TLID.t, ~f: PT.UserType.t => PT.UserType.t): model => {
  ...m,
  userTipes: Map.updateIfPresent(~key=tlid, ~f, m.userTipes),
}

let remove = (m: model, ut: PT.UserType.t): model => {
  ...m,
  userTipes: Map.remove(~key=ut.utTLID, m.userTipes),
}

let fromList = (uts: list<PT.UserType.t>): TLID.Dict.t<PT.UserType.t> =>
  uts |> List.map(~f=(ut: PT.UserType.t) => (ut.utTLID, ut)) |> TLID.Dict.fromList

let allNames = (tipes: TLID.Dict.t<PT.UserType.t>): list<string> =>
  tipes |> Map.filterMapValues(~f=(ut: PT.UserType.t) => B.toOption(ut.utName))

let toTUserType = (tipe: PT.UserType.t): option<DType.t> =>
  tipe.utName |> B.toOption |> Option.map(~f=n => DType.TUserType(n, tipe.utVersion))

let replaceDefinitionElement = (
  old: blankOrData,
  new_: blankOrData,
  tipe: PT.UserType.t,
): PT.UserType.t => {
  let sId = P.toID(old)
  switch tipe.utDefinition {
  | UTRecord(fields) =>
    let newFields = List.map(~f=f =>
      if B.toID(f.urfName) == sId {
        switch new_ {
        | PTypeFieldName(new_) => {...f, urfName: B.replace(sId, new_, f.urfName)}
        | _ => f
        }
      } else if B.toID(f.urfTipe) == sId {
        switch new_ {
        | PTypeFieldTipe(new_) => {...f, urfTipe: B.replace(sId, new_, f.urfTipe)}
        | _ => f
        }
      } else {
        f
      }
    , fields)

    {...tipe, utDefinition: UTRecord(newFields)}
  }
}

let replaceTypeName = (old: blankOrData, new_: blankOrData, tipe: PT.UserType.t): PT.UserType.t => {
  let sId = P.toID(old)
  if B.toID(tipe.utName) == sId {
    switch new_ {
    | PTypeName(new_) => {...tipe, utName: B.replace(sId, new_, tipe.utName)}
    | _ => tipe
    }
  } else {
    tipe
  }
}

let replace = (old: blankOrData, new_: blankOrData, tipe: PT.UserType.t): PT.UserType.t =>
  tipe |> replaceTypeName(old, new_) |> replaceDefinitionElement(old, new_)

let extend = (tipe: PT.UserType.t): PT.UserType.t =>
  switch tipe.utDefinition {
  | UTRecord(fields) =>
    let newFields = Belt.List.concat(fields, list{{urfName: B.new_(), urfTipe: B.new_()}})
    {...tipe, utDefinition: UTRecord(newFields)}
  }

let removeField = (tipe: PT.UserType.t, field: PT.UserType.RecordField.t): PT.UserType.t =>
  switch tipe.utDefinition {
  | UTRecord(fields) =>
    let newFields = List.filter(~f=f => field != f, fields)
    {...tipe, utDefinition: UTRecord(newFields)}
  }
