open Prelude

/* Dark */
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let blankOrData = (t: userTipe): list<blankOrData> => {
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

let toID = (ut: userTipe): TLID.t => ut.utTLID

let upsert = (m: model, ut: userTipe): model => {
  ...m,
  userTipes: Map.add(~key=ut.utTLID, ~value=ut, m.userTipes),
}

let update = (m: model, ~tlid: TLID.t, ~f: userTipe => userTipe): model => {
  ...m,
  userTipes: Map.updateIfPresent(~key=tlid, ~f, m.userTipes),
}

let remove = (m: model, ut: userTipe): model => {
  ...m,
  userTipes: Map.remove(~key=ut.utTLID, m.userTipes),
}

let fromList = (uts: list<userTipe>): TLIDDict.t<userTipe> =>
  uts |> List.map(~f=ut => (ut.utTLID, ut)) |> TLIDDict.fromList

let allNames = (tipes: TLIDDict.t<userTipe>): list<string> =>
  tipes |> Map.filterMapValues(~f=t => B.toOption(t.utName))

let toTUserType = (tipe: userTipe): option<tipe> =>
  tipe.utName |> B.toOption |> Option.map(~f=n => TUserType(n, tipe.utVersion))

let replaceDefinitionElement = (old: blankOrData, new_: blankOrData, tipe: userTipe): userTipe => {
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

let replaceTypeName = (old: blankOrData, new_: blankOrData, tipe: userTipe): userTipe => {
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

let replace = (old: blankOrData, new_: blankOrData, tipe: userTipe): userTipe =>
  tipe |> replaceTypeName(old, new_) |> replaceDefinitionElement(old, new_)

let extend = (tipe: userTipe): userTipe =>
  switch tipe.utDefinition {
  | UTRecord(fields) =>
    let newFields = Belt.List.concat(fields, list{{urfName: B.new_(), urfTipe: B.new_()}})
    {...tipe, utDefinition: UTRecord(newFields)}
  }

let removeField = (tipe: userTipe, field: userRecordField): userTipe =>
  switch tipe.utDefinition {
  | UTRecord(fields) =>
    let newFields = List.filter(~f=f => field != f, fields)
    {...tipe, utDefinition: UTRecord(newFields)}
  }
