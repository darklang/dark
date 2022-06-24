open Prelude

// Dark
module B = BlankOr

// ------------------------
// PointerData
// ------------------------

let typeOf = (pd: blankOrData): blankOrType =>
  switch pd {
  | PEventModifier(_) => EventModifier
  | PEventName(_) => EventName
  | PEventSpace(_) => EventSpace
  | PDBName(_) => DBName
  | PDBColName(_) => DBColName
  | PDBColType(_) => DBColType
  | PFnName(_) => FnName
  | PFnReturnTipe(_) => FnReturnTipe
  | PParamName(_) => ParamName
  | PParamTipe(_) => ParamTipe
  | PTypeName(_) => TypeName
  | PTypeFieldName(_) => TypeFieldName
  | PTypeFieldTipe(_) => TypeFieldTipe
  }

let toID = (pd: blankOrData): ID.t =>
  switch pd {
  | PEventModifier(d) => B.toID(d)
  | PEventName(d) => B.toID(d)
  | PEventSpace(d) => B.toID(d)
  | PDBName(d) => B.toID(d)
  | PDBColName(d) => B.toID(d)
  | PDBColType(d) => B.toID(d)
  | PFnName(d) => B.toID(d)
  | PFnReturnTipe(d) => B.toID(d)
  | PParamName(d) => B.toID(d)
  | PParamTipe(d) => B.toID(d)
  | PTypeName(d) => B.toID(d)
  | PTypeFieldName(d) => B.toID(d)
  | PTypeFieldTipe(d) => B.toID(d)
  }

let isBlank = (pd: blankOrData): bool =>
  switch pd {
  | PEventModifier(str)
  | PEventName(str)
  | PEventSpace(str)
  | PDBName(str)
  | PDBColName(str)
  | PDBColType(str)
  | PFnName(str)
  | PParamName(str)
  | PTypeName(str)
  | PTypeFieldName(str) =>
    B.isBlank(str)
  | PFnReturnTipe(t) | PTypeFieldTipe(t) | PParamTipe(t) => B.isBlank(t)
  }

let toContent = (pd: blankOrData): string => {
  let bs2s = s => s |> B.toOption |> Option.unwrap(~default="")
  switch pd {
  | PEventModifier(d)
  | PEventName(d)
  | PEventSpace(d)
  | PDBName(d)
  | PDBColName(d)
  | PDBColType(d)
  | PFnName(d)
  | PParamName(d)
  | PTypeName(d)
  | PTypeFieldName(d) =>
    bs2s(d)
  | PFnReturnTipe(d) | PParamTipe(d) | PTypeFieldTipe(d) =>
    d |> B.toOption |> Option.map(~f=Prelude.tipe2str) |> Option.unwrap(~default="")
  }
}
