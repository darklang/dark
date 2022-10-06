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
  | PFnReturnType(_) => FnReturnType
  | PParamName(_) => ParamName
  | PParamType(_) => ParamType
  | PTypeName(_) => TypeName
  | PTypeFieldName(_) => TypeFieldName
  | PTypeFieldType(_) => TypeFieldType
  }

let toID = (pd: blankOrData): id =>
  switch pd {
  | PEventModifier(d) => B.toID(d)
  | PEventName(d) => B.toID(d)
  | PEventSpace(d) => B.toID(d)
  | PDBName(d) => B.toID(d)
  | PDBColName(d) => B.toID(d)
  | PDBColType(d) => B.toID(d)
  | PFnName(d) => B.toID(d)
  | PFnReturnType(d) => B.toID(d)
  | PParamName(d) => B.toID(d)
  | PParamType(d) => B.toID(d)
  | PTypeName(d) => B.toID(d)
  | PTypeFieldName(d) => B.toID(d)
  | PTypeFieldType(d) => B.toID(d)
  }

let isBlank = (pd: blankOrData): bool =>
  switch pd {
  | PEventModifier(str)
  | PEventName(str)
  | PEventSpace(str)
  | PDBName(str)
  | PDBColName(str)
  | PFnName(str)
  | PParamName(str)
  | PTypeName(str)
  | PTypeFieldName(str) =>
    B.isBlank(str)
  | PDBColType(t)
  | PFnReturnType(t)
  | PTypeFieldType(t)
  | PParamType(t) =>
    B.isBlank(t)
  }

let toContent = (pd: blankOrData): string => {
  let bs2s = s => s |> B.toOption |> Option.unwrap(~default="")
  switch pd {
  | PEventModifier(d)
  | PEventName(d)
  | PEventSpace(d)
  | PDBName(d)
  | PDBColName(d)
  | PFnName(d)
  | PParamName(d)
  | PTypeName(d)
  | PTypeFieldName(d) =>
    bs2s(d)
  | PFnReturnType(d) | PParamType(d) | PTypeFieldType(d) | PDBColType(d) =>
    d |> B.toOption |> Option.map(~f=DType.tipe2str) |> Option.unwrap(~default="")
  }
}
