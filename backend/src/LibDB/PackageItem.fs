module LibDB.PackageItem

open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes


let fnPackageHash (fn : PT.FQFnName.FQFnName) : Option<Hash> =
  match fn with
  | PT.FQFnName.Package hash -> Some hash
  | PT.FQFnName.Builtin _ -> None
  // A method call depends on the trait TYPE (the dependency extractor records that
  // separately); there is no fn hash to depend on until dispatch picks an impl.
  | PT.FQFnName.TraitMethod _ -> None


let typePackageHash (typ : PT.FQTypeName.FQTypeName) : Option<Hash> =
  match typ with
  | PT.FQTypeName.Package hash -> Some hash


let valuePackageHash (value : PT.FQValueName.FQValueName) : Option<Hash> =
  match value with
  | PT.FQValueName.Package hash -> Some hash
  | PT.FQValueName.Builtin _ -> None

let traitPackageHash (t : PT.FQTraitName.FQTraitName) : Option<Hash> =
  match t with
  | PT.FQTraitName.Package h -> Some h
