module Builtins.Language.Libs.LanguageTools

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution


let builtinValue () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinValue ())

let builtinFnParam () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinFnParam ())
let builtinFn () = FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinFn ())

let builtinFnPurity () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinFnPurity ())

let purityToDT (p : Previewable) : Dval =
  let typeName = builtinFnPurity ()
  let caseName =
    match p with
    | Pure -> "Pure"
    | ImpurePreviewable -> "ImpurePreviewable"
    | Impure -> "Impure"
  DEnum(typeName, typeName, [], caseName, [])

let private builtinValueToDT (name : FQValueName.Builtin) (data : BuiltInValue) =
  let fields =
    [ "name", RT2DT.FQValueName.Builtin.toDT name
      "description", DString data.description
      "type", RT2DT.TypeReference.toDT data.typ ]
  DRecord(builtinValue (), builtinValue (), [], Map fields)

let private builtinFnToDT (name : FQFnName.Builtin) (data : BuiltInFn) =
  let parameters =
    data.parameters
    |> List.map (fun p ->
      let fields =
        [ "name", DString p.name; "type", RT2DT.TypeReference.toDT p.typ ]
      DRecord(builtinFnParam (), builtinFnParam (), [], Map fields))
    |> Dval.list (KTCustomType(builtinFnParam (), []))
  let fields =
    [ "name", RT2DT.FQFnName.Builtin.toDT name
      "description", DString data.description
      "typeParams", data.typeParams |> List.map DString |> Dval.list KTString
      "parameters", parameters
      "returnType", RT2DT.TypeReference.toDT data.returnType
      "purity", purityToDT data.previewable ]
  DRecord(builtinFn (), builtinFn (), [], Map fields)

let fns () : List<BuiltInFn> =
  [ { name = fn "getAllBuiltinValues" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok (builtinValue ()), []) |> TList
      description =
        "Returns a list of the Builtin values (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [| DUnit |] ->
          let vals =
            exeState.values.builtIn
            |> Dictionary.toSortedList
            |> List.map (fun (name, data) -> builtinValueToDT name data)

          DList(VT.customType (builtinValue ()) [], vals) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "getAllBuiltinFns" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok (builtinFn ()), []) |> TList
      description =
        "Returns a list of the Builtin functions (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [| DUnit |] ->
          let fns =
            exeState.fns.builtIn
            |> Dictionary.toSortedList
            |> List.map (fun (name, data) -> builtinFnToDT name data)

          DList(VT.customType (builtinFn ()) [], fns) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "getBuiltinFn" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "version" TInt32 "" ]
      returnType = TCustomType(NR.ok (builtinFn ()), []) |> TypeReference.option
      description = "Returns metadata for one builtin function, if it exists."
      fn =
        (function
        | exeState, _, _, [| DString name; DInt32 version |] ->
          let name : FQFnName.Builtin = { name = name; version = version }
          let value =
            match exeState.fns.builtIn.TryGetValue name with
            | true, data -> Some(builtinFnToDT name data)
            | false, _ -> None
          value |> Dval.option (KTCustomType(builtinFn (), [])) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "getBuiltinValue" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "version" TInt32 "" ]
      returnType = TCustomType(NR.ok (builtinValue ()), []) |> TypeReference.option
      description = "Returns metadata for one builtin value, if it exists."
      fn =
        (function
        | exeState, _, _, [| DString name; DInt32 version |] ->
          let name : FQValueName.Builtin = { name = name; version = version }
          let value =
            match exeState.values.builtIn.TryGetValue name with
            | true, data -> Some(builtinValueToDT name data)
            | false, _ -> None
          value |> Dval.option (KTCustomType(builtinValue (), [])) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
