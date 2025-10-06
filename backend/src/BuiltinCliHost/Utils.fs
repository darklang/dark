module BuiltinCliHost.Utils

// CLEANUP consider migrating the contents of this to a ?T2DT module
// (or just rebranding some of the stuff here under a such-named submodule)

open Prelude

module PT = LibExecution.ProgramTypes
module VT = LibExecution.ValueType
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageHashes = LibExecution.PackageHashes
module DvalDecoder = LibExecution.DvalDecoder

open LibExecution.RuntimeTypes

module CliScript =
  type Definitions =
    { types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn> }

  type PTCliScriptModule =
    { types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn>
      submodules : Definitions
      exprs : List<PT.Expr> }

  let typeName =
    FQTypeName.fqPackage
      PackageHashes.Type.LanguageTools.Parser.CliScript.pTCliScriptModule

  let packageType =
    FQTypeName.fqPackage
      PackageHashes.Type.LanguageTools.ProgramTypes.PackageType.packageType
  let packageValue =
    FQTypeName.fqPackage
      PackageHashes.Type.LanguageTools.ProgramTypes.PackageValue.packageValue
  let packageFn =
    FQTypeName.fqPackage
      PackageHashes.Type.LanguageTools.ProgramTypes.PackageFn.packageFn

  let submoduleToDT (m : Definitions) : Dval =
    let fields =
      [ "types",
        DList(
          VT.customType packageType [],
          m.types |> List.map PT2DT.PackageType.toDT
        )
        "values",
        DList(
          VT.customType packageValue [],
          m.values |> List.map PT2DT.PackageValue.toDT
        )
        "fns",
        DList(VT.customType packageFn [], m.fns |> List.map PT2DT.PackageFn.toDT) ]

    DRecord(typeName, typeName, [], Map fields)

  let toDT (m : PTCliScriptModule) : Dval =
    let fields =
      [ "types",
        DList(
          VT.customType packageType [],
          m.types |> List.map PT2DT.PackageType.toDT
        )
        "values",
        DList(
          VT.customType packageValue [],
          m.values |> List.map PT2DT.PackageValue.toDT
        )
        "fns",
        DList(VT.customType packageFn [], m.fns |> List.map PT2DT.PackageFn.toDT)
        "submodules", m.submodules |> submoduleToDT
        "exprs", DList(VT.unknownTODO, m.exprs |> List.map PT2DT.Expr.toDT) ]

    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : PTCliScriptModule =
    match d with
    | DTuple(DRecord(_, _, _, fields), _, _) ->
      let types =
        match Map.tryFind "types" fields with
        | Some(DList(_, types)) ->
          List.map (fun t -> t |> PT2DT.PackageType.fromDT) types
        | _ ->
          Exception.raiseInternal "Invalid PTCliScriptModule, missing types field" []
      let values =
        match Map.tryFind "values" fields with
        | Some(DList(_, values)) ->
          List.map (fun v -> v |> PT2DT.PackageValue.fromDT) values
        | _ ->
          Exception.raiseInternal
            "Invalid PTCliScriptModule, missing values field"
            []
      let fns =
        match Map.tryFind "fns" fields with
        | Some(DList(_, fns)) -> List.map (fun f -> f |> PT2DT.PackageFn.fromDT) fns
        | _ ->
          Exception.raiseInternal "Invalid PTCliScriptModule, missing fns field" []
      let exprs =
        match Map.tryFind "exprs" fields with
        | Some(DList(_, exprs)) -> List.map (fun e -> e |> PT2DT.Expr.fromDT) exprs
        | _ ->
          Exception.raiseInternal "Invalid PTCliScriptModule, missing exprs field" []

      let submodules : Definitions =
        match Map.tryFind "submodules" fields with
        | Some(DList(_, submodules)) ->
          let types =
            submodules
            |> List.map (fun m ->
              match m with
              | DRecord(_, _, _, m) ->
                match Map.tryFind "types" m with
                | Some(DList(_, types)) ->
                  List.map (fun t -> t |> PT2DT.PackageType.fromDT) types
                | _ ->
                  Exception.raiseInternal
                    "Invalid PTCliScriptModule, missing types field in submodule"
                    []
              | _ ->
                Exception.raiseInternal
                  "Invalid PTCliScriptModule, submodules field should be a record"
                  [])
            |> List.concat

          let values =
            submodules
            |> List.map (fun m ->
              match m with
              | DRecord(_, _, _, m) ->
                match Map.tryFind "values" m with
                | Some(DList(_, values)) ->
                  List.map (fun v -> v |> PT2DT.PackageValue.fromDT) values
                | _ ->
                  Exception.raiseInternal
                    "Invalid PTCliScriptModule, missing values field in submodule"
                    []
              | _ ->
                Exception.raiseInternal
                  "Invalid PTCliScriptModule, submodules field should be a record"
                  [])
            |> List.concat

          let fns =
            submodules
            |> List.map (fun m ->
              match m with
              | DRecord(_, _, _, m) ->
                match Map.tryFind "fns" m with
                | Some(DList(_, fns)) ->
                  List.map (fun f -> f |> PT2DT.PackageFn.fromDT) fns
                | _ ->
                  Exception.raiseInternal
                    "Invalid PTCliScriptModule, missing fns field in submodule"
                    []
              | _ ->
                Exception.raiseInternal
                  "Invalid PTCliScriptModule, submodules field should be a record"
                  [])
            |> List.concat

          { types = types; values = values; fns = fns }
        | _ ->
          Exception.raiseInternal
            "Invalid PTCliScriptModule, missing submodules field"
            []

      { types = types
        values = values
        fns = fns
        submodules = submodules
        exprs = exprs }

    | _ -> Exception.raiseInternal "Invalid PTCliScriptModule" []


module ScriptsToDarkTypes =
  let scriptTypeName = FQTypeName.fqPackage PackageHashes.Type.Cli.script

  let toDT (script : LibPackageManager.Scripts.Script) : Dval =
    let fields =
      [ ("id", DString(string script.id))
        ("name", DString script.name)
        ("text", DString script.text) ]
    DRecord(scriptTypeName, scriptTypeName, [], Map.ofList fields)

  let fromDT (d : Dval) : LibPackageManager.Scripts.Script =
    match d with
    | DRecord(_, _, _, fields) ->
      { LibPackageManager.Scripts.id =
          System.Guid.Parse(DvalDecoder.field "id" fields |> DvalDecoder.string)
        LibPackageManager.Scripts.name =
          DvalDecoder.field "name" fields |> DvalDecoder.string
        LibPackageManager.Scripts.text =
          DvalDecoder.field "text" fields |> DvalDecoder.string }
    | _ -> Exception.raiseInternal "Invalid Script" []
