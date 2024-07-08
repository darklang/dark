module BuiltinCliHost.Utils

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs

open LibExecution.RuntimeTypes

module CliScript =
  type Definitions =
    { types : List<PT.PackageType.PackageType>
      constants : List<PT.PackageConstant.PackageConstant>
      fns : List<PT.PackageFn.PackageFn> }

  type PTCliScriptModule =
    { types : List<PT.PackageType.PackageType>
      constants : List<PT.PackageConstant.PackageConstant>
      fns : List<PT.PackageFn.PackageFn>
      submodules : Definitions
      exprs : List<PT.Expr> }

  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.Parser.CliScript.pTCliScriptModule

  let packageType =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.PackageType.packageType
  let packageConstant =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.PackageConstant.packageConstant
  let packageFn =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.PackageFn.packageFn

  let submoduleToDT (m : Definitions) : Dval =
    let fields =
      [ "types",
        DList(
          VT.customType packageType [],
          m.types |> List.map PT2DT.PackageType.toDT
        )
        "constants",
        DList(
          VT.customType packageConstant [],
          m.constants |> List.map PT2DT.PackageConstant.toDT
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
        "constants",
        DList(
          VT.customType packageConstant [],
          m.constants |> List.map PT2DT.PackageConstant.toDT
        )
        "fns",
        DList(VT.customType packageFn [], m.fns |> List.map PT2DT.PackageFn.toDT)
        "submodules", m.submodules |> submoduleToDT
        "exprs", DList(VT.unknownTODO, m.exprs |> List.map PT2DT.Expr.toDT) ]

    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : PTCliScriptModule =
    match d with
    | DRecord(_, _, _, fields) ->
      let types =
        match Map.tryFind "types" fields with
        | Some(DList(_, types)) ->
          List.map (fun t -> t |> PT2DT.PackageType.fromDT) types
        | _ ->
          Exception.raiseInternal "Invalid PTCliScriptModule, missing types field" []
      let constants =
        match Map.tryFind "constants" fields with
        | Some(DList(_, constants)) ->
          List.map (fun c -> c |> PT2DT.PackageConstant.fromDT) constants
        | _ ->
          Exception.raiseInternal
            "Invalid PTCliScriptModule, missing constants field"
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

          let constants =
            submodules
            |> List.map (fun m ->
              match m with
              | DRecord(_, _, _, m) ->
                match Map.tryFind "constants" m with
                | Some(DList(_, constants)) ->
                  List.map (fun c -> c |> PT2DT.PackageConstant.fromDT) constants
                | _ ->
                  Exception.raiseInternal
                    "Invalid PTCliScriptModule, missing constants field in submodule"
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

          { types = types; constants = constants; fns = fns }
        | _ ->
          Exception.raiseInternal
            "Invalid PTCliScriptModule, missing submodules field"
            []

      { types = types
        constants = constants
        fns = fns
        submodules = submodules
        exprs = exprs }

    | _ -> Exception.raiseInternal "Invalid PTCliScriptModule" []
