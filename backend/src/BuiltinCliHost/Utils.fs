module BuiltinCliHost.Utils

open Prelude

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs

open LibExecution.RuntimeTypes

module CliScript =
  type PTCliScriptModule =
    { // These will end up in the package manager
      types : List<PT.PackageType.PackageType>
      constants : List<PT.PackageConstant.PackageConstant>
      fns : List<PT.PackageFn.PackageFn>
      submodules : PT.Packages //TODO
      exprs : List<PT.Expr> }

  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.Parser.CliScript.pTCliScriptModule

  //TODO: toDT

  let fromDT (d : Dval) : PTCliScriptModule =
    match d with
    | DRecord(_, _, _, fields) ->
      let types =
        match Map.tryFind "types" fields with
        | Some(DList(_, types)) ->
          List.map (fun t -> t |> PT2DT.PackageType.fromDT) types
        | _ -> []
      let constants =
        match Map.tryFind "constants" fields with
        | Some(DList(_, constants)) ->
          List.map (fun c -> c |> PT2DT.PackageConstant.fromDT) constants
        | _ -> []
      let fns =
        match Map.tryFind "fns" fields with
        | Some(DList(_, fns)) -> List.map (fun f -> f |> PT2DT.PackageFn.fromDT) fns
        | _ -> []
      let exprs =
        match Map.tryFind "exprs" fields with
        | Some(DList(_, exprs)) -> List.map (fun e -> e |> PT2DT.Expr.fromDT) exprs
        | _ -> []

      let submodules : PT.Packages =
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
                | _ -> []
              | _ -> [])
            |> List.concat

          let constants =
            submodules
            |> List.map (fun m ->
              match m with
              | DRecord(_, _, _, m) ->
                match Map.tryFind "constants" m with
                | Some(DList(_, constants)) ->
                  List.map (fun c -> c |> PT2DT.PackageConstant.fromDT) constants
                | _ -> []
              | _ -> [])
            |> List.concat

          let fns =
            submodules
            |> List.map (fun m ->
              match m with
              | DRecord(_, _, _, m) ->
                match Map.tryFind "fns" m with
                | Some(DList(_, fns)) ->
                  List.map (fun f -> f |> PT2DT.PackageFn.fromDT) fns
                | _ -> []
              | _ -> [])
            |> List.concat

          { types = types; constants = constants; fns = fns }
        | _ -> { types = []; constants = []; fns = [] }

      { types = types
        constants = constants
        fns = fns
        submodules = submodules
        exprs = exprs }

    | _ -> Exception.raiseInternal "Invalid PTCliScriptModule" []
