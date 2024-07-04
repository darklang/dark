module BuiltinCliHost.Utils

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs

open LibExecution.RuntimeTypes

module Canvas =
  type PTCanvasModule =
    { // These will end up in the package manager
      types : List<PT.PackageType.PackageType>
      constants : List<PT.PackageConstant.PackageConstant>
      fns : List<PT.PackageFn.PackageFn>

      dbs : List<PT.DB.T>
      // TODO: consider breaking this down into httpHandlers, crons, workers, and repls
      handlers : List<PT.Handler.Spec * PT.Expr>
      exprs : List<PT.Expr> }

  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.Parser.Canvas.pTCanvasModule
  let toDT (c : PTCanvasModule) : Dval =
    let fields =
      [ "types",
        DList(ValueType.unknownTODO, List.map PT2DT.PackageType.toDT c.types)
        "constants",
        DList(ValueType.unknownTODO, List.map PT2DT.PackageConstant.toDT c.constants)
        "fns", DList(ValueType.unknownTODO, List.map PT2DT.PackageFn.toDT c.fns)
        "exprs", DList(ValueType.unknownTODO, List.map PT2DT.Expr.toDT c.exprs) ]
    DRecord(typeName, typeName, [], Map fields)


  let fromDT (d : Dval) : PTCanvasModule =
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
      { types = types
        constants = constants
        fns = fns
        dbs = []
        handlers = []
        exprs = exprs }

    | _ -> Exception.raiseInternal "Invalid PTCanvasModule" []
