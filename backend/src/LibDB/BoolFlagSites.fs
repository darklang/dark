/// Calls that pass a bare `true` or `false` to a package fn's `Bool` parameter, when the fn
/// takes two or more. `f(x, true)` does not say what `true` means; a two-case enum named for
/// it does (#6150). A fn of one parameter, such as a predicate or a setter, is left out, and so
/// is `Stdlib.Bool`, whose Bools are the values themselves.
module LibDB.BoolFlagSites

open Prelude

module PT = LibExecution.ProgramTypes
module Ast = LibExecution.ProgramTypesAst

type Site = { caller : string; callee : string }

/// The package fns <expr> calls with a literal Bool where the callee declares a `Bool`.
let private bareBoolCallees
  (fns : Map<PT.Hash, PT.PackageFn.PackageFn>)
  (expr : PT.Expr)
  : List<PT.Hash> =
  // `offset` is 1 for a piped call, whose first parameter is the pipe's value.
  let check (callee : PT.Hash) (offset : int) (args : List<PT.Expr>) =
    match Map.tryFind callee fns with
    | Some fn when NEList.length fn.parameters >= 2 ->
      let parameters = NEList.toList fn.parameters
      let flagged =
        args
        |> List.indexed
        |> List.exists (fun (i, arg) ->
          match arg, List.tryItem (i + offset) parameters with
          | PT.EBool _, Some p -> p.typ = PT.TBool
          | _ -> false)
      if flagged then [ callee ] else []
    | _ -> []
  let package (name : PT.NameResolution<PT.FQFnName.FQFnName>) =
    match name.resolved with
    | Ok(resolved : PT.ResolvedName<PT.FQFnName.FQFnName>) ->
      match resolved.name with
      | PT.FQFnName.Package p -> Some p
      | _ -> None
    | Error _ -> None
  let rec walk (e : PT.Expr) : List<PT.Hash> =
    let here =
      match e with
      | PT.EApply(_, PT.EFnName(_, name, _, _), _, args) ->
        package name
        |> Option.map (fun p -> check p 0 (NEList.toList args))
        |> Option.defaultValue []
      | PT.EPipe(_, _, parts) ->
        parts
        |> List.collect (function
          | PT.EPipeFnCall(_, name, _, args, _) ->
            package name
            |> Option.map (fun p -> check p 1 args)
            |> Option.defaultValue []
          | _ -> [])
      | _ -> []
    here @ List.collect walk (Ast.subExprs e)
  walk expr

/// Every live package fn's bare-Bool calls.
let find (names : Map<PT.Hash, string>) : Ply<List<Site>> =
  uply {
    let! loaded =
      names
      |> Map.keys
      |> Ply.List.mapSequentially (fun h ->
        LibDB.PackageManager.pt.getFn h |> Ply.map (Option.map (fun fn -> (h, fn))))
    let fns = loaded |> List.choose identity |> Map.ofList
    return
      fns
      |> Map.toList
      |> List.collect (fun (h, fn) ->
        bareBoolCallees fns fn.body
        |> List.map (fun callee ->
          let nameOf h' = Map.tryFind h' names |> Option.defaultValue ""
          { caller = nameOf h; callee = nameOf callee }))
      |> List.filter (fun s -> not (s.callee.StartsWith "Darklang.Stdlib.Bool."))
  }

/// The baseline's lines: `<count> <caller> <callee>`, sorted.
let baselineLines (sites : List<Site>) : List<string> =
  sites
  |> List.countBy identity
  |> List.map (fun (s, count) -> $"{count} {s.caller} {s.callee}")
  |> List.sort
