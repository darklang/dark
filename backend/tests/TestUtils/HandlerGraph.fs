/// Build a router `Dval` (a `DApplicable` that takes a `Stdlib.Http.Request`
/// and returns a `Stdlib.Http.Response`) from raw HTTP-handler records
/// parsed from `.test` fixtures.
///
/// The router is `fun request -> (<body>)` — no `routeRequest` wrapping.
/// This keeps the lambda's return type untyped (Dval), so a handler that
/// returns the wrong shape (e.g. an `Int64` instead of a `Response`) still
/// flows through to F#'s `toHttpResponse`, which handles wrong-shape via
/// `wrongTypeResponse`. Matches the historical handler model where each
/// handler was a top-level fn without an enforced return type.
///
/// All current fixtures have a single handler. If a future fixture needs
/// multi-handler dispatch, wrap the bodies in
/// `Stdlib.HttpServer.routeRequest` at the call site (the type system
/// will then enforce a `Response` return type, which is fine when the
/// fixtures want it).
///
/// Lives in TestUtils (not in Builtins.Http.Server) because it depends on
/// the parser pipeline (`TestUtils.parsePTExpr` invokes a Dark-side parser
/// via `Execution.executeFunction`), which would balloon
/// Builtins.Http.Server's dep graph if introduced there.
module TestUtils.HandlerGraph

open System.Threading.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution


/// One HTTP handler as parsed from a `.test` fixture: the `route`, `method`,
/// and the raw Dark source for the handler body.
type RawHandler = { route : string; method : string; code : string }


/// Build a router Dval (DApplicable) from a list of raw handlers. The
/// resulting lambda's instruction cache is populated on `exeState`, so it
/// can be passed directly as the `handler` argument to `httpServerServe`
/// running on the same `exeState`.
let buildRouter
  (exeState : RT.ExecutionState)
  (handlers : List<RawHandler>)
  : Task<RT.Dval> =
  task {
    let routerSource =
      match handlers with
      | [] -> "fun request -> Darklang.Stdlib.Http.notFound ()"
      | [ h ] -> $"""fun request -> ({h.code})"""
      | _ ->
        Exception.raiseInternal
          "HandlerGraph.buildRouter: multi-handler fixtures aren't supported.
           Wrap handler bodies in Stdlib.HttpServer.routeRequest at the
           call site if you need dispatch."
          [ "handlerCount", List.length handlers ]

    let! ptExpr = TestUtils.parsePTExpr routerSource
    let rtInstrs = PT2RT.Expr.toRT Map.empty 0 None ptExpr

    let! result = Execution.executeExpr exeState rtInstrs

    match result with
    | Ok dval -> return dval
    | Error(rte, _callStack) ->
      let! errStr = Execution.runtimeErrorToString exeState rte
      let asString =
        match errStr with
        | Ok(RT.DString s) -> s
        | _ -> string rte
      return
        Exception.raiseInternal
          "HandlerGraph.buildRouter failed to evaluate router source"
          [ "source", routerSource; "rte", asString ]
  }
