/// Build a router `Dval` (a `DApplicable` that takes a `Stdlib.Http.Request`
/// and returns a `Stdlib.Http.Response`) from a list of raw HTTP-handler
/// records parsed from `.test` fixtures.
///
/// The generated router uses `Stdlib.HttpServer.routeRequest` to dispatch
/// by method+route. Each handler's body is wrapped in `fun request -> <body>`
/// so handler code can reference `request` directly (matching the `.test`
/// fixture convention).
///
/// Lives in TestUtils (not in Builtins.Http.Server) because it depends on the
/// parser pipeline (`TestUtils.parsePTExpr` invokes a Dark-side parser via
/// `Execution.executeFunction`), which would balloon Builtins.Http.Server's
/// dep graph if introduced there.
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


/// Escape a string so it parses as a Dark string literal: backslash, then
/// quote.
let private escapeStringLiteral (s : string) : string =
  s.Replace("\\", "\\\\").Replace("\"", "\\\"")


/// Build the source for a single Stdlib.HttpServer.Handler record literal.
let private handlerRecordSource (h : RawHandler) : string =
  let route = escapeStringLiteral h.route
  let methodStr = escapeStringLiteral h.method
  // Wrap the handler body in `fun request -> (...)`. Outer parens around
  // <code> let multi-line bodies parse as a single expression in any context.
  $"""Darklang.Stdlib.HttpServer.Handler {{ route = "{route}"; method = "{methodStr}"; handler = fun request -> ({h.code}) }}"""


/// Build a router Dval (DApplicable) from a list of raw handlers. The
/// resulting lambda's instruction cache is populated on `exeState`, so it
/// can be passed directly as the `handler` argument to `httpServerServe`
/// running on the same `exeState`.
///
/// **Single-handler fast path** (all 10 ported fixtures): when there is
/// exactly one handler, the router is `fun request -> (<body>)` with no
/// `routeRequest` wrapping. This lets the handler return any Dval — even a
/// wrong-shape one (e.g. an `Int64` instead of a `Response`) — which the
/// F# side's `toHttpResponse` then handles via `wrongTypeResponse`. This
/// matches the historical handler model, where each handler was a top-level
/// fn without an enforced return type.
///
/// **Multi-handler path**: falls back to `Stdlib.HttpServer.routeRequest`
/// with typed `Handler` records. The type system enforces that each body
/// returns `Response`, so wrong-shape returns aren't testable here. None
/// of the current fixtures need this, but it's there for the future.
let buildRouter
  (exeState : RT.ExecutionState)
  (handlers : List<RawHandler>)
  : Task<RT.Dval> =
  task {
    let routerSource =
      match handlers with
      | [] -> "fun request -> Darklang.Stdlib.Http.notFound ()"
      | [ h ] ->
        // Single-handler: skip routeRequest dispatch entirely so the lambda's
        // return type stays untyped (Dval) and wrong-shape returns flow through.
        $"""fun request -> ({h.code})"""
      | many ->
        let recs = many |> List.map handlerRecordSource |> String.concat "; "
        $"""fun request -> Darklang.Stdlib.HttpServer.routeRequest [ {recs} ] request"""

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
