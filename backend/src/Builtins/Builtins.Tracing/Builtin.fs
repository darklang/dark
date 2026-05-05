/// Builtins that read and manipulate the trace store.
///
/// Companion to `LibDB.Tracing` (the recorder side); this project owns
/// the *reader* surface — `tracesList`, `tracesView`, `tracesFind`, etc.
/// Used by the `darklang traces` CLI commands and any other consumer
/// (dashboards, dev tools) that wants to query recorded executions.
module Builtins.Tracing.Builtin

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames = []


let builtins () = Builtin.combine [ Libs.Traces.builtins () ] fnRenames
