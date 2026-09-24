/// Live values for the editor: run a function again on the inputs its last recorded call had,
/// and hand back what every call inside it produced, keyed by the source expression that made
/// it. The editor (the LSP's inlay hints, the workbench's detail pane) puts the values beside
/// the code. Data only: what a value looks like is Dark's to decide.
///
/// Two builtins. `tracesLastInputs` finds the inputs; `liveRun` runs the current version of
/// the function on them under a tracer that collects `TraceExpr` results. The effects the run
/// makes are performed, not replayed: a function's last recorded call is one row in a trace, and
/// its effect log is the whole run's, keyed by process and ordinal, which a single call taken
/// out of the middle cannot line up with. So a function that reads the clock shows a fresh time.
module Builtins.Matter.Libs.LiveValues

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Sqlite
open LibExecution.Effects

module Dval = LibExecution.Dval
module BinarySer = LibSerialization.Binary.Serialization
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution
module Execution = LibExecution.Execution
module TracesRefs = LibExecution.PackageRefs.Type.Tracing

let private dvalKT () = RT2DT.Dval.knownType ()

let private valuesKT () =
  KTTuple(ValueType.Known KTInt64, ValueType.Known(dvalKT ()), [])

let fns () : List<BuiltInFn> =
  [ { name = fn "tracesCallsOf" 0
      typeParams = []
      parameters =
        [ Param.make
            "keys"
            (TList TString)
            "how the trace store names the function: its dotted name, and its content hashes"
          Param.make "limit" TInt64 "" ]
      returnType =
        TList(TCustomType(NR.ok (FQTypeName.fqPackage (TracesRefs.call ())), []))
      description =
        "The recorded calls of this function, newest first, across runs. Classic's trace dots, "
        + "for any function: one row per call, with the run it was made in."
      fn =
        (function
        | _, _, _, [| DList(_, hashes); DInt64 limit |] ->
          uply {
            let tn = FQTypeName.fqPackage (TracesRefs.call ())
            let hashes =
              hashes
              |> List.choose (fun h ->
                match h with
                | DString s -> Some s
                | _ -> None)
            if List.isEmpty hashes then
              return Dval.list (KTCustomType(tn, [])) []
            else
              let placeholders =
                hashes |> List.mapi (fun i _ -> $"@h{i}") |> String.concat ", "
              let! rows =
                Sql.query
                  $"SELECT c.call_id, c.trace_id, c.ord, t.handler_desc, t.timestamp
                    FROM trace_fn_calls c
                    JOIN traces t ON t.id = c.trace_id
                    WHERE c.fn_hash IN ({placeholders}) AND c.kind = 'function'
                    ORDER BY t.timestamp DESC, c.rowid DESC
                    LIMIT @limit"
                |> Sql.parameters (
                  ("limit", Sql.int64 limit)
                  :: (hashes |> List.mapi (fun i h -> $"h{i}", Sql.string h))
                )
                |> Sql.executeAsync (fun read ->
                  DRecord(
                    tn,
                    tn,
                    [],
                    Map
                      [ "callId", DString(read.string "call_id")
                        "traceId", DString(read.string "trace_id")
                        "entry", DString(read.string "handler_desc")
                        "created", DString(read.string "timestamp")
                        "ord", DInt64(read.int64 "ord") ]
                  ))
              return Dval.list (KTCustomType(tn, [])) rows
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesCallArgs" 0
      typeParams = []
      parameters = [ Param.make "callId" TString "" ]
      returnType = TypeReference.option (TList(TVariable "a"))
      description =
        "The arguments one recorded call was given, for replaying that call rather than the "
        + "newest one. None when no call has that id."
      fn =
        (function
        | _, _, _, [| DString callId |] ->
          uply {
            let! row =
              Sql.query "SELECT args FROM trace_fn_calls WHERE call_id = @c LIMIT 1"
              |> Sql.parameters [ "c", Sql.string callId ]
              |> Sql.executeRowOptionAsync (fun read -> read.bytes "args")
            match row with
            | None -> return Dval.optionNone (KTList ValueType.Unknown)
            | Some bytes ->
              match BinarySer.RT.Dval.deserialize "trace_fn_calls.args" bytes with
              | DList(_, items) ->
                return
                  Dval.optionSome
                    (KTList ValueType.Unknown)
                    (DList(ValueType.Unknown, items))
              | _ -> return Dval.optionNone (KTList ValueType.Unknown)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesLastInputs" 0
      typeParams = []
      parameters =
        [ Param.make
            "keys"
            (TList TString)
            "how the trace store names the function: its dotted name, and its content hashes (current first) for a call recorded before the name resolved" ]
      returnType = TypeReference.option (TList(TVariable "a"))
      description =
        "The arguments of the most recent recorded call to any of these versions, "
        + "or None when no trace holds one."
      fn =
        (function
        | _, _, _, [| DList(_, hashes) |] ->
          uply {
            let hashes =
              hashes
              |> List.choose (fun h ->
                match h with
                | DString s -> Some s
                | _ -> None)
            if List.isEmpty hashes then
              return Dval.optionNone (KTList ValueType.Unknown)
            else
              let placeholders =
                hashes |> List.mapi (fun i _ -> $"@h{i}") |> String.concat ", "
              let! row =
                Sql.query
                  $"SELECT args FROM trace_fn_calls
                    WHERE fn_hash IN ({placeholders}) AND kind = 'function'
                    ORDER BY rowid DESC LIMIT 1"
                |> Sql.parameters (
                  hashes |> List.mapi (fun i h -> $"h{i}", Sql.string h)
                )
                |> Sql.executeRowOptionAsync (fun read -> read.bytes "args")
              match row with
              | None -> return Dval.optionNone (KTList ValueType.Unknown)
              | Some bytes ->
                match BinarySer.RT.Dval.deserialize "trace_fn_calls.args" bytes with
                | DList(_, items) ->
                  return
                    Dval.optionSome
                      (KTList ValueType.Unknown)
                      (DList(ValueType.Unknown, items))
                | _ -> return Dval.optionNone (KTList ValueType.Unknown)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "liveRun" 0
      typeParams = []
      parameters =
        [ Param.make "hash" TString "the version of the function to run"
          Param.make
            "args"
            (TList(TVariable "a"))
            "its arguments, as a recorded call had them" ]
      returnType =
        TTuple(
          TypeReference.option (TCustomType(NR.ok (RT2DT.Dval.typeName ()), [])),
          TList(TTuple(TInt64, TCustomType(NR.ok (RT2DT.Dval.typeName ()), []), [])),
          [ TypeReference.option TString ]
        )
      description =
        "Runs the function at <param hash> on <param args>: what it returned (None when the run "
        + "failed), the value every call inside it produced up to then, keyed by the source "
        + "expression's id (an Int64, as ProgramTypes spells ids), and the runtime error's "
        + "message when it failed."
      fn =
        (function
        | exeState, vm, _, [| DString hash; DList(_, args) |] ->
          uply {
            let collected = ResizeArray<Dval>()
            // The function is the approval root of the run, as a view or a router handed to a
            // host is; the caller's access still bounds it.
            let asRoot =
              let root =
                LibDB.PolicyStore.rootState exeState vm.activeAccess [ Hash hash ]
              { root with
                  tracing =
                    { Execution.noTracing with
                        skipTracing = false
                        storeExprResult =
                          fun exprId dv ->
                            collected.Add(
                              DTuple(DInt64(int64 exprId), RT2DT.Dval.toDT dv, [])
                            ) } }
            let applicable =
              AppNamedFn
                { name = FQFnName.Package(Hash hash)
                  typeSymbolTable = TST.empty
                  typeArgs = []
                  access = None
                  argsSoFar = [] }
            let args = NEList.ofListWithDefault DUnit args
            let values () = Dval.list (valuesKT ()) (List.ofSeq collected)
            match!
              Execution.executeApplicable asRoot asRoot.access applicable args
            with
            | Ok result ->
              return
                DTuple(
                  Dval.optionSome (dvalKT ()) (RT2DT.Dval.toDT result),
                  values (),
                  [ Dval.optionNone KTString ]
                )
            | Error(rte, _) ->
              let! message = Execution.runtimeErrorMessage asRoot rte
              return
                DTuple(
                  Dval.optionNone (dvalKT ()),
                  values (),
                  [ Dval.optionSome KTString (DString message) ]
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      // The run's own calls are checked one by one under the caller's access; the builtin
      // itself does nothing to the host, like `applicableTryApply`.
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
