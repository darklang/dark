/// Live values for the editor: run a function again on the inputs its last recorded call had,
/// and hand back what every call inside it produced, keyed by the source expression that made
/// it. The editor (the LSP's inlay hints, the workbench's detail pane) puts the values beside
/// the code. Data only: what a value looks like is Dark's to decide.
///
/// Two builtins. `tracesLastInputs` finds the inputs; `liveReplay` runs the current version of
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

let private dvalKT () = RT2DT.Dval.knownType ()

let private valuesKT () =
  KTTuple(ValueType.Known KTInt64, ValueType.Known(dvalKT ()), [])

let fns () : List<BuiltInFn> =
  [ { name = fn "tracesLastInputs" 0
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


    { name = fn "liveReplay" 0
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
              let guest =
                LibDB.PolicyStore.guestState
                  exeState.accountID
                  LibExecution.Permissions.Policy.allowAll
                  []
                  [ Hash hash ]
                  exeState
              { guest with
                  access =
                    guest.access
                    |> LibExecution.Permissions.Access.constrainBy vm.activeAccess
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
              let! rendered = Execution.runtimeErrorToString asRoot rte
              let message =
                match rendered with
                | Ok(DString s) -> s
                | Ok other -> string other
                | Error _ -> string rte
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
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
