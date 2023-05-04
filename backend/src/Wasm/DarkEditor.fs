namespace Wasm

open System
open System.Threading.Tasks

open Microsoft.JSInterop

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes


type EditorSource =
  { types : List<UserType.T>
    fns : List<UserFunction.T>
    exprs : List<Expr> }

type DarkEditor() =
  static let debug arg = WasmHelpers.callJSFunction "console.log" [ arg ]

  static let getState types fns =
    let packageFns = Map.empty // TODO

    let libraries : Libraries =
      let stdlibFns, stdlibTypes =
        LibExecution.StdLib.combine
          [ StdLibExecution.StdLib.contents; LibWASM.contents ]
          []
          []

      { stdlibTypes =
          stdlibTypes |> List.map (fun typ -> FQTypeName.Stdlib typ.name, typ) |> Map

        stdlibFns =
          stdlibFns |> List.map (fun fn -> FQFnName.Stdlib fn.name, fn) |> Map

        packageFns = packageFns }

    let program : ProgramContext =
      { canvasID = CanvasID.Empty
        internalFnsAllowed = true
        dbs = Map.empty
        userFns = Map.fromListBy (fun fn -> fn.name) fns
        userTypes = Map.fromListBy (fun typ -> typ.name) types
        secrets = List.empty }

    { libraries = libraries
      tracing = LibExecution.Execution.noTracing Real
      program = program
      test = LibExecution.Execution.noTestContext
      reportException = consoleReporter
      notify = consoleNotifier
      tlid = gid ()
      callstack = Set.empty
      onExecutionPath = true
      executingFnName = None }


  [<JSInvokable>]
  static member LoadClient(urlOfCode : string) : Task<string> =
    task {
      let httpClient = new System.Net.Http.HttpClient()
      let! response = httpClient.GetAsync urlOfCode |> Async.AwaitTask
      let! loadedFromEndpoint =
        response.Content.ReadAsStringAsync() |> Async.AwaitTask

      let source = Json.Vanilla.deserialize<EditorSource> loadedFromEndpoint

      let! (_, initialState) =
        source.exprs
        |> Task.foldSequentially
             (fun (state, _lastResult) expr ->
               task {
                 let inputVars = Map.empty

                 let! (result : Dval) =
                   LibExecution.Execution.executeExpr state inputVars expr

                 // do I actually need to pass state around so much below?
                 // if so, do I need to update anything int he state?
                 // (anything affected by this execution?)
                 return state, result
               })
             (getState source.types source.fns, DUnit)

      LibWASM.editor <-
        { Types = source.types; Functions = source.fns; CurrentState = initialState }

      return Json.Vanilla.serialize initialState
    }


  [<JSInvokable>]
  static member HandleEvent(serializedEvent : string) : Ply<string> =
    uply {
      let state = getState LibWASM.editor.Types LibWASM.editor.Functions

      let! result =
        LibExecution.Interpreter.callFn
          state
          (gid ())
          (FQFnName.User { modules = []; function_ = "handleEvent"; version = 0 })
          []
          [ DString serializedEvent ]

      return LibExecution.DvalReprDeveloper.toRepr result
    }


  // just for debugging
  [<JSInvokable>]
  static member ExportClient() : string = Json.Vanilla.serialize LibWASM.editor
