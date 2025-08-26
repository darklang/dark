/// Builtin functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module BuiltinCliHost.Libs.Cli

open System.Threading.Tasks
open FSharp.Control.Tasks


open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs
module Json = BuiltinExecution.Libs.Json
module C2DT = LibExecution.CommonToDarkTypes
module Scripts = LibPackageManager.Scripts
module ScriptsToDT = BuiltinCliHost.Utils.ScriptsToDarkTypes

module Utils = BuiltinCliHost.Utils


module ExecutionError =
  let fqTypeName = FQTypeName.fqPackage PackageIDs.Type.Cli.executionError
  let typeRef = TCustomType(Ok fqTypeName, [])

// Script type definitions
let scriptTypeName = ScriptsToDT.scriptTypeName
let scriptType = TCustomType(Ok scriptTypeName, [])


module Config =
  let packageManagerRT = LibPackageManager.PackageManager.rt
  let packageManagerPT = LibPackageManager.PackageManager.pt

  let builtinsToUse : RT.Builtins =
    LibExecution.Builtin.combine
      [ BuiltinExecution.Builtin.builtins
          BuiltinExecution.Libs.HttpClient.defaultConfig
          packageManagerPT
        BuiltinCli.Builtin.builtins ]
      []


let execute
  (parentState : RT.ExecutionState)
  (mod' : Utils.CliScript.PTCliScriptModule)
  (_args : List<Dval>) // CLEANUP update to List<String>, and extract in builtin
  : Ply<RT.ExecutionResult> =
  uply {
    let (program : Program) =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        secrets = []
        dbs = Map.empty }

    let types =
      List.concat
        [ mod'.types |> List.map PT2RT.PackageType.toRT
          mod'.submodules.types |> List.map PT2RT.PackageType.toRT ]

    let values =
      List.concat
        [ mod'.values |> List.map PT2RT.PackageValue.toRT
          mod'.submodules.values |> List.map PT2RT.PackageValue.toRT ]

    let fns =
      List.concat
        [ mod'.fns |> List.map PT2RT.PackageFn.toRT
          mod'.submodules.fns |> List.map PT2RT.PackageFn.toRT ]

    let packageManager =
      Config.packageManagerRT |> PackageManager.withExtras types values fns

    let tracing = Exe.noTracing

    let state =
      Exe.createState
        Config.builtinsToUse
        packageManager
        tracing
        parentState.reportException
        parentState.notify
        program

    match mod'.exprs with
    | [] ->
      return
        RuntimeError.CLIs.NoExpressionsToExecute
        |> RuntimeError.CLI
        |> raiseUntargetedRTE
    | exprs ->
      let exprInsrts = exprs |> List.map (PT2RT.Expr.toRT Map.empty 0)
      let results = exprInsrts |> List.map (Exe.executeExpr state)
      match List.tryLast results with
      | Some lastResult -> return! lastResult
      | None ->
        return
          Exception.raiseInternal
            "No results from executing expressions (which should be impossible..)"
            []
  }


let fns : List<BuiltInFn> =
  [ { name = fn "cliParseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "args" (TList TString) "" ]
      returnType = TypeReference.result TInt64 ExecutionError.typeRef
      description =
        "Parses Dark code as a script, and and executes it, returning an exit code"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTInt64 errType
        let resultError = Dval.resultError KTInt64 errType
        (function
        | exeState,
          _,
          [],
          [ DString filename; DString code; DList(_vtTODO, scriptArgs) ] ->
          uply {
            let exnError (e : exn) : RuntimeError.Error =
              RuntimeError.UncaughtException(
                Exception.getMessages e |> String.concat "\n",
                Exception.toMetadata e
                |> List.map (fun (k, v) -> (k, DString(string v)))
              )

            let onMissingType =
              FQTypeName.Package
                PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
            let onMissingAllow = DEnum(onMissingType, onMissingType, [], "Allow", [])

            let getPmFnName =
              FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm

            let! execResult =
              Exe.executeFunction exeState getPmFnName [] (NEList.singleton DUnit)

            let! pm =
              uply {
                match execResult with
                | Ok dval -> return dval
                | Error(rte, _cs) ->
                  let! rteString = Exe.runtimeErrorToString exeState rte
                  match rteString with
                  | Ok rte ->
                    return
                      Exception.raiseInternal
                        "Error executing pm function"
                        [ "rte", rte ]

                  | Error(nestedRte, _cs) ->
                    return
                      Exception.raiseInternal
                        "Error running runtimeErrorToString"
                        [ "original rte", rte; "nested rte", nestedRte ]
              }
            let args =
              NEList.ofList
                (DString "CliScript")
                [ DString "ScriptName"
                  onMissingAllow
                  pm
                  DString filename
                  DString code ]

            let parseCliScriptFnName =
              FQFnName.Package
                PackageIDs.Fn.LanguageTools.Parser.CliScript.parseCliScript

            let! execResult =
              Exe.executeFunction exeState parseCliScriptFnName [] args

            let! (parsedScript :
              Result<Utils.CliScript.PTCliScriptModule, RuntimeError.Error>) =
              uply {
                match execResult with
                | Ok dval ->
                  match C2DT.Result.fromDT identity dval identity with
                  | Ok parsedModuleAndUnresolvedNames ->
                    return
                      (Utils.CliScript.fromDT parsedModuleAndUnresolvedNames) |> Ok
                  | Error(DString errMsg) ->
                    return Error(RuntimeError.UncaughtException(errMsg, []))
                  | Error _ ->
                    return
                      Exception.raiseInternal
                        "Invalid error format from parseCliScript"
                        [ "dval", dval ]
                | Error(rte, _cs) ->
                  let! rteString = Exe.runtimeErrorToString exeState rte
                  match rteString with
                  | Ok errorDval ->
                    return
                      Exception.raiseInternal
                        "Error executing parseCliScript function"
                        [ "rte", errorDval ]
                  | Error(nestedRte, _cs) ->
                    return
                      Exception.raiseInternal
                        "Error running runtimeErrorToString"
                        [ "original rte", rte; "nested rte", nestedRte ]
              }

            try
              match parsedScript with
              | Ok mod' ->
                match! execute exeState mod' scriptArgs with
                | Ok(DInt64 i) -> return resultOk (DInt64 i)
                | Ok result ->
                  return
                    RuntimeError.CLIs.NonIntReturned result
                    |> RuntimeError.CLI
                    |> RT2DT.RuntimeError.toDT
                    |> resultError
                | Error(e, callStack) ->
                  // TODO: do this, some better way
                  // (probably pass it back in a structured way)
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing Script. Call-stack:\n{csString}\n"

                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return exnError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // TODO: better handling for functions that takes string args
    { name = fn "cliExecuteFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""
          Param.make "args" (TList(TCustomType(Ok PT2DT.Expr.typeName, []))) "" ]
      returnType = TypeReference.result TString ExecutionError.typeRef
      description =
        "Executes an arbitrary Dark package function using the new darklang parser"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTString errType
        let resultError = Dval.resultError KTString errType

        function
        | exeState, _, [], [ DString functionName; DList(_vtTODO, args) ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) : Dval =
              let fields =
                [ ("msg", DString msg)
                  ("metadata",
                   DDict(
                     VT.string,
                     metadata |> List.map (Tuple2.mapSecond DString) |> Map
                   )) ]

              DRecord(
                ExecutionError.fqTypeName,
                ExecutionError.fqTypeName,
                [],
                Map fields
              )

            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata

            try
              let resolveFn =
                FQFnName.Package
                  PackageIDs.Fn.LanguageTools.NameResolver.FnName.resolve

              let onMissingType =
                FQTypeName.Package
                  PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
              let onMissingAllow =
                DEnum(onMissingType, onMissingType, [], "Allow", [])

              let parserRangeType =
                FQTypeName.Package PackageIDs.Type.LanguageTools.Parser.range
              let pointType =
                FQTypeName.Package PackageIDs.Type.LanguageTools.Parser.point
              let pointFields = [ ("row", DInt64 0); ("column", DInt64 0) ]
              let fields =
                [ ("start", DRecord(pointType, pointType, [], Map pointFields))
                  ("end_", DRecord(pointType, pointType, [], Map pointFields)) ]

              let rangeParser =
                DRecord(parserRangeType, parserRangeType, [], Map fields)
              let writtenTypesNameType =
                FQTypeName.Package PackageIDs.Type.LanguageTools.WrittenTypes.name

              let parts = functionName.Split('.') |> List.ofArray
              let currentModule = DList(VT.string, [])
              let nameArg =
                DEnum(
                  writtenTypesNameType,
                  writtenTypesNameType,
                  [],
                  "Unresolved",
                  [ rangeParser; DList(VT.string, parts |> List.map DString) ]
                )

              let pm = FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm
              let! execResult =
                Exe.executeFunction exeState pm [] (NEList.singleton RT.Dval.DUnit)
              let! pm =
                uply {
                  match execResult with
                  | Ok dval -> return dval
                  | Error(rte, _cs) ->
                    let! rteString =
                      Exe.rteToString RT2DT.RuntimeError.toDT exeState rte
                    return
                      Exception.raiseInternal
                        "Error executing pm function"
                        [ "rte", rteString ]
                }

              let resolveFnArgs =
                NEList.ofList
                  onMissingAllow
                  [ pm; RT.DString "Cli"; currentModule; nameArg ]

              let! execResult =
                Exe.executeFunction exeState resolveFn [] resolveFnArgs

              let! fnNameResult =
                uply {
                  match execResult with
                  | Ok dval ->
                    match
                      C2DT.Result.fromDT
                        PT2DT.FQFnName.fromDT
                        dval
                        PT2DT.NameResolutionError.fromDT
                    with
                    | Ok fnName -> return Ok fnName
                    | Error nameErr ->
                      match nameErr with
                      | PT.NameResolutionError.NotFound names ->
                        let nameStr = names |> String.concat "."
                        return Error $"Function not found: {nameStr}"
                      | PT.NameResolutionError.InvalidName names ->
                        let nameStr = names |> String.concat "."
                        return Error $"Invalid function name: {nameStr}"
                  | Error(rte) ->
                    return
                      Exception.raiseInternal
                        "Error executing resolve function"
                        [ "rte", rte ]
                }

              match fnNameResult with
              | Ok fnName ->
                let! fn =
                  match PT2RT.FQFnName.toRT fnName with
                  | FQFnName.Package pkg -> exeState.fns.package pkg
                  | _ ->
                    Exception.raiseInternal
                      "Error constructing package function name"
                      [ "fn", fn ]

                match fn with
                | None -> return resultError (DString "Function not found")
                | Some f ->
                  let newArgs =
                    args
                    |> List.collect (fun dval ->
                      match dval with
                      | DEnum(_, _, _, _, fields) -> fields |> List.tail
                      | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ])

                  let newArgs = if newArgs = [] then [ RT.DUnit ] else newArgs

                  let! result =
                    Exe.executeFunction
                      exeState
                      (FQFnName.Package f.id)
                      []
                      (NEList.ofList newArgs.Head newArgs.Tail)

                  match result with
                  | Error(rte, _cs) ->
                    // TODO we should probably return the error here as-is, and handle by calling the
                    // toSegments on the error within the CLI
                    match! Exe.runtimeErrorToString exeState rte with
                    | Ok(DString s) -> return s |> DString |> resultError
                    | _ ->
                      let rte =
                        rte |> RT2DT.RuntimeError.toDT |> DvalReprDeveloper.toRepr
                      return
                        $"An error occured trying to print a runtime error:\n\noriginal error:\n{rte}"
                        |> DString
                        |> resultError

                  | Ok value ->
                    match value with
                    | DString s -> return resultOk (DString s)
                    | _ ->
                      let asString = DvalReprDeveloper.toRepr value
                      return resultOk (DString asString)
              | Error errMsg -> return resultError (DString errMsg)
            with e ->
              return exnError e
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliEvaluateExpression" 0
      typeParams = []
      parameters = [ Param.make "expression" TString "" ]
      returnType = TypeReference.result TString ExecutionError.typeRef
      description = "Evaluates a Dark expression and returns the result as a Strin"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTString errType
        let resultError = Dval.resultError KTString errType
        (function
        | exeState, _, [], [ DString expression ] ->
          uply {
            let exnError (e : exn) : RuntimeError.Error =
              RuntimeError.UncaughtException(
                Exception.getMessages e |> String.concat "\n",
                Exception.toMetadata e
                |> List.map (fun (k, v) -> (k, DString(string v)))
              )

            let onMissingType =
              FQTypeName.Package
                PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
            let onMissingAllow = DEnum(onMissingType, onMissingType, [], "Allow", [])

            let getPmFnName =
              FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm

            let! execResult =
              Exe.executeFunction exeState getPmFnName [] (NEList.singleton DUnit)

            let! pm =
              uply {
                match execResult with
                | Ok dval -> return dval
                | Error(rte, _cs) ->
                  let! rteString = Exe.runtimeErrorToString exeState rte
                  match rteString with
                  | Ok rte ->
                    return
                      Exception.raiseInternal
                        "Error executing pm function"
                        [ "rte", rte ]
                  | Error(nestedRte, _cs) ->
                    return
                      Exception.raiseInternal
                        "Error running runtimeErrorToString"
                        [ "original rte", rte; "nested rte", nestedRte ]
              }

            let args =
              NEList.ofList
                (DString "CliScript")
                [ DString "ExprWrapper"
                  onMissingAllow
                  pm
                  DString "exprWrapper"
                  DString expression ]

            let parseCliScriptFnName =
              FQFnName.Package
                PackageIDs.Fn.LanguageTools.Parser.CliScript.parseCliScript

            let! execResult =
              Exe.executeFunction exeState parseCliScriptFnName [] args

            let! (parsedScript :
              Result<Utils.CliScript.PTCliScriptModule, RuntimeError.Error>) =
              uply {
                match execResult with
                | Ok dval ->
                  match C2DT.Result.fromDT identity dval identity with
                  | Ok parsedModuleAndUnresolvedNames ->
                    return
                      (Utils.CliScript.fromDT parsedModuleAndUnresolvedNames) |> Ok
                  | Error(DString errMsg) ->
                    return Error(RuntimeError.UncaughtException(errMsg, []))
                  | Error _ ->
                    return
                      Exception.raiseInternal
                        "Invalid error format from parseCliScript"
                        [ "dval", dval ]
                | Error(rte, _cs) ->
                  let! rteString = Exe.runtimeErrorToString exeState rte
                  match rteString with
                  | Ok errorDval ->
                    return
                      Exception.raiseInternal
                        "Error executing parseCliScript function"
                        [ "rte", errorDval ]
                  | Error(nestedRte, _cs) ->
                    return
                      Exception.raiseInternal
                        "Error running runtimeErrorToString"
                        [ "original rte", rte; "nested rte", nestedRte ]
              }

            try
              match parsedScript with
              | Ok mod' ->
                match! execute exeState mod' [] with
                | Ok result ->
                  match result with
                  | DString s -> return resultOk (DString s)
                  | _ ->
                    let asString = DvalReprDeveloper.toRepr result
                    return resultOk (DString asString)
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing expression. Call-stack:\n{csString}\n"
                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return exnError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Script management functions
    { name = fn "cliScriptsList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList scriptType
      description = "List all stored scripts"
      fn =
        function
        | _, _, _, [ DUnit ] ->
          uply {
            let! scripts = Scripts.list ()
            let dvals = scripts |> List.map ScriptsToDT.toDT
            return Dval.list (KTCustomType(scriptTypeName, [])) dvals
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliScriptsGet" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option scriptType
      description = "Get a script by name"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! scriptOpt = Scripts.get name
            match scriptOpt with
            | Some script ->
              return
                Dval.optionSome
                  (KTCustomType(scriptTypeName, []))
                  (ScriptsToDT.toDT script)
            | None -> return Dval.optionNone (KTCustomType(scriptTypeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliScriptsAdd" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "text" TString "" ]
      returnType = TypeReference.result scriptType TString
      description = "Add a new script"
      fn =
        (function
        | _, _, _, [ DString name; DString text ] ->
          uply {
            let! result = Scripts.add name text
            match result with
            | Ok script ->
              return
                Dval.resultOk
                  (KTCustomType(scriptTypeName, []))
                  KTString
                  (ScriptsToDT.toDT script)
            | Error err ->
              return
                Dval.resultError
                  (KTCustomType(scriptTypeName, []))
                  KTString
                  (DString err)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliScriptsUpdate" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "text" TString "" ]
      returnType = TypeReference.result TUnit TString
      description = "Update an existing script's text"
      fn =
        (function
        | _, _, _, [ DString name; DString text ] ->
          uply {
            let! result = Scripts.update name text
            match result with
            | Ok() -> return Dval.resultOk KTUnit KTString DUnit
            | Error err -> return Dval.resultError KTUnit KTString (DString err)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliScriptsDelete" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.result TUnit TString
      description = "Delete a script by name"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! result = Scripts.delete name
            match result with
            | Ok() -> return Dval.resultOk KTUnit KTString DUnit
            | Error err -> return Dval.resultError KTUnit KTString (DString err)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliGetTerminalHeight" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Get the current terminal viewport height in number of lines"
      fn =
        (function
        | _, _, [], [ DUnit ] ->
          uply {
            try
              // First try environment variable (most reliable across different terminals)
              match System.Environment.GetEnvironmentVariable("LINES") with
              | null ->
                // Try Console.WindowHeight
                let height = System.Console.WindowHeight

                // If we get exactly 24, it might be a default value
                // Let's try alternative methods
                if height = 24 then
                  // On Unix systems, try tput command
                  if
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.Linux
                    )
                    || System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.OSX
                    )
                  then
                    try
                      let p = new System.Diagnostics.Process()
                      p.StartInfo.FileName <- "/bin/sh"
                      p.StartInfo.Arguments <-
                        "-c \"tput lines 2>/dev/null || echo 24\""
                      p.StartInfo.UseShellExecute <- false
                      p.StartInfo.RedirectStandardOutput <- true
                      p.StartInfo.CreateNoWindow <- true
                      if p.Start() then
                        let output = p.StandardOutput.ReadToEnd().Trim()
                        p.WaitForExit()
                        match System.Int32.TryParse(output) with
                        | true, h when h > 0 -> return DInt64(int64 h)
                        | _ -> return DInt64(int64 height)
                      else
                        return DInt64(int64 height)
                    with _ ->
                      return DInt64(int64 height)
                  else
                    return DInt64(int64 height)
                else
                  return DInt64(int64 height)
              | lines ->
                match System.Int32.TryParse(lines) with
                | true, h when h > 0 -> return DInt64(int64 h)
                | _ -> return DInt64 24L
            with _ ->
              // Fallback if unable to detect terminal size
              return DInt64 24L
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliGetTerminalWidth" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Get the current terminal viewport width in number of columns"
      fn =
        (function
        | _, _, [], [ DUnit ] ->
          uply {
            try
              // First try environment variable (most reliable across different terminals)
              match System.Environment.GetEnvironmentVariable("COLUMNS") with
              | null ->
                // Try Console.WindowWidth
                let width = System.Console.WindowWidth

                // If we get exactly 80, it might be a default value
                // Let's try alternative methods
                if width = 80 then
                  // On Unix systems, try tput command
                  if
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.Linux
                    )
                    || System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.OSX
                    )
                  then
                    try
                      let p = new System.Diagnostics.Process()
                      p.StartInfo.FileName <- "/bin/sh"
                      p.StartInfo.Arguments <-
                        "-c \"tput cols 2>/dev/null || echo 80\""
                      p.StartInfo.UseShellExecute <- false
                      p.StartInfo.RedirectStandardOutput <- true
                      p.StartInfo.CreateNoWindow <- true
                      if p.Start() then
                        let output = p.StandardOutput.ReadToEnd().Trim()
                        p.WaitForExit()
                        match System.Int32.TryParse(output) with
                        | true, w when w > 0 -> return DInt64(int64 w)
                        | _ -> return DInt64(int64 width)
                      else
                        return DInt64(int64 width)
                    with _ ->
                      return DInt64(int64 width)
                  else
                    return DInt64(int64 width)
                else
                  return DInt64(int64 width)
              | columns ->
                match System.Int32.TryParse(columns) with
                | true, w when w > 0 -> return DInt64(int64 w)
                | _ -> return DInt64 80L
            with _ ->
              // Fallback if unable to detect terminal size
              return DInt64 80L
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "getBuildHash" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the git hash of the current CLI build"
      fn =
        function
        | _, _, [], [ DUnit ] -> uply { return DString LibConfig.Config.buildHash }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
