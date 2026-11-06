/// Managed launch/supervision units. No capture, retry, deadline or waiting loops.
module Builtins.Cli.Libs.RuntimeChild

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts
module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval
module Host = LibExecution.Host
module Interpreter = LibExecution.Interpreter
module Op = LibExecution.HostTypes

let private make name parameters result kt body : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = parameters
    returnType = TypeReference.result result TInt64
    description = "One execution-owned child runtime operation"
    fn =
      (fun struct (_, vm, _, values) ->
        let op, shape = body values
        Interpreter.requestHost vm op (fun outcome ->
          match outcome with
          | Ok response -> Ply(Dval.resultOk kt KTInt64 (shape response))
          | Error error ->
            Ply(Dval.resultError kt KTInt64 (Dval.dint64 (int64 error.errno)))))
    previewable = Impure
    // Host checks the retained executable/argv grant, including on pipe uses.
    callEffects = set [ Effect.Process ]
    deprecated = NotDeprecated }

let private handle = [ Param.make "handle" TInt "Owned child token" ]
let private child name result kt constructor shape =
  make name handle result kt (function
    | [| DInt token |] -> constructor (DarkInt.toBigInt token), shape
    | _ -> incorrectArgs ())

let fns () =
  let optionKT =
    KTCustomType(Dval.optionType (), [ LibExecution.ValueType.known KTInt64 ])
  [ make
      "runtimeChildStart"
      [ Param.make "program" TString "Executable"
        Param.make "args" (TList TString) "Arguments"
        Param.make
          "inheritTerminal"
          TBool
          "Inherit terminal rather than capture pipes" ]
      TInt
      KTInt
      (function
       | [| DString program; DList(_, args); DBool inheritTerminal |] ->
         let args =
           args
           |> List.map (function
             | DString value -> value
             | _ -> incorrectArgs ())
         Op.Operation.ChildStart(program, args, inheritTerminal),
         (function
         | Op.Response.ChildHandle token -> Dval.int token
         | _ -> incorrectArgs ())
       | _ -> incorrectArgs ())
    child
      "runtimeChildDescriptors"
      (TList TInt)
      (KTList(LibExecution.ValueType.known KTInt))
      Op.Operation.ChildDescriptors
      (function
       | Op.Response.ChildDescriptors tokens ->
         Dval.list KTInt (List.map Dval.int tokens)
       | _ -> incorrectArgs ())
    child
      "runtimeChildPoll"
      (TypeReference.option TInt64)
      optionKT
      Op.Operation.ChildPoll
      (function
       | Op.Response.ChildStatus None -> Dval.optionNone KTInt64
       | Op.Response.ChildStatus(Some code) ->
         Dval.optionSome KTInt64 (Dval.dint64 (int64 code))
       | _ -> incorrectArgs ())
    child "runtimeChildWait" TInt64 KTInt64 Op.Operation.ChildWait (function
      | Op.Response.ChildStatus(Some code) -> Dval.dint64 (int64 code)
      | _ -> incorrectArgs ())
    child "runtimeChildKill" TUnit KTUnit Op.Operation.ChildKill (fun _ -> DUnit)
    child "runtimeChildClose" TUnit KTUnit Op.Operation.ChildClose (fun _ -> DUnit)
    make
      "runtimeChildClosePort"
      [ Param.make "handle" TInt "Owned child token"
        Param.make "index" TInt64 "stdin=0, stdout=1, stderr=2" ]
      TUnit
      KTUnit
      (function
       | [| DInt token; DInt64 index |] ->
         Op.Operation.ChildClosePort(
           DarkInt.toBigInt token,
           if index >= 0L && index <= 2L then int index else -1
         ),
         (fun _ -> DUnit)
       | _ -> incorrectArgs ())
    make
      "runtimeChildBufferGet"
      [ Param.make "handle" TInt "Owned child token"
        Param.make "index" TInt64 "Owned byte cell, 0 or 1" ]
      TBytes
      KTBytes
      (function
       | [| DInt token; DInt64 index |] ->
         Op.Operation.ChildBufferGet(
           DarkInt.toBigInt token,
           if index >= 0L && index <= 1L then int index else -1
         ),
         (function
         | Op.Response.Bytes bytes -> LibExecution.Bytes.newEphemeral bytes
         | _ -> incorrectArgs ())
       | _ -> incorrectArgs ())
    { name = fn "runtimeChildBufferSet" 0
      typeParams = []
      parameters =
        [ Param.make "handle" TInt "Owned child token"
          Param.make "index" TInt64 "Owned byte cell, 0 or 1"
          Param.make
            "bytes"
            TBytes
            "Replacement value; no concatenation or limit policy" ]
      returnType = TypeReference.result TUnit TInt64
      description = "Replace one owned byte cell; Dark owns accumulation and limits"
      fn =
        (fun struct (state, vm, _, values) ->
          match values with
          | [| DInt token; DInt64 index; DBytes reference |] ->
            LibExecution.Bytes.withBytes state reference (fun bytes ->
              let op =
                Op.Operation.ChildBufferSet(
                  DarkInt.toBigInt token,
                  (if index >= 0L && index <= 1L then int index else -1),
                  bytes
                )
              Interpreter.requestHost vm op (fun outcome ->
                match outcome with
                | Ok _ -> Ply(Dval.resultOk KTUnit KTInt64 DUnit)
                | Error error ->
                  Ply(
                    Dval.resultError
                      KTUnit
                      KTInt64
                      (Dval.dint64 (int64 error.errno))
                  )))
          | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = Builtin.make [] (fns ())
