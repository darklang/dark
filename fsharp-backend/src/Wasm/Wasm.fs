namespace Wasm

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Prelude.Tablecloth

module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module RT = LibExecution.RuntimeTypes

module TestExpr =
  open LibExecution.Shortcuts

  // see big.tests
  // Expected result is "a string to be used as the test case\nbytes: a string to be used as the test case\nbase64Encode: YSBzdHJpbmcgdG8gYmUgdXNlZCBhcyB0aGUgdGVzdCBjYXNl\nhex64Encode: 6120737472696E6720746F20626520757365642061732074686520746573742063617365\nstring length: 36\nbytes length: 36\nbool: false\nfloat: 1234567.98765\nanother double: 2489377.51259\nstring reverse: esac tset eht sa desu eb ot gnirts a\nstring contains: false\nuuid: 9c59ba07-1e20-4ce2-a2de-6a95391d67b2"
  let expr =
    eLet
      "str"
      (eStr "a string to be used as the test case")
      (eLet
        "bytes"
        (eFn "String" "toBytes" 0 [ eVar "str" ])
        (eLet
          "base64Encode"
          (eFn "Bytes" "base64Encode" 0 [ eVar "bytes" ])
          (eLet
            "hexEncode"
            (eFn "Bytes" "hexEncode" 0 [ eVar "bytes" ])
            (eLet
              "sl"
              (eFn "String" "length" 1 [ eVar "str" ])
              (eLet
                "bl"
                (eFn "Bytes" "length" 0 [ eVar "bytes" ])
                (eLet
                  "t"
                  (eBool true)
                  (eLet
                    "f"
                    (eBool false)
                    (eLet
                      "r"
                      (ePipeApply
                        (eStdFnVal "Bool" "not" 0)
                        [ eFn "Bool" "xor" 0 [ eVar "t"; eVar "f" ] ])
                      (eLet
                        "d0"
                        (eFloat Positive 1234567I 987654I)
                        (eLet
                          "d1"
                          (ePipeApply
                            (eStdFnVal "Float" "sqrt" 0)
                            [ ePipeApply
                                (eStdFnVal "Float" "add" 0)
                                [ ePipeApply
                                    (eStdFnVal "Float" "power" 0)
                                    [ eVar "d0"; eFloat Positive 2I 0I ]
                                  eFloat Positive 1I 0I ] ])
                          (eLet
                            "reverse"
                            (eFn "String" "reverse" 0 [ eVar "str" ])
                            (eLet
                              "contains"
                              (eFn
                                "String"
                                "contains"
                                0
                                [ eStr "a string"; eVar "str" ])
                              (eLet
                                "uuid"
                                (eFn
                                  "String"
                                  "toUUID"
                                  1
                                  [ eStr "9c59ba07-1e20-4ce2-a2de-6a95391d67b2" ])
                                (ePipeApply
                                  (eStdFnVal "" "++" 0)
                                  [ ePipeApply
                                      (eStdFnVal "" "++" 0)
                                      [ ePipeApply
                                          (eStdFnVal "" "++" 0)
                                          [ ePipeApply
                                              (eStdFnVal "" "++" 0)
                                              [ ePipeApply
                                                  (eStdFnVal "" "++" 0)
                                                  [ ePipeApply
                                                      (eStdFnVal "" "++" 0)
                                                      [ ePipeApply
                                                          (eStdFnVal "" "++" 0)
                                                          [ ePipeApply
                                                              (eStdFnVal "" "++" 0)
                                                              [ ePipeApply
                                                                  (eStdFnVal
                                                                    ""
                                                                    "++"
                                                                    0)
                                                                  [ ePipeApply
                                                                      (eStdFnVal
                                                                        ""
                                                                        "++"
                                                                        0)
                                                                      [ ePipeApply
                                                                          (eStdFnVal
                                                                            ""
                                                                            "++"
                                                                            0)
                                                                          [ eVar
                                                                              "str"
                                                                            eFn
                                                                              ""
                                                                              "toString"
                                                                              0
                                                                              [ eVar
                                                                                  "bytes" ] ]
                                                                        eVar
                                                                          "base64Encode" ]
                                                                    eVar "hexEncode" ]
                                                                eFn
                                                                  ""
                                                                  "toString"
                                                                  0
                                                                  [ eVar "sl" ] ]
                                                            eFn
                                                              ""
                                                              "toString"
                                                              0
                                                              [ eVar "bl" ] ]
                                                        eFn
                                                          ""
                                                          "toString"
                                                          0
                                                          [ eVar "r" ] ]
                                                    eFn "" "toString" 0 [ eVar "d0" ] ]
                                                eFn "" "toString" 0 [ eVar "d1" ] ]
                                            eVar "reverse" ]
                                        eFn "" "toString" 0 [ eVar "contains" ] ]
                                    eFn "" "toString" 0 [ eVar "uuid" ] ]))))))))))))))


module Program =
  let stdlib =
    LibExecution.StdLib.StdLib.fns
    |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)


  // call this from JS with DotNet.invokeMethod('Wasm', 'run', 7)
  // or DotNet.invokeMethodAsync('Wasm', 'run', 8)
  [<Microsoft.JSInterop.JSInvokable>]
  let run (arg : int) : Task<string> =
    task {
      let expr = LibExecution.Shortcuts.eInt arg
      let expr = TestExpr.expr
      // FSTODO: get packages from caller
      let libraries : RT.Libraries = { stdlib = stdlib; packageFns = Map.empty }
      let tracing = LibExecution.Execution.noTracing RT.Preview

      let uuid = System.Guid.NewGuid()

      // FSTODO: get all this info from the caller
      let program : RT.ProgramContext =
        { accountID = uuid
          canvasID = uuid
          userFns = Map.empty
          userTypes = Map.empty
          dbs = Map.empty
          secrets = [] }

      let tlid = id 7
      let state = Exe.createState libraries tracing tlid program
      let! result = Exe.executeExpr state Map.empty expr

      return result.ToString()
    }


  [<EntryPoint>]
  let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault([||])
    builder.Build().RunAsync() |> ignore
    0
