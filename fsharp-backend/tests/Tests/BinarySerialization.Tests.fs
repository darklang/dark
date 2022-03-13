module Tests.BinarySerialization

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBackend.BinarySerialization

let allExprTypes =
  " do
      let x1 = 5
      let x2 = 6
      let bool = true
      let bool = false
      let str = \"a string\"
      let char = 'a'
      let float = -6.5
      let n = null
      let b = blank
      let i =
        if Bool.isError_ster 6
        then
          if 5 <> 6
          then 5 + 2
          else (fun y -> 2 + y)
        else
          x.y + Int.add_v0 6 2 + [5;6;7]
      let r =
        { field = 5 |> (+) 2
          constructor = Ok (Error (Just Nothing))
        }
      let m =
        match Mod.function_v2 with
        | Ok x -> v
        | 5 -> 6
        | true -> 7
        | 'c' -> 'c'
        | \"string\" -> \"string\"
        | null -> null
        | var -> 6 + var
        | 5.6 -> 5.6
        | blank -> 6
      4"

let expr = FSharpToExpr.parsePTExpr allExprTypes

let testToplevels : List<PT.Toplevel.T> =
  let ids : PT.Handler.ids = { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
  let http : PT.Handler.T =
    let spec = PT.Handler.HTTP("/path", "GET", ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  let worker : PT.Handler.T =
    let spec = PT.Handler.Worker("name", ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  let oldWorker : PT.Handler.T =
    let spec = PT.Handler.OldWorker("MODULE", "name", ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  let repl : PT.Handler.T =
    let spec = PT.Handler.REPL("name", ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  let cron1 : PT.Handler.T =
    let spec = PT.Handler.Cron("name", None, ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  let cron2 : PT.Handler.T =
    let spec = PT.Handler.Cron("name", Some PT.Handler.Every12Hours, ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  let unknown : PT.Handler.T =
    let spec = PT.Handler.UnknownHandler("name", "", ids)
    { spec = spec; tlid = 0UL; ast = expr; pos = { x = 6; y = 6 } }
  [ PT.Toplevel.TLHandler http
    PT.Toplevel.TLHandler worker
    PT.Toplevel.TLHandler cron1
    PT.Toplevel.TLHandler cron2
    PT.Toplevel.TLHandler repl
    PT.Toplevel.TLHandler unknown
    PT.Toplevel.TLHandler oldWorker ]



let roundtripTest =
  test "serializeProgramTypes" {
    testToplevels
    |> List.iter (fun tl ->
      let expected = tl
      let actual =
        expected
        |> BinarySerialization.serializeToplevel
        |> BinarySerialization.deserializeToplevel

      Expect.equal actual expected "")
  }

let tests = testList "BinarySerialization" [ roundtripTest ]
