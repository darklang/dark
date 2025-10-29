module LibPackageManager.ScriptsToDT

open Prelude

module PackageIDs = LibExecution.PackageIDs
module DvalDecoder = LibExecution.DvalDecoder

open LibExecution.RuntimeTypes

let scriptTypeName = FQTypeName.fqPackage PackageIDs.Type.Cli.script

let toDT (script : Scripts.Script) : Dval =
  let fields =
    [ ("id", DString(string script.id))
      ("name", DString script.name)
      ("text", DString script.text) ]
  DRecord(scriptTypeName, scriptTypeName, [], Map.ofList fields)

let fromDT (d : Dval) : Scripts.Script =
  match d with
  | DRecord(_, _, _, fields) ->
    { Scripts.id =
        System.Guid.Parse(DvalDecoder.field "id" fields |> DvalDecoder.string)
      Scripts.name = DvalDecoder.field "name" fields |> DvalDecoder.string
      Scripts.text = DvalDecoder.field "text" fields |> DvalDecoder.string }
  | _ -> Exception.raiseInternal "Invalid Script" []
