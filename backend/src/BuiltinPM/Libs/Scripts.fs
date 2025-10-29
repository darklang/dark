module BuiltinPM.Libs.Scripts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Scripts = LibPackageManager.Scripts
module PackageIDs = LibExecution.PackageIDs
module ScriptsToDT = LibPackageManager.ScriptsToDT

open Builtin.Shortcuts


let scriptTypeName = ScriptsToDT.scriptTypeName
let scriptType = TCustomType(Ok scriptTypeName, [])


// TODO: Consider migrating scripts away from a dedicated SQLite table to just
// making 'Script' a Darklang type that we respect in a special way, without
// requiring a ton of special F#/SQL code. They could potentially just be stored
// in a UserDB like any other data.
let fns : List<BuiltInFn> =
  [ { name = fn "pmScriptsList" 0
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


    { name = fn "pmScriptsGet" 0
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


    { name = fn "pmScriptsAdd" 0
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


    { name = fn "pmScriptsUpdate" 0
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


    { name = fn "pmScriptsDelete" 0
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
      deprecated = NotDeprecated } ]

let builtins = Builtin.make [] fns
