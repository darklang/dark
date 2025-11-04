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

open Builtin.Shortcuts


let scriptTypeName = FQTypeName.fqPackage PackageIDs.Type.Cli.script
let scriptType = TCustomType(Ok scriptTypeName, [])


/// TODO: Consider migrating scripts away from a dedicated SQLite table to just
/// making 'Script' a Darklang type that we respect in a special way, without
/// requiring a ton of special F#/SQL code. They could potentially just be stored
/// in a UserDB like any other data.
///
/// A similar argument could soon be made for Tests, HttpHandlers, Docs,
/// and other sorts of 'TopLevels' that we respect.
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
            return
              scripts
              |> List.map Scripts.toDT
              |> Dval.list (KTCustomType(scriptTypeName, []))
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
        function
        | _, _, _, [ DString name ] ->
          uply {
            let! scriptOpt = Scripts.get name
            return
              scriptOpt
              |> Option.map Scripts.toDT
              |> Dval.option (KTCustomType(scriptTypeName, []))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmScriptsAdd" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "text" TString "" ]
      returnType = TypeReference.result scriptType TString
      description = "Add a new script"
      fn =
        function
        | _, _, _, [ DString name; DString text ] ->
          uply {
            let! result = Scripts.add name text
            return
              result
              |> Result.map Scripts.toDT
              |> Result.mapError DString
              |> Dval.result (KTCustomType(scriptTypeName, [])) KTString
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmScriptsUpdate" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "text" TString "" ]
      returnType = TypeReference.result TUnit TString
      description = "Update an existing script's text"
      fn =
        function
        | _, _, _, [ DString name; DString text ] ->
          uply {
            let! result = Scripts.update name text
            return
              result
              |> Result.map (fun () -> DUnit)
              |> Result.mapError DString
              |> Dval.result KTUnit KTString
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmScriptsDelete" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.result TUnit TString
      description = "Delete a script by name"
      fn =
        function
        | _, _, _, [ DString name ] ->
          uply {
            let! result = Scripts.delete name
            return
              result
              |> Result.map (fun () -> DUnit)
              |> Result.mapError DString
              |> Dval.result KTUnit KTString
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = Builtin.make [] fns
