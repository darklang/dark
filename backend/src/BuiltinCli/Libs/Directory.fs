/// Standard libraries for Directories
module BuiltinCli.Libs.Directory

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
module VT = ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let constants : List<BuiltInConstant> = []

let fn = fn [ "Directory" ]

let fns : List<BuiltInFn> =
  [ { name = fn "current" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TString
      description = "Returns the current working directory"
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let contents = System.IO.Directory.GetCurrentDirectory()
            return DString contents
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "create" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Creates a new directory at the specified <param path>. If the directory already exists, no action is taken. Returns a Result type indicating success or failure."
      fn =
        let resultOk r = Dval.resultOk KTUnit KTString r |> Ply
        let resultError r = Dval.resultError KTUnit KTString r |> Ply
        (function
        | _, _, [ DString path ] ->
          try
            System.IO.Directory.CreateDirectory(path)
            |> ignore<System.IO.DirectoryInfo>
            resultOk DUnit
          with e ->
            resultError (DString e.Message)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "delete" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Deletes the directory at the specified <param path>. If <param recursive> is set to true, it will delete the directory and its contents. If set to false (default), it will only delete an empty directory. Returns a Result type indicating success or failure."
      fn =
        let resultOk r = Dval.resultOk KTUnit KTString r |> Ply
        let resultError r = Dval.resultError KTUnit KTString r |> Ply
        (function
        | _, _, [ DString path ] ->
          try
            System.IO.Directory.Delete(path, false)
            resultOk DUnit
          with e ->
            resultError (DString e.Message)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TList TString
      description = "Returns the directory at <param path>"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            // TODO make async
            let contents =
              try
                System.IO.Directory.EnumerateFileSystemEntries path |> Seq.toList
              with _ ->
                []

            return DList(VT.string, List.map DString contents)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : Builtin.Contents = (fns, constants)
