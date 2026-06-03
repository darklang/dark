/// Standard libraries for Directories
module Builtins.Cli.Libs.Directory

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "directoryCurrent" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TString
      description = "Returns the current working directory"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let contents = System.IO.Directory.GetCurrentDirectory()
            return DString contents
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.fileRead
      deprecated = NotDeprecated }


    { name = fn "directoryList" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TList TString
      description = "Returns the directory at <param path>"
      fn =
        (function
        | state, _, _, [ DString path ] ->
          uply {
            // precise check: this exact path must be covered (gate checked file presence).
            LibExecution.CapabilityCheck.requireFileRead state.grantedCaps path
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
      capabilities = LibExecution.Capabilities.Needs.fileRead
      deprecated = NotDeprecated }


    { name = fn "getCurrentExecutablePath" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the full path to the currently running executable"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let exePath =
              System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName
            return DString exePath
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
