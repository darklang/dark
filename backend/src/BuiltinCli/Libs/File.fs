/// Standard libraries for Files
module BuiltinCli.Libs.File

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
module VT = ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "File" ]

let fns : List<BuiltInFn> =
  [ { name = fn "read" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Reads the contents of a file specified by <param path> asynchronously and returns its contents as Bytes wrapped in a Result"
      fn =
        let resultOk = Dval.resultOk VT.bytes VT.string
        let resultError = Dval.resultError VT.bytes VT.string
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let! contents = System.IO.File.ReadAllBytesAsync path
              return resultOk (DBytes contents)
            with e ->
              return resultError (DString($"Error reading file: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "write" 0
      typeParams = []
      parameters = [ Param.make "contents" TBytes ""; Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Writes the specified byte array <param contents> to the file specified by <param path> asynchronously"
      fn =
        let resultOk = Dval.resultOk VT.unit VT.string
        let resultError = Dval.resultError VT.unit VT.string
        (function
        | _, _, [ DBytes contents; DString path ] ->
          uply {
            try
              do! System.IO.File.WriteAllBytesAsync(path, contents)
              return resultOk DUnit
            with e ->
              return resultError (DString($"Error writing file: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "append" 0
      typeParams = []
      parameters = [ Param.make "path" TString ""; Param.make "content" TBytes "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Appends the given <param content> to the file at the specified <param path>. If the file does not exist, a new file is created with the content. Returns a Result type indicating success or failure."
      fn =
        let resultOk = Dval.resultOk VT.unit VT.string
        let resultError = Dval.resultError VT.unit VT.string
        (function
        | _, _, [ DBytes content; DString path ] ->
          uply {
            try
              do! System.IO.File.WriteAllBytesAsync(path, content)
              return resultOk DUnit
            with e ->
              return resultError (DString e.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "createTemp" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TypeReference.result TString TString
      description =
        "Creates a new temporary file with a unique name in the system's temporary directory. Returns a Result type containing the temporary file path or an error if the creation fails."
      fn =
        let resultOk = Dval.resultOk VT.string VT.string
        let resultError = Dval.resultError VT.string VT.string
        (function
        | _, _, [ DUnit ] ->
          uply {
            try
              let tempPath = System.IO.Path.GetTempFileName()
              return resultOk (DString tempPath)
            with e ->
              return resultError (DString e.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "isDirectory" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if the file specified by <param path> is a directory, or false if it is a file or does not exist"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let attrs = System.IO.File.GetAttributes(path)
              let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
              return DBool isDir
            with e ->
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "isNormal" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if the file specified by <param path> is a normal file (not a directory), or false if it is a directory or does not exist"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let attrs = System.IO.File.GetAttributes(path)
              let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
              let exists =
                System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
              return DBool(exists && not isDir)
            with e ->
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "exists" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if a file or directory exists at the specified <param path>, or false otherwise"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let exists =
                System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
              return DBool exists
            with _ ->
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "size" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Returns the size of the file at the specified <param path> in bytes, or an error if the file does not exist or an error occurs"
      fn =
        let resultOk = Dval.resultOk VT.int VT.string
        let resultError = Dval.resultError VT.int VT.string
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let fileInfo = System.IO.FileInfo(path)
              return resultOk (DInt fileInfo.Length)
            with e ->
              return resultError (DString e.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : Builtin.Contents = (fns, types, constants)
