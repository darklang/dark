/// Standard libraries for Files
module Builtins.Cli.Libs.File

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Blob = LibExecution.Blob
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
open Builtin.Shortcuts


// Structured error for file I/O. Mirrors Stdlib.Cli.FileSystem.FileError so
// callers can pattern-match instead of grepping .NET exception text.
module FileError =
  let fqTypeName () =
    FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.FileSystem.fileError ())

  /// Classify a .NET exception into a FileError DEnum. NotFound covers both
  /// missing files and missing parent directories (which present the same way
  /// to a user). PermissionDenied covers UnauthorizedAccess. Anything else
  /// becomes Other with the underlying message.
  let fromException (e : exn) : Dval =
    let typeName = fqTypeName ()
    let (caseName, fields) =
      match e with
      | :? System.IO.FileNotFoundException
      | :? System.IO.DirectoryNotFoundException -> "NotFound", []
      | :? System.UnauthorizedAccessException -> "PermissionDenied", []
      | _ -> "Other", [ DString e.Message ]
    DEnum(typeName, typeName, [], caseName, fields)


let fns () : List<BuiltInFn> =
  [ { name = fn "fileRead" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType =
        TypeReference.result TBlob (TCustomType(NR.ok (FileError.fqTypeName ()), []))
      description =
        "Reads the contents of a file at <param path> asynchronously into an ephemeral Blob, wrapped in a Result."
      fn =
        let errType = KTCustomType(FileError.fqTypeName (), [])
        let resultOk = Dval.resultOk KTBlob errType
        let resultError = Dval.resultError KTBlob errType
        (function
        | _, _, _, [ DString path ] ->
          uply {
            try
              let path =
                path.Replace(
                  "$HOME",
                  System.Environment.GetEnvironmentVariable "HOME"
                )

              let! contents = System.IO.File.ReadAllBytesAsync path
              return resultOk (Blob.newEphemeral contents)
            with e ->
              return resultError (FileError.fromException e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "fileWrite" 0
      typeParams = []
      parameters = [ Param.make "contents" TBlob ""; Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Writes <param contents> to the file at <param path> asynchronously."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | state, _, _, [ DBlob ref; DString path ] ->
          uply {
            try
              let path =
                path.Replace(
                  "$HOME",
                  System.Environment.GetEnvironmentVariable "HOME"
                )

              let! bytes = Blob.readBytes state ref
              do! System.IO.File.WriteAllBytesAsync(path, bytes)
              return resultOk DUnit
            with e ->
              return resultError (DString($"Error writing file: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "fileDelete" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit TString
      description = "Deletes the file specified by <param path>"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          uply {
            try
              System.IO.File.Delete path
              return Dval.resultOk KTUnit KTString DUnit
            with e ->
              return
                Dval.resultError
                  KTUnit
                  KTString
                  (DString $"Error deleting file: {e.Message}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "fileAppendText" 0
      typeParams = []
      parameters = [ Param.make "path" TString ""; Param.make "content" TString "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Appends the given <param content> to the file at the specified <param path>. If the file does not exist, a new file is created with the content. Returns a Result type indicating success or failure."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DString path; DString content ] ->
          uply {
            try
              do! System.IO.File.AppendAllTextAsync(path, content)
              return resultOk DUnit
            with e ->
              return resultError (DString e.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "fileIsDirectory" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if the file specified by <param path> is a directory, or false if it is a file or does not exist"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          uply {
            try
              let attrs = System.IO.File.GetAttributes(path)
              let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
              return DBool isDir
            with _ ->
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "fileExists" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if a file or directory exists at the specified <param path>, or false otherwise"
      fn =
        (function
        | _, _, _, [ DString path ] ->
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
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
