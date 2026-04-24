/// Standard libraries for Files
module BuiltinCli.Libs.File

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "fileRead" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TBlob TString
      description =
        "Reads the contents of a file at <param path> asynchronously into an ephemeral Blob, wrapped in a Result."
      fn =
        let resultOk = Dval.resultOk KTBlob KTString
        let resultError = Dval.resultError KTBlob KTString
        (function
        | state, _, _, [ DString path ] ->
          uply {
            try
              let path =
                path.Replace(
                  "$HOME",
                  System.Environment.GetEnvironmentVariable "HOME"
                )

              let! contents = System.IO.File.ReadAllBytesAsync path
              return resultOk (Dval.newEphemeralBlob state contents)
            with e ->
              return resultError (DString($"Error reading file: {e.Message}"))
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

              let! bytes =
                match ref with
                | Ephemeral id ->
                  let mutable bs : byte[] = null
                  if state.blobStore.TryGetValue(id, &bs) then
                    Ply bs
                  else
                    Exception.raiseInternal "ephemeral blob not found" [ "id", id ]
                | Persistent(hash, _) ->
                  uply {
                    let! got = state.blobs.get hash
                    match got with
                    | Some bs -> return bs
                    | None ->
                      return
                        Exception.raiseInternal
                          "persistent blob missing in package_blobs"
                          [ "hash", hash ]
                  }

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


    { name = fn "fileCreateTemp" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TypeReference.result TString TString
      description =
        "Creates a new temporary file with a unique name in the system's temporary directory. Returns a Result type containing the temporary file path or an error if the creation fails."
      fn =
        let resultOk r = Dval.resultOk KTString KTString r |> Ply
        let resultError r = Dval.resultError KTString KTString r |> Ply
        (function
        | _, _, _, [ DUnit ] ->
          try
            let tempPath = System.IO.Path.GetTempFileName()
            resultOk (DString tempPath)
          with e ->
            resultError (DString e.Message)
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


    { name = fn "fileIsNormal" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if the file specified by <param path> is a normal file (not a directory), or false if it is a directory or does not exist"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          uply {
            try
              let attrs = System.IO.File.GetAttributes(path)
              let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
              let exists =
                System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
              return DBool(exists && not isDir)
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
      deprecated = NotDeprecated }


    { name = fn "fileSize" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Returns the size of the file at the specified <param path> in bytes, or an error if the file does not exist or an error occurs"
      fn =
        let resultOk r = Dval.resultOk KTInt64 KTString r |> Ply
        let resultError r = Dval.resultError KTInt64 KTString r |> Ply
        (function
        | _, _, _, [ DString path ] ->
          try
            let fileInfo = System.IO.FileInfo(path)
            resultOk (DInt64 fileInfo.Length)
          with e ->
            resultError (DString e.Message)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
