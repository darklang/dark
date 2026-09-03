/// Standard libraries for Files
module Builtins.Cli.Libs.File

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Blob = LibExecution.Blob
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
open Builtin.Shortcuts


// Structured error for file I/O. Mirrors Stdlib.Cli.FileSystem.FileError so
// callers can pattern-match instead of grepping .NET exception text.
module FileError =
  let fqTypeName () =
    FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.FileSystem.fileError ())

  let typeRef () = TCustomType(NR.ok (fqTypeName ()), [])

  let knownType () = KTCustomType(fqTypeName (), [])

  /// Map a host-boundary failure into a FileError DEnum. NotFound covers both
  /// missing files and missing parent directories (which present the same way
  /// to a user).
  let fromFailure (failure : Host.Failure) : Dval =
    let typeName = fqTypeName ()
    let (caseName, fields) =
      match failure.kind with
      | Host.FailureKind.NotFound -> "NotFound", []
      | Host.FailureKind.PermissionDenied -> "PermissionDenied", []
      | Host.FailureKind.Other -> "Other", [ DString failure.message ]
    DEnum(typeName, typeName, [], caseName, fields)

/// Run one file operation and map its outcome to `Result<Unit, FileError>`.
let private fileUnitOp
  (state : ExecutionState)
  (vm : VMState)
  (op : Host.Operation)
  : Ply<Dval> =
  uply {
    match! PermissionCheck.performHost state vm op with
    | Ok response ->
      Host.expectUnit response
      return Dval.resultOk KTUnit (FileError.knownType ()) DUnit
    | Error failure ->
      let error = FileError.fromFailure failure
      return Dval.resultError KTUnit (FileError.knownType ()) error
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "fileRead" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TBlob (FileError.typeRef ())
      description =
        "Reads the contents of a file at <param path> asynchronously into an ephemeral Blob, wrapped in a Result."
      fn =
        let resultOk = Dval.resultOk KTBlob (FileError.knownType ())
        let resultError = Dval.resultError KTBlob (FileError.knownType ())
        (function
        | state, vm, _, [| DString path |] ->
          uply {
            let op = Host.Operation.FileRead(Host.expandHome path)
            match! PermissionCheck.performHost state vm op with
            | Ok response ->
              return resultOk (Blob.newEphemeral (Host.expectBytes response))
            | Error failure -> return resultError (FileError.fromFailure failure)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "fileWrite" 0
      typeParams = []
      parameters = [ Param.make "contents" TBlob ""; Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit (FileError.typeRef ())
      description =
        "Writes <param contents> to the file at <param path> asynchronously."
      fn =
        (function
        | state, vm, _, [| DBlob ref; DString path |] ->
          uply {
            let! bytes = Blob.readBytes state ref
            let op = Host.Operation.FileWrite(Host.expandHome path, bytes)
            return! fileUnitOp state vm op
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileWrite ]
      deprecated = NotDeprecated }


    { name = fn "fileDelete" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TUnit (FileError.typeRef ())
      description = "Deletes the file specified by <param path>"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          fileUnitOp state vm (Host.Operation.FileDelete(Host.expandHome path))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileWrite ]
      deprecated = NotDeprecated }


    { name = fn "fileAppendText" 0
      typeParams = []
      parameters = [ Param.make "path" TString ""; Param.make "content" TString "" ]
      returnType = TypeReference.result TUnit (FileError.typeRef ())
      description =
        "Appends the given <param content> to the file at the specified <param path>. If the file does not exist, a new file is created with the content. Returns a Result type indicating success or failure."
      fn =
        (function
        | state, vm, _, [| DString path; DString content |] ->
          fileUnitOp
            state
            vm
            (Host.Operation.FileAppendText(Host.expandHome path, content))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileWrite ]
      deprecated = NotDeprecated }


    { name = fn "fileIsDirectory" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if the file specified by <param path> is a directory, or false if it is a file or does not exist"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          uply {
            let op = Host.Operation.FileStat(Host.expandHome path)
            match! PermissionCheck.performHost state vm op with
            | Ok response ->
              let (_exists, isDirectory) = Host.expectStat response
              return DBool isDirectory
            | Error _ -> return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "fileExists" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBool
      description =
        "Returns true if a file or directory exists at the specified <param path>, or false otherwise"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          uply {
            let op = Host.Operation.FileStat(Host.expandHome path)
            match! PermissionCheck.performHost state vm op with
            | Ok response ->
              let (exists, _isDirectory) = Host.expectStat response
              return DBool exists
            | Error _ -> return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
