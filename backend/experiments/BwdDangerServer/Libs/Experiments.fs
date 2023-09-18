module BwdDangerServer.Libs.Experiments

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

// This makes extra careful that we're only accessing files where we expect to
/// find files, and that we're not checking outside these directories
///
/// Note: none of these are async because System.IO is not async
module RestrictedFileIO =
  type Mode =
    | Check
    | Dir
    | Read

  module Config =
    type Root =
      | BackendStatic
      | CanvasesFiles


    let dir (root : Root) : string =
      match root with
      | BackendStatic -> "/home/dark/app/backend/static/"
      | CanvasesFiles -> "/home/dark/app/canvases/"

  let checkFilename (root : Config.Root) (mode : Mode) (f : string) =
    let dir = Config.dir root
    let f : string = $"{dir}{f}"

    let debug (name : string) (value : bool) =
      if value then print $"checkFilename failed: {name}: {value}"
      value

    if
      (f.Contains ".." |> debug "dots"
       || f.Contains "~" |> debug "tilde"
       || f.EndsWith "." |> debug "ends dot"
       || (mode <> Dir && f.EndsWith "/") |> debug "ends slash"
       || (not (dir.EndsWith "/")) |> debug "dir no slash"
       || f.EndsWith "etc/passwd" |> debug "etc"
       // being used wrong
       || f.EndsWith "//" |> debug "double slash"
       // check for irregular file
       || (mode = Read
           && (System.IO.File.GetAttributes f <> System.IO.FileAttributes.Normal)
           && (System.IO.File.GetAttributes f <> System.IO.FileAttributes.ReadOnly))
          |> debug "irreg")
    then
      Exception.raiseInternal "FILE SECURITY VIOLATION" [ "file", f ]
    else
      f

  let fileExists root f : bool =
    let f = checkFilename root Check f
    System.IO.File.Exists f

  let readfile (root : Config.Root) (f : string) : string =
    f |> checkFilename root Read |> System.IO.File.ReadAllText

  let readfileBytes (root : Config.Root) (f : string) : byte[] =
    f |> checkFilename root Read |> System.IO.File.ReadAllBytes

  let tryReadFile (root : Config.Root) (f : string) : string option =
    if fileExists root f then
      f |> checkFilename root Read |> System.IO.File.ReadAllText |> Some
    else
      None


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []
let fn = fn [ "Experiments" ]

let fns : List<BuiltInFn> =
  [ { name = fn "parseAndSerializeProgram" 0
      typeParams = []
      parameters = [ Param.make "code" TString ""; Param.make "filename" TString "" ]
      returnType = TypeReference.result (TDict TString) TString
      description =
        "Parses Dark code and serializes the result to JSON. Expects only types, fns, and exprs."
      fn =
        function
        | state, _, [ DString code; DString filename ] ->
          let okType = ValueType.Known(KTDict(ValueType.Known KTString))
          let errType = ValueType.Known KTString

          uply {
            try
              // TODO: this needs builtins and packages
              let! canvas =
                LibParser.Canvas.parse LibParser.NameResolver.empty filename code

              let t = ExecutionState.availableTypes state

              let types = List.map (PT2RT.UserType.toRT t) canvas.types
              let fns = List.map (PT2RT.UserFunction.toRT t) canvas.fns
              let exprs = List.map (PT2RT.Expr.toRT t) canvas.exprs

              return
                [ "types", DString(Json.Vanilla.serialize types)
                  "fns", DString(Json.Vanilla.serialize fns)
                  "exprs", DString(Json.Vanilla.serialize exprs) ]
                |> Dval.dict (ValueType.Known KTString)
                |> Dval.resultOk okType errType
            with e ->
              let error = Exception.getMessages e |> String.concat " "
              let metadata = Exception.nestedMetadata e
              let metadataDval = metadata |> Map.ofList
              return
                DString($"Error parsing code: {error} {metadataDval}")
                |> Dval.resultError okType errType
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "readFromStaticDir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Reads a file at backend/static/<param path>, and returns its contents as Bytes wrapped in a Result"
      fn =
        (function
        | _, _, [ DString path ] ->
          let okType = ValueType.Known KTBytes
          let errType = ValueType.Known KTString
          uply {
            try
              let contents =
                RestrictedFileIO.readfileBytes
                  RestrictedFileIO.Config.BackendStatic
                  path
              return DBytes contents |> Dval.resultOk okType errType
            with e ->
              return
                DString($"Error reading file: {e.Message}")
                |> Dval.resultError okType errType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "readFromCanvases" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Reads a file at canvases/<param path>, and returns its contents as Bytes wrapped in a Result"
      fn =
        (function
        | _, _, [ DString path ] ->
          let okType = ValueType.Known KTBytes
          let errType = ValueType.Known KTString
          uply {
            try
              let contents =
                RestrictedFileIO.readfileBytes
                  RestrictedFileIO.Config.CanvasesFiles
                  path
              return Dval.resultOk okType errType (DBytes contents)
            with e ->
              return
                Dval.resultError
                  okType
                  errType
                  (DString($"Error reading file: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents : LibExecution.Builtin.Contents = (fns, types, constants)
