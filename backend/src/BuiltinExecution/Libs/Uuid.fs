module BuiltinExecution.Libs.Uuid

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude
open LibExecution.Builtin.Shortcuts
module VT = ValueType
module Dval = LibExecution.Dval

let constants : List<BuiltInConstant> = []

let fn = fn [ "Uuid" ]

module ParseError =
  type ParseError = | BadFormat

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []

    let typeName = FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Uuid" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "generate" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "Generate a new <type Uuid> v4 according to RFC 4122"
      fn =
        (function
        | _, _, [ DUnit ] -> Ply(DUuid(System.Guid.NewGuid()))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      // similarly to DateTime.now, it's not particularly fun for this to change
      // when live programming
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "uuid" TString "" ]
      returnType =
        TypeReference.result
          TInt64
          (TCustomType(
            Ok(
              FQTypeName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "Uuid" ]
                  name =  "ParseError"
                  version = 0 }
            ),
            []
          ))
      description =
        "Parse a <type Uuid> of form {{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}}"
      fn =
        let resultOk = Dval.resultOk KTUuid KTString
        let typeName =
          FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Uuid" ] "ParseError" 0
        let resultError = Dval.resultError KTUuid (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          match System.Guid.TryParse s with
          | true, x -> x |> DUuid |> resultOk |> Ply
          | _ -> ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "uuid" TUuid "" ]
      returnType = TString
      description =
        "Stringify <param uuid> to the format XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
      fn =
        (function
        | _, _, [ DUuid uuid ] -> Ply(DString(string uuid))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, constants)
