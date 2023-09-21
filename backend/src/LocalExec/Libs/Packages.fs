module LocalExec.Libs.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibCloud.Db

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module PT2DT = LibExecution.ProgramTypesToDarkTypes

let types : List<BuiltInType> =
  [ { name = typ [ "LocalExec"; "Packages" ] "Function" 0
      description = "The name of a package function"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "owner"; typ = TString }
                [ { name = "modules"; typ = TList TString }
                  { name = "name"; typ = TString }
                  { name = "version"; typ = TInt } ]
            ) }
      deprecated = NotDeprecated }


    { name = typ [ "LocalExec"; "Packages" ] "Type" 0
      description = "The name of a package type"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "owner"; typ = TString }
                [ { name = "modules"; typ = TList TString }
                  { name = "name"; typ = TString }
                  { name = "version"; typ = TInt } ]
            ) }
      deprecated = NotDeprecated }


    { name = typ [ "LocalExec"; "Packages" ] "Constant" 0
      description = "The name of a package constant"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "owner"; typ = TString }
                [ { name = "modules"; typ = TList TString }
                  { name = "name"; typ = TString }
                  { name = "version"; typ = TInt } ]
            ) }
      deprecated = NotDeprecated }


    { name = typ [ "LocalExec"; "Packages" ] "Package" 0
      description = "A package, with types, constants, and functions"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "types"
                  typ =
                    TList(
                      TCustomType(
                        Ok(
                          TypeName.fqPackage
                            "Darklang"
                            [ "LanguageTools"; "Stdlib"; "ProgramTypes" ]
                            "PackageType"
                            0
                        ),
                        []
                      )
                    ) }
                [ { name = "constants"
                    typ =
                      TList(
                        TCustomType(
                          Ok(
                            TypeName.fqPackage
                              "Darklang"
                              [ "LanguageTools"; "Stdlib"; "ProgramTypes" ]
                              "PackageConstant"
                              0
                          ),
                          []
                        )
                      ) }
                  { name = "fns"
                    typ =
                      TList(
                        TCustomType(
                          Ok(
                            TypeName.fqPackage
                              "Darklang"
                              [ "LanguageTools"
                                "Stdlib"
                                "ProgramTypes"
                                "PackageFn" ]
                              "PackageFn"
                              0
                          ),
                          []
                        )
                      ) } ]
            ) }
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "Packages" ] "listFunctions" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            Ok(FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Function" 0)),
            []
          )
        )
      description = "List all package functions"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! fns =
              Sql.query
                "SELECT owner, modules, fnname, version FROM package_functions_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "fnname",
                 read.string "modules",
                 read.int "version"))

            let fns =
              fns
              |> List.map (fun (owner, fnname, modules, version) ->
                Dval.record
                  (FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Function" 0))
                  (Some [])
                  [ ("owner", DString owner)
                    ("modules",
                     modules
                     |> String.split "."
                     |> List.map DString
                     |> Dval.list VT.string)
                    ("name", DString fnname)
                    ("version", DInt version) ])

            return Dval.list VT.unknownTODO fns
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listTypes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            Ok(FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Type" 0)),
            []
          )
        )
      description = "List all package types"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! types =
              Sql.query
                "SELECT owner, modules, typename, version FROM package_types_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "typename",
                 read.string "modules",
                 read.int "version"))

            let types =
              types
              |> List.map (fun (owner, typename, modules, version) ->
                Dval.record
                  (FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Type" 0))
                  (Some [])
                  [ ("owner", DString owner)
                    ("modules",
                     modules
                     |> String.split "."
                     |> List.map DString
                     |> Dval.list VT.unknownTODO)
                    ("name", DString typename)
                    ("version", DInt version) ])

            return Dval.list VT.unknownTODO types
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listConstants" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            Ok(FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Constant" 0)),
            []
          )
        )
      description = "List all package constants"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! consts =
              Sql.query
                "SELECT owner, modules, name, version FROM package_constants_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "name",
                 read.string "modules",
                 read.int "version"))

            let consts =
              consts
              |> List.map (fun (owner, fnname, modules, version) ->
                Dval.record
                  (FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Constant" 0))
                  (Some [])
                  [ ("owner", DString owner)
                    ("modules",
                     modules
                     |> String.split "."
                     |> List.map DString
                     |> Dval.list VT.unknownTODO)
                    ("name", DString fnname)
                    ("version", DInt version) ])

            return Dval.list VT.unknownTODO consts
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents : LibExecution.Builtin.Contents = (fns, types, constants)
