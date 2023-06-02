module StdLibDarkInternal.Libs.ProgramTypes

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let modul = [ "DarkInternal"; "ProgramTypes" ]

let typ
  (submodules : List<string>)
  (name : string)
  (version : int)
  : FQTypeName.StdlibTypeName =
  FQTypeName.stdlibTypeName' (modul @ submodules) name version

let fn (name : string) (version : int) : FQFnName.StdlibFnName =
  FQFnName.stdlibFnName' modul name version


let idField = TCustomType(FQTypeName.Stdlib(typ [] "ID" 0), [])


let types : List<BuiltInType> =
  [
    // { name = typ [] "ID" 0
    //   typeParams = []
    //   definition = CustomType.Alias TInt
    //   deprecated = NotDeprecated
    //   description = "It's an identifier" }


    { name = typ [ "FQTypeName" ] "UserTypeName" 0
      typeParams = []
      definition =
        CustomType.Record(
          { name = "modules"
            typ = TList(TString)
            description = "The module in which the type is namespaced" },

          [ { name = "typ"
              typ = TString
              description = "The name of the user-defined type" }

            { name = "version"
              typ = TInt
              description = "The version (0 or greater) of the type" } ]
        )
      deprecated = NotDeprecated
      description =
        "Darklang's available types (int, List<T>, user-defined types, etc.)" }


    // { name = typ [ "FQTypeName" ] "T" 0
    //   typeParams = []
    //   definition =
    //     CustomType.Enum(
    //       { name = "User"
    //         fields =
    //           [ { typ =
    //                 TCustomType(
    //                   FQTypeName.Stdlib(typ [ "FQTypeName" ] "UserTypeName" 0),
    //                   []
    //                 )
    //               label = None
    //               description = "User-defined type" } ]
    //         description = "User-defined type" },

    //       [
    //       // TODO: | Stdlib of StdlibTypeName
    //       // TODO: | Package of PackageTypeName
    //       ]
    //     )
    //   deprecated = NotDeprecated
    //   description =
    //     "Darklang's available types (int, List<T>, user-defined types, etc.)" }


    { name = typ [] "TypeReference" 0
      typeParams = []
      definition =
        CustomType.Enum(
          { name = "TInt"; fields = []; description = "An integer" },

          [] // TODO: many more cases
        )
      deprecated = NotDeprecated
      description =
        "Darklang's available types (int, List<T>, user-defined types, etc.)" }


    // TODO: other types in the CustomType module

    { name = typ [ "CustomType" ] "T" 0
      typeParams = []
      definition =
        CustomType.Enum(
          { name = "Alias"
            fields =
              [ { typ = TCustomType(FQTypeName.Stdlib(typ [] "TypeReference" 0), [])
                  label = None
                  description = "Type that's being aliased" } ]
            description =
              "Alias/abbreviation of an existing type with an alternative name, to capture some meaning" },

          [
          // TODO: Record
          // TODO: Enum
          ]
        )
      deprecated = NotDeprecated
      description =
        "A stdlib-, user-, or package- defined type (an alias, record, or enum)" }


    { name = typ [] "UserType" 0
      typeParams = []
      definition =
        CustomType.Record(
          // TODO: tlid should probably be an alias of TInt
          { name = "tlid"; typ = TInt; description = "The toplevel identifier" },

          [ { name = "name"
              typ =
                TCustomType(
                  FQTypeName.Stdlib(typ [ "FQTypeName" ] "UserTypeName" 0),
                  []
                )
              description = "Name" }

            { name = "definition"
              typ = TCustomType(FQTypeName.Stdlib(typ [ "CustomType" ] "T" 0), [])
              description = "The type definition (Alias, Record, Enum, etc.)" } ]
        )
      deprecated = NotDeprecated
      description = "A type that a user defined" }


    // { name = typ [] "Expr" 0
    //   typeParams = []
    //   definition =
    //     // TODO: a lot more cases
    //     CustomType.Enum(
    //       { name = "EInt"
    //         fields =
    //           [ { typ = idField; label = None; description = "identifier" }
    //             { typ = TInt; label = None; description = "the value" } ]
    //         description = "Integer value" },

    //       [ { name = "EBool"
    //           fields =
    //             [ { typ = idField; label = None; description = "identifier" }
    //               { typ = TBool; label = None; description = "the value" } ]
    //           description = "Boolean value" } ]
    //     )
    //   deprecated = NotDeprecated
    //   description = "An expression - the main part of the language" }

    // TODO: FQFnName
    // TODO: LetPattern
    // TODO: MatchPattern
    // TODO: BinaryOperation
    // TODO: InfixFnName
    // TODO: Infix
    // TODO: StringSegment
    // TODO: PipeExpr
    // TODO: Deprecation
    // TODO: Handler
    // TODO: DB
    // TODO: UserFunction
    // TODO: TopLevel
    // TODO: PackageFn
    // TODO: PackageType
    // ? TODO: Op
    // ? TODO: Secret
    // ? TODO: Canvas/Package
    ]


let fns : List<BuiltInFn> =
  [ { name = fn "TODO" 0
      typeParams = []
      parameters = []
      returnType = TUnit
      description = "TODO"
      fn =
        (function
        | _, _, [] -> Ply(DUnit)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
