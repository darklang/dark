/// Converts strings of F# into Dark. Used for testing.
module Parser.Parser

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module PTP = ProgramTypes

module UserFunction =
  let rec private parseArgPat
    availableTypes
    (pat : SynPat)
    : PT.UserFunction.Parameter =
    let r = parseArgPat availableTypes

    match pat with
    | SynPat.Paren (pat, _) -> r pat
    | SynPat.Const (SynConst.Unit, _) ->
      { id = gid (); name = "unit"; typ = PT.TUnit; description = "" }
    | SynPat.Typed (SynPat.Named (SynIdent (id, _), _, _, _), typ, _) ->
      { id = gid ()
        name = id.idText
        typ = PTP.DType.fromSynType availableTypes typ
        description = "" }
    | _ -> Exception.raiseInternal "Unsupported argPat" [ "pat", pat ]

  let private parseSignature
    availableTypes
    (pat : SynPat)
    : string * List<string> * List<PT.UserFunction.Parameter> =
    match pat with
    | SynPat.LongIdent (SynLongIdent ([ name ], _, _), _, typeArgPats, argPats, _, _) ->
      let typeParams =
        match typeArgPats with
        | None -> []
        | Some (SynValTyparDecls (pats, _canInfer)) ->
          match pats with
          | None -> []
          | Some typeParams ->
            match typeParams with
            | SynTyparDecls.PostfixList (decls, constraints, _range) ->
              match constraints with
              | [] ->
                decls
                |> List.map (fun decl ->
                  let SynTyparDecl (_, decl) = decl

                  match decl with
                  | SynTyparDecl (_, SynTypar (name, TyparStaticReq.None, _)) ->
                    name.idText
                  | _ ->
                    Exception.raiseInternal
                      "Unsupported type parameter"
                      [ "decl", decl ])
              | _ ->
                Exception.raiseInternal
                  "Unsupported constraints in function type arg declaration"
                  [ "pat", pat; "constraints", constraints ]

            | SynTyparDecls.PrefixList _
            | SynTyparDecls.SinglePrefix _ ->
              Exception.raiseInternal
                "Unsupported type params of function declaration"
                [ "pat", pat; "typeParams", typeParams ]

      let parameters =
        match argPats with
        | SynArgPats.Pats pats -> List.map (parseArgPat availableTypes) pats
        | SynArgPats.NamePatPairs _ ->
          Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]

      (name.idText, typeParams, parameters)

    | _ -> Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]

  let fromSynBinding availableTypes (binding : SynBinding) : PT.UserFunction.T =
    match binding with
    // CLEANUP use returnInfo
    | SynBinding (_, _, _, _, _, _, _, pat, _returnInfo, expr, a, b, c) ->
      let (name, typeParams, parameters) = parseSignature availableTypes pat

      { tlid = gid ()
        name = name
        typeParams = typeParams
        parameters = parameters
        returnType = PT.TVariable "a"
        description = ""
        infix = false
        body = PTP.Expr.fromSynExpr availableTypes expr }


module CustomType =
  module Enum =
    let private parseField
      availableTypes
      (typ : SynField)
      : PT.CustomType.EnumField =
      match typ with
      | SynField (_, _, fieldName, typ, _, _, _, _, _) ->
        { id = gid ()
          typ = PTP.DType.fromSynType availableTypes typ
          label = fieldName |> Option.map (fun id -> id.idText) }

    let private parseCase
      availableTypes
      (case : SynUnionCase)
      : PT.CustomType.EnumCase =
      match case with
      | SynUnionCase (_, SynIdent (id, _), typ, _, _, _, _) ->
        match typ with
        | SynUnionCaseKind.Fields fields ->
          { id = gid ()
            name = id.idText
            fields = List.map (parseField availableTypes) fields }
        | _ -> Exception.raiseInternal $"Unsupported enum case" [ "case", case ]

    let fromCases availableTypes typeDef (cases : List<SynUnionCase>) =
      let parseCase = parseCase availableTypes

      let firstCase, additionalCases =
        match cases with
        | [] ->
          Exception.raiseInternal
            $"Can't parse enum without any cases"
            [ "typeDef", typeDef ]
        | firstCase :: additionalCases -> firstCase, additionalCases

      PT.CustomType.Enum(parseCase firstCase, List.map parseCase additionalCases)

  module Record =
    let private parseField
      availableTypes
      (field : SynField)
      : PT.CustomType.RecordField =
      match field with
      | SynField (_, _, Some id, typ, _, _, _, _, _) ->
        { id = gid ()
          name = id.idText
          typ = PTP.DType.fromSynType availableTypes typ }
      | _ -> Exception.raiseInternal $"Unsupported field" [ "field", field ]

    let fromFields availableTypes typeDef (fields : List<SynField>) =
      match fields with
      | [] ->
        Exception.raiseInternal
          $"Unsupported record type with no fields"
          [ "typeDef", typeDef ]
      | firstField :: additionalFields ->
        let parseField = parseField availableTypes

        PT.CustomType.Record(
          parseField firstField,
          List.map parseField additionalFields
        )


module UserType =
  let fromSynTypeDefn availableTypes (typeDef : SynTypeDefn) : PT.UserType.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _),
                                           _),
                   _,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = { typ = id.idText; version = 0 }
        definition = CustomType.Record.fromFields availableTypes typeDef fields }

    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_, cases, _),
                                           _),
                   _,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = { typ = id.idText; version = 0 }
        definition = CustomType.Enum.fromCases availableTypes typeDef cases }
    | _ ->
      Exception.raiseInternal $"Unsupported type definition" [ "typeDef", typeDef ]


module UserDB =
  let private parseDBSchemaField availableTypes (field : SynField) : PT.DB.Col =
    match field with
    | SynField (_, _, Some id, typ, _, _, _, _, _) ->
      { name = Some(id.idText) // CLEANUP
        nameID = gid ()
        typ = Some(PTP.DType.fromSynType availableTypes typ)
        typeID = gid () }
    | _ -> Exception.raiseInternal $"Unsupported DB schema field" [ "field", field ]

  let fromSynTypeDefn availableTypes (typeDef : SynTypeDefn) : PT.DB.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _),
                                           _),
                   _members,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = id.idText
        nameID = gid ()
        version = 0
        cols = List.map (parseDBSchemaField availableTypes) fields }
    | _ ->
      Exception.raiseInternal $"Unsupported db definition" [ "typeDef", typeDef ]


module PackageFn =
  let fromSynBinding
    ((p1, p2, p3) : string * string * string)
    (binding : SynBinding)
    : PT.Package.Fn =
    let availableTypes = [] // eventually, we'll likely need package types supported here
    let userFn = UserFunction.fromSynBinding availableTypes binding
    { name =
        { owner = p1
          package = p2
          module_ = p3
          function_ = userFn.name
          version = 0 }
      typeParams = userFn.typeParams
      parameters =
        userFn.parameters
        |> List.map (fun (p : PT.UserFunction.Parameter) ->
          { name = p.name; typ = p.typ; description = "" })
      returnType = userFn.returnType
      description = userFn.description
      deprecated = false
      author = ""
      tlid = userFn.tlid
      body = userFn.body }





type Test = { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

let convertToTest availableTypes (ast : SynExpr) : Test =
  let convert (x : SynExpr) : PT.Expr = PTP.Expr.fromSynExpr availableTypes x

  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_,
                              _,
                              SynExpr.LongIdent (_,
                                                 SynLongIdent ([ ident ], _, _),
                                                 _,
                                                 _),
                              actual,
                              _),
                 expected,
                 range) when ident.idText = "op_Equality" ->
    // Exception.raiseInternal $"whole thing: {actual}"
    { name = "test"
      lineNumber = range.Start.Line
      actual = convert actual
      expected = convert expected }
  | _ -> Exception.raiseInternal "Test case not in format `x = y`" [ "ast", ast ]

type Module<'expr> =
  { types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    packageFns : List<PT.Package.Fn>
    modules : List<string * Module<'expr>>
    exprs : List<'expr> }

let emptyModule =
  { types = []; dbs = []; fns = []; modules = []; exprs = []; packageFns = [] }

type TestModule = Module<Test>

type TypeDefs =
  list<LibExecution.ProgramTypes.FQTypeName.T * LibExecution.ProgramTypes.UserType.Definition>


let parseFile
  (parseExprFn : TypeDefs -> SynExpr -> 'expr)
  (stdlibTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (parsedAsFSharp : ParsedImplFileInput)
  : Module<'expr> =
  let longIdentToList (li : LongIdent) : List<string> =
    li |> List.map (fun id -> id.idText)


  let parseTypeDecl
    (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
    (typeDef : SynTypeDefn)
    : List<PT.DB.T> * List<PT.UserType.T> =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
      let attrs = attrs |> List.map (fun attr -> attr.Attributes) |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        [ UserDB.fromSynTypeDefn availableTypes typeDef ], []
      else
        [], [ UserType.fromSynTypeDefn availableTypes typeDef ]

  let getPackage (attrs : SynAttributes) : Option<string * string * string> =
    attrs
    |> List.map (fun attr -> attr.Attributes)
    |> List.concat
    |> List.filterMap (fun (attr : SynAttribute) ->
      if longIdentToList attr.TypeName.LongIdent = [ "Package" ] then
        match attr.ArgExpr with
        | SynExpr.Paren (SynExpr.Tuple (_,
                                        [ SynExpr.Const (SynConst.String (p1, _, _),
                                                         _)
                                          SynExpr.Const (SynConst.String (p2, _, _),
                                                         _)
                                          SynExpr.Const (SynConst.String (p3, _, _),
                                                         _) ],
                                        _,
                                        _),
                         _,
                         _,
                         _) -> Some(p1, p2, p3)
        | _ -> None
      else
        None)
    |> List.tryHead

  let rec parseModule
    (parent : Module<'expr>)
    (attrs : SynAttributes)
    (decls : List<SynModuleDecl>)
    : Module<'expr> =
    let package = getPackage attrs

    List.fold
      { types = parent.types
        fns = parent.fns
        packageFns = parent.packageFns
        dbs = parent.dbs
        modules = []
        exprs = [] }
      (fun m decl ->
        let availableTypes =
          (m.types @ parent.types)
          |> List.map (fun t -> PT.FQTypeName.User t.name, t.definition)
          |> (@) stdlibTypes

        match decl with
        | SynModuleDecl.Let (_, bindings, _) ->
          match package with
          | Some package ->
            let newPackageFns = List.map (PackageFn.fromSynBinding package) bindings
            { m with packageFns = m.packageFns @ newPackageFns }

          | None ->
            let newUserFns =
              List.map (UserFunction.fromSynBinding availableTypes) bindings
            { m with fns = m.fns @ newUserFns }

        | SynModuleDecl.Types (defns, _) ->
          let (dbs, types) =
            List.map (parseTypeDecl availableTypes) defns |> List.unzip
          { m with
              types = m.types @ List.concat types
              dbs = m.dbs @ List.concat dbs }

        | SynModuleDecl.Expr (expr, _) ->
          { m with exprs = m.exprs @ [ parseExprFn availableTypes expr ] }

        | SynModuleDecl.NestedModule (SynComponentInfo (attrs,
                                                        _,
                                                        _,
                                                        [ name ],
                                                        _,
                                                        _,
                                                        _,
                                                        _),
                                      _,
                                      decls,
                                      _,
                                      _,
                                      _) ->
          let nested = parseModule m attrs decls
          { m with modules = m.modules @ [ (name.idText, nested) ] }
        | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
      decls

  match parsedAsFSharp with
  | ParsedImplFileInput (_,
                         _,
                         _,
                         _,
                         _,
                         [ SynModuleOrNamespace (_, _, _, decls, _, attrs, _, _, _) ],
                         _,
                         _,
                         _) -> parseModule emptyModule attrs decls
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parsedAsFsharp", parsedAsFSharp ]



// Below are the fns that we intend to expose to the rest of the codebase
let parseTestFile
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (filename : string)
  : Module<Test> =
  let code = System.IO.File.ReadAllText filename
  let parsedAsFSharp = Utils.parseAsFSharpSourceFile code
  parseFile convertToTest availableTypes parsedAsFSharp

let parseModule
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (filename : string)
  : Module<PT.Expr> =
  let code = System.IO.File.ReadAllText filename
  let parsedAsFSharp = Utils.parseAsFSharpSourceFile code
  parseFile PTP.Expr.fromSynExpr availableTypes parsedAsFSharp
