module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude

open RuntimeTypes

module D = DvalDecoder

let languageToolsTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage "Darklang" ("LanguageTools" :: submodules) name version


let rtTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  languageToolsTyp ("RuntimeTypes" :: submodules) name version

let errorsTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  languageToolsTyp ("Errors" :: submodules) name version





module FQName =
  let ownerField m = m |> D.stringField "owner"
  let modulesField m = m |> D.stringListField "modules"

  let nameField m = m |> D.field "name"
  let versionField m = m |> D.intField "version"

  module BuiltIn =
    let toDT (nameMapper : 'name -> Dval) (u : FQName.BuiltIn<'name>) : Dval =
      DvalUtils.record
        (rtTyp [ "FQName" ] "BuiltIn" 0)
        [ "modules", DvalUtils.list valueTypeTODO (List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.BuiltIn<'name> =
      match d with
      | DRecord(_, _, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module UserProgram =
    let toDT (nameMapper : 'name -> Dval) (u : FQName.UserProgram<'name>) : Dval =
      DvalUtils.record
        (rtTyp [ "FQName" ] "UserProgram" 0)
        [ "modules", DvalUtils.list valueTypeTODO (List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (v : Dval) : FQName.UserProgram<'name> =
      match v with
      | DRecord(_, _, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module Package =
    let toDT (nameMapper : 'name -> Dval) (u : FQName.Package<'name>) : Dval =
      DvalUtils.record
        (rtTyp [ "FQName" ] "Package" 0)
        [ "owner", DString u.owner
          "modules", DvalUtils.list valueTypeTODO (List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.Package<'name> =
      match d with
      | DRecord(_, _, _, m) ->
        let owner = ownerField m
        let modules =
          modulesField m
          |> NEList.ofListUnsafe "Expected modules to be a non-empty list" []
        let name = nameField m |> nameMapper
        let version = versionField m

        { owner = owner
          modules = NEList.toList modules
          name = name
          version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []


  let toDT (nameMapper : 'name -> Dval) (u : FQName.FQName<'name>) : Dval =
    let caseName, fields =
      match u with
      | FQName.UserProgram u -> "UserProgram", [ UserProgram.toDT nameMapper u ]
      | FQName.Package u -> "Package", [ Package.toDT nameMapper u ]
      | FQName.BuiltIn u -> "BuiltIn", [ BuiltIn.toDT nameMapper u ]

    DvalUtils.enum (rtTyp [ "FQName" ] "FQName" 0) caseName fields

  let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.FQName<'name> =
    match d with
    | DEnum(_, _, "UserProgram", [ u ]) ->
      FQName.UserProgram(UserProgram.fromDT nameMapper u)
    | DEnum(_, _, "Package", [ u ]) -> FQName.Package(Package.fromDT nameMapper u)
    | DEnum(_, _, "BuiltIn", [ u ]) -> FQName.BuiltIn(BuiltIn.fromDT nameMapper u)
    | _ -> Exception.raiseInternal "Invalid FQName" []


module TypeName =
  module Name =
    let toDT (u : TypeName.Name) : Dval =
      let caseName, fields =
        match u with
        | TypeName.TypeName name -> "TypeName", [ DString name ]

      DvalUtils.enum (rtTyp [ "TypeName" ] "Name" 0) caseName fields

    let fromDT (d : Dval) : TypeName.Name =
      match d with
      | DEnum(_, _, "TypeName", [ DString name ]) -> TypeName.TypeName(name)
      | _ -> Exception.raiseInternal "Invalid TypeName" []

  module BuiltIn =
    let toDT (u : TypeName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u
    let fromDT (d : Dval) : TypeName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : TypeName.UserProgram) : Dval = FQName.UserProgram.toDT Name.toDT u

    let fromDT (d : Dval) : TypeName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : TypeName.Package) : Dval = FQName.Package.toDT Name.toDT u
    let fromDT (d : Dval) : TypeName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : TypeName.TypeName) : Dval = FQName.toDT Name.toDT u
  let fromDT (d : Dval) : TypeName.TypeName = FQName.fromDT Name.fromDT d


module FnName =
  module Name =
    let toDT (u : FnName.Name) : Dval =
      let caseName, fields =
        match u with
        | FnName.FnName name -> "FnName", [ DString name ]

      DvalUtils.enum (rtTyp [ "FnName" ] "Name" 0) caseName fields

    let fromDT (d : Dval) : FnName.Name =
      match d with
      | DEnum(_, _, "FnName", [ DString name ]) -> FnName.FnName(name)
      | _ -> Exception.raiseInternal "Invalid FnName" []

  module BuiltIn =
    let toDT (u : FnName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u
    let fromDT (d : Dval) : FnName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : FnName.UserProgram) : Dval = FQName.UserProgram.toDT Name.toDT u
    let fromDT (d : Dval) : FnName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : FnName.Package) : Dval = FQName.Package.toDT Name.toDT u
    let fromDT (d : Dval) : FnName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : FnName.FnName) : Dval = FQName.toDT Name.toDT u
  let fromDT (d : Dval) : FnName.FnName = FQName.fromDT Name.fromDT d


module ConstantName =
  module Name =
    let toDT (u : ConstantName.Name) : Dval =
      let caseName, fields =
        match u with
        | ConstantName.ConstantName name -> "ConstantName", [ DString name ]

      DvalUtils.enum (rtTyp [ "ConstantName" ] "Name" 0) caseName fields

    let fromDT (d : Dval) : ConstantName.Name =
      match d with
      | DEnum(_, _, "ConstantName", [ DString name ]) ->
        ConstantName.ConstantName(name)
      | _ -> Exception.raiseInternal "Invalid ConstantName" []

  module BuiltIn =
    let toDT (u : ConstantName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u
    let fromDT (d : Dval) : ConstantName.BuiltIn =
      FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : ConstantName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.toDT u

    let fromDT (d : Dval) : ConstantName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : ConstantName.Package) : Dval = FQName.Package.toDT Name.toDT u
    let fromDT (d : Dval) : ConstantName.Package =
      FQName.Package.fromDT Name.fromDT d

  let toDT (u : ConstantName.ConstantName) : Dval = FQName.toDT Name.toDT u
  let fromDT (d : Dval) : ConstantName.ConstantName = FQName.fromDT Name.fromDT d


module NameResolution =
  let toDT (f : 'p -> Dval) (result : NameResolution<'p>) : Dval =
    match result with
    | Ok name -> DvalUtils.resultOk (f name)
    | Error err -> DvalUtils.resultError (RuntimeError.toDT err)

  let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
    match d with
    | DEnum(tn, _, "Ok", [ v ]) when tn = DvalUtils.resultType -> Ok(f v)
    | DEnum(tn, _, "Error", [ v ]) when tn = DvalUtils.resultType ->
      Error(RuntimeError.fromDT v)
    | _ -> Exception.raiseInternal "Invalid NameResolution" []

module TypeReference =
  let rec toDT (t : TypeReference) : Dval =
    let name, fields =
      match t with
      | TVariable name -> "TVariable", [ DString name ]

      | TUnit -> "TUnit", []
      | TBool -> "TBool", []
      | TInt -> "TInt", []
      | TFloat -> "TFloat", []
      | TChar -> "TChar", []
      | TString -> "TString", []
      | TDateTime -> "TDateTime", []
      | TUuid -> "TUuid", []
      | TBytes -> "TBytes", []
      | TPassword -> "TPassword", []

      | TList inner -> "TList", [ toDT inner ]

      | TTuple(first, second, theRest) ->
        "TTuple",
        [ toDT first
          toDT second
          DvalUtils.list valueTypeTODO (List.map toDT theRest) ]

      | TDict inner -> "TDict", [ toDT inner ]

      | TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT TypeName.toDT typeName
          DvalUtils.list valueTypeTODO (List.map toDT typeArgs) ]

      | TDB inner -> "TDB", [ toDT inner ]
      | TFn(args, ret) ->
        "TFn",
        [ DvalUtils.list valueTypeTODO (List.map toDT (NEList.toList args))
          toDT ret ]

    DvalUtils.enum (rtTyp [] "TypeReference" 0) name fields

  let rec fromDT (d : Dval) : TypeReference =
    match d with
    | DEnum(_, _, "TVariable", [ DString name ]) -> TVariable(name)

    | DEnum(_, _, "TUnit", []) -> TUnit
    | DEnum(_, _, "TBool", []) -> TBool
    | DEnum(_, _, "TInt", []) -> TInt
    | DEnum(_, _, "TFloat", []) -> TFloat
    | DEnum(_, _, "TChar", []) -> TChar
    | DEnum(_, _, "TString", []) -> TString
    | DEnum(_, _, "TDateTime", []) -> TDateTime
    | DEnum(_, _, "TUuid", []) -> TUuid
    | DEnum(_, _, "TBytes", []) -> TBytes
    | DEnum(_, _, "TPassword", []) -> TPassword

    | DEnum(_, _, "TList", [ inner ]) -> TList(fromDT inner)

    | DEnum(_, _, "TTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "TDict", [ inner ]) -> TDict(fromDT inner)

    | DEnum(_, _, "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      TCustomType(
        NameResolution.fromDT TypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, "TDB", [ inner ]) -> TDB(fromDT inner)
    | DEnum(_, _, "TFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
      TFn(NEList.ofList (fromDT firstArg) (List.map fromDT otherArgs), fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" [ "typeRef", d ]

module Param =
  let toDT (p : Param) : Dval =
    DvalUtils.record
      (rtTyp [] "Param" 0)
      [ ("name", DString p.name); ("typ", TypeReference.toDT p.typ) ]


module LetPattern =
  let rec toDT (p : LetPattern) : Dval =
    let name, fields =
      match p with
      | LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | LPUnit id -> "LPUnit", [ DInt(int64 id) ]
      | LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          DvalUtils.list valueTypeTODO (List.map toDT theRest) ]

    DvalUtils.enum (rtTyp [] "LetPattern" 0) name fields

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, "LPVariable", [ DInt id; DString name ]) ->
      LPVariable(uint64 id, name)
    | DEnum(_, _, "LPUnit", [ DInt id ]) -> LPUnit(uint64 id)
    | DEnum(_, _, "LPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let rec toDT (p : MatchPattern) : Dval =
    let name, fields =
      match p with
      | MPVariable(id, name) -> "MPVariable", [ DInt(int64 id); DString name ]

      | MPUnit id -> "MPUnit", [ DInt(int64 id) ]
      | MPBool(id, b) -> "MPBool", [ DInt(int64 id); DBool b ]
      | MPInt(id, i) -> "MPInt", [ DInt(int64 id); DInt i ]
      | MPFloat(id, f) -> "MPFloat", [ DInt(int64 id); DFloat f ]
      | MPChar(id, c) -> "MPChar", [ DInt(int64 id); DString c ]
      | MPString(id, s) -> "MPString", [ DInt(int64 id); DString s ]

      | MPList(id, inner) ->
        "MPList",
        [ DInt(int64 id); DvalUtils.list valueTypeTODO (List.map toDT inner) ]
      | MPListCons(id, head, tail) ->
        "MPListCons", [ DInt(int64 id); toDT head; toDT tail ]
      | MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          DvalUtils.list valueTypeTODO (List.map toDT theRest) ]
      | MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt(int64 id)
          DString caseName
          DvalUtils.list valueTypeTODO (List.map toDT fieldPats) ]

    DvalUtils.enum (rtTyp [] "MatchPattern" 0) name fields

  let rec fromDT (d : Dval) : MatchPattern =
    match d with
    | DEnum(_, _, "MPVariable", [ DInt id; DString name ]) ->
      MPVariable(uint64 id, name)

    | DEnum(_, _, "MPUnit", [ DInt id ]) -> MPUnit(uint64 id)
    | DEnum(_, _, "MPBool", [ DInt id; DBool b ]) -> MPBool(uint64 id, b)
    | DEnum(_, _, "MPInt", [ DInt id; DInt i ]) -> MPInt(uint64 id, i)
    | DEnum(_, _, "MPFloat", [ DInt id; DFloat f ]) -> MPFloat(uint64 id, f)
    | DEnum(_, _, "MPChar", [ DInt id; DString c ]) -> MPChar(uint64 id, c)
    | DEnum(_, _, "MPString", [ DInt id; DString s ]) -> MPString(uint64 id, s)

    | DEnum(_, _, "MPList", [ DInt id; DList(_vtTODO, inner) ]) ->
      MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, "MPListCons", [ DInt id; head; tail ]) ->
      MPListCons(uint64 id, fromDT head, fromDT tail)
    | DEnum(_, _, "MPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | DEnum(_, _, "MPEnum", [ DInt id; DString caseName; DList(_vtTODO, fieldPats) ]) ->
      MPEnum(uint64 id, caseName, List.map fromDT fieldPats)
    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module StringSegment =
  let toDT (exprToDT : Expr -> Dval) (s : StringSegment) : Dval =
    let name, fields =
      match s with
      | StringText text -> "StringText", [ DString text ]
      | StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]

    DvalUtils.enum (rtTyp [] "StringSegment" 0) name fields

  let fromDT (exprFromDT : Dval -> Expr) (d : Dval) : StringSegment =
    match d with
    | DEnum(_, _, "StringText", [ DString text ]) -> StringText text
    | DEnum(_, _, "StringInterpolation", [ expr ]) ->
      StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module Expr =
  let rec toDT (e : Expr) : Dval =
    let name, fields =
      match e with
      | EUnit id -> "EUnit", [ DInt(int64 id) ]

      // simple data
      | EBool(id, b) -> "EBool", [ DInt(int64 id); DBool b ]
      | EInt(id, i) -> "EInt", [ DInt(int64 id); DInt i ]
      | EFloat(id, f) -> "EFloat", [ DInt(int64 id); DFloat f ]
      | EChar(id, c) -> "EChar", [ DInt(int64 id); DString c ]
      | EString(id, segments) ->
        "EString",
        [ DInt(int64 id)
          DvalUtils.list valueTypeTODO (List.map (StringSegment.toDT toDT) segments) ]

      // structures of data
      | EList(id, inner) ->
        "EList",
        [ DInt(int64 id); DvalUtils.list valueTypeTODO (List.map toDT inner) ]

      | EDict(id, pairs) ->
        "EDict",
        [ DInt(int64 id)
          DvalUtils.list
            valueTypeTODO
            (List.map (fun (k, v) -> DTuple(DString k, toDT v, [])) pairs) ]

      | ETuple(id, first, second, theRest) ->
        "ETuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          DvalUtils.list valueTypeTODO (List.map toDT theRest) ]

      | ERecord(id, name, fields) ->
        let fields =
          (NEList.toList fields)
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecord",
        [ DInt(int64 id); TypeName.toDT name; DvalUtils.list valueTypeTODO (fields) ]

      | EEnum(id, typeName, caseName, fields) ->
        "EEnum",
        [ DInt(int64 id)
          TypeName.toDT typeName
          DString caseName
          DvalUtils.list valueTypeTODO (List.map toDT fields) ]

      // declaring and accessing variables
      | ELet(id, lp, expr, body) ->
        "ELet", [ DInt(int64 id); LetPattern.toDT lp; toDT expr; toDT body ]

      | EFieldAccess(id, expr, fieldName) ->
        "EFieldAccess", [ DInt(int64 id); toDT expr; DString fieldName ]

      | EVariable(id, varName) -> "EVariable", [ DInt(int64 id); DString varName ]


      // control flow
      | EIf(id, cond, thenExpr, elseExpr) ->
        let elseExpr = elseExpr |> Option.map toDT |> DvalUtils.option
        "EIf", [ DInt(int64 id); toDT cond; toDT thenExpr; elseExpr ]

      | EMatch(id, arg, cases) ->
        let cases =
          (NEList.toList cases)
          |> List.map (fun (pattern, expr) ->
            DTuple(MatchPattern.toDT pattern, toDT expr, []))

        "EMatch", [ DInt(int64 id); toDT arg; DvalUtils.list valueTypeTODO cases ]



      | ELambda(id, args, body) ->
        let variables =
          (NEList.toList args)
          |> List.map (fun (id, varName) ->
            DTuple(DInt(int64 id), DString varName, []))
          |> DvalUtils.list valueTypeTODO

        "ELambda", [ DInt(int64 id); variables; toDT body ]

      | EConstant(id, name) ->
        "EConstant", [ DInt(int64 id); ConstantName.toDT name ]

      | EApply(id, name, typeArgs, args) ->
        "EApply",
        [ DInt(int64 id)
          toDT name
          DvalUtils.list valueTypeTODO (List.map TypeReference.toDT typeArgs)
          DvalUtils.list valueTypeTODO (List.map toDT (NEList.toList args)) ]

      | EFnName(id, name) -> "EFnName", [ DInt(int64 id); FnName.toDT name ]

      | ERecordUpdate(id, record, updates) ->
        let updates =
          NEList.toList updates
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecordUpdate",
        [ DInt(int64 id); toDT record; DvalUtils.list valueTypeTODO updates ]

      | EAnd(id, left, right) -> "EAnd", [ DInt(int64 id); toDT left; toDT right ]
      | EOr(id, left, right) -> "EOr", [ DInt(int64 id); toDT left; toDT right ]
      // Let the error straight through
      | EError(id, rtError, exprs) ->
        "EError",
        [ DInt(int64 id)
          RuntimeTypes.RuntimeError.toDT rtError
          List.map toDT exprs |> DvalUtils.list valueTypeTODO ]


    DvalUtils.enum (rtTyp [] "Expr" 0) name fields

  let rec fromDT (d : Dval) : Expr =
    match d with
    | DEnum(_, _, "EUnit", [ DInt id ]) -> EUnit(uint64 id)

    // simple data
    | DEnum(_, _, "EBool", [ DInt id; DBool b ]) -> EBool(uint64 id, b)
    | DEnum(_, _, "EInt", [ DInt id; DInt i ]) -> EInt(uint64 id, i)
    | DEnum(_, _, "EFloat", [ DInt id; DFloat f ]) -> EFloat(uint64 id, f)
    | DEnum(_, _, "EChar", [ DInt id; DString c ]) -> EChar(uint64 id, c)
    | DEnum(_, _, "EString", [ DInt id; DList(_vtTODO, segments) ]) ->
      EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    // structures of data
    | DEnum(_, _, "EList", [ DInt id; DList(_vtTODO, inner) ]) ->
      EList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, "EDict", [ DInt id; DList(_vtTODO, pairsList) ]) ->
      let pairs =
        pairsList
        |> List.collect (fun pair ->
          match pair with
          | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
          | _ -> [])
      EDict(uint64 id, pairs)


    | DEnum(_, _, "ETuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "ERecord", [ DInt id; typeName; DList(_vtTODO1, fieldsList) ]) ->
      let fields =
        fieldsList
        |> List.collect (fun field ->
          match field with
          | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
          | _ -> [])
      ERecord(
        uint64 id,
        TypeName.fromDT typeName,
        NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one field in ERecord"
          []
          fields
      )


    | DEnum(_,
            _,
            "EEnum",
            [ DInt id; typeName; DString caseName; DList(_vtTODO, fields) ]) ->
      EEnum(uint64 id, TypeName.fromDT typeName, caseName, List.map fromDT fields)

    // declaring and accessing variables
    | DEnum(_, _, "ELet", [ DInt id; lp; expr; body ]) ->
      ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, "EFieldAccess", [ DInt id; expr; DString fieldName ]) ->
      EFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, "EVariable", [ DInt id; DString varName ]) ->
      EVariable(uint64 id, varName)

    // control flow
    | DEnum(_, _, "EIf", [ DInt id; cond; thenExpr; elseExpr ]) ->
      let elseExpr =
        match elseExpr with
        | DEnum(_, _, "Some", [ dv ]) -> Some(fromDT dv)
        | DEnum(_, _, "None", []) -> None
        | _ ->
          Exception.raiseInternal "Invalid else expression" [ "elseExpr", elseExpr ]
      EIf(uint64 id, fromDT cond, fromDT thenExpr, elseExpr)

    | DEnum(_, _, "EMatch", [ DInt id; arg; DList(_vtTODO, cases) ]) ->
      let cases =
        cases
        |> List.collect (fun case ->
          match case with
          | DTuple(pattern, expr, _) ->
            [ (MatchPattern.fromDT pattern, fromDT expr) ]
          | _ -> [])
      EMatch(
        uint64 id,
        fromDT arg,
        NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one case in EMatch"
          []
          cases
      )


    | DEnum(_, _, "ELambda", [ DInt id; DList(_vtTODO, variables); body ]) ->
      let args =
        variables
        |> List.collect (fun arg ->
          match arg with
          | DTuple(DInt argId, DString varName, _) -> [ (uint64 argId, varName) ]
          | _ -> [])

      ELambda(
        uint64 id,
        NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one bound variable in ELambda"
          []
          args,
        fromDT body
      )


    | DEnum(_,
            _,
            "EApply",
            [ DInt id; name; DList(_vtTODO1, typeArgs); DList(_vtTODO2, args) ]) ->
      let args =
        NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one argument in EApply"
          []
          args

      EApply(
        uint64 id,
        fromDT name,
        List.map TypeReference.fromDT typeArgs,
        NEList.map fromDT args
      )

    | DEnum(_, _, "EFnName", [ DInt id; name ]) ->
      EFnName(uint64 id, FnName.fromDT name)

    | DEnum(_, _, "ERecordUpdate", [ DInt id; record; DList(_vtTODO, updates) ]) ->
      let updates =
        updates
        |> List.collect (fun update ->
          match update with
          | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
          | _ -> [])
      ERecordUpdate(
        uint64 id,
        fromDT record,
        NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one field update in ERecordUpdate"
          []
          updates
      )

    // now for EAnd, EOr and EError
    | DEnum(_, _, "EAnd", [ DInt id; left; right ]) ->
      EAnd(uint64 id, fromDT left, fromDT right)

    | DEnum(_, _, "EOr", [ DInt id; left; right ]) ->
      EOr(uint64 id, fromDT left, fromDT right)

    | DEnum(_, _, "EError", [ DInt id; rtError; DList(_vtTODO, exprs) ]) ->
      EError(uint64 id, RuntimeError.fromDT rtError, List.map fromDT exprs)


    | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ]


module RuntimeError =
  let toDT (e : RuntimeError) : Dval =
    e |> RuntimeTypes.RuntimeError.toDT |> Dval.toDT

  let fromDT (d : Dval) : RuntimeError =
    d |> Dval.fromDT |> RuntimeTypes.RuntimeError.fromDT


module Dval =
  module KnownType =
    let toDT (kt : KnownType) : Dval =
      let caseName, fields =
        match kt with
        | KTUnit -> "KTUnit", []
        | KTBool -> "KTBool", []
        | KTInt -> "KTInt", []
        | KTFloat -> "KTFloat", []
        | KTChar -> "KTChar", []
        | KTString -> "KTString", []
        | KTUuid -> "KTUuid", []
        | KTDateTime -> "KTDateTime", []
        | KTBytes -> "KTBytes", []

        | KTList inner -> "KTList", [ ValueType.toDT inner ]
        | KTTuple(first, second, theRest) ->
          "KTTuple",
          [ ValueType.toDT first
            ValueType.toDT second
            DvalUtils.list valueTypeTODO (List.map ValueType.toDT theRest) ]
        | KTDict inner -> "KTDict", [ ValueType.toDT inner ]

        | KTCustomType(typeName, typeArgs) ->
          "KTCustomType",
          [ TypeName.toDT typeName
            DvalUtils.list valueTypeTODO (List.map ValueType.toDT typeArgs) ]

        | KTFn(args, ret) ->
          "KTFn",
          [ DvalUtils.list
              valueTypeTODO
              (List.map ValueType.toDT (NEList.toList args))
            ValueType.toDT ret ]

        | KTDB d -> "KTDB", [ ValueType.toDT d ]
        | KTPassword -> "KTPassword", []

      let typeName = rtTyp [] "KnownType" 0
      DEnum(typeName, typeName, caseName, fields)

    let fromDT (d : Dval) : KnownType =
      match d with
      | DEnum(_, _, "KTUnit", []) -> KTUnit
      | DEnum(_, _, "KTBool", []) -> KTBool
      | DEnum(_, _, "KTInt", []) -> KTInt
      | DEnum(_, _, "KTFloat", []) -> KTFloat
      | DEnum(_, _, "KTChar", []) -> KTChar
      | DEnum(_, _, "KTString", []) -> KTString
      | DEnum(_, _, "KTUuid", []) -> KTUuid
      | DEnum(_, _, "KTDateTime", []) -> KTDateTime
      | DEnum(_, _, "KTBytes", []) -> KTBytes

      | DEnum(_, _, "KTList", [ inner ]) -> KTList(ValueType.fromDT inner)
      | DEnum(_, _, "KTTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
        KTTuple(
          ValueType.fromDT first,
          ValueType.fromDT second,
          List.map ValueType.fromDT theRest
        )
      | DEnum(_, _, "KTDict", [ inner ]) -> KTDict(ValueType.fromDT inner)

      | DEnum(_, _, "KTCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
        KTCustomType(TypeName.fromDT typeName, List.map ValueType.fromDT typeArgs)

      | DEnum(_, _, "KTFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
        KTFn(
          NEList.ofList
            (ValueType.fromDT firstArg)
            (List.map ValueType.fromDT otherArgs),
          ValueType.fromDT ret
        )
      | DEnum(_, _, "KTDB", [ inner ]) -> KTDB(ValueType.fromDT inner)
      | DEnum(_, _, "KTPassword", []) -> KTPassword

      | _ -> Exception.raiseInternal "Invalid KnownType" []

  module ValueType =
    let toDT (vt : ValueType) : Dval =
      let typeName = rtTyp [] "ValueType" 0
      match vt with
      | ValueType.Unknown -> DEnum(typeName, typeName, "Unknown", [])
      | ValueType.Known kt ->
        DEnum(typeName, typeName, "Known", [ KnownType.toDT kt ])

    let fromDT (d : Dval) : ValueType =
      match d with
      | DEnum(_, _, "Unknown", []) -> ValueType.Unknown
      | DEnum(_, _, "Known", [ kt ]) -> ValueType.Known(KnownType.fromDT kt)

      | _ -> Exception.raiseInternal "Invalid ValueType" []

  module DvalSource =
    let toDT (s : DvalSource) : Dval =
      let name, fields =
        match s with
        | SourceNone -> "SourceNone", []
        | SourceID(tlid, id) -> "SourceID", [ DInt(int64 tlid); DInt(int64 id) ]

      let typeName = rtTyp [] "DvalSource" 0
      DEnum(typeName, typeName, name, fields)

    let fromDT (d : Dval) : DvalSource =
      match d with
      | DEnum(_, _, "SourceNone", []) -> SourceNone
      | DEnum(_, _, "SourceID", [ DInt tlid; DInt id ]) ->
        SourceID(uint64 tlid, uint64 id)
      | _ -> Exception.raiseInternal "Invalid DvalSource" []


  module LambdaImpl =
    let toDT (l : LambdaImpl) : Dval =
      let typeName = rtTyp [] "LambdaImpl" 0

      let fields =
        [ "typeSymbolTable", DDict(Map.map TypeReference.toDT l.typeSymbolTable)
          "symtable", DDict(Map.map Dval.toDT l.symtable)
          "parameters",
          DvalUtils.list
            valueTypeTODO
            (List.map
              (fun (id, name) -> DTuple(DInt(int64 id), DString name, []))
              (NEList.toList l.parameters))
          "body", Expr.toDT l.body ]
        |> Map.ofList

      DRecord(typeName, typeName, valueTypesTODO, fields)

    let fromDT (d : Dval) : LambdaImpl =
      match d with
      | DRecord(_, _, _, fields) ->
        let typeSymbolTable =
          fields |> D.mapField "typeSymbolTable" |> Map.map TypeReference.fromDT

        let symtable = fields |> D.mapField "symtable" |> Map.map Dval.fromDT

        let parameters =
          fields
          |> D.listField "parameters"
          |> List.map (fun d ->
            match d with
            | DTuple(DInt id, DString name, _) -> (uint64 id, name)
            | _ -> Exception.raiseInternal "Invalid LambdaImpl" [])
          |> NEList.ofListUnsafe
            "RT2DT.Dval.fromDT expected at least one parameter in LambdaImpl"
            []
        let body = fields |> D.field "body" |> Expr.fromDT

        { typeSymbolTable = typeSymbolTable
          symtable = symtable
          parameters = parameters
          body = body }

      | _ -> Exception.raiseInternal "Invalid LambdaImpl" []

  module FnValImpl =
    let toDT (fnValImpl : FnValImpl) : Dval =
      let name, fields =
        match fnValImpl with
        | Lambda lambda -> "Lambda", [ LambdaImpl.toDT lambda ]
        | NamedFn fnName -> "NamedFn", [ FnName.toDT fnName ]

      let typeName = rtTyp [] "FnValImpl" 0

      DEnum(typeName, typeName, name, fields)

    let fromDT (d : Dval) : FnValImpl =
      match d with
      | DEnum(_, _, "Lambda", [ lambda ]) -> Lambda(LambdaImpl.fromDT lambda)
      | DEnum(_, _, "NamedFn", [ fnName ]) -> NamedFn(FnName.fromDT fnName)
      | _ -> Exception.raiseInternal "Invalid FnValImpl" []

  let rec toDT (dv : Dval) : Dval =
    let name, fields =
      match dv with
      | DUnit -> "DUnit", []
      | DBool b -> "DBool", [ DBool b ]
      | DInt i -> "DInt", [ DInt i ]
      | DFloat f -> "DFloat", [ DFloat f ]
      | DChar c -> "DChar", [ DChar c ]
      | DString s -> "DString", [ DString s ]
      | DUuid u -> "DUuid", [ DUuid u ]
      | DDateTime d -> "DDateTime", [ DDateTime d ]
      | DBytes b -> "DBytes", [ DBytes b ]


      | DList(vt, l) ->
        "DList",
        [ ValueType.toDT vt; DvalUtils.list valueTypeTODO (List.map toDT l) ]
      | DTuple(first, second, theRest) ->
        "DTuple",
        [ toDT first
          toDT second
          DvalUtils.list valueTypeTODO (List.map toDT theRest) ]

      | DFnVal fnImpl -> "DFnVal", [ FnValImpl.toDT fnImpl ]

      | DError(source, err) ->
        "DError", [ DvalSource.toDT source; RuntimeError.toDT err ]

      | DDB name -> "DDB", [ DString name ]

      | DPassword p -> "DPassword", [ DPassword p ]

      | DDict map -> "DDict", [ DDict(Map.map toDT map) ]

      | DRecord(runtimeTypeName, sourceTypeName, typeArgs, map) ->
        "DRecord",
        [ TypeName.toDT runtimeTypeName
          TypeName.toDT sourceTypeName
          typeArgs |> List.map ValueType.toDT |> DvalUtils.list valueTypeTODO
          DDict(Map.map toDT map) ]

      | DEnum(runtimeTypeName, sourceTypeName, caseName, fields) ->
        "DEnum",
        [ TypeName.toDT runtimeTypeName
          TypeName.toDT sourceTypeName
          DString caseName
          DvalUtils.list valueTypeTODO (List.map toDT fields) ]

    DvalUtils.enum (rtTyp [ "Dval" ] "Dval" 0) name fields


  let fromDT (d : Dval) : Dval =
    match d with
    | DEnum(_, _, "DInt", [ DInt i ]) -> DInt i
    | DEnum(_, _, "DFloat", [ DFloat f ]) -> DFloat f
    | DEnum(_, _, "DBool", [ DBool b ]) -> DBool b
    | DEnum(_, _, "DUnit", []) -> DUnit
    | DEnum(_, _, "DString", [ DString s ]) -> DString s
    | DEnum(_, _, "DChar", [ DChar c ]) -> DChar c

    | DEnum(_, _, "DList", [ vt; DList(_vtTODO, l) ]) ->
      DList(ValueType.fromDT vt, List.map fromDT l)
    | DEnum(_, _, "DTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      DTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "DFnVal", [ fnImpl ]) -> DFnVal(FnValImpl.fromDT fnImpl)

    | DEnum(_, _, "DError", [ source; err ]) ->
      DError(DvalSource.fromDT source, RuntimeError.fromDT err)

    | DEnum(_, _, "DDB", [ DString name ]) -> DDB name

    | DEnum(_, _, "DDateTime", [ DDateTime d ]) -> DDateTime d
    | DEnum(_, _, "DPassword", [ DPassword p ]) -> DPassword p
    | DEnum(_, _, "DUuid", [ DUuid u ]) -> DUuid u
    | DEnum(_, _, "DBytes", [ DBytes b ]) -> DBytes b

    | DEnum(_, _, "DDict", [ DDict map ]) -> DDict(Map.map fromDT map)
    | DEnum(_,
            _,
            "DRecord",
            [ runtimeTypeName; sourceTypeName; DList(_, typeArgs); DDict map ]) ->
      DRecord(
        TypeName.fromDT runtimeTypeName,
        TypeName.fromDT sourceTypeName,
        List.map ValueType.fromDT typeArgs,
        Map.map fromDT map
      )
    | DEnum(_,
            _,
            "DEnum",
            [ runtimeTypeName
              sourceTypeName
              DString caseName
              DList(_vtTODO, fields) ]) ->
      DEnum(
        TypeName.fromDT runtimeTypeName,
        TypeName.fromDT sourceTypeName,
        caseName,
        List.map fromDT fields
      )
    | _ -> Exception.raiseInternal "Invalid Dval" []
