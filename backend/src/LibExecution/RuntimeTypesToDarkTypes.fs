module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude
open Tablecloth

open RuntimeTypes

module D = DvalDecoder

let languageToolsTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage "Darklang" ("LanguageTools" :: submodules) name version


// Note: doesn't actually exist at this point
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
      Dval.record
        (rtTyp [ "FQName" ] "BuiltIn" 0)
        [ "modules", DList(List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.BuiltIn<'name> =
      match d with
      | DRecord(_, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module UserProgram =
    let toDT (nameMapper : 'name -> Dval) (u : FQName.UserProgram<'name>) : Dval =
      Dval.record
        (rtTyp [ "FQName" ] "UserProgram" 0)
        [ "modules", DList(List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (v : Dval) : FQName.UserProgram<'name> =
      let unwrap = Exception.unwrapOptionInternal
      match v with
      | DRecord(_, _, m) ->

        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module Package =
    let toDT (nameMapper : 'name -> Dval) (u : FQName.Package<'name>) : Dval =
      Dval.record
        (rtTyp [ "FQName" ] "Package" 0)
        [ "owner", DString u.owner
          "modules", DList(List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.Package<'name> =
      let unwrap = Exception.unwrapOptionInternal
      match d with
      | DRecord(_, _, m) ->
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

    Dval.enum (rtTyp [ "FQName" ] "FQName" 0) caseName fields

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

      Dval.enum (rtTyp [ "TypeName" ] "Name" 0) caseName fields

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

      Dval.enum (rtTyp [ "FnName" ] "Name" 0) caseName fields

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

      Dval.enum (rtTyp [ "ConstantName" ] "Name" 0) caseName fields

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
    | Ok name -> Dval.resultOk (f name)
    | Error err -> Dval.resultError (RuntimeTypes.RuntimeError.toDT err)

  let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
    match d with
    | DEnum(tn, _, "Ok", [ v ]) when tn = Dval.resultType -> Ok(f v)
    | DEnum(tn, _, "Error", [ v ]) when tn = Dval.resultType ->
      Error(RuntimeTypes.RuntimeError.fromDT v)
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
        "TTuple", [ toDT first; toDT second; DList(List.map toDT theRest) ]

      | TDict inner -> "TDict", [ toDT inner ]

      | TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT TypeName.toDT typeName; DList(List.map toDT typeArgs) ]

      | TDB inner -> "TDB", [ toDT inner ]
      | TFn(args, ret) ->
        "TFn", [ DList(List.map toDT (NEList.toList args)); toDT ret ]

    Dval.enum (rtTyp [] "TypeReference" 0) name fields

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

    | DEnum(_, _, "TTuple", [ first; second; DList theRest ]) ->
      TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "TDict", [ inner ]) -> TDict(fromDT inner)

    | DEnum(_, _, "TCustomType", [ typeName; DList typeArgs ]) ->
      TCustomType(
        NameResolution.fromDT TypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, "TDB", [ inner ]) -> TDB(fromDT inner)
    | DEnum(_, _, "TFn", [ DList(firstArg :: otherArgs); ret ]) ->
      TFn(NEList.ofList (fromDT firstArg) (List.map fromDT otherArgs), fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" [ "typeRef", d ]

module Param =
  let toDT (p : Param) : Dval =
    Dval.record
      (rtTyp [] "BuiltInParam" 0)
      [ ("name", DString p.name)
        ("typ", TypeReference.toDT p.typ)
        // RTETODO
        ("description", DString "") ]


module LetPattern =
  let rec toDT (p : LetPattern) : Dval =
    let name, fields =
      match p with
      | LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | LPUnit id -> "LPUnit", [ DInt(int64 id) ]
      | LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]

    Dval.enum (rtTyp [] "LetPattern" 0) name fields

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, "LPVariable", [ DInt id; DString name ]) ->
      LPVariable(uint64 id, name)
    | DEnum(_, _, "LPUnit", [ DInt id ]) -> LPUnit(uint64 id)
    | DEnum(_, _, "LPTuple", [ DInt id; first; second; DList theRest ]) ->
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

      | MPList(id, inner) -> "MPList", [ DInt(int64 id); DList(List.map toDT inner) ]
      | MPListCons(id, head, tail) ->
        "MPListCons", [ DInt(int64 id); toDT head; toDT tail ]
      | MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]
      | MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt(int64 id); DString caseName; DList(List.map toDT fieldPats) ]

    Dval.enum (rtTyp [] "MatchPattern" 0) name fields

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

    | DEnum(_, _, "MPList", [ DInt id; DList inner ]) ->
      MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, "MPListCons", [ DInt id; head; tail ]) ->
      MPListCons(uint64 id, fromDT head, fromDT tail)
    | DEnum(_, _, "MPTuple", [ DInt id; first; second; DList theRest ]) ->
      MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | DEnum(_, _, "MPEnum", [ DInt id; DString caseName; DList fieldPats ]) ->
      MPEnum(uint64 id, caseName, List.map fromDT fieldPats)
    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module StringSegment =
  let toDT (exprToDT : Expr -> Dval) (s : StringSegment) : Dval =
    let name, fields =
      match s with
      | StringText text -> "StringText", [ DString text ]
      | StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]

    Dval.enum (rtTyp [] "StringSegment" 0) name fields

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
        [ DInt(int64 id); DList(List.map (StringSegment.toDT toDT) segments) ]

      // structures of data
      | EList(id, inner) -> "EList", [ DInt(int64 id); DList(List.map toDT inner) ]

      | EDict(id, pairs) ->
        "EDict",
        [ DInt(int64 id)
          DList(List.map (fun (k, v) -> DTuple(DString k, toDT v, [])) pairs) ]

      | ETuple(id, first, second, theRest) ->
        "ETuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]

      | ERecord(id, name, fields) ->
        let fields =
          (NEList.toList fields)
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecord", [ DInt(int64 id); TypeName.toDT name; DList(fields) ]

      | EEnum(id, typeName, caseName, fields) ->
        "EEnum",
        [ DInt(int64 id)
          TypeName.toDT typeName
          DString caseName
          DList(List.map toDT fields) ]

      // declaring and accessing variables
      | ELet(id, lp, expr, body) ->
        "ELet", [ DInt(int64 id); LetPattern.toDT lp; toDT expr; toDT body ]

      | EFieldAccess(id, expr, fieldName) ->
        "EFieldAccess", [ DInt(int64 id); toDT expr; DString fieldName ]

      | EVariable(id, varName) -> "EVariable", [ DInt(int64 id); DString varName ]


      // control flow
      | EIf(id, cond, ifTrue, ifFalse) ->
        "EIf", [ DInt(int64 id); toDT cond; toDT ifTrue; toDT ifFalse ]

      | EMatch(id, arg, cases) ->
        let cases =
          (NEList.toList cases)
          |> List.map (fun (pattern, expr) ->
            DTuple(MatchPattern.toDT pattern, toDT expr, []))

        "EMatch", [ DInt(int64 id); toDT arg; DList(cases) ]



      | ELambda(id, args, body) ->
        let variables =
          (NEList.toList args)
          |> List.map (fun (id, varName) ->
            DTuple(DInt(int64 id), DString varName, []))
          |> DList

        "ELambda", [ DInt(int64 id); variables; toDT body ]

      | EConstant(id, name) ->
        "EConstant", [ DInt(int64 id); ConstantName.toDT name ]

      | EApply(id, name, typeArgs, args) ->
        "EApply",
        [ DInt(int64 id)
          toDT name
          DList(List.map TypeReference.toDT typeArgs)
          DList(List.map toDT (NEList.toList args)) ]

      | EFnName(id, name) -> "EFnName", [ DInt(int64 id); FnName.toDT name ]

      | ERecordUpdate(id, record, updates) ->
        let updates =
          NEList.toList updates
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecordUpdate", [ DInt(int64 id); toDT record; DList(updates) ]

      | EAnd(id, left, right) -> "EAnd", [ DInt(int64 id); toDT left; toDT right ]
      | EOr(id, left, right) -> "EOr", [ DInt(int64 id); toDT left; toDT right ]
      // Let the error straight through
      | EError(id, rtError, exprs) ->
        "EError",
        [ DInt(int64 id)
          RuntimeTypes.RuntimeError.toDT rtError
          List.map toDT exprs |> DList ]


    Dval.enum (rtTyp [] "Expr" 0) name fields

  let rec fromDT (d : Dval) : Expr =
    match d with
    | DEnum(_, _, "EUnit", [ DInt id ]) -> EUnit(uint64 id)

    // simple data
    | DEnum(_, _, "EBool", [ DInt id; DBool b ]) -> EBool(uint64 id, b)
    | DEnum(_, _, "EInt", [ DInt id; DInt i ]) -> EInt(uint64 id, i)
    | DEnum(_, _, "EFloat", [ DInt id; DFloat f ]) -> EFloat(uint64 id, f)
    | DEnum(_, _, "EChar", [ DInt id; DString c ]) -> EChar(uint64 id, c)
    | DEnum(_, _, "EString", [ DInt id; DList segments ]) ->
      EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    // structures of data
    | DEnum(_, _, "EList", [ DInt id; DList inner ]) ->
      EList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, "EDict", [ DInt id; DList pairsList ]) ->
      let pairs =
        pairsList
        |> List.collect (fun pair ->
          match pair with
          | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
          | _ -> [])
      EDict(uint64 id, pairs)


    | DEnum(_, _, "ETuple", [ DInt id; first; second; DList theRest ]) ->
      ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "ERecord", [ DInt id; typeName; DList fieldsList ]) ->
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


    | DEnum(_, _, "EEnum", [ DInt id; typeName; DString caseName; DList fields ]) ->
      EEnum(uint64 id, TypeName.fromDT typeName, caseName, List.map fromDT fields)

    // declaring and accessing variables
    | DEnum(_, _, "ELet", [ DInt id; lp; expr; body ]) ->
      ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, "EFieldAccess", [ DInt id; expr; DString fieldName ]) ->
      EFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, "EVariable", [ DInt id; DString varName ]) ->
      EVariable(uint64 id, varName)

    // control flow
    | DEnum(_, _, "EIf", [ DInt id; cond; ifTrue; ifFalse ]) ->
      EIf(uint64 id, fromDT cond, fromDT ifTrue, fromDT ifFalse)

    | DEnum(_, _, "EMatch", [ DInt id; arg; DList cases ]) ->
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


    | DEnum(_, _, "ELambda", [ DInt id; DList variables; body ]) ->
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


    | DEnum(_, _, "EApply", [ DInt id; name; DList typeArgs; DList args ]) ->
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

    | DEnum(_, _, "ERecordUpdate", [ DInt id; record; DList updates ]) ->
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

    | DEnum(_, _, "EError", [ DInt id; rtError; DList exprs ]) ->
      EError(uint64 id, RuntimeError.fromDT rtError, List.map fromDT exprs)


    | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ]


module RuntimeError =
  let toDT (e : RuntimeError) : Dval =
    let asDval = RuntimeError.toDT e
    Dval.toDT asDval


module Dval =
  module DvalSource =
    let toDT (s : DvalSource) : Dval =
      let name, fields =
        match s with
        | SourceNone -> "SourceNone", []
        | SourceID(tlid, id) -> "SourceID", [ DInt(int64 tlid); DInt(int64 id) ]

      let typeName = rtTyp [] "DvalSource" 0
      DEnum(typeName, typeName, name, fields)

  module LambdaImpl =
    let toDT (l : LambdaImpl) : Dval =
      let typeName = rtTyp [] "LambdaImpl" 0

      DRecord(
        typeName,
        typeName,
        Map.ofList
          [ "typeArgTable", DDict(Map.map TypeReference.toDT l.typeArgTable)
            "symtable", DDict(Map.map Dval.toDT l.symtable)
            "parameters",
            DList(
              List.map
                (fun (id, name) -> DTuple(DInt(int64 id), DString name, []))
                (NEList.toList l.parameters)
            )
            "body", Expr.toDT l.body ]
      )


  module FnValImpl =
    let toDT (fnValImpl : FnValImpl) : Dval =
      let name, fields =
        match fnValImpl with
        | Lambda lambda -> "Lambda", [ LambdaImpl.toDT lambda ]
        | NamedFn fnName -> "NamedFn", [ FnName.toDT fnName ]

      let typeName = rtTyp [] "FnValImpl" 0

      DEnum(typeName, typeName, name, fields)

  let rec toDT (dv : Dval) : Dval =
    let name, fields =
      match dv with
      | DInt i -> "DInt", [ DInt i ]
      | DFloat f -> "DFloat", [ DFloat f ]
      | DBool b -> "DBool", [ DBool b ]
      | DUnit -> "DUnit", []
      | DString s -> "DString", [ DString s ]
      | DChar c -> "DChar", [ DChar c ]

      | DList l -> "DList", [ DList(List.map toDT l) ]
      | DTuple(first, second, theRest) ->
        "DTuple", [ toDT first; toDT second; DList(List.map toDT theRest) ]

      | DFnVal fnImpl -> "DFnVal", [ FnValImpl.toDT fnImpl ]

      | DError(source, err) ->
        "DError", [ DvalSource.toDT source; RuntimeError.toDT err ]

      | DIncomplete source -> "DIncomplete", [ DvalSource.toDT source ]

      | DDB name -> "DDB", [ DString name ]

      | DDateTime d -> "DDateTime", [ DDateTime d ]
      | DPassword p -> "DPassword", [ DPassword p ]
      | DUuid u -> "DUuid", [ DUuid u ]
      | DBytes b -> "DBytes", [ DBytes b ]

      | DDict map -> "DDict", [ DDict(Map.map toDT map) ]
      | DRecord(runtimeTypeName, sourceTypeName, map) ->
        "DRecord",
        [ TypeName.toDT runtimeTypeName
          TypeName.toDT sourceTypeName
          DDict(Map.map toDT map) ]
      | DEnum(runtimeTypeName, sourceTypeName, caseName, fields) ->
        "DEnum",
        [ TypeName.toDT runtimeTypeName
          TypeName.toDT sourceTypeName
          DString caseName
          DList(List.map toDT fields) ]

    Dval.enum (rtTyp [] "Dval" 0) name fields
