module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude

open RuntimeTypes

module VT = ValueType
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
    let toDT
      (nameType : KnownType)
      (nameMapper : 'name -> Dval)
      (u : FQName.BuiltIn<'name>)
      : Dval =
      let fields =
        [ "modules", DList(VT.string, List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]
      let typeName = rtTyp [ "FQName" ] "BuiltIn" 0
      DRecord(typeName, typeName, [ VT.known nameType ], Map fields)

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.BuiltIn<'name> =
      match d with
      | DRecord(_, _, _, fields) ->
        { modules = modulesField fields
          name = nameField fields |> nameMapper
          version = versionField fields }
      | _ -> Exception.raiseInternal "Unexpected value" []

  module UserProgram =
    let toDT
      (nameType : KnownType)
      (nameMapper : 'name -> Dval)
      (u : FQName.UserProgram<'name>)
      : Dval =
      let fields =
        [ "modules", DList(VT.string, List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]
      let typeName = rtTyp [ "FQName" ] "UserProgram" 0
      DRecord(typeName, typeName, [ VT.known nameType ], Map fields)

    let fromDT (nameMapper : Dval -> 'name) (v : Dval) : FQName.UserProgram<'name> =
      match v with
      | DRecord(_, _, _, fields) ->
        { modules = modulesField fields
          name = nameField fields |> nameMapper
          version = versionField fields }
      | _ -> Exception.raiseInternal "Unexpected value" []

  module Package =
    let toDT
      (nameType : KnownType)
      (nameMapper : 'name -> Dval)
      (u : FQName.Package<'name>)
      : Dval =
      let fields =
        [ "owner", DString u.owner
          "modules", DList(VT.string, List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]
      let typeName = rtTyp [ "FQName" ] "Package" 0
      DRecord(typeName, typeName, [ VT.known nameType ], Map fields)

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.Package<'name> =
      match d with
      | DRecord(_, _, _, fields) ->
        { owner = ownerField fields
          modules =
            modulesField fields
            |> NEList.ofListUnsafe "Expected modules to be a non-empty list" []
            |> NEList.toList
          name = nameField fields |> nameMapper
          version = versionField fields }
      | _ -> Exception.raiseInternal "Unexpected value" []


  let toDT
    (nameType : KnownType)
    (nameMapper : 'name -> Dval)
    (u : FQName.FQName<'name>)
    : Dval =
    let (caseName, fields) =
      match u with
      | FQName.UserProgram u ->
        "UserProgram", [ UserProgram.toDT nameType nameMapper u ]
      | FQName.Package u -> "Package", [ Package.toDT nameType nameMapper u ]
      | FQName.BuiltIn u -> "BuiltIn", [ BuiltIn.toDT nameType nameMapper u ]
    let typeName = rtTyp [ "FQName" ] "FQName" 0
    DEnum(
      typeName,
      typeName,
      Dval.ignoreAndUseEmpty [ VT.known nameType ],
      caseName,
      fields
    )

  let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.FQName<'name> =
    match d with
    | DEnum(_, _, _typeArgsDEnumTODO, "UserProgram", [ u ]) ->
      FQName.UserProgram(UserProgram.fromDT nameMapper u)
    | DEnum(_, _, _typeArgsDEnumTODO, "Package", [ u ]) ->
      FQName.Package(Package.fromDT nameMapper u)
    | DEnum(_, _, _typeArgsDEnumTODO, "BuiltIn", [ u ]) ->
      FQName.BuiltIn(BuiltIn.fromDT nameMapper u)
    | _ -> Exception.raiseInternal "Invalid FQName" []


module TypeName =
  let knownType = KTCustomType(rtTyp [ "TypeName" ] "TypeName" 0, [])

  module Name =
    let knownType = KTCustomType(rtTyp [ "TypeName" ] "Name" 0, [])

    let toDT (u : TypeName.Name) : Dval =
      let (caseName, fields) =
        match u with
        | TypeName.TypeName name -> "TypeName", [ DString name ]
      let typeName = rtTyp [ "TypeName" ] "Name" 0
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : TypeName.Name =
      match d with
      | DEnum(_, _, [], "TypeName", [ DString name ]) -> TypeName.TypeName(name)
      | _ -> Exception.raiseInternal "Invalid TypeName" []

  module BuiltIn =
    let toDT (u : TypeName.BuiltIn) : Dval =
      FQName.BuiltIn.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : TypeName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : TypeName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.knownType Name.toDT u

    let fromDT (d : Dval) : TypeName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : TypeName.Package) : Dval =
      FQName.Package.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : TypeName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : TypeName.TypeName) : Dval = FQName.toDT Name.knownType Name.toDT u
  let fromDT (d : Dval) : TypeName.TypeName = FQName.fromDT Name.fromDT d


module FnName =
  module Name =
    let knownType = KTCustomType(rtTyp [ "FnName" ] "Name" 0, [])

    let toDT (u : FnName.Name) : Dval =
      let (caseName, fields) =
        match u with
        | FnName.FnName name -> "FnName", [ DString name ]
      let typeName = rtTyp [ "FnName" ] "Name" 0
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : FnName.Name =
      match d with
      | DEnum(_, _, [], "FnName", [ DString name ]) -> FnName.FnName(name)
      | _ -> Exception.raiseInternal "Invalid FnName" []

  module BuiltIn =
    let toDT (u : FnName.BuiltIn) : Dval =
      FQName.BuiltIn.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : FnName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : FnName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : FnName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : FnName.Package) : Dval =
      FQName.Package.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : FnName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : FnName.FnName) : Dval = FQName.toDT Name.knownType Name.toDT u
  let fromDT (d : Dval) : FnName.FnName = FQName.fromDT Name.fromDT d


module ConstantName =
  module Name =
    let knownType = KTCustomType(rtTyp [ "ConstantName" ] "Name" 0, [])

    let toDT (u : ConstantName.Name) : Dval =
      let (caseName, fields) =
        match u with
        | ConstantName.ConstantName name -> "ConstantName", [ DString name ]
      let typeName = rtTyp [ "ConstantName" ] "Name" 0
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : ConstantName.Name =
      match d with
      | DEnum(_, _, [], "ConstantName", [ DString name ]) ->
        ConstantName.ConstantName(name)
      | _ -> Exception.raiseInternal "Invalid ConstantName" []

  module BuiltIn =
    let toDT (u : ConstantName.BuiltIn) : Dval =
      FQName.BuiltIn.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : ConstantName.BuiltIn =
      FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : ConstantName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.knownType Name.toDT u

    let fromDT (d : Dval) : ConstantName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : ConstantName.Package) : Dval =
      FQName.Package.toDT Name.knownType Name.toDT u
    let fromDT (d : Dval) : ConstantName.Package =
      FQName.Package.fromDT Name.fromDT d

  let toDT (u : ConstantName.ConstantName) : Dval =
    FQName.toDT Name.knownType Name.toDT u
  let fromDT (d : Dval) : ConstantName.ConstantName = FQName.fromDT Name.fromDT d


module NameResolution =
  let toDT
    (nameValueType : KnownType)
    (f : 'p -> Dval)
    (result : NameResolution<'p>)
    : Dval =
    let errType =
      KTCustomType(
        TypeName.fqPackage
          "Darklang"
          [ "LanguageTools"; "RuntimeErrors"; "NameResolution" ]
          "Error"
          0,
        []
      )

    match result with
    | Ok name -> Dval.resultOk nameValueType errType (f name)
    | Error err -> RuntimeError.toDT err |> Dval.resultError nameValueType errType

  let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
    match d with
    | DEnum(tn, _, _typeArgsDEnumTODO, "Ok", [ v ]) when tn = Dval.resultType ->
      Ok(f v)
    | DEnum(tn, _, _typeArgsDEnumTODO, "Error", [ v ]) when tn = Dval.resultType ->
      Error(RuntimeError.fromDT v)
    | _ -> Exception.raiseInternal "Invalid NameResolution" []

module TypeReference =
  let knownType =
    KTCustomType(
      TypeName.fqPackage
        "Darklang"
        [ "LanguageTools"; "RuntimeTypes" ]
        "TypeReference"
        0,
      []
    )

  let rec toDT (t : TypeReference) : Dval =
    let (caseName, fields) =
      match t with
      | TVariable name -> "TVariable", [ DString name ]

      | TUnit -> "TUnit", []
      | TBool -> "TBool", []
      | TInt -> "TInt", []
      | TInt8 -> "TInt8", []
      | TUInt8 -> "TUInt8", []
      | TFloat -> "TFloat", []
      | TChar -> "TChar", []
      | TString -> "TString", []
      | TDateTime -> "TDateTime", []
      | TUuid -> "TUuid", []
      | TBytes -> "TBytes", []

      | TList inner -> "TList", [ toDT inner ]

      | TTuple(first, second, theRest) ->
        "TTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | TDict inner -> "TDict", [ toDT inner ]

      | TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT TypeName.knownType TypeName.toDT typeName
          DList(VT.known knownType, List.map toDT typeArgs) ]

      | TDB inner -> "TDB", [ toDT inner ]
      | TFn(args, ret) ->
        let args = args |> NEList.toList |> List.map toDT |> Dval.list knownType
        "TFn", [ args; toDT ret ]

    let typeName = rtTyp [] "TypeReference" 0
    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : TypeReference =
    match d with
    | DEnum(_, _, [], "TVariable", [ DString name ]) -> TVariable(name)

    | DEnum(_, _, [], "TUnit", []) -> TUnit
    | DEnum(_, _, [], "TBool", []) -> TBool
    | DEnum(_, _, [], "TInt", []) -> TInt
    | DEnum(_, _, [], "TInt8", []) -> TInt8
    | DEnum(_, _, [], "TUInt8", []) -> TUInt8
    | DEnum(_, _, [], "TFloat", []) -> TFloat
    | DEnum(_, _, [], "TChar", []) -> TChar
    | DEnum(_, _, [], "TString", []) -> TString
    | DEnum(_, _, [], "TDateTime", []) -> TDateTime
    | DEnum(_, _, [], "TUuid", []) -> TUuid
    | DEnum(_, _, [], "TBytes", []) -> TBytes

    | DEnum(_, _, [], "TList", [ inner ]) -> TList(fromDT inner)

    | DEnum(_, _, [], "TTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "TDict", [ inner ]) -> TDict(fromDT inner)

    | DEnum(_, _, [], "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      TCustomType(
        NameResolution.fromDT TypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, [], "TDB", [ inner ]) -> TDB(fromDT inner)
    | DEnum(_, _, [], "TFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
      TFn(NEList.ofList (fromDT firstArg) (List.map fromDT otherArgs), fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" [ "typeRef", d ]

module Param =
  let toDT (p : Param) : Dval =
    let fields = [ ("name", DString p.name); ("typ", TypeReference.toDT p.typ) ]
    let typeName = rtTyp [] "Param" 0
    DRecord(typeName, typeName, [], Map fields)


module LetPattern =
  let knownType = KTCustomType(rtTyp [] "LetPattern" 0, [])

  let rec toDT (p : LetPattern) : Dval =
    let (caseName, fields) =
      match p with
      | LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | LPUnit id -> "LPUnit", [ DInt(int64 id) ]
      | LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]

    let typeName = rtTyp [] "LetPattern" 0
    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, [], "LPVariable", [ DInt id; DString name ]) ->
      LPVariable(uint64 id, name)
    | DEnum(_, _, [], "LPUnit", [ DInt id ]) -> LPUnit(uint64 id)
    | DEnum(_, _, [], "LPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let knownType = KTCustomType(rtTyp [] "MatchPattern" 0, [])

  let rec toDT (p : MatchPattern) : Dval =
    let (caseName, fields) =
      match p with
      | MPVariable(id, name) -> "MPVariable", [ DInt(int64 id); DString name ]

      | MPUnit id -> "MPUnit", [ DInt(int64 id) ]
      | MPBool(id, b) -> "MPBool", [ DInt(int64 id); DBool b ]
      | MPInt(id, i) -> "MPInt", [ DInt(int64 id); DInt i ]
      | MPInt8(id, i) -> "MPInt8", [ DInt(int64 id); DInt8 i ]
      | MPUInt8(id, i) -> "MPUInt8", [ DInt(int64 id); DUInt8 i ]
      | MPFloat(id, f) -> "MPFloat", [ DInt(int64 id); DFloat f ]
      | MPChar(id, c) -> "MPChar", [ DInt(int64 id); DString c ]
      | MPString(id, s) -> "MPString", [ DInt(int64 id); DString s ]

      | MPList(id, inner) ->
        "MPList", [ DInt(int64 id); DList(VT.known knownType, List.map toDT inner) ]
      | MPListCons(id, head, tail) ->
        "MPListCons", [ DInt(int64 id); toDT head; toDT tail ]

      | MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]

      | MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt(int64 id)
          DString caseName
          DList(VT.known knownType, List.map toDT fieldPats) ]

    let typeName = rtTyp [] "MatchPattern" 0
    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : MatchPattern =
    match d with
    | DEnum(_, _, [], "MPVariable", [ DInt id; DString name ]) ->
      MPVariable(uint64 id, name)

    | DEnum(_, _, [], "MPUnit", [ DInt id ]) -> MPUnit(uint64 id)
    | DEnum(_, _, [], "MPBool", [ DInt id; DBool b ]) -> MPBool(uint64 id, b)
    | DEnum(_, _, [], "MPInt", [ DInt id; DInt i ]) -> MPInt(uint64 id, i)
    | DEnum(_, _, [], "MPInt8", [ DInt id; DInt8 i ]) -> MPInt8(uint64 id, i)
    | DEnum(_, _, [], "MPUInt8", [ DInt id; DUInt8 i ]) -> MPUInt8(uint64 id, i)
    | DEnum(_, _, [], "MPFloat", [ DInt id; DFloat f ]) -> MPFloat(uint64 id, f)
    | DEnum(_, _, [], "MPChar", [ DInt id; DString c ]) -> MPChar(uint64 id, c)
    | DEnum(_, _, [], "MPString", [ DInt id; DString s ]) -> MPString(uint64 id, s)

    | DEnum(_, _, [], "MPList", [ DInt id; DList(_vtTODO, inner) ]) ->
      MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, [], "MPListCons", [ DInt id; head; tail ]) ->
      MPListCons(uint64 id, fromDT head, fromDT tail)

    | DEnum(_, _, [], "MPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_,
            _,
            [],
            "MPEnum",
            [ DInt id; DString caseName; DList(_vtTODO, fieldPats) ]) ->
      MPEnum(uint64 id, caseName, List.map fromDT fieldPats)

    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module StringSegment =
  let knownType = KTCustomType(rtTyp [] "StringSegment" 0, [])

  let toDT (exprToDT : Expr -> Dval) (s : StringSegment) : Dval =
    let (caseName, fields) =
      match s with
      | StringText text -> "StringText", [ DString text ]
      | StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]
    let typeName = rtTyp [] "StringSegment" 0
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (exprFromDT : Dval -> Expr) (d : Dval) : StringSegment =
    match d with
    | DEnum(_, _, [], "StringText", [ DString text ]) -> StringText text
    | DEnum(_, _, [], "StringInterpolation", [ expr ]) ->
      StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module Expr =
  let knownType = KTCustomType(rtTyp [] "Expr" 0, [])

  let rec toDT (e : Expr) : Dval =
    let (caseName, fields) =
      match e with
      | EUnit id -> "EUnit", [ DInt(int64 id) ]

      | EBool(id, b) -> "EBool", [ DInt(int64 id); DBool b ]
      | EInt(id, i) -> "EInt", [ DInt(int64 id); DInt i ]
      | EInt8(id, i) -> "EInt8", [ DInt(int64 id); DInt8 i ]
      | EUInt8(id, i) -> "EUInt8", [ DInt(int64 id); DUInt8 i ]
      | EFloat(id, f) -> "EFloat", [ DInt(int64 id); DFloat f ]
      | EChar(id, c) -> "EChar", [ DInt(int64 id); DString c ]
      | EString(id, segments) ->
        let segments =
          DList(
            VT.known StringSegment.knownType,
            List.map (StringSegment.toDT toDT) segments
          )
        "EString", [ DInt(int64 id); segments ]

      | EList(id, exprs) ->
        "EList", [ DInt(int64 id); Dval.list knownType (List.map toDT exprs) ]

      | EDict(id, entries) ->
        let entries =
          entries
          |> List.map (fun (k, v) -> DTuple(DString k, toDT v, []))
          |> fun entries ->
              DList(VT.tuple VT.string (ValueType.known knownType) [], entries)
        "EDict", [ DInt(int64 id); entries ]

      | ETuple(id, first, second, theRest) ->
        "ETuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          Dval.list knownType (List.map toDT theRest) ]

      | ERecord(id, typeName, fields) ->
        let fields =
          fields
          |> NEList.toList
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))
          |> fun fields ->
              DList(VT.tuple VT.string (ValueType.known knownType) [], fields)
        "ERecord", [ DInt(int64 id); TypeName.toDT typeName; fields ]

      | EEnum(id, typeName, caseName, fields) ->
        "EEnum",
        [ DInt(int64 id)
          TypeName.toDT typeName
          DString caseName
          Dval.list knownType (List.map toDT fields) ]

      // declaring and accessing variables
      | ELet(id, lp, expr, body) ->
        "ELet", [ DInt(int64 id); LetPattern.toDT lp; toDT expr; toDT body ]

      | EFieldAccess(id, expr, fieldName) ->
        "EFieldAccess", [ DInt(int64 id); toDT expr; DString fieldName ]

      | EVariable(id, varName) -> "EVariable", [ DInt(int64 id); DString varName ]


      // control flow
      | EIf(id, cond, thenExpr, elseExpr) ->
        "EIf",
        [ DInt(int64 id)
          toDT cond
          toDT thenExpr
          elseExpr |> Option.map toDT |> Dval.option knownType ]

      | EMatch(id, arg, cases) ->
        let cases =
          cases
          |> NEList.toList
          |> List.map (fun case ->
            let pattern = MatchPattern.toDT case.pat
            let whenCondition =
              case.whenCondition |> Option.map toDT |> Dval.option knownType
            let expr = toDT case.rhs
            let typeName = (rtTyp [] "MatchCase" 0)
            DRecord(
              typeName,
              typeName,
              [],
              Map
                [ ("pat", pattern)
                  ("whenCondition", whenCondition)
                  ("rhs", expr) ]
            ))
          |> Dval.list (KTCustomType((rtTyp [] "MatchCase" 0), []))
        "EMatch", [ DInt(int64 id); toDT arg; cases ]


      | ELambda(id, pats, body) ->
        let variables =
          (NEList.toList pats)
          |> List.map LetPattern.toDT
          |> Dval.list (KTTuple(VT.int, VT.string, []))
        "ELambda", [ DInt(int64 id); variables; toDT body ]

      | EConstant(id, name) ->
        "EConstant", [ DInt(int64 id); ConstantName.toDT name ]

      | EApply(id, expr, typeArgs, args) ->
        let typeArgs =
          typeArgs
          |> List.map TypeReference.toDT
          |> Dval.list TypeReference.knownType
        let args =
          Dval.list TypeReference.knownType (args |> NEList.toList |> List.map toDT)
        "EApply", [ DInt(int64 id); toDT expr; typeArgs; args ]

      | EFnName(id, name) -> "EFnName", [ DInt(int64 id); FnName.toDT name ]

      | ERecordUpdate(id, record, updates) ->
        let updates =
          NEList.toList updates
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))
          |> Dval.list (KTTuple(VT.string, VT.known knownType, []))
        "ERecordUpdate", [ DInt(int64 id); toDT record; updates ]

      | EAnd(id, left, right) -> "EAnd", [ DInt(int64 id); toDT left; toDT right ]

      | EOr(id, left, right) -> "EOr", [ DInt(int64 id); toDT left; toDT right ]

      // Let the error straight through
      | EError(id, rtError, exprs) ->
        "EError",
        [ DInt(int64 id)
          RuntimeTypes.RuntimeError.toDT rtError
          Dval.list knownType (List.map toDT exprs) ]


    let typeName = rtTyp [] "Expr" 0
    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : Expr =
    match d with
    | DEnum(_, _, [], "EUnit", [ DInt id ]) -> EUnit(uint64 id)

    | DEnum(_, _, [], "EBool", [ DInt id; DBool b ]) -> EBool(uint64 id, b)
    | DEnum(_, _, [], "EInt", [ DInt id; DInt i ]) -> EInt(uint64 id, i)
    | DEnum(_, _, [], "EInt8", [ DInt id; DInt8 i ]) -> EInt8(uint64 id, i)
    | DEnum(_, _, [], "EUInt8", [ DInt id; DUInt8 i ]) -> EUInt8(uint64 id, i)
    | DEnum(_, _, [], "EFloat", [ DInt id; DFloat f ]) -> EFloat(uint64 id, f)
    | DEnum(_, _, [], "EChar", [ DInt id; DString c ]) -> EChar(uint64 id, c)
    | DEnum(_, _, [], "EString", [ DInt id; DList(_vtTODO, segments) ]) ->
      EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    | DEnum(_, _, [], "EList", [ DInt id; DList(_vtTODO, inner) ]) ->
      EList(uint64 id, List.map fromDT inner)

    | DEnum(_, _, [], "EDict", [ DInt id; DList(_vtTODO, pairsList) ]) ->
      let pairs =
        pairsList
        |> List.collect (fun pair ->
          match pair with
          | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
          | _ -> [])
      EDict(uint64 id, pairs)


    | DEnum(_, _, [], "ETuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "ERecord", [ DInt id; typeName; DList(_vtTODO1, fieldsList) ]) ->
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
            [],
            "EEnum",
            [ DInt id; typeName; DString caseName; DList(_vtTODO, fields) ]) ->
      EEnum(uint64 id, TypeName.fromDT typeName, caseName, List.map fromDT fields)

    | DEnum(_, _, [], "ELet", [ DInt id; lp; expr; body ]) ->
      ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, [], "EFieldAccess", [ DInt id; expr; DString fieldName ]) ->
      EFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, [], "EVariable", [ DInt id; DString varName ]) ->
      EVariable(uint64 id, varName)

    | DEnum(_, _, [], "EIf", [ DInt id; cond; thenExpr; elseExpr ]) ->
      let elseExpr =
        match elseExpr with
        | DEnum(_, _, _typeArgsDEnumTODO, "Some", [ dv ]) -> Some(fromDT dv)
        | DEnum(_, _, _typeArgsDEnumTODO, "None", []) -> None
        | _ ->
          Exception.raiseInternal "Invalid else expression" [ "elseExpr", elseExpr ]
      EIf(uint64 id, fromDT cond, fromDT thenExpr, elseExpr)

    | DEnum(_, _, [], "EMatch", [ DInt id; arg; DList(_vtTODO, cases) ]) ->
      let cases =
        cases
        |> List.collect (fun case ->
          match case with
          | DRecord(_, _, _, fields) ->
            let whenCondition =
              match Map.tryFind "whenCondition" fields with
              | Some(DEnum(_, _, _, "Some", [ value ])) -> Some(fromDT value)
              | Some(DEnum(_, _, _, "None", [])) -> None
              | _ -> None
            match Map.tryFind "pat" fields, Map.tryFind "rhs" fields with
            | Some pat, Some rhs ->
              [ { pat = MatchPattern.fromDT pat
                  whenCondition = whenCondition
                  rhs = fromDT rhs } ]
            | _ -> []
          | _ -> [])
      EMatch(
        uint64 id,
        fromDT arg,
        NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one case in EMatch"
          []
          cases
      )

    | DEnum(_, _, [], "ELambda", [ DInt id; DList(_vtTODO, pats); body ]) ->
      let pats =
        pats
        |> List.map LetPattern.fromDT
        |> NEList.ofListUnsafe
          "RT2DT.Expr.fromDT expected at least one bound variable in ELambda"
          []
      ELambda(uint64 id, pats, fromDT body)


    | DEnum(_,
            _,
            [],
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

    | DEnum(_, _, [], "EFnName", [ DInt id; name ]) ->
      EFnName(uint64 id, FnName.fromDT name)

    | DEnum(_, _, [], "ERecordUpdate", [ DInt id; record; DList(_vtTODO, updates) ]) ->
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
    | DEnum(_, _, [], "EAnd", [ DInt id; left; right ]) ->
      EAnd(uint64 id, fromDT left, fromDT right)

    | DEnum(_, _, [], "EOr", [ DInt id; left; right ]) ->
      EOr(uint64 id, fromDT left, fromDT right)

    | DEnum(_, _, [], "EError", [ DInt id; rtError; DList(_vtTODO, exprs) ]) ->
      EError(uint64 id, RuntimeError.fromDT rtError, List.map fromDT exprs)


    | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ]


module RuntimeError =
  let toDT (e : RuntimeError) : Dval =
    e |> RuntimeTypes.RuntimeError.toDT |> Dval.toDT

  let fromDT (d : Dval) : RuntimeError =
    d |> Dval.fromDT |> RuntimeTypes.RuntimeError.fromDT


module Dval =
  let knownType = KTCustomType(rtTyp [ "Dval" ] "Dval" 0, [])

  module KnownType =
    let toDT (kt : KnownType) : Dval =
      let (caseName, fields) =
        match kt with
        | KTUnit -> "KTUnit", []
        | KTBool -> "KTBool", []
        | KTInt -> "KTInt", []
        | KTInt8 -> "KTInt8", []
        | KTUInt8 -> "KTUInt8", []
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
            DList(VT.known ValueType.knownType, List.map ValueType.toDT theRest) ]
        | KTDict inner -> "KTDict", [ ValueType.toDT inner ]

        | KTCustomType(typeName, typeArgs) ->
          "KTCustomType",
          [ TypeName.toDT typeName
            DList(VT.known ValueType.knownType, List.map ValueType.toDT typeArgs) ]

        | KTFn(args, ret) ->
          let args =
            args
            |> NEList.toList
            |> List.map ValueType.toDT
            |> Dval.list ValueType.knownType
          "KTFn", [ args; ValueType.toDT ret ]

        | KTDB d -> "KTDB", [ ValueType.toDT d ]

      let typeName = rtTyp [] "KnownType" 0
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : KnownType =
      match d with
      | DEnum(_, _, [], "KTUnit", []) -> KTUnit
      | DEnum(_, _, [], "KTBool", []) -> KTBool
      | DEnum(_, _, [], "KTInt", []) -> KTInt
      | DEnum(_, _, [], "KTInt8", []) -> KTInt8
      | DEnum(_, _, [], "KTUInt8", []) -> KTUInt8
      | DEnum(_, _, [], "KTFloat", []) -> KTFloat
      | DEnum(_, _, [], "KTChar", []) -> KTChar
      | DEnum(_, _, [], "KTString", []) -> KTString
      | DEnum(_, _, [], "KTUuid", []) -> KTUuid
      | DEnum(_, _, [], "KTDateTime", []) -> KTDateTime
      | DEnum(_, _, [], "KTBytes", []) -> KTBytes

      | DEnum(_, _, [], "KTList", [ inner ]) -> KTList(ValueType.fromDT inner)
      | DEnum(_, _, [], "KTTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
        KTTuple(
          ValueType.fromDT first,
          ValueType.fromDT second,
          List.map ValueType.fromDT theRest
        )
      | DEnum(_, _, [], "KTDict", [ inner ]) -> KTDict(ValueType.fromDT inner)

      | DEnum(_, _, [], "KTCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
        KTCustomType(TypeName.fromDT typeName, List.map ValueType.fromDT typeArgs)

      | DEnum(_, _, [], "KTFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
        KTFn(
          NEList.ofList
            (ValueType.fromDT firstArg)
            (List.map ValueType.fromDT otherArgs),
          ValueType.fromDT ret
        )
      | DEnum(_, _, [], "KTDB", [ inner ]) -> KTDB(ValueType.fromDT inner)

      | _ -> Exception.raiseInternal "Invalid KnownType" []

  module ValueType =
    let knownType = KTCustomType(rtTyp [] "ValueType" 0, [])

    let toDT (vt : ValueType) : Dval =
      let (caseName, fields) =
        match vt with
        | ValueType.Unknown -> "Unknown", []
        | ValueType.Known kt ->
          let kt = KnownType.toDT kt
          "Known", [ kt ]
      let typeName = rtTyp [] "ValueType" 0
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : ValueType =
      match d with
      | DEnum(_, _, [], "Unknown", []) -> ValueType.Unknown
      | DEnum(_, _, [], "Known", [ kt ]) -> ValueType.Known(KnownType.fromDT kt)
      | _ -> Exception.raiseInternal "Invalid ValueType" []


  module LambdaImpl =
    let toDT (l : LambdaImpl) : Dval =
      let parameters =
        l.parameters
        |> NEList.toList
        |> List.map LetPattern.toDT
        |> fun p -> DList(VT.tuple VT.int VT.string [], p)
      let fields =
        [ ("typeSymbolTable",
           DDict(
             VT.known TypeReference.knownType,
             Map.map TypeReference.toDT l.typeSymbolTable
           ))
          ("symtable", DDict(VT.known knownType, Map.map Dval.toDT l.symtable))
          ("parameters", parameters)
          ("body", Expr.toDT l.body) ]

      let typeName = rtTyp [] "LambdaImpl" 0
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : LambdaImpl =
      match d with
      | DRecord(_, _, _, fields) ->
        { typeSymbolTable =
            fields |> D.mapField "typeSymbolTable" |> Map.map TypeReference.fromDT

          symtable = fields |> D.mapField "symtable" |> Map.map Dval.fromDT

          tlid = 0UL

          parameters =
            fields
            |> D.listField "parameters"
            |> List.map LetPattern.fromDT
            |> NEList.ofListUnsafe
              "RT2DT.Dval.fromDT expected at least one parameter in LambdaImpl"
              []

          body = fields |> D.field "body" |> Expr.fromDT }

      | _ -> Exception.raiseInternal "Invalid LambdaImpl" []

  module FnValImpl =
    let toDT (fnValImpl : FnValImpl) : Dval =
      let (caseName, fields) =
        match fnValImpl with
        | Lambda lambda -> "Lambda", [ LambdaImpl.toDT lambda ]
        | NamedFn fnName -> "NamedFn", [ FnName.toDT fnName ]
      let typeName = rtTyp [] "FnValImpl" 0
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : FnValImpl =
      match d with
      | DEnum(_, _, [], "Lambda", [ lambda ]) -> Lambda(LambdaImpl.fromDT lambda)
      | DEnum(_, _, [], "NamedFn", [ fnName ]) -> NamedFn(FnName.fromDT fnName)
      | _ -> Exception.raiseInternal "Invalid FnValImpl" []

  let rec toDT (dv : Dval) : Dval =
    let (caseName, fields) =
      match dv with
      | DUnit -> "DUnit", []
      | DBool b -> "DBool", [ DBool b ]
      | DInt i -> "DInt", [ DInt i ]
      | DInt8 i -> "DInt8", [ DInt8 i ]
      | DUInt8 i -> "DUInt8", [ DUInt8 i ]
      | DFloat f -> "DFloat", [ DFloat f ]
      | DChar c -> "DChar", [ DChar c ]
      | DString s -> "DString", [ DString s ]
      | DUuid u -> "DUuid", [ DUuid u ]
      | DDateTime d -> "DDateTime", [ DDateTime d ]
      | DBytes b -> "DBytes", [ DBytes b ]

      | DList(vt, items) ->
        "DList",
        [ ValueType.toDT vt; DList(VT.known knownType, List.map toDT items) ]

      | DTuple(first, second, theRest) ->
        "DTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | DFnVal fnImpl -> "DFnVal", [ FnValImpl.toDT fnImpl ]

      | DDB name -> "DDB", [ DString name ]

      | DDict(vt, entries) ->
        "DDict",
        [ ValueType.toDT vt; DDict(VT.known knownType, Map.map toDT entries) ]

      | DRecord(runtimeTypeName, sourceTypeName, typeArgs, fields) ->
        "DRecord",
        [ TypeName.toDT runtimeTypeName
          TypeName.toDT sourceTypeName
          DList(VT.known ValueType.knownType, List.map ValueType.toDT typeArgs)
          DDict(VT.known knownType, Map.map toDT fields) ]

      | DEnum(runtimeTypeName, sourceTypeName, typeArgs, caseName, fields) ->
        "DEnum",
        [ TypeName.toDT runtimeTypeName
          TypeName.toDT sourceTypeName
          DList(VT.known ValueType.knownType, List.map ValueType.toDT typeArgs)
          DString caseName
          DList(VT.known knownType, List.map toDT fields) ]

    let typeName = rtTyp [ "Dval" ] "Dval" 0
    DEnum(typeName, typeName, [], caseName, fields)


  let fromDT (d : Dval) : Dval =
    match d with
    | DEnum(_, _, [], "DInt", [ DInt i ]) -> DInt i
    | DEnum(_, _, [], "DInt8", [ DInt8 i ]) -> DInt8 i
    | DEnum(_, _, [], "DUInt8", [ DUInt8 i ]) -> DUInt8 i
    | DEnum(_, _, [], "DFloat", [ DFloat f ]) -> DFloat f
    | DEnum(_, _, [], "DBool", [ DBool b ]) -> DBool b
    | DEnum(_, _, [], "DUnit", []) -> DUnit
    | DEnum(_, _, [], "DString", [ DString s ]) -> DString s
    | DEnum(_, _, [], "DChar", [ DChar c ]) -> DChar c

    | DEnum(_, _, [], "DList", [ vt; DList(_vtTODO, l) ]) ->
      DList(ValueType.fromDT vt, List.map fromDT l)
    | DEnum(_, _, [], "DTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      DTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "DFnVal", [ fnImpl ]) -> DFnVal(FnValImpl.fromDT fnImpl)

    | DEnum(_, _, [], "DDB", [ DString name ]) -> DDB name

    | DEnum(_, _, [], "DDateTime", [ DDateTime d ]) -> DDateTime d
    | DEnum(_, _, [], "DUuid", [ DUuid u ]) -> DUuid u
    | DEnum(_, _, [], "DBytes", [ DBytes b ]) -> DBytes b

    | DEnum(_, _, [], "DDict", [ vt; DDict(_vtTODO, map) ]) ->
      DDict(ValueType.fromDT vt, Map.map fromDT map)

    | DEnum(_,
            _,
            [],
            "DRecord",
            [ runtimeTypeName; sourceTypeName; DList(_, typeArgs); DDict(_, entries) ]) ->
      DRecord(
        TypeName.fromDT runtimeTypeName,
        TypeName.fromDT sourceTypeName,
        List.map ValueType.fromDT typeArgs,
        Map.map fromDT entries
      )
    | DEnum(_,
            _,
            [],
            "DEnum",
            [ runtimeTypeName
              sourceTypeName
              DList(_vtTODO1, typeArgs)
              DString caseName
              DList(_vtTODO2, fields) ]) ->
      DEnum(
        TypeName.fromDT runtimeTypeName,
        TypeName.fromDT sourceTypeName,
        List.map ValueType.fromDT typeArgs,
        caseName,
        List.map fromDT fields
      )

    | _ -> Exception.raiseInternal "Invalid Dval" []
