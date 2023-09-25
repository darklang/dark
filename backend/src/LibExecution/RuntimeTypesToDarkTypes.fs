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
      (nameValueType : ValueType)
      (nameMapper : 'name -> Ply<Dval>)
      (u : FQName.BuiltIn<'name>)
      : Ply<Dval> =
      uply {
        let! name = nameMapper u.name
        return!
          Dval.record
            (rtTyp [ "FQName" ] "BuiltIn" 0)
            (Some [ nameValueType ])
            [ "modules", Dval.list VT.unknownTODO (List.map DString u.modules)
              "name", name
              "version", DInt u.version ]
      }

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : FQName.BuiltIn<'name> =
      match d with
      | DRecord(_, _, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module UserProgram =
    let toDT
      (nameValueType : ValueType)
      (nameMapper : 'name -> Ply<Dval>)
      (u : FQName.UserProgram<'name>)
      : Ply<Dval> =
      uply {
        let! name = nameMapper u.name
        return!
          Dval.record
            (rtTyp [ "FQName" ] "UserProgram" 0)
            (Some [ nameValueType ])
            [ "modules", Dval.list VT.unknownTODO (List.map DString u.modules)
              "name", name
              "version", DInt u.version ]
      }

    let fromDT (nameMapper : Dval -> 'name) (v : Dval) : FQName.UserProgram<'name> =
      match v with
      | DRecord(_, _, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module Package =
    let toDT
      (nameValueType : ValueType)
      (nameMapper : 'name -> Ply<Dval>)
      (u : FQName.Package<'name>)
      : Ply<Dval> =
      uply {
        let! name = nameMapper u.name
        return!
          Dval.record
            (rtTyp [ "FQName" ] "Package" 0)
            (Some [ nameValueType ])
            [ "owner", DString u.owner
              "modules", Dval.list VT.unknownTODO (List.map DString u.modules)
              "name", name
              "version", DInt u.version ]
      }

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


  let toDT
    (nameValueType : ValueType)
    (nameMapper : 'name -> Ply<Dval>)
    (u : FQName.FQName<'name>)
    : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match u with
          | FQName.UserProgram u ->
            let! name = UserProgram.toDT nameValueType nameMapper u
            return "UserProgram", [ name ]
          | FQName.Package u ->
            let! name = Package.toDT nameValueType nameMapper u
            return "Package", [ name ]
          | FQName.BuiltIn u ->
            let! name = BuiltIn.toDT nameValueType nameMapper u
            return "BuiltIn", [ name ]
        }

      let typeName = rtTyp [ "FQName" ] "FQName" 0
      return! Dval.enum typeName typeName VT.typeArgsTODO' caseName fields
    }

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
  module Name =
    let valueType = VT.unknownTODO // @Darklang.LanguageTools.RuntimeTypes.TypeName.Name

    let toDT (u : TypeName.Name) : Ply<Dval> =
      let caseName, fields =
        match u with
        | TypeName.TypeName name -> "TypeName", [ DString name ]

      let typeName = rtTyp [ "TypeName" ] "Name" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : TypeName.Name =
      match d with
      | DEnum(_, _, [], "TypeName", [ DString name ]) -> TypeName.TypeName(name)
      | _ -> Exception.raiseInternal "Invalid TypeName" []

  module BuiltIn =
    let toDT (u : TypeName.BuiltIn) : Ply<Dval> =
      FQName.BuiltIn.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : TypeName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : TypeName.UserProgram) : Ply<Dval> =
      FQName.UserProgram.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : TypeName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : TypeName.Package) : Ply<Dval> =
      FQName.Package.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : TypeName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : TypeName.TypeName) : Ply<Dval> =
    FQName.toDT Name.valueType Name.toDT u
  let fromDT (d : Dval) : TypeName.TypeName = FQName.fromDT Name.fromDT d


module FnName =
  module Name =
    let valueType = VT.unknownTODO // @Darklang.LanguageTools.RuntimeTypes.FnName.Name

    let toDT (u : FnName.Name) : Ply<Dval> =
      let caseName, fields =
        match u with
        | FnName.FnName name -> "FnName", [ DString name ]

      let typeName = rtTyp [ "FnName" ] "Name" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : FnName.Name =
      match d with
      | DEnum(_, _, [], "FnName", [ DString name ]) -> FnName.FnName(name)
      | _ -> Exception.raiseInternal "Invalid FnName" []

  module BuiltIn =
    let toDT (u : FnName.BuiltIn) : Ply<Dval> =
      FQName.BuiltIn.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : FnName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : FnName.UserProgram) : Ply<Dval> =
      FQName.UserProgram.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : FnName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : FnName.Package) : Ply<Dval> =
      FQName.Package.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : FnName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : FnName.FnName) : Ply<Dval> = FQName.toDT Name.valueType Name.toDT u
  let fromDT (d : Dval) : FnName.FnName = FQName.fromDT Name.fromDT d


module ConstantName =
  module Name =
    let valueType = VT.unknownTODO // @Darklang.LanguageTools.RuntimeTypes.ConstantName.Name

    let toDT (u : ConstantName.Name) : Ply<Dval> =
      let caseName, fields =
        match u with
        | ConstantName.ConstantName name -> "ConstantName", [ DString name ]

      let typeName = rtTyp [ "ConstantName" ] "Name" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : ConstantName.Name =
      match d with
      | DEnum(_, _, [], "ConstantName", [ DString name ]) ->
        ConstantName.ConstantName(name)
      | _ -> Exception.raiseInternal "Invalid ConstantName" []

  module BuiltIn =
    let toDT (u : ConstantName.BuiltIn) : Ply<Dval> =
      FQName.BuiltIn.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : ConstantName.BuiltIn =
      FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : ConstantName.UserProgram) : Ply<Dval> =
      FQName.UserProgram.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : ConstantName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : ConstantName.Package) : Ply<Dval> =
      FQName.Package.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : ConstantName.Package =
      FQName.Package.fromDT Name.fromDT d

  let toDT (u : ConstantName.ConstantName) : Ply<Dval> =
    FQName.toDT Name.valueType Name.toDT u
  let fromDT (d : Dval) : ConstantName.ConstantName = FQName.fromDT Name.fromDT d


module NameResolution =
  let toDT
    (nameValueType : ValueType)
    (f : 'p -> Ply<Dval>)
    (result : NameResolution<'p>)
    : Ply<Dval> =
    uply {
      let errType = VT.unknownTODO // NameResolutionError

      match result with
      | Ok name ->
        let! name = f name
        return! Dval.resultOk nameValueType errType name
      | Error err ->
        return!
          RuntimeError.toDT err |> Ply.bind (Dval.resultError nameValueType errType)
    }

  let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
    match d with
    | DEnum(tn, _, _typeArgsDEnumTODO, "Ok", [ v ]) when tn = Dval.resultType ->
      Ok(f v)
    | DEnum(tn, _, _typeArgsDEnumTODO, "Error", [ v ]) when tn = Dval.resultType ->
      Error(RuntimeError.fromDT v)
    | _ -> Exception.raiseInternal "Invalid NameResolution" []

module TypeReference =
  let valueType = ValueType.unknownTODO // @Darklang.LanguageTools.RuntimeTypes.TypeReference

  let rec toDT (t : TypeReference) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match t with
          | TVariable name -> return "TVariable", [ DString name ]

          | TUnit -> return "TUnit", []
          | TBool -> return "TBool", []
          | TInt -> return "TInt", []
          | TFloat -> return "TFloat", []
          | TChar -> return "TChar", []
          | TString -> return "TString", []
          | TDateTime -> return "TDateTime", []
          | TUuid -> return "TUuid", []
          | TBytes -> return "TBytes", []

          | TList inner ->
            let! inner = toDT inner
            return "TList", [ inner ]

          | TTuple(first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "TTuple", [ first; second; theRest ]

          | TDict inner ->
            let! inner = toDT inner
            return "TDict", [ inner ]

          | TCustomType(typeName, typeArgs) ->
            let! typeName =
              NameResolution.toDT ValueType.unknownTODO TypeName.toDT typeName
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "TCustomType", [ typeName; typeArgs ]

          | TDB inner ->
            let! inner = toDT inner
            return "TDB", [ inner ]
          | TFn(args, ret) ->
            let! args =
              args
              |> NEList.toList
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            let! ret = toDT ret
            return "TFn", [ args; ret ]
        }

      let typeName = rtTyp [] "TypeReference" 0
      return! Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : TypeReference =
    match d with
    | DEnum(_, _, [], "TVariable", [ DString name ]) -> TVariable(name)

    | DEnum(_, _, [], "TUnit", []) -> TUnit
    | DEnum(_, _, [], "TBool", []) -> TBool
    | DEnum(_, _, [], "TInt", []) -> TInt
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
  let toDT (p : Param) : Ply<Dval> =
    uply {
      let! typ = TypeReference.toDT p.typ
      return!
        Dval.record
          (rtTyp [] "Param" 0)
          (Some [])
          [ ("name", DString p.name); ("typ", typ) ]
    }


module LetPattern =
  let rec toDT (p : LetPattern) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match p with
          | LPVariable(id, name) ->
            return "LPVariable", [ DInt(int64 id); DString name ]
          | LPUnit id -> return "LPUnit", [ DInt(int64 id) ]
          | LPTuple(id, first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "LPTuple", [ DInt(int64 id); first; second; theRest ]
        }

      let typeName = rtTyp [] "LetPattern" 0
      return! Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, [], "LPVariable", [ DInt id; DString name ]) ->
      LPVariable(uint64 id, name)
    | DEnum(_, _, [], "LPUnit", [ DInt id ]) -> LPUnit(uint64 id)
    | DEnum(_, _, [], "LPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let rec toDT (p : MatchPattern) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match p with
          | MPVariable(id, name) ->
            return "MPVariable", [ DInt(int64 id); DString name ]

          | MPUnit id -> return "MPUnit", [ DInt(int64 id) ]
          | MPBool(id, b) -> return "MPBool", [ DInt(int64 id); DBool b ]
          | MPInt(id, i) -> return "MPInt", [ DInt(int64 id); DInt i ]
          | MPFloat(id, f) -> return "MPFloat", [ DInt(int64 id); DFloat f ]
          | MPChar(id, c) -> return "MPChar", [ DInt(int64 id); DString c ]
          | MPString(id, s) -> return "MPString", [ DInt(int64 id); DString s ]

          | MPList(id, inner) ->
            let! inner =
              inner
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "MPList", [ DInt(int64 id); inner ]
          | MPListCons(id, head, tail) ->
            let! head = toDT head
            let! tail = toDT tail
            return "MPListCons", [ DInt(int64 id); head; tail ]
          | MPTuple(id, first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "MPTuple", [ DInt(int64 id); first; second; theRest ]
          | MPEnum(id, caseName, fieldPats) ->
            let! fieldPats =
              fieldPats
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "MPEnum", [ DInt(int64 id); DString caseName; fieldPats ]
        }

      let typeName = rtTyp [] "MatchPattern" 0
      return! Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : MatchPattern =
    match d with
    | DEnum(_, _, [], "MPVariable", [ DInt id; DString name ]) ->
      MPVariable(uint64 id, name)

    | DEnum(_, _, [], "MPUnit", [ DInt id ]) -> MPUnit(uint64 id)
    | DEnum(_, _, [], "MPBool", [ DInt id; DBool b ]) -> MPBool(uint64 id, b)
    | DEnum(_, _, [], "MPInt", [ DInt id; DInt i ]) -> MPInt(uint64 id, i)
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
  let toDT (exprToDT : Expr -> Ply<Dval>) (s : StringSegment) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match s with
          | StringText text -> return "StringText", [ DString text ]
          | StringInterpolation expr ->
            let! expr = exprToDT expr
            return "StringInterpolation", [ expr ]
        }

      let typeName = rtTyp [] "StringSegment" 0
      return! Dval.enum typeName typeName (Some []) caseName fields
    }

  let fromDT (exprFromDT : Dval -> Expr) (d : Dval) : StringSegment =
    match d with
    | DEnum(_, _, [], "StringText", [ DString text ]) -> StringText text
    | DEnum(_, _, [], "StringInterpolation", [ expr ]) ->
      StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module Expr =
  let valueType = VT.unknownTODO // @Darklang.LanguageTools.RuntimeTypes.Expr

  let rec toDT (e : Expr) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match e with
          | EUnit id -> return "EUnit", [ DInt(int64 id) ]

          // simple data
          | EBool(id, b) -> return "EBool", [ DInt(int64 id); DBool b ]
          | EInt(id, i) -> return "EInt", [ DInt(int64 id); DInt i ]
          | EFloat(id, f) -> return "EFloat", [ DInt(int64 id); DFloat f ]
          | EChar(id, c) -> return "EChar", [ DInt(int64 id); DString c ]
          | EString(id, segments) ->
            let! segments =
              segments
              |> Ply.List.mapSequentially (StringSegment.toDT toDT)
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EString", [ DInt(int64 id); segments ]

          // structures of data
          | EList(id, exprs) ->
            let! exprs =
              exprs |> Ply.List.mapSequentially toDT |> Ply.map (Dval.list valueType)
            return "EList", [ DInt(int64 id); exprs ]

          | EDict(id, entries) ->
            let! entries =
              entries
              |> Ply.List.mapSequentially (fun (k, v) ->
                uply {
                  let! v = toDT v
                  return DTuple(DString k, v, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EDict", [ DInt(int64 id); entries ]

          | ETuple(id, first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "ETuple", [ DInt(int64 id); first; second; theRest ]

          | ERecord(id, typeName, fields) ->
            let! typeName = TypeName.toDT typeName
            let! fields =
              fields
              |> NEList.toList
              |> Ply.List.mapSequentially (fun (name, expr) ->
                uply {
                  let! expr = toDT expr
                  return DTuple(DString name, expr, [])
                })
            return
              "ERecord",
              [ DInt(int64 id); typeName; Dval.list VT.unknownTODO fields ]

          | EEnum(id, typeName, caseName, fields) ->
            let! typeName = TypeName.toDT typeName
            let! fields =
              fields
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list valueType)
            return "EEnum", [ DInt(int64 id); typeName; DString caseName; fields ]

          // declaring and accessing variables
          | ELet(id, lp, expr, body) ->
            let! lp = LetPattern.toDT lp
            let! expr = toDT expr
            let! body = toDT body
            return "ELet", [ DInt(int64 id); lp; expr; body ]

          | EFieldAccess(id, expr, fieldName) ->
            let! expr = toDT expr
            return "EFieldAccess", [ DInt(int64 id); expr; DString fieldName ]

          | EVariable(id, varName) ->
            return "EVariable", [ DInt(int64 id); DString varName ]


          // control flow
          | EIf(id, cond, thenExpr, elseExpr) ->
            let! cond = toDT cond
            let! thenExpr = toDT thenExpr
            let! elseExpr =
              elseExpr |> Ply.Option.map toDT |> Ply.bind (Dval.option valueType)
            return "EIf", [ DInt(int64 id); cond; thenExpr; elseExpr ]

          | EMatch(id, arg, cases) ->
            let! arg = toDT arg
            let! cases =
              cases
              |> NEList.toList
              |> Ply.List.mapSequentially (fun (pattern, expr) ->
                uply {
                  let! pattern = MatchPattern.toDT pattern
                  let! expr = toDT expr
                  return DTuple(pattern, expr, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EMatch", [ DInt(int64 id); arg; cases ]


          | ELambda(id, args, body) ->
            let variables =
              (NEList.toList args)
              |> List.map (fun (id, varName) ->
                DTuple(DInt(int64 id), DString varName, []))
              |> Dval.list VT.unknownTODO
            let! body = toDT body
            return "ELambda", [ DInt(int64 id); variables; body ]

          | EConstant(id, name) ->
            let! name = ConstantName.toDT name
            return "EConstant", [ DInt(int64 id); name ]

          | EApply(id, expr, typeArgs, args) ->
            let! expr = toDT expr
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially TypeReference.toDT
              |> Ply.map (Dval.list TypeReference.valueType)
            let! args =
              args
              |> NEList.toList
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list TypeReference.valueType)
            return "EApply", [ DInt(int64 id); expr; typeArgs; args ]

          | EFnName(id, name) ->
            let! name = FnName.toDT name
            return "EFnName", [ DInt(int64 id); name ]

          | ERecordUpdate(id, record, updates) ->
            let! record = toDT record
            let! updates =
              NEList.toList updates
              |> Ply.List.mapSequentially (fun (name, expr) ->
                uply {
                  let! expr = toDT expr
                  return DTuple(DString name, expr, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)
            return "ERecordUpdate", [ DInt(int64 id); record; updates ]

          | EAnd(id, left, right) ->
            let! left = toDT left
            let! right = toDT right
            return "EAnd", [ DInt(int64 id); left; right ]

          | EOr(id, left, right) ->
            let! left = toDT left
            let! right = toDT right
            return "EOr", [ DInt(int64 id); left; right ]

          // Let the error straight through
          | EError(id, rtError, exprs) ->
            let! exprs =
              exprs |> Ply.List.mapSequentially toDT |> Ply.map (Dval.list valueType)
            return
              "EError",
              [ DInt(int64 id); RuntimeTypes.RuntimeError.toDT rtError; exprs ]
        }


      let typeName = rtTyp [] "Expr" 0
      return! Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : Expr =
    match d with
    | DEnum(_, _, [], "EUnit", [ DInt id ]) -> EUnit(uint64 id)

    // simple data
    | DEnum(_, _, [], "EBool", [ DInt id; DBool b ]) -> EBool(uint64 id, b)
    | DEnum(_, _, [], "EInt", [ DInt id; DInt i ]) -> EInt(uint64 id, i)
    | DEnum(_, _, [], "EFloat", [ DInt id; DFloat f ]) -> EFloat(uint64 id, f)
    | DEnum(_, _, [], "EChar", [ DInt id; DString c ]) -> EChar(uint64 id, c)
    | DEnum(_, _, [], "EString", [ DInt id; DList(_vtTODO, segments) ]) ->
      EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    // structures of data
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

    // declaring and accessing variables
    | DEnum(_, _, [], "ELet", [ DInt id; lp; expr; body ]) ->
      ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, [], "EFieldAccess", [ DInt id; expr; DString fieldName ]) ->
      EFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, [], "EVariable", [ DInt id; DString varName ]) ->
      EVariable(uint64 id, varName)

    // control flow
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


    | DEnum(_, _, [], "ELambda", [ DInt id; DList(_vtTODO, variables); body ]) ->
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
  let toDT (e : RuntimeError) : Ply<Dval> =
    e |> RuntimeTypes.RuntimeError.toDT |> Dval.toDT

  let fromDT (d : Dval) : RuntimeError =
    d |> Dval.fromDT |> RuntimeTypes.RuntimeError.fromDT


module Dval =
  let valueType = VT.unknownTODO // @Darklang.LanguageTools.RuntimeTypes.Dval.Dval

  module KnownType =
    let toDT (kt : KnownType) : Ply<Dval> =
      uply {
        let! (caseName, fields) =
          uply {
            match kt with
            | KTUnit -> return "KTUnit", []
            | KTBool -> return "KTBool", []
            | KTInt -> return "KTInt", []
            | KTFloat -> return "KTFloat", []
            | KTChar -> return "KTChar", []
            | KTString -> return "KTString", []
            | KTUuid -> return "KTUuid", []
            | KTDateTime -> return "KTDateTime", []
            | KTBytes -> return "KTBytes", []

            | KTList inner ->
              let! inner = ValueType.toDT inner
              return "KTList", [ inner ]
            | KTTuple(first, second, theRest) ->
              let! first = ValueType.toDT first
              let! second = ValueType.toDT second
              let! theRest =
                theRest
                |> Ply.List.mapSequentially ValueType.toDT
                |> Ply.map (Dval.list ValueType.valueType)
              return "KTTuple", [ first; second; theRest ]
            | KTDict inner ->
              let! inner = ValueType.toDT inner
              return "KTDict", [ inner ]

            | KTCustomType(typeName, typeArgs) ->
              let! typeName = TypeName.toDT typeName
              let! typeArgs =
                typeArgs
                |> Ply.List.mapSequentially ValueType.toDT
                |> Ply.map (Dval.list ValueType.valueType)
              return "KTCustomType", [ typeName; typeArgs ]

            | KTFn(args, ret) ->
              let! args =
                args
                |> NEList.toList
                |> Ply.List.mapSequentially ValueType.toDT
                |> Ply.map (Dval.list ValueType.valueType)
              let! ret = ValueType.toDT ret
              return "KTFn", [ args; ret ]

            | KTDB d ->
              let! d = ValueType.toDT d
              return "KTDB", [ d ]
          }

        let typeName = rtTyp [] "KnownType" 0
        return! Dval.enum typeName typeName (Some []) caseName fields
      }

    let fromDT (d : Dval) : KnownType =
      match d with
      | DEnum(_, _, [], "KTUnit", []) -> KTUnit
      | DEnum(_, _, [], "KTBool", []) -> KTBool
      | DEnum(_, _, [], "KTInt", []) -> KTInt
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
    let valueType = VT.unknownTODO

    let toDT (vt : ValueType) : Ply<Dval> =
      uply {
        let! (caseName, fields) =
          uply {
            match vt with
            | ValueType.Unknown -> return "Unknown", []
            | ValueType.Known kt ->
              let! kt = KnownType.toDT kt
              return "Known", [ kt ]
          }

        let typeName = rtTyp [] "ValueType" 0
        return! Dval.enum typeName typeName (Some []) caseName fields
      }

    let fromDT (d : Dval) : ValueType =
      match d with
      | DEnum(_, _, [], "Unknown", []) -> ValueType.Unknown
      | DEnum(_, _, [], "Known", [ kt ]) -> ValueType.Known(KnownType.fromDT kt)

      | _ -> Exception.raiseInternal "Invalid ValueType" []

  module DvalSource =
    let toDT (s : DvalSource) : Ply<Dval> =
      let caseName, fields =
        match s with
        | SourceNone -> "SourceNone", []
        | SourceID(tlid, id) -> "SourceID", [ DInt(int64 tlid); DInt(int64 id) ]

      let typeName = rtTyp [] "DvalSource" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : DvalSource =
      match d with
      | DEnum(_, _, [], "SourceNone", []) -> SourceNone
      | DEnum(_, _, [], "SourceID", [ DInt tlid; DInt id ]) ->
        SourceID(uint64 tlid, uint64 id)
      | _ -> Exception.raiseInternal "Invalid DvalSource" []


  module LambdaImpl =
    let toDT (l : LambdaImpl) : Ply<Dval> =
      uply {
        let! tst =
          l.typeSymbolTable
          |> Ply.Map.mapSequentially TypeReference.toDT
          |> Ply.map (Dval.dictFromMap VT.unknownTODO)
        let! symtable =
          l.symtable
          |> Ply.Map.mapSequentially Dval.toDT
          |> Ply.map (Dval.dictFromMap VT.unknownTODO)
        let parameters =
          l.parameters
          |> NEList.toList
          |> List.map (fun (id, name) -> DTuple(DInt(int64 id), DString name, []))
          |> Dval.list VT.unknownTODO
        let! body = Expr.toDT l.body
        return!
          Dval.record
            (rtTyp [] "LambdaImpl" 0)
            (Some [])
            [ "typeSymbolTable", tst
              "symtable", symtable
              "parameters", parameters
              "body", body ]
      }

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
    let toDT (fnValImpl : FnValImpl) : Ply<Dval> =
      uply {
        let! (caseName, fields) =
          uply {
            match fnValImpl with
            | Lambda lambda ->
              let! lambda = LambdaImpl.toDT lambda
              return "Lambda", [ lambda ]
            | NamedFn fnName ->
              let! fnName = FnName.toDT fnName
              return "NamedFn", [ fnName ]
          }

        let typeName = rtTyp [] "FnValImpl" 0
        return! Dval.enum typeName typeName (Some []) caseName fields
      }

    let fromDT (d : Dval) : FnValImpl =
      match d with
      | DEnum(_, _, [], "Lambda", [ lambda ]) -> Lambda(LambdaImpl.fromDT lambda)
      | DEnum(_, _, [], "NamedFn", [ fnName ]) -> NamedFn(FnName.fromDT fnName)
      | _ -> Exception.raiseInternal "Invalid FnValImpl" []

  let rec toDT (dv : Dval) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match dv with
          | DUnit -> return "DUnit", []
          | DBool b -> return "DBool", [ DBool b ]
          | DInt i -> return "DInt", [ DInt i ]
          | DFloat f -> return "DFloat", [ DFloat f ]
          | DChar c -> return "DChar", [ DChar c ]
          | DString s -> return "DString", [ DString s ]
          | DUuid u -> return "DUuid", [ DUuid u ]
          | DDateTime d -> return "DDateTime", [ DDateTime d ]
          | DBytes b -> return "DBytes", [ DBytes b ]


          | DList(vt, items) ->
            let! vt = ValueType.toDT vt
            let! items =
              items
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "DList", [ vt; items ]

          | DTuple(first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "DTuple", [ first; second; theRest ]

          | DFnVal fnImpl ->
            let! fnImpl = FnValImpl.toDT fnImpl
            return "DFnVal", [ fnImpl ]

          | DDB name -> return "DDB", [ DString name ]

          | DDict(vt, entries) ->
            let! vt = ValueType.toDT vt
            let! entries =
              entries
              |> Ply.Map.mapSequentially toDT
              |> Ply.map (Dval.dictFromMap VT.unknownTODO)
            return "DDict", [ vt; entries ]

          | DRecord(runtimeTypeName, sourceTypeName, typeArgs, fields) ->
            let! runtimeTypeName = TypeName.toDT runtimeTypeName
            let! sourceTypeName = TypeName.toDT sourceTypeName
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially ValueType.toDT
              |> Ply.map (Dval.list ValueType.valueType)
            let! fields =
              fields
              |> Ply.Map.mapSequentially toDT
              |> Ply.map (Dval.dictFromMap VT.unknownTODO)
            return "DRecord", [ runtimeTypeName; sourceTypeName; typeArgs; fields ]

          | DEnum(runtimeTypeName, sourceTypeName, typeArgs, caseName, fields) ->
            let! runtimeTypeName = TypeName.toDT runtimeTypeName
            let! sourceTypeName = TypeName.toDT sourceTypeName
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially ValueType.toDT
              |> Ply.map (Dval.list ValueType.valueType)
            let! fields =
              fields
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return
              "DEnum",
              [ runtimeTypeName; sourceTypeName; typeArgs; DString caseName; fields ]
        }

      let typeName = rtTyp [ "Dval" ] "Dval" 0
      return! Dval.enum typeName typeName (Some []) caseName fields
    }


  let fromDT (d : Dval) : Dval =
    match d with
    | DEnum(_, _, [], "DInt", [ DInt i ]) -> DInt i
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
      // CLEANUP should this be Dval.enum instead?
      DEnum(
        TypeName.fromDT runtimeTypeName,
        TypeName.fromDT sourceTypeName,
        List.map ValueType.fromDT typeArgs,
        caseName,
        List.map fromDT fields
      )

    | _ -> Exception.raiseInternal "Invalid Dval" []
