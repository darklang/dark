module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude
open Tablecloth

open RuntimeTypes

module D = Decode

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


// TODO: consolidate with the RT2DT equivalent
module Decode =
  let unwrap = Exception.unwrapOptionInternal

  let string (dv : Dval) : string =
    dv |> Dval.asString |> unwrap $"Expected a string" []

  let list (dv : Dval) : List<Dval> =
    dv |> Dval.asList |> unwrap $"Expected a list" []

  let int64 (dv : Dval) : int64 =
    dv |> Dval.asInt |> unwrap $"Expected an int" [] |> FSharp.Core.Operators.int64

  let int (dv : Dval) : int =
    dv |> Dval.asInt |> unwrap $"Expected an int" [] |> FSharp.Core.Operators.int

  let float (dv : Dval) : float = dv |> Dval.asFloat |> unwrap $"Expected a float" []

  let uint64 (dv : Dval) : uint64 =
    dv |> Dval.asInt |> unwrap $"Expected an int" [] |> FSharp.Core.Operators.uint64

  let id (dv : Dval) : id = uint64 dv
  let tlid (dv : Dval) : tlid = uint64 dv

  let uuid (dv : Dval) : System.Guid =
    dv |> Dval.asUuid |> unwrap $"Expected a uuid" []

  let bool (dv : Dval) : bool = dv |> Dval.asBool |> unwrap $"Expected a bool" []

  let stringList (dv : Dval) : List<string> = dv |> list |> List.map string

  let dict (dv : Dval) : DvalMap = dv |> Dval.asDict |> unwrap $"Expected a dict" []

  let tuple2 (dv : Dval) : Dval * Dval =
    dv |> Dval.asTuple2 |> unwrap $"Expected a tuple2" []

  let tuple3 (dv : Dval) : Dval * Dval * Dval =
    dv |> Dval.asTuple3 |> unwrap $"Expected a tuple3" []

  let field (name : string) (m : DvalMap) : Dval =
    m |> Map.get name |> unwrap $"Expected {name}' field" []




module FQName =
  let ownerField m = m |> D.field "owner" |> D.string
  let modulesField m = m |> D.field "modules" |> D.stringList

  let nameField m = m |> D.field "name"
  let versionField m = m |> D.field "version" |> D.int

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
      [ ("name", DString p.name); ("typ", TypeReference.toDT p.typ) ]


module LetPattern =
  let rec toDT (p : LetPattern) : Dval =
    let name, fields =
      match p with
      | LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]

    Dval.enum (rtTyp [] "LetPattern" 0) name fields

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, "LPVariable", [ DInt id; DString name ]) ->
      LPVariable(uint64 id, name)
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

  // let fromDT (d : Dval) : LambdaImpl =
  //   match d with
  //   | DRecord(_, _, fields) ->
  //     let typeArgTable =
  //       fields |> D.field "typeArgTable" |> D.dict |> Map.map TypeReference.fromDT
  //     let symtable = fields |> D.field "symtable" |> D.dict |> Map.map Dval.fromDT
  //     let parameters =
  //       fields
  //       |> D.field "parameters"
  //       |> D.list
  //       |> List.map (fun p ->
  //         p |> D.tuple2 |> (fun (id, name) -> (D.uint64 id, D.string name)))
  //     let body = fields |> D.field "body" |> Expr.fromDT

  //     { typeArgTable = typeArgTable
  //       symtable = symtable
  //       parameters = parameters
  //       body = body }

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

//   let fromDT (dv: Dval): Dval =
//     let name, fields =
//       match dv with
//       | DEnum(_, _, "DInt", [ DInt i ]) -> "DInt", [ DInt i ]
//       | DEnum(_, _, "DFloat", [ DFloat f ]) -> "DFloat", [ DFloat f ]
//       | DEnum(_, _, "DBool", [ DBool b ]) -> "DBool", [ DBool b ]
//       | DEnum(_, _, "DUnit", []) -> "DUnit", []
//       | DEnum(_, _, "DString", [ DString s ]) -> "DString", [ DString s ]
//       | DEnum(_, _, "DChar", [ DChar c ]) -> "DChar", [ DChar c ]

//       | DEnum(_, _, "DList", [ DList l ]) -> "DList", [ DList(List.map fromDT l) ]
//       | DEnum(_, _, "DTuple", [ first; second; DList theRest ]) -> "DTuple", [ fromDT first; fromDT second; DList(List.map fromDT theRest) ]

//       | DEnum(_, _, "DFnVal", [ fnImpl ]) -> "DFnVal", [ LambdaImpl.fromDT fnImpl ]

//       | DEnum(_, _, "DError", [ source; err ]) -> "DError", [ source; err ]

//       | DEnum(_, _, "DIncomplete", [ source ]) -> "DIncomplete", [ source ]

//       | DEnum(_, _, "DDB", [ DString name ]) -> "DDB", [ DString name ]
//       | DEnum(_, _, "DDateTime", [ DDateTime d ]) -> "DDateTime", [ DDateTime d ]
//       | DEnum(_, _, "DPassword", [ DPassword p ]) -> "DPassword", [ DPassword p ]
//       | DEnum(_, _, "DUuid", [ DUuid u ]) -> "DUuid", [ DUuid u ]



// module Deprecation =
//   let toDT (inner : 'a -> Dval) (d : Deprecation<'a>) : Dval =
//     let (caseName, fields) =
//       match d with
//       | Deprecation.NotDeprecated -> "NotDeprecated", []
//       | Deprecation.RenamedTo replacement -> "RenamedTo", [ inner replacement ]
//       | Deprecation.ReplacedBy replacement -> "ReplacedBy", [ inner replacement ]
//       | Deprecation.DeprecatedBecause reason ->
//         "DeprecatedBecause", [ DString reason ]

//     Dval.enum (rtTyp [] "Deprecation" 0) caseName fields

//   let fromDT (inner : Dval -> 'a) (d : Dval) : Deprecation<'a> =
//     match d with
//     | DEnum(_, _, "NotDeprecated", []) -> Deprecation.NotDeprecated
//     | DEnum(_, _, "RenamedTo", [ replacement ]) ->
//       Deprecation.RenamedTo(inner replacement)
//     | DEnum(_, _, "ReplacedBy", [ replacement ]) ->
//       Deprecation.ReplacedBy(inner replacement)
//     | DEnum(_, _, "DeprecatedBecause", [ DString reason ]) ->
//       Deprecation.DeprecatedBecause(reason)
//     | _ -> Exception.raiseInternal "Invalid Deprecation" []


// module TypeDeclaration =
//   module RecordField =
//     let toDT (rf : TypeDeclaration.RecordField) : Dval =
//       Dval.record
//         (rtTyp [ "TypeDeclaration" ] "RecordField" 0)
//         [ "name", DString rf.name
//           "typ", TypeReference.toDT rf.typ ]

//     let fromDT (d : Dval) : TypeDeclaration.RecordField =
//       match d with
//       | DRecord(_, _, fields) ->
//         let name = fields |> D.field "name" |> D.string
//         let typ = fields |> D.field "typ" |> TypeReference.fromDT

//         { name = name; typ = typ }

//       | _ -> Exception.raiseInternal "Invalid RecordField" []


//   module EnumCase =
//     let toDT (ec : TypeDeclaration.EnumCase) : Dval =
//       Dval.record
//         (rtTyp [ "TypeDeclaration" ] "EnumCase" 0)
//         [ "name", DString ec.name
//           "fields", DList(List.map TypeReference.toDT ec.fields) ]

//     let fromDT (d : Dval) : TypeDeclaration.EnumCase =
//       match d with
//       | DRecord(_, _, fields) ->
//         let name = fields |> D.field "name" |> D.string
//         let fields = fields |> D.field "fields" |> D.list |> List.map TypeReference.fromDT

//         { name = name; fields = fields }

//       | _ -> Exception.raiseInternal "Invalid EnumCase" []


//   module Definition =
//     let toDT (d : TypeDeclaration.Definition) : Dval =
//       let caseName, fields =
//         match d with
//         | TypeDeclaration.Alias typeRef -> "Alias", [ TypeReference.toDT typeRef ]

//         | TypeDeclaration.Record fields ->
//           "Record", [ fields |> NEList.toList |> List.map RecordField.toDT |> DList ]

//         | TypeDeclaration.Enum cases ->
//           "Enum", [ cases |> NEList.toList |> List.map EnumCase.toDT |> DList ]

//       Dval.enum (rtTyp [ "TypeDeclaration" ] "Definition" 0) caseName fields

//     let fromDT (d : Dval) : TypeDeclaration.Definition =
//       match d with
//       | DEnum(_, _, "Alias", [ typeRef ]) ->
//         TypeDeclaration.Alias(TypeReference.fromDT typeRef)

//       | DEnum(_, _, "Record", [ DList(firstField :: additionalFields) ]) ->
//         let fields = NEList.ofList firstField additionalFields
//         TypeDeclaration.Record(NEList.map RecordField.fromDT fields)

//       | DEnum(_, _, "Enum", [ DList(firstCase :: additionalCases) ]) ->
//         let cases = NEList.ofList firstCase additionalCases
//         TypeDeclaration.Enum(NEList.map EnumCase.fromDT cases)

//       | _ -> Exception.raiseInternal "Invalid TypeDeclaration.Definition" []


//   let toDT (td : TypeDeclaration.T) : Dval =
//     Dval.record
//       (rtTyp [ "TypeDeclaration" ] "T" 0)
//       [ "typeParams", DList(List.map DString td.typeParams)
//         "definition", Definition.toDT td.definition ]

//   let fromDT (d : Dval) : TypeDeclaration.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let typeParams = fields |> D.field "typeParams" |> D.stringList
//       let definition = fields |> D.field "definition" |> Definition.fromDT

//       { typeParams = typeParams; definition = definition }

//     | _ -> Exception.raiseInternal "Invalid TypeDeclaration" []


// module Handler =
//   module CronInterval =
//     let toDT (ci : Handler.CronInterval) : Dval =
//       let name, fields =
//         match ci with
//         | Handler.CronInterval.EveryMinute -> "EveryMinute", []
//         | Handler.CronInterval.EveryHour -> "EveryHour", []
//         | Handler.CronInterval.Every12Hours -> "Every12Hours", []
//         | Handler.CronInterval.EveryDay -> "EveryDay", []
//         | Handler.CronInterval.EveryWeek -> "EveryWeek", []
//         | Handler.CronInterval.EveryFortnight -> "EveryFortnight", []

//       Dval.enum (rtTyp [ "Handler" ] "CronInterval" 0) name fields

//     let fromDT (d : Dval) : Handler.CronInterval =
//       match d with
//       | DEnum(_, _, "EveryMinute", []) -> Handler.CronInterval.EveryMinute
//       | DEnum(_, _, "EveryHour", []) -> Handler.CronInterval.EveryHour
//       | DEnum(_, _, "Every12Hours", []) -> Handler.CronInterval.Every12Hours
//       | DEnum(_, _, "EveryDay", []) -> Handler.CronInterval.EveryDay
//       | DEnum(_, _, "EveryWeek", []) -> Handler.CronInterval.EveryWeek
//       | DEnum(_, _, "EveryFortnight", []) -> Handler.CronInterval.EveryFortnight
//       | _ -> Exception.raiseInternal "Invalid CronInterval" []


//   module Spec =
//     let toDT (s : Handler.Spec) : Dval =
//       let name, fields =
//         match s with
//         | Handler.Spec.HTTP(route, method) ->
//           "HTTP", [ DString route; DString method ]
//         | Handler.Spec.Worker name -> "Worker", [ DString name ]
//         | Handler.Spec.Cron(name, interval) ->
//           "Cron", [ DString name; CronInterval.toDT interval ]
//         | Handler.Spec.REPL name -> "REPL", [ DString name ]

//       Dval.enum (rtTyp [ "Handler" ] "Spec" 0) name fields

//     let fromDT (d : Dval) : Handler.Spec =
//       match d with
//       | DEnum(_, _, "HTTP", [ DString route; DString method ]) ->
//         Handler.Spec.HTTP(route, method)
//       | DEnum(_, _, "Worker", [ DString name ]) -> Handler.Spec.Worker(name)
//       | DEnum(_, _, "Cron", [ DString name; interval ]) ->
//         Handler.Spec.Cron(name, CronInterval.fromDT interval)
//       | DEnum(_, _, "REPL", [ DString name ]) -> Handler.Spec.REPL(name)
//       | _ -> Exception.raiseInternal "Invalid Spec" []

//   let toDT (h : Handler.T) : Dval =
//     Dval.record
//       (rtTyp [ "Handler" ] "T" 0)
//       [ "tlid", DInt(int64 h.tlid)
//         "ast", Expr.toDT h.ast
//         "spec", Spec.toDT h.spec ]

//   let fromDT (d : Dval) : Handler.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.uint64
//       let ast = fields |> D.field "ast" |> Expr.fromDT
//       let spec = fields |> D.field "spec" |> Spec.fromDT

//       { tlid = tlid; ast = ast; spec = spec }

//     | _ -> Exception.raiseInternal "Invalid Handler" []


// module DB =
//   let toDT (db : DB.T) : Dval =
//     Dval.record
//       (rtTyp [ "DB" ] "T" 0)
//       [ "tlid", DInt(int64 db.tlid)
//         "name", DString db.name
//         "version", DInt db.version
//         "typ", TypeReference.toDT db.typ ]

//   let fromDT (d : Dval) : DB.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.tlid
//       let name = fields |> D.field "name" |> D.string
//       let version = fields |> D.field "version" |> D.int
//       let typ = fields |> D.field "typ" |> TypeReference.fromDT
//       { tlid = tlid; name = name; version = version; typ = typ }

//     | _ -> Exception.raiseInternal "Invalid DB" []


// module UserType =
//   let toDT (userType : UserType.T) : Dval =
//     Dval.record
//       (rtTyp [ "UserType" ] "T" 0)
//       [ "tlid", DInt(int64 userType.tlid)
//         "name", TypeName.UserProgram.toDT userType.name
//         "declaration", TypeDeclaration.toDT userType.declaration ]

//   let fromDT (d : Dval) : UserType.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.tlid
//       let name = fields |> D.field "name" |> TypeName.UserProgram.fromDT
//       let declaration = fields |> D.field "declaration" |> TypeDeclaration.fromDT

//       { tlid = tlid
//         name = name
//         declaration = declaration }

//     | _ -> Exception.raiseInternal "Invalid UserType" []


// module UserFunction =
//   module Parameter =
//     let toDT (p : UserFunction.Parameter) : Dval =
//       Dval.record
//         (rtTyp [ "UserFunction" ] "Parameter" 0)
//         [ "name", DString p.name
//           "typ", TypeReference.toDT p.typ ]

//     let fromDT (d : Dval) : UserFunction.Parameter =
//       match d with
//       | DRecord(_, _, fields) ->
//         let name = fields |> D.field "name" |> D.string
//         let typ = fields |> D.field "typ" |> TypeReference.fromDT

//         { name = name; typ = typ }

//       | _ -> Exception.raiseInternal "Invalid UserFunction.Parameter" []


//   let toDT (userFn : UserFunction.T) : Dval =
//     Dval.record
//       (rtTyp [ "UserFunction" ] "T" 0)
//       [ "tlid", DInt(int64 userFn.tlid)
//         "name", FnName.UserProgram.toDT userFn.name
//         "typeParams", DList(List.map DString userFn.typeParams)
//         "parameters", DList(List.map Parameter.toDT userFn.parameters)
//         "returnType", TypeReference.toDT userFn.returnType
//         "body", Expr.toDT userFn.body ]

//   let fromDT (d : Dval) : UserFunction.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.tlid
//       let name = fields |> D.field "name" |> FnName.UserProgram.fromDT
//       let typeParams = fields |> D.field "typeParams" |> D.stringList
//       let parameters = fields |> D.field "parameters" |> D.list |> List.map Parameter.fromDT
//       let returnType = fields |> D.field "returnType" |> TypeReference.fromDT
//       let body = fields |> D.field "body" |> Expr.fromDT

//       { tlid = tlid
//         name = name
//         typeParams = typeParams
//         parameters = parameters
//         returnType = returnType
//         body = body }

//     | _ -> Exception.raiseInternal "Invalid UserFunction" []

// module UserConstant =
//   let toDT (userConstant : UserConstant.T) : Dval =
//     Dval.record
//       (rtTyp [ "UserConstant" ] "T" 0)
//       [ "tlid", DInt(int64 userConstant.tlid)
//         "name", ConstantName.UserProgram.toDT userConstant.name
//         "body", Const.toDT userConstant.body ]

//   let fromDT (d : Dval) : UserConstant.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.tlid

//       let name = fields |> D.field "name" |> ConstantName.UserProgram.fromDT
//       let body = fields |> D.field "body" |> Const.fromDT

//       { tlid = tlid
//         name = name
//         body = body }

//     | _ -> Exception.raiseInternal "Invalid UserConstant" []

// module Secret =
//   let toDT (s : Secret.T) : Dval =
//     Dval.record
//       (rtTyp [ "Secret" ] "T" 0)
//       [ "name", DString s.name; "value", DString s.value; "version", DInt s.version ]

//   let fromDT (d : Dval) : Secret.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let name = fields |> D.field "name" |> D.string
//       let value = fields |> D.field "value" |> D.string
//       let version = fields |> D.field "version" |> D.int

//       { name = name; value = value; version = version }

//     | _ -> Exception.raiseInternal "Invalid Secret" []


// module PackageType =
//   let toDT (p : PackageType.T) : Dval =
//     Dval.record
//       (rtTyp [ "PackageType" ] "T" 0)
//       [ "name", TypeName.Package.toDT p.name
//         "declaration", TypeDeclaration.toDT p.declaration ]

//   let fromDT (d : Dval) : PackageType.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let name = fields |> D.field "name" |> TypeName.Package.fromDT
//       let declaration = fields |> D.field "declaration" |> TypeDeclaration.fromDT

//       { name = name
//         declaration = declaration }

//     | _ -> Exception.raiseInternal "Invalid PackageType" []


// module PackageFn =
//   module Parameter =
//     let toDT (p : PackageFn.Parameter) : Dval =
//       Dval.record
//         (rtTyp [ "PackageFn" ] "Parameter" 0)
//         [ "name", DString p.name
//           "typ", TypeReference.toDT p.typ ]

//     let fromDT (d : Dval) : PackageFn.Parameter =
//       match d with
//       | DRecord(_, _, fields) ->
//         let name = fields |> D.field "name" |> D.string
//         let typ = fields |> D.field "typ" |> TypeReference.fromDT

//         { name = name; typ = typ }

//       | _ -> Exception.raiseInternal "Invalid PackageFn.Parameter" []

//   let toDT (p : PackageFn.T) : Dval =
//     Dval.record
//       (rtTyp [ "PackageFn" ] "T" 0)
//       [ "tlid", DInt(int64 p.tlid)
//         "name", FnName.Package.toDT p.name
//         "body", Expr.toDT p.body
//         "typeParams", DList(List.map DString p.typeParams)
//         "parameters", DList(List.map Parameter.toDT p.parameters)
//         "returnType", TypeReference.toDT p.returnType ]

//   let fromDT (d : Dval) : PackageFn.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.uint64
//       let name = fields |> D.field "name" |> FnName.Package.fromDT
//       let body = fields |> D.field "body" |> Expr.fromDT
//       let typeParams = fields |> D.field "typeParams" |> D.stringList

//       let parameters = fields |> D.field "parameters" |> D.list |> List.map Parameter.fromDT
//       let returnType = fields |> D.field "returnType" |> TypeReference.fromDT

//       { tlid = tlid
//         name = name
//         body = body
//         typeParams = typeParams
//         parameters = parameters
//         returnType = returnType }

//     | _ -> Exception.raiseInternal "Invalid PackageFn" []

// module PackageConstant =
//   let toDT (p : PackageConstant.T) : Dval =
//     Dval.record
//       (rtTyp [ "PackageConstant" ] "T" 0)
//       [ "name", ConstantName.Package.toDT p.name
//         "body", Const.toDT p.body ]

//   let fromDT (d : Dval) : PackageConstant.T =
//     match d with
//     | DRecord(_, _, fields) ->
//       let tlid = fields |> D.field "tlid" |> D.uint64
//       let id = fields |> D.uuid "id"
//       let name = fields |> D.field "name" |> ConstantName.Package.fromDT
//       let body = fields |> D.field "body" |> Const.fromDT

//       { name = name
//         body = body }

//     | _ -> Exception.raiseInternal "Invalid PackageConstant" []
//     | DEnum(_, _, "EAnd", [ DInt id; left; right ]) ->
//     | DEnum(_, _, "EAnd", [ DInt id; left; right ]) ->
