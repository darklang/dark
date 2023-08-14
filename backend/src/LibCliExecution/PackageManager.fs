module LibCliExecution.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

open System

module LanguageToolsTypesFork =
  type ID = uint64
  type TLID = uint64

  type Sign =
    | Positive
    | Negative

  module NameResolution =
    type ErrorType =
      | NotFound
      | MissingModuleName
      | InvalidPackageName

    type NameType =
      | Function
      | Type
      | Constant

    type Error = { errorType : ErrorType; nameType : NameType; names : List<string> }

  module ProgramTypes =
    type NameResolution<'a> = Result<'a, NameResolution.Error>

    module FQName =
      type BuiltIn<'name> = { modules : List<string>; name : 'name; version : int }

      type Package<'name> =
        { owner : string; modules : List<string>; name : 'name; version : int }

      type T<'name> =
        | BuiltIn of BuiltIn<'name>
        | Package of Package<'name>

    module TypeName =
      type Name = TypeName of String
      type T = FQName.T<Name>
      type BuiltIn = FQName.BuiltIn<Name>
      type Package = FQName.Package<Name>

    module FnName =
      type Name = FnName of String
      type T = FQName.T<Name>
      type BuiltIn = FQName.BuiltIn<Name>
      type Package = FQName.Package<Name>

    module ConstantName =
      type Name = ConstantName of string
      type T = FQName.T<Name>
      type BuiltIn = FQName.BuiltIn<Name>
      type Package = FQName.Package<Name>



    type TypeReference =
      | TVariable of String
      | TUnit
      | TBool
      | TInt
      | TFloat
      | TChar
      | TString
      | TDateTime
      | TUuid
      | TBytes
      | TPassword
      | TList of TypeReference
      | TTuple of TypeReference * TypeReference * List<TypeReference>
      | TDict of TypeReference
      | TCustomType of NameResolution<TypeName.T> * typeArgs : List<TypeReference>
      | TDB of TypeReference
      | TFn of NEList<TypeReference> * TypeReference

    type LetPattern =
      | LPVariable of ID * name : String
      | LPTuple of ID * LetPattern * LetPattern * List<LetPattern>

    type MatchPattern =
      | MPVariable of ID * String
      | MPUnit of ID
      | MPBool of ID * bool
      | MPInt of ID * int
      | MPFloat of ID * Sign * String * String
      | MPChar of ID * String
      | MPString of ID * String
      | MPList of ID * List<MatchPattern>
      | MPListCons of ID * head : MatchPattern * tail : MatchPattern
      | MPTuple of ID * MatchPattern * MatchPattern * List<MatchPattern>
      | MPEnum of ID * caseName : String * fieldPats : List<MatchPattern>

    type BinaryOperation =
      | BinOpAnd
      | BinOpOr

    type InfixFnName =
      | ArithmeticPlus
      | ArithmeticMinus
      | ArithmeticMultiply
      | ArithmeticDivide
      | ArithmeticModulo
      | ArithmeticPower
      | ComparisonGreaterThan
      | ComparisonGreaterThanOrEqual
      | ComparisonLessThan
      | ComparisonLessThanOrEqual
      | ComparisonEquals
      | ComparisonNotEquals
      | StringConcat

    type Infix =
      | InfixFnCall of InfixFnName
      | BinOp of BinaryOperation

    type StringSegment =
      | StringText of string
      | StringInterpolation of Expr

    and PipeExpr =
      | EPipeVariable of ID * string
      | EPipeLambda of ID * NEList<ID * string> * Expr
      | EPipeInfix of ID * Infix * Expr
      | EPipeFnCall of
        ID *
        NameResolution<FnName.T> *
        typeArgs : List<TypeReference> *
        args : List<Expr>
      | EPipeEnum of
        ID *
        typeName : NameResolution<TypeName.T> *
        caseName : String *
        fields : List<Expr>


    and Expr =
      | EUnit of ID

      | EBool of ID * bool
      | EInt of ID * int
      | EFloat of ID * Sign * string * string
      | EChar of ID * string
      | EString of ID * List<StringSegment>

      | EConstant of ID * NameResolution<ConstantName.T>

      | EList of ID * List<Expr>
      | EDict of ID * List<String * Expr>
      | ETuple of ID * Expr * Expr * List<Expr>
      | ERecord of ID * NameResolution<TypeName.T> * List<string * Expr>
      | ERecordUpdate of ID * record : Expr * updates : NEList<String * Expr>
      | EEnum of
        ID *
        typeName : NameResolution<TypeName.T> *
        caseName : String *
        fields : List<Expr>

      | ELet of ID * LetPattern * Expr * Expr
      | EFieldAccess of ID * Expr * String
      | EVariable of ID * String

      | EIf of ID * cond : Expr * thenExpr : Expr * elseExpr : option<Expr>
      | EMatch of ID * arg : Expr * cases : List<MatchPattern * Expr>
      | EPipe of ID * Expr * List<PipeExpr>

      | EInfix of ID * Infix * Expr * Expr
      | ELambda of ID * NEList<ID * string> * Expr
      | EApply of ID * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
      | EFnName of ID * NameResolution<FnName.T>

    type Deprecation<'name> =
      | NotDeprecated
      | RenamedTo of 'name
      | ReplacedBy of 'name
      | DeprecatedBecause of string

    module TypeDeclaration =
      type RecordField = { name : string; typ : TypeReference; description : string }

      type EnumField =
        { typ : TypeReference; label : Option<string>; description : string }

      type EnumCase =
        { name : string; fields : List<EnumField>; description : string }

      type Definition =
        | Alias of TypeReference
        | Record of NEList<RecordField>
        | Enum of NEList<EnumCase>

      type T = { typeParams : List<string>; definition : Definition }


    module PackageType =
      type T =
        { tlid : TLID
          id : Guid
          name : TypeName.Package
          declaration : TypeDeclaration.T
          description : string
          deprecated : Deprecation<TypeName.T> }


    module PackageFn =
      type Parameter = { name : String; typ : TypeReference; description : String }

      type T =
        { tlid : TLID
          id : Guid
          name : FnName.Package
          body : Expr
          typeParams : List<String>
          parameters : NEList<Parameter>
          returnType : TypeReference
          description : String
          deprecated : Deprecation<FnName.T> }

    type Const =
      | CInt of int64
      | CBool of bool
      | CString of string
      | CChar of string
      | CFloat of Sign * string * string
      | CUnit
      | CTuple of first : Const * second : Const * rest : List<Const>
      | CEnum of NameResolution<TypeName.T> * caseName : string * List<Const>

    module PackageConstant =
      type T =
        { tlid : TLID
          id : Guid
          name : ConstantName.Package
          description : String
          deprecated : Deprecation<ConstantName.T>
          body : Const }

module ET = LanguageToolsTypesFork
module EPT = ET.ProgramTypes

module ExternalTypesToProgramTypes =
  module NameResolution =
    module NameType =

      let toPT
        (nameType : ET.NameResolution.NameType)
        : LibExecution.Errors.NameResolution.NameType =
        match nameType with
        | ET.NameResolution.Type -> LibExecution.Errors.NameResolution.Type
        | ET.NameResolution.Function -> LibExecution.Errors.NameResolution.Function
        | ET.NameResolution.Constant -> LibExecution.Errors.NameResolution.Constant

    module ErrorType =
      let toPT
        (err : ET.NameResolution.ErrorType)
        : LibExecution.Errors.NameResolution.ErrorType =
        match err with
        | ET.NameResolution.ErrorType.NotFound ->
          LibExecution.Errors.NameResolution.NotFound
        | ET.NameResolution.MissingModuleName ->
          LibExecution.Errors.NameResolution.MissingModuleName
        | ET.NameResolution.InvalidPackageName ->
          LibExecution.Errors.NameResolution.InvalidPackageName

    module Error =
      let toPT
        (err : ET.NameResolution.Error)
        : LibExecution.Errors.NameResolution.Error =
        { errorType = ErrorType.toPT err.errorType
          nameType = NameType.toPT err.nameType
          names = err.names }

    let toPT
      (f : 's -> 'p)
      (result : EPT.NameResolution<'s>)
      : PT.NameResolution<'p> =
      match result with
      | Ok name -> Ok(f name)
      | Error err -> Error(Error.toPT err)


  module Sign =
    let toPT (s : ET.Sign) : Sign =
      match s with
      | ET.Positive -> Positive
      | ET.Negative -> Negative

  module TypeName =
    module BuiltIn =
      let toPT (b : EPT.TypeName.BuiltIn) : PT.TypeName.BuiltIn =
        { modules = b.modules
          name =
            match b.name with
            | EPT.TypeName.Name.TypeName name -> PT.TypeName.TypeName name
          version = b.version }

    module Package =
      let toPT (p : EPT.TypeName.Package) : PT.TypeName.Package =
        { owner = p.owner
          modules = p.modules
          name =
            match p.name with
            | EPT.TypeName.Name.TypeName name -> PT.TypeName.TypeName name
          version = p.version }

    let toPT (fqfn : EPT.TypeName.T) : PT.TypeName.T =
      match fqfn with
      | EPT.FQName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
      | EPT.FQName.Package p -> PT.FQName.Package(Package.toPT p)


  module FnName =
    module BuiltIn =
      let toPT (b : EPT.FnName.BuiltIn) : PT.FnName.BuiltIn =
        { modules = b.modules
          name =
            match b.name with
            | EPT.FnName.Name.FnName name -> PT.FnName.FnName name
          version = b.version }

    module Package =
      let toPT (p : EPT.FnName.Package) : PT.FnName.Package =
        { owner = p.owner
          modules = p.modules
          name =
            match p.name with
            | EPT.FnName.Name.FnName name -> PT.FnName.FnName name
          version = p.version }

    let toPT (fqfn : EPT.FnName.T) : PT.FnName.T =
      match fqfn with
      | EPT.FQName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
      | EPT.FQName.Package p -> PT.FQName.Package(Package.toPT p)

  module ConstantName =
    module BuiltIn =
      let toPT (b : EPT.ConstantName.BuiltIn) : PT.ConstantName.BuiltIn =
        { modules = b.modules
          name =
            match b.name with
            | EPT.ConstantName.Name.ConstantName name ->
              PT.ConstantName.ConstantName name
          version = b.version }

    module Package =
      let toPT (p : EPT.ConstantName.Package) : PT.ConstantName.Package =
        { owner = p.owner
          modules = p.modules
          name =
            match p.name with
            | EPT.ConstantName.Name.ConstantName name ->
              PT.ConstantName.ConstantName name
          version = p.version }

    let toPT (fqfn : EPT.ConstantName.T) : PT.ConstantName.T =
      match fqfn with
      | EPT.FQName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
      | EPT.FQName.Package p -> PT.FQName.Package(Package.toPT p)



  module InfixFnName =
    let toPT (name : ET.ProgramTypes.InfixFnName) : PT.InfixFnName =
      match name with
      | EPT.ArithmeticPlus -> PT.ArithmeticPlus
      | EPT.ArithmeticMinus -> PT.ArithmeticMinus
      | EPT.ArithmeticMultiply -> PT.ArithmeticMultiply
      | EPT.ArithmeticDivide -> PT.ArithmeticDivide
      | EPT.ArithmeticModulo -> PT.ArithmeticModulo
      | EPT.ArithmeticPower -> PT.ArithmeticPower
      | EPT.ComparisonGreaterThan -> PT.ComparisonGreaterThan
      | EPT.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
      | EPT.ComparisonLessThan -> PT.ComparisonLessThan
      | EPT.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
      | EPT.ComparisonEquals -> PT.ComparisonEquals
      | EPT.ComparisonNotEquals -> PT.ComparisonNotEquals
      | EPT.StringConcat -> PT.StringConcat

  module TypeReference =
    let rec toPT (t : EPT.TypeReference) : PT.TypeReference =
      match t with
      | EPT.TInt -> PT.TInt
      | EPT.TFloat -> PT.TFloat
      | EPT.TBool -> PT.TBool
      | EPT.TUnit -> PT.TUnit
      | EPT.TString -> PT.TString
      | EPT.TList typ -> PT.TList(toPT typ)
      | EPT.TTuple(firstType, secondType, otherTypes) ->
        PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
      | EPT.TDict typ -> PT.TDict(toPT typ)
      | EPT.TDB typ -> PT.TDB(toPT typ)
      | EPT.TDateTime -> PT.TDateTime
      | EPT.TChar -> PT.TChar
      | EPT.TPassword -> PT.TPassword
      | EPT.TUuid -> PT.TUuid
      | EPT.TCustomType(t, typeArgs) ->
        PT.TCustomType(NameResolution.toPT TypeName.toPT t, List.map toPT typeArgs)
      | EPT.TBytes -> PT.TBytes
      | EPT.TVariable(name) -> PT.TVariable(name)
      | EPT.TFn(paramTypes, returnType) ->
        PT.TFn(NEList.map toPT paramTypes, toPT returnType)

  module BinaryOperation =
    let toPT (binop : EPT.BinaryOperation) : PT.BinaryOperation =
      match binop with
      | EPT.BinOpAnd -> PT.BinOpAnd
      | EPT.BinOpOr -> PT.BinOpOr

  module Infix =
    let toPT (infix : EPT.Infix) : PT.Infix =
      match infix with
      | EPT.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
      | EPT.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

  module LetPattern =
    let rec toPT (p : EPT.LetPattern) : PT.LetPattern =
      match p with
      | EPT.LPVariable(id, str) -> PT.LPVariable(id, str)
      | EPT.LPTuple(id, first, second, theRest) ->
        PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

  module MatchPattern =
    let rec toPT (p : EPT.MatchPattern) : PT.MatchPattern =
      match p with
      | EPT.MPVariable(id, str) -> PT.MPVariable(id, str)
      | EPT.MPEnum(id, caseName, fieldPats) ->
        PT.MPEnum(id, caseName, List.map toPT fieldPats)
      | EPT.MPInt(id, i) -> PT.MPInt(id, i)
      | EPT.MPBool(id, b) -> PT.MPBool(id, b)
      | EPT.MPChar(id, c) -> PT.MPChar(id, c)
      | EPT.MPString(id, s) -> PT.MPString(id, s)
      | EPT.MPFloat(id, s, w, f) -> PT.MPFloat(id, Sign.toPT s, w, f)
      | EPT.MPUnit id -> PT.MPUnit id
      | EPT.MPTuple(id, first, second, theRest) ->
        PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
      | EPT.MPList(id, pats) -> PT.MPList(id, List.map toPT pats)
      | EPT.MPListCons(id, head, tail) -> PT.MPListCons(id, toPT head, toPT tail)


  module Expr =
    let rec toPT (e : EPT.Expr) : PT.Expr =
      match e with
      | EPT.EChar(id, char) -> PT.EChar(id, char)
      | EPT.EInt(id, num) -> PT.EInt(id, num)
      | EPT.EString(id, segment) ->
        PT.EString(id, List.map stringSegmentToPT segment)
      | EPT.EFloat(id, sign, whole, fraction) ->
        PT.EFloat(id, Sign.toPT sign, whole, fraction)
      | EPT.EBool(id, b) -> PT.EBool(id, b)
      | EPT.EUnit id -> PT.EUnit id
      | EPT.EConstant(id, name) ->
        PT.EConstant(id, NameResolution.toPT ConstantName.toPT name)
      | EPT.EVariable(id, var) -> PT.EVariable(id, var)
      | EPT.EFieldAccess(id, obj, fieldname) ->
        PT.EFieldAccess(id, toPT obj, fieldname)
      | EPT.EApply(id, name, typeArgs, args) ->
        PT.EApply(
          id,
          toPT name,
          List.map TypeReference.toPT typeArgs,
          NEList.map toPT args
        )
      | EPT.ELambda(id, vars, body) -> PT.ELambda(id, vars, toPT body)
      | EPT.ELet(id, pat, rhs, body) ->
        PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
      | EPT.EIf(id, cond, thenExpr, elseExpr) ->
        PT.EIf(id, toPT cond, toPT thenExpr, Option.map toPT elseExpr)
      | EPT.EList(id, exprs) -> PT.EList(id, List.map toPT exprs)
      | EPT.ETuple(id, first, second, theRest) ->
        PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
      | EPT.ERecord(id, typeName, fields) ->
        PT.ERecord(
          id,
          NameResolution.toPT TypeName.toPT typeName,
          List.map (Tuple2.mapSecond toPT) fields
        )
      | EPT.ERecordUpdate(id, record, updates) ->
        PT.ERecordUpdate(
          id,
          toPT record,
          updates |> NEList.map (fun (name, expr) -> (name, toPT expr))
        )
      | EPT.EPipe(pipeID, expr1, rest) ->
        PT.EPipe(pipeID, toPT expr1, List.map pipeExprToPT rest)
      | EPT.EEnum(id, typeName, caseName, exprs) ->
        PT.EEnum(
          id,
          NameResolution.toPT TypeName.toPT typeName,
          caseName,
          List.map toPT exprs
        )
      | EPT.EMatch(id, mexpr, pairs) ->
        PT.EMatch(
          id,
          toPT mexpr,
          List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
        )
      | EPT.EInfix(id, infix, arg1, arg2) ->
        PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
      | EPT.EDict(id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)
      | EPT.EFnName(id, name) -> PT.EFnName(id, NameResolution.toPT FnName.toPT name)

    and stringSegmentToPT (segment : EPT.StringSegment) : PT.StringSegment =
      match segment with
      | EPT.StringText text -> PT.StringText text
      | EPT.StringInterpolation expr -> PT.StringInterpolation(toPT expr)

    and pipeExprToPT (pipeExpr : EPT.PipeExpr) : PT.PipeExpr =
      match pipeExpr with
      | EPT.EPipeVariable(id, name) -> PT.EPipeVariable(id, name)
      | EPT.EPipeLambda(id, args, body) -> PT.EPipeLambda(id, args, toPT body)
      | EPT.EPipeInfix(id, infix, first) ->
        PT.EPipeInfix(id, Infix.toPT infix, toPT first)
      | EPT.EPipeFnCall(id, fnName, typeArgs, args) ->
        PT.EPipeFnCall(
          id,
          NameResolution.toPT FnName.toPT fnName,
          List.map TypeReference.toPT typeArgs,
          List.map toPT args
        )
      | EPT.EPipeEnum(id, typeName, caseName, fields) ->
        PT.EPipeEnum(
          id,
          NameResolution.toPT TypeName.toPT typeName,
          caseName,
          List.map toPT fields
        )

  module Deprecation =
    type Deprecation<'name> =
      | NotDeprecated
      | RenamedTo of 'name
      | ReplacedBy of 'name
      | DeprecatedBecause of string

    let toPT
      (f : 'name1 -> 'name2)
      (d : EPT.Deprecation<'name1>)
      : PT.Deprecation<'name2> =
      match d with
      | EPT.NotDeprecated -> PT.NotDeprecated
      | EPT.RenamedTo name -> PT.RenamedTo(f name)
      | EPT.ReplacedBy name -> PT.ReplacedBy(f name)
      | EPT.DeprecatedBecause reason -> PT.DeprecatedBecause reason

  module TypeDeclaration =
    module RecordField =
      let toPT
        (f : EPT.TypeDeclaration.RecordField)
        : PT.TypeDeclaration.RecordField =
        { name = f.name
          typ = TypeReference.toPT f.typ
          description = f.description }

    module EnumField =
      let toPT (f : EPT.TypeDeclaration.EnumField) : PT.TypeDeclaration.EnumField =
        { typ = TypeReference.toPT f.typ
          label = f.label
          description = f.description }

    module EnumCase =
      let toPT (c : EPT.TypeDeclaration.EnumCase) : PT.TypeDeclaration.EnumCase =
        { name = c.name
          fields = List.map EnumField.toPT c.fields
          description = c.description }

    module Definition =
      let toPT (d : EPT.TypeDeclaration.Definition) : PT.TypeDeclaration.Definition =
        match d with
        | EPT.TypeDeclaration.Alias typ ->
          PT.TypeDeclaration.Alias(TypeReference.toPT typ)
        | EPT.TypeDeclaration.Record fields ->
          PT.TypeDeclaration.Record(NEList.map RecordField.toPT fields)
        | EPT.TypeDeclaration.Enum cases ->
          PT.TypeDeclaration.Enum(NEList.map EnumCase.toPT cases)

    let toPT (d : EPT.TypeDeclaration.T) : PT.TypeDeclaration.T =
      { typeParams = d.typeParams; definition = Definition.toPT d.definition }

  module PackageFn =
    module Parameter =
      let toPT (p : EPT.PackageFn.Parameter) : PT.PackageFn.Parameter =
        { name = p.name
          typ = TypeReference.toPT p.typ
          description = p.description }

    let toPT (fn : EPT.PackageFn.T) : PT.PackageFn.T =
      { name = FnName.Package.toPT fn.name
        parameters = NEList.map Parameter.toPT fn.parameters
        returnType = TypeReference.toPT fn.returnType
        description = fn.description
        deprecated = Deprecation.toPT FnName.toPT fn.deprecated
        body = Expr.toPT fn.body
        typeParams = fn.typeParams
        id = fn.id
        tlid = fn.tlid }

  module PackageType =
    let toPT (pt : EPT.PackageType.T) : PT.PackageType.T =
      { name = TypeName.Package.toPT pt.name
        description = pt.description
        declaration = TypeDeclaration.toPT pt.declaration
        deprecated = Deprecation.toPT TypeName.toPT pt.deprecated
        id = pt.id
        tlid = pt.tlid }

  module Const =
    let rec toPT (c : EPT.Const) : PT.Const =
      match c with
      | EPT.CInt i -> PT.CInt i
      | EPT.CBool b -> PT.CBool b
      | EPT.CString s -> PT.CString s
      | EPT.CChar c -> PT.CChar c
      | EPT.CFloat(s, w, f) -> PT.CFloat(Sign.toPT s, w, f)
      | EPT.CUnit -> PT.CUnit
      | EPT.CTuple(first, second, rest) ->
        PT.CTuple(toPT first, toPT second, List.map toPT rest)
      | EPT.CEnum(typeName, caseName, fields) ->
        PT.CEnum(
          NameResolution.toPT TypeName.toPT typeName,
          caseName,
          List.map toPT fields
        )

  module PackageConstant =
    let toPT (c : EPT.PackageConstant.T) : PT.PackageConstant.T =
      { name = ConstantName.Package.toPT c.name
        description = c.description
        deprecated = Deprecation.toPT ConstantName.toPT c.deprecated
        id = c.id
        tlid = c.tlid
        body = Const.toPT c.body }



module ET2PT = ExternalTypesToProgramTypes

let cachedPly (expiration : TimeSpan) (f : Ply<'a>) : Ply<'a> =
  let mutable value = None
  let mutable lastUpdate = DateTime.MinValue

  uply {
    match value, DateTime.Now - lastUpdate > expiration with
    | Some value, false -> return value
    | _ ->
      let! newValue = f
      value <- Some newValue
      lastUpdate <- DateTime.Now
      return newValue
  }

// TODO: fetch one by one
// TODO: copy back to LibCloud/LibCloudExecution, or relocate somewhere central
// TODO: what should we do when the shape of types at the corresponding endpoints change?
let packageManager : RT.PackageManager =
  let httpClient = new System.Net.Http.HttpClient()

  let allTypes =
    uply {
      let! response =
        httpClient.GetAsync "http://dark-packages.dlio.localhost:11003/types"
      let! allTypesStr = response.Content.ReadAsStringAsync()

      return
        Json.Vanilla.deserialize<List<LanguageToolsTypesFork.ProgramTypes.PackageType.T>>
          allTypesStr
        |> List.map (ET2PT.PackageType.toPT >> PT2RT.PackageType.toRT)
    }
    |> cachedPly (TimeSpan.FromMinutes 1.)

  let allFns =
    uply {
      let! response =
        httpClient.GetAsync "http://dark-packages.dlio.localhost:11003/functions"
      let! allTypesStr = response.Content.ReadAsStringAsync()

      return
        Json.Vanilla.deserialize<List<LanguageToolsTypesFork.ProgramTypes.PackageFn.T>>
          allTypesStr
        |> List.map (ET2PT.PackageFn.toPT >> PT2RT.PackageFn.toRT)
    }
    |> cachedPly (TimeSpan.FromMinutes 1.)

  let getAllConstants =
    uply {
      let! response =
        httpClient.GetAsync "http://dark-packages.dlio.localhost:11003/constants"
      let! allConstantsStr = response.Content.ReadAsStringAsync()

      let parsedConstants =
        Json.Vanilla.deserialize<List<LanguageToolsTypesFork.ProgramTypes.PackageConstant.T>>
          allConstantsStr

      return
        parsedConstants
        |> List.map (ET2PT.PackageConstant.toPT >> PT2RT.PackageConstant.toRT)
    }

  { getType =
      fun typeName ->
        uply {
          let! allTypes = allTypes
          let found =
            List.find (fun (typ : RT.PackageType.T) -> typ.name = typeName) allTypes
          return found
        }

    getFn =
      fun fnName ->
        uply {
          let! allFns = allFns
          let found =
            List.find (fun (typ : RT.PackageFn.T) -> typ.name = fnName) allFns
          return found
        }

    getConstant =
      fun constantName ->
        uply {
          let! allConstants = getAllConstants
          let found =
            List.find
              (fun (typ : RT.PackageConstant.T) -> typ.name = constantName)
              allConstants
          return found
        }

    init = uply { return () } }
