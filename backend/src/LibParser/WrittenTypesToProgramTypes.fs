/// Conversion functions from WrittenTypes to ProgramTypes
module LibParser.WrittenTypesToProgramTypes

open Prelude

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module FS2WT = FSharpToWrittenTypes
module NRE = LibExecution.NameResolutionError
module NR = NameResolver

module InfixFnName =
  let toPT (name : WT.InfixFnName) : PT.InfixFnName =
    match name with
    | WT.ArithmeticPlus -> PT.ArithmeticPlus
    | WT.ArithmeticMinus -> PT.ArithmeticMinus
    | WT.ArithmeticMultiply -> PT.ArithmeticMultiply
    | WT.ArithmeticDivide -> PT.ArithmeticDivide
    | WT.ArithmeticModulo -> PT.ArithmeticModulo
    | WT.ArithmeticPower -> PT.ArithmeticPower
    | WT.ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | WT.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | WT.ComparisonLessThan -> PT.ComparisonLessThan
    | WT.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | WT.ComparisonEquals -> PT.ComparisonEquals
    | WT.ComparisonNotEquals -> PT.ComparisonNotEquals
    | WT.StringConcat -> PT.StringConcat

module TypeReference =
  let rec toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (t : WT.TypeReference)
    : Ply<PT.TypeReference> =
    let toPT = toPT pm onMissing currentModule
    uply {
      match t with
      | WT.TUnit -> return PT.TUnit
      | WT.TBool -> return PT.TBool
      | WT.TInt64 -> return PT.TInt64
      | WT.TUInt64 -> return PT.TUInt64
      | WT.TInt8 -> return PT.TInt8
      | WT.TUInt8 -> return PT.TUInt8
      | WT.TInt16 -> return PT.TInt16
      | WT.TUInt16 -> return PT.TUInt16
      | WT.TInt32 -> return PT.TInt32
      | WT.TUInt32 -> return PT.TUInt32
      | WT.TInt128 -> return PT.TInt128
      | WT.TUInt128 -> return PT.TUInt128
      | WT.TFloat -> return PT.TFloat
      | WT.TChar -> return PT.TChar
      | WT.TString -> return PT.TString
      | WT.TDateTime -> return PT.TDateTime
      | WT.TUuid -> return PT.TUuid

      | WT.TList typ -> return! toPT typ |> Ply.map PT.TList

      | WT.TTuple(firstType, secondType, otherTypes) ->
        let! firstType = toPT firstType
        let! secondType = toPT secondType
        let! otherTypes = Ply.List.mapSequentially toPT otherTypes
        return PT.TTuple(firstType, secondType, otherTypes)

      | WT.TDict typ -> return! toPT typ |> Ply.map PT.TDict

      | WT.TCustomType(t, typeArgs) ->
        let! t = NR.resolveTypeName pm onMissing currentModule t
        let! typeArgs = Ply.List.mapSequentially toPT typeArgs
        return PT.TCustomType(t, typeArgs)

      | WT.TFn(paramTypes, returnType) ->
        let! paramTypes = Ply.NEList.mapSequentially toPT paramTypes
        let! returnType = toPT returnType
        return PT.TFn(paramTypes, returnType)

      | WT.TDB typ -> return! toPT typ |> Ply.map PT.TDB

      | WT.TVariable(name) -> return PT.TVariable(name)
    }

module BinaryOperation =
  let toPT (binop : WT.BinaryOperation) : PT.BinaryOperation =
    match binop with
    | WT.BinOpAnd -> PT.BinOpAnd
    | WT.BinOpOr -> PT.BinOpOr

module Infix =
  let toPT (infix : WT.Infix) : PT.Infix =
    match infix with
    | WT.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
    | WT.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

module LetPattern =
  let rec toPT (p : WT.LetPattern) : PT.LetPattern =
    match p with
    | WT.LPVariable(id, str) -> PT.LPVariable(id, str)
    | WT.LPTuple(id, first, second, theRest) ->
      PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)
    | WT.LPUnit id -> PT.LPUnit id

module MatchPattern =
  let rec toPT (p : WT.MatchPattern) : PT.MatchPattern =
    match p with
    | WT.MPVariable(id, str) -> PT.MPVariable(id, str)
    | WT.MPEnum(id, caseName, fieldPats) ->
      PT.MPEnum(id, caseName, List.map toPT fieldPats)
    | WT.MPInt64(id, i) -> PT.MPInt64(id, i)
    | WT.MPUInt64(id, i) -> PT.MPUInt64(id, i)
    | WT.MPInt8(id, i) -> PT.MPInt8(id, i)
    | WT.MPUInt8(id, i) -> PT.MPUInt8(id, i)
    | WT.MPInt16(id, i) -> PT.MPInt16(id, i)
    | WT.MPUInt16(id, i) -> PT.MPUInt16(id, i)
    | WT.MPInt32(id, i) -> PT.MPInt32(id, i)
    | WT.MPUInt32(id, i) -> PT.MPUInt32(id, i)
    | WT.MPInt128(id, i) -> PT.MPInt128(id, i)
    | WT.MPUInt128(id, i) -> PT.MPUInt128(id, i)
    | WT.MPBool(id, b) -> PT.MPBool(id, b)
    | WT.MPChar(id, c) -> PT.MPChar(id, c)
    | WT.MPString(id, s) -> PT.MPString(id, s)
    | WT.MPFloat(id, s, w, f) -> PT.MPFloat(id, s, w, f)
    | WT.MPUnit id -> PT.MPUnit id
    | WT.MPTuple(id, first, second, theRest) ->
      PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
    | WT.MPList(id, pats) -> PT.MPList(id, List.map toPT pats)
    | WT.MPListCons(id, head, tail) -> PT.MPListCons(id, toPT head, toPT tail)


module Expr =
  let resolveTypeName
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (names : List<string>)
    (caseName : string) // used for errors
    : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
    match names with
    | [] ->
      Ply(
        Error(
          { nameType = NRE.Type; errorType = NRE.MissingEnumModuleName caseName }
        )
      )
    | head :: tail ->
      let name = NEList.ofList head tail |> WT.Unresolved
      NR.resolveTypeName pm onMissing currentModule name

  let rec toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (e : WT.Expr)
    : Ply<PT.Expr> =
    let toPT = toPT builtins pm onMissing currentModule
    uply {
      match e with
      | WT.EChar(id, char) -> return PT.EChar(id, char)
      | WT.EInt64(id, num) -> return PT.EInt64(id, num)
      | WT.EUInt64(id, num) -> return PT.EUInt64(id, num)
      | WT.EInt8(id, num) -> return PT.EInt8(id, num)
      | WT.EUInt8(id, num) -> return PT.EUInt8(id, num)
      | WT.EInt16(id, num) -> return PT.EInt16(id, num)
      | WT.EUInt16(id, num) -> return PT.EUInt16(id, num)
      | WT.EInt32(id, num) -> return PT.EInt32(id, num)
      | WT.EUInt32(id, num) -> return PT.EUInt32(id, num)
      | WT.EInt128(id, num) -> return PT.EInt128(id, num)
      | WT.EUInt128(id, num) -> return PT.EUInt128(id, num)
      | WT.EString(id, segments) ->
        let! segments =
          Ply.List.mapSequentially
            (stringSegmentToPT builtins pm onMissing currentModule)
            segments
        return PT.EString(id, segments)
      | WT.EFloat(id, sign, whole, fraction) ->
        return PT.EFloat(id, sign, whole, fraction)
      | WT.EBool(id, b) -> return PT.EBool(id, b)
      | WT.EUnit id -> return PT.EUnit id
      | WT.EVariable(id, var) ->
        // This could be a UserConstant
        let! constant =
          NR.resolveConstantName
            (builtins.constants |> Map.keys |> Set)
            pm
            NR.OnMissing.Allow
            currentModule
            (WT.Unresolved(NEList.singleton var))
        match constant with
        | Ok _ as name -> return PT.EConstant(id, name)
        | Error _ -> return PT.EVariable(id, var)
      | WT.ERecordFieldAccess(id, obj, fieldname) ->
        let! obj = toPT obj
        return PT.ERecordFieldAccess(id, obj, fieldname)
      | WT.EApply(id, (WT.EFnName(_, name)), [], { head = WT.EPlaceHolder }) ->
        // There are no arguments, so this could be a function name or a constant
        let! fnName =
          NR.resolveFnName
            (builtins.fns |> Map.keys |> Set)
            pm
            NR.OnMissing.Allow
            currentModule
            name
        match fnName with
        | Ok _ as name -> return PT.EFnName(id, name)
        | Error _ ->
          let! name =
            NR.resolveConstantName
              (builtins.constants |> Map.keys |> Set)
              pm
              onMissing
              currentModule
              name
          return PT.EConstant(id, name)
      | WT.EApply(id, name, typeArgs, args) ->
        let! name = toPT name
        let! typeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            typeArgs
        let! args = Ply.NEList.mapSequentially toPT args

        return PT.EApply(id, name, typeArgs, args)
      | WT.EFnName(id, name) ->
        let! fnName =
          NR.resolveFnName
            (builtins.fns |> Map.keys |> Set)
            pm
            NR.OnMissing.Allow
            currentModule
            name
        match fnName, name with
        | Error _, WT.Unresolved { head = varName; tail = [] } when
          not (String.isCapitalized varName)
          ->
          // If it's a single name, and it's not resolved, treat it as a variable. If
          // it's not a variable, the "missing-varname" error will still be appropriate.
          return PT.EVariable(id, varName)
        | _, _ -> return PT.EFnName(id, fnName)
      | WT.ELambda(id, pats, body) ->
        let! body = toPT body
        return PT.ELambda(id, NEList.map LetPattern.toPT pats, body)
      | WT.ELet(id, pat, rhs, body) ->
        let! rhs = toPT rhs
        let! body = toPT body
        return PT.ELet(id, LetPattern.toPT pat, rhs, body)
      | WT.EIf(id, cond, thenExpr, elseExpr) ->
        let! cond = toPT cond
        let! thenExpr = toPT thenExpr
        let! elseExpr =
          uply {
            match elseExpr with
            | Some value ->
              let! newValue = toPT value
              return Some newValue
            | None -> return None
          }
        return PT.EIf(id, cond, thenExpr, elseExpr)
      | WT.EList(id, exprs) ->
        let! exprs = Ply.List.mapSequentially toPT exprs
        return PT.EList(id, exprs)
      | WT.ETuple(id, first, second, theRest) ->
        let! first = toPT first
        let! second = toPT second
        let! theRest = Ply.List.mapSequentially toPT theRest
        return PT.ETuple(id, first, second, theRest)
      | WT.ERecord(id, typeName, fields) ->
        let! typeName = NR.resolveTypeName pm onMissing currentModule typeName
        let! fields =
          Ply.List.mapSequentially
            (fun (fieldName, fieldExpr) ->
              uply {
                let! fieldExpr = toPT fieldExpr
                return (fieldName, fieldExpr)
              })
            fields
        return PT.ERecord(id, typeName, fields)
      | WT.ERecordUpdate(id, record, updates) ->
        let! record = toPT record
        let! updates =
          updates
          |> Ply.NEList.mapSequentially (fun (name, expr) ->
            uply {
              let! expr = toPT expr
              return (name, expr)
            })
        return PT.ERecordUpdate(id, record, updates)
      | WT.EPipe(pipeID, expr1, rest) ->
        let! expr1 = toPT expr1
        let! rest =
          Ply.List.mapSequentially
            (pipeExprToPT builtins pm onMissing currentModule)
            rest
        return PT.EPipe(pipeID, expr1, rest)
      | WT.EEnum(id, typeName, caseName, exprs) ->
        let! typeName = resolveTypeName pm onMissing currentModule typeName caseName
        let! exprs = Ply.List.mapSequentially toPT exprs
        return PT.EEnum(id, typeName, caseName, exprs)
      | WT.EMatch(id, mexpr, cases) ->
        let! mexpr = toPT mexpr
        let! cases =
          Ply.List.mapSequentially
            (fun (case : WT.MatchCase) ->
              uply {
                let mp = MatchPattern.toPT case.pat
                let! whenCondition =
                  uply {
                    match case.whenCondition with
                    | Some whenExpr ->
                      let! whenExpr = toPT whenExpr
                      return Some whenExpr
                    | None -> return None
                  }
                let! expr = toPT case.rhs
                let result : PT.MatchCase =
                  { pat = mp; whenCondition = whenCondition; rhs = expr }
                return result
              })
            cases

        return PT.EMatch(id, mexpr, cases)
      | WT.EInfix(id, infix, arg1, arg2) ->
        let! arg1 = toPT arg1
        let! arg2 = toPT arg2
        return PT.EInfix(id, Infix.toPT infix, arg1, arg2)
      | WT.EDict(id, pairs) ->
        let! pairs =
          Ply.List.mapSequentially
            (fun (key, value) ->
              uply {
                let! value = toPT value
                return (key, value)
              })
            pairs
        return PT.EDict(id, pairs)
      | WT.EPlaceHolder ->
        return Exception.raiseInternal "Invalid parse - placeholder not removed" []
    }

  and stringSegmentToPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (segment : WT.StringSegment)
    : Ply<PT.StringSegment> =
    match segment with
    | WT.StringText text -> Ply(PT.StringText text)
    | WT.StringInterpolation expr ->
      toPT builtins pm onMissing currentModule expr
      |> Ply.map (fun interpolated -> PT.StringInterpolation interpolated)

  and pipeExprToPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (pipeExpr : WT.PipeExpr)
    : Ply<PT.PipeExpr> =
    let toPT = toPT builtins pm onMissing currentModule

    uply {
      match pipeExpr with
      | WT.EPipeVariableOrFnCall(id, name) ->
        let! resolved =
          let asUserFnName = WT.Name.Unresolved(NEList.singleton name)
          NR.resolveFnName
            (builtins.fns |> Map.keys |> Set)
            pm
            NR.OnMissing.Allow
            currentModule
            asUserFnName

        return
          match resolved with
          | Ok name -> PT.EPipeFnCall(id, Ok name, [], [])
          | Error _ -> PT.EPipeVariable(id, name, [])

      | WT.EPipeLambda(id, pats, body) ->
        let! body = toPT body
        return PT.EPipeLambda(id, NEList.map LetPattern.toPT pats, body)

      | WT.EPipeInfix(id, infix, first) ->
        let! first = toPT first
        return PT.EPipeInfix(id, Infix.toPT infix, first)

      | WT.EPipeFnCall(id,
                       (WT.Unresolved { head = varName; tail = [] } as name),
                       [],
                       args) ->
        // Special case for variables with arguments. Since it could be a userfn, we
        // need to check that first. We do a similar thing converting EFnNames.
        let! fnName =
          NR.resolveFnName
            (builtins.fns |> Map.keys |> Set)
            pm
            NR.OnMissing.Allow
            currentModule
            name
        let! args = Ply.List.mapSequentially toPT args
        match fnName with
        | Ok name -> return PT.EPipeFnCall(id, Ok name, [], args)
        | Error _ -> return PT.EPipeVariable(id, varName, args)

      | WT.EPipeFnCall(id, name, typeArgs, args) ->
        let! fnName =
          NR.resolveFnName
            (builtins.fns |> Map.keys |> Set)
            pm
            onMissing
            currentModule
            name
        let! typeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            typeArgs
        let! args = Ply.List.mapSequentially toPT args
        return PT.EPipeFnCall(id, fnName, typeArgs, args)

      | WT.EPipeEnum(id, typeName, caseName, fields) ->
        let! typeName = resolveTypeName pm onMissing currentModule typeName caseName
        let! fields = Ply.List.mapSequentially toPT fields
        return PT.EPipeEnum(id, typeName, caseName, fields)
    }

module Const =
  let rec toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (c : WT.Const)
    : Ply<PT.Const> =
    let toPT = toPT pm onMissing currentModule
    uply {
      match c with
      | WT.CUnit -> return PT.CUnit
      | WT.CBool b -> return PT.CBool b
      | WT.CInt64 i -> return PT.CInt64 i
      | WT.CUInt64 i -> return PT.CUInt64 i
      | WT.CInt8 i -> return PT.CInt8 i
      | WT.CUInt8 i -> return PT.CUInt8 i
      | WT.CInt16 i -> return PT.CInt16 i
      | WT.CUInt16 i -> return PT.CUInt16 i
      | WT.CInt32 i -> return PT.CInt32 i
      | WT.CUInt32 i -> return PT.CUInt32 i
      | WT.CInt128 i -> return PT.CInt128 i
      | WT.CUInt128 i -> return PT.CUInt128 i
      | WT.CFloat(sign, w, f) -> return PT.CFloat(sign, w, f)
      | WT.CChar c -> return PT.CChar c
      | WT.CString s -> return PT.CString s

      | WT.CList items ->
        let! items = Ply.List.mapSequentially toPT items
        return PT.CList items

      | WT.CDict items ->
        let! items =
          Ply.List.mapSequentially
            (fun (key, value) ->
              uply {
                let! value = toPT value
                return (key, value)
              })
            items
        return PT.CDict items

      | WT.CTuple(first, second, theRest) ->
        let! first = toPT first
        let! second = toPT second
        let! theRest = Ply.List.mapSequentially toPT theRest
        return PT.CTuple(first, second, theRest)

      | WT.CEnum(typeName, caseName, fields) ->
        let! typeName =
          Expr.resolveTypeName pm onMissing currentModule typeName caseName
        let! fields = Ply.List.mapSequentially toPT fields
        return PT.CEnum(typeName, caseName, fields)
    }


module TypeDeclaration =
  module RecordField =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (f : WT.TypeDeclaration.RecordField)
      : Ply<PT.TypeDeclaration.RecordField> =
      uply {
        let! typ = TypeReference.toPT pm onMissing currentModule f.typ
        return { name = f.name; typ = typ; description = f.description }
      }

  module EnumField =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (f : WT.TypeDeclaration.EnumField)
      : Ply<PT.TypeDeclaration.EnumField> =
      uply {
        let! typ = TypeReference.toPT pm onMissing currentModule f.typ
        return { typ = typ; label = f.label; description = f.description }
      }

  module EnumCase =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (c : WT.TypeDeclaration.EnumCase)
      : Ply<PT.TypeDeclaration.EnumCase> =
      uply {
        let! fields =
          Ply.List.mapSequentially
            (EnumField.toPT pm onMissing currentModule)
            c.fields
        return { name = c.name; fields = fields; description = c.description }
      }

  module Definition =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (d : WT.TypeDeclaration.Definition)
      : Ply<PT.TypeDeclaration.Definition> =
      uply {
        match d with
        | WT.TypeDeclaration.Alias typ ->
          let! typ = TypeReference.toPT pm onMissing currentModule typ
          return PT.TypeDeclaration.Alias typ

        | WT.TypeDeclaration.Record fields ->
          let! fields =
            Ply.NEList.mapSequentially
              (RecordField.toPT pm onMissing currentModule)
              fields
          return PT.TypeDeclaration.Record fields

        | WT.TypeDeclaration.Enum cases ->
          let! cases =
            Ply.NEList.mapSequentially
              (EnumCase.toPT pm onMissing currentModule)
              cases
          return PT.TypeDeclaration.Enum cases
      }


  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (d : WT.TypeDeclaration.T)
    : Ply<PT.TypeDeclaration.T> =
    uply {
      let! def = Definition.toPT pm onMissing currentModule d.definition
      return { typeParams = d.typeParams; definition = def }
    }


// If it's one of the package items that we reference in F# code,
// make sure that we set its ID corrently at parse-time.
//
// CLEANUP: expose the equivalent of this via some Builtin for the Darklang WT2PT stuff?
// I suppose that's only really needed when we do the switch-over.
module PackageIDs = LibExecution.PackageIDs

module PackageType =
  module Name =
    let toPT (n : WT.PackageType.Name) : PT.PackageType.Name =
      { owner = n.owner; modules = n.modules; name = n.name }

  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (pt : WT.PackageType.PackageType)
    : Ply<PT.PackageType.PackageType> =
    uply {
      let! declaration =
        TypeDeclaration.toPT pm onMissing currentModule pt.declaration
      return
        { id = PackageIDs.Type.idForName pt.name.owner pt.name.modules pt.name.name
          name = Name.toPT pt.name
          description = pt.description
          declaration = declaration
          deprecated = PT.NotDeprecated }
    }

module PackageConstant =
  module Name =
    let toPT (n : WT.PackageConstant.Name) : PT.PackageConstant.Name =
      { owner = n.owner; modules = n.modules; name = n.name }

  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (c : WT.PackageConstant.PackageConstant)
    : Ply<PT.PackageConstant.PackageConstant> =
    uply {
      let! body = Const.toPT pm onMissing currentModule c.body
      return
        { id = PackageIDs.Constant.idForName c.name.owner c.name.modules c.name.name
          name = Name.toPT c.name
          description = c.description
          deprecated = PT.NotDeprecated
          body = body }
    }


module PackageFn =
  module Name =
    let toPT (n : WT.PackageFn.Name) : PT.PackageFn.Name =
      { owner = n.owner; modules = n.modules; name = n.name }

  module Parameter =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (p : WT.PackageFn.Parameter)
      : Ply<PT.PackageFn.Parameter> =
      uply {
        let! typ = TypeReference.toPT pm onMissing currentModule p.typ
        return { name = p.name; typ = typ; description = p.description }
      }

  let toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (fn : WT.PackageFn.PackageFn)
    : Ply<PT.PackageFn.PackageFn> =
    uply {
      let! parameters =
        Ply.NEList.mapSequentially
          (Parameter.toPT pm onMissing currentModule)
          fn.parameters
      let! returnType = TypeReference.toPT pm onMissing currentModule fn.returnType
      let! body = Expr.toPT builtins pm onMissing currentModule fn.body

      return
        { id = PackageIDs.Fn.idForName fn.name.owner fn.name.modules fn.name.name
          name = Name.toPT fn.name
          parameters = parameters
          returnType = returnType
          description = fn.description
          deprecated = PT.NotDeprecated
          body = body
          typeParams = fn.typeParams }
    }




module DB =
  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (db : WT.DB.T)
    : Ply<PT.DB.T> =
    uply {
      let! typ = TypeReference.toPT pm onMissing currentModule db.typ
      return { tlid = gid (); name = db.name; version = db.version; typ = typ }
    }


module Handler =
  module CronInterval =
    let toPT (ci : WT.Handler.CronInterval) : PT.Handler.CronInterval =
      match ci with
      | WT.Handler.EveryDay -> PT.Handler.EveryDay
      | WT.Handler.EveryWeek -> PT.Handler.EveryWeek
      | WT.Handler.EveryFortnight -> PT.Handler.EveryFortnight
      | WT.Handler.EveryHour -> PT.Handler.EveryHour
      | WT.Handler.Every12Hours -> PT.Handler.Every12Hours
      | WT.Handler.EveryMinute -> PT.Handler.EveryMinute

  module Spec =
    let toPT (s : WT.Handler.Spec) : PT.Handler.Spec =
      match s with
      | WT.Handler.HTTP(route, method) -> PT.Handler.HTTP(route, method)
      | WT.Handler.Worker name -> PT.Handler.Worker name
      | WT.Handler.Cron(name, interval) ->
        PT.Handler.Cron(name, CronInterval.toPT interval)
      | WT.Handler.REPL name -> PT.Handler.REPL name

  let toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (h : WT.Handler.T)
    : Ply<PT.Handler.T> =
    uply {
      let! ast = Expr.toPT builtins pm onMissing currentModule h.ast
      return { tlid = gid (); ast = ast; spec = Spec.toPT h.spec }
    }
