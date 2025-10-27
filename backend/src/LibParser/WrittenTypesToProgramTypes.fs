/// Conversion functions from WrittenTypes to ProgramTypes
module LibParser.WrittenTypesToProgramTypes

open Prelude

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module FS2WT = FSharpToWrittenTypes
type NRE = PT.NameResolutionError
module NR = NameResolver

// CLEANUP: Context could be simplified to use `currentFunction: string option` instead of full name path
type Context =
  { // Full qualified name path of the current function being parsed (e.g. ["MyModule"; "myFunction"])
    // Used for detecting self-recursive calls and converting them to ESelf expressions
    currentFnName : List<string> option
    // Whether we're currently inside a function body during parsing
    // Used to determine when variable shadowing of function names should reset the context
    isInFunction : bool
    // Maps parameter names to their indices within the current function
    // Used to convert EVariable references to EArg when they refer to function parameters
    argMap : Map<string, int> }

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
      | WT.TInt8 -> return PT.TInt8
      | WT.TUInt8 -> return PT.TUInt8
      | WT.TInt16 -> return PT.TInt16
      | WT.TUInt16 -> return PT.TUInt16
      | WT.TInt32 -> return PT.TInt32
      | WT.TUInt32 -> return PT.TUInt32
      | WT.TInt64 -> return PT.TInt64
      | WT.TUInt64 -> return PT.TUInt64
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
  let rec toPT (context : Context) (p : WT.LetPattern) : (Context * PT.LetPattern) =
    match p with
    | WT.LPVariable(id, varName) ->
      let newContext =
        { context with
            // If this variable shadows the self function name, clear it
            currentFnName =
              match context.currentFnName with
              | Some qualifiedName ->
                // Only clear if the variable name matches the last part of the qualified name
                match List.rev qualifiedName with
                | lastName :: _ when lastName = varName -> None
                | _ -> context.currentFnName
              | None -> None
            // Remove this variable from argMap as it's now shadowed by the let binding
            argMap = Map.remove varName context.argMap }
      (newContext, PT.LPVariable(id, varName))
    | WT.LPTuple(id, first, second, theRest) ->
      let (context1, first') = toPT context first
      let (context2, second') = toPT context1 second
      let (finalContext, theRest') =
        theRest
        |> List.fold
          (fun (ctx, acc) pat ->
            let (newCtx, pat') = toPT ctx pat
            (newCtx, pat' :: acc))
          (context2, [])
        |> fun (ctx, acc) -> (ctx, List.rev acc)
      (finalContext, PT.LPTuple(id, first', second', theRest'))
    | WT.LPUnit id -> (context, PT.LPUnit id)

module MatchPattern =
  let rec toPT
    (context : Context)
    (p : WT.MatchPattern)
    : (Context * PT.MatchPattern) =
    match p with
    | WT.MPVariable(id, varName) ->
      let newContext =
        { context with
            // If this variable shadows the self function name, clear it
            currentFnName =
              match context.currentFnName with
              | Some qualifiedName ->
                // Only clear if the variable name matches the last part of the qualified name
                match List.rev qualifiedName with
                | lastName :: _ when lastName = varName -> None
                | _ -> context.currentFnName
              | None -> None
            // Remove this variable from argMap as it's now shadowed by the match pattern
            argMap = Map.remove varName context.argMap }
      (newContext, PT.MPVariable(id, varName))
    | WT.MPEnum(id, caseName, fieldPats) ->
      let (finalContext, convertedPats) =
        fieldPats
        |> List.fold
          (fun (ctx, acc) pat ->
            let (newCtx, pat') = toPT ctx pat
            (newCtx, pat' :: acc))
          (context, [])
        |> fun (ctx, acc) -> (ctx, List.rev acc)
      (finalContext, PT.MPEnum(id, caseName, convertedPats))
    | WT.MPInt64(id, i) -> (context, PT.MPInt64(id, i))
    | WT.MPUInt64(id, i) -> (context, PT.MPUInt64(id, i))
    | WT.MPInt8(id, i) -> (context, PT.MPInt8(id, i))
    | WT.MPUInt8(id, i) -> (context, PT.MPUInt8(id, i))
    | WT.MPInt16(id, i) -> (context, PT.MPInt16(id, i))
    | WT.MPUInt16(id, i) -> (context, PT.MPUInt16(id, i))
    | WT.MPInt32(id, i) -> (context, PT.MPInt32(id, i))
    | WT.MPUInt32(id, i) -> (context, PT.MPUInt32(id, i))
    | WT.MPInt128(id, i) -> (context, PT.MPInt128(id, i))
    | WT.MPUInt128(id, i) -> (context, PT.MPUInt128(id, i))
    | WT.MPBool(id, b) -> (context, PT.MPBool(id, b))
    | WT.MPChar(id, c) -> (context, PT.MPChar(id, c))
    | WT.MPString(id, s) -> (context, PT.MPString(id, s))
    | WT.MPFloat(id, s, w, f) -> (context, PT.MPFloat(id, s, w, f))
    | WT.MPUnit id -> (context, PT.MPUnit id)
    | WT.MPTuple(id, first, second, theRest) ->
      let (context1, first') = toPT context first
      let (context2, second') = toPT context1 second
      let (finalContext, theRest') =
        theRest
        |> List.fold
          (fun (ctx, acc) pat ->
            let (newCtx, pat') = toPT ctx pat
            (newCtx, pat' :: acc))
          (context2, [])
        |> fun (ctx, acc) -> (ctx, List.rev acc)
      (finalContext, PT.MPTuple(id, first', second', theRest'))
    | WT.MPList(id, pats) ->
      let (finalContext, convertedPats) =
        pats
        |> List.fold
          (fun (ctx, acc) pat ->
            let (newCtx, pat') = toPT ctx pat
            (newCtx, pat' :: acc))
          (context, [])
        |> fun (ctx, acc) -> (ctx, List.rev acc)
      (finalContext, PT.MPList(id, convertedPats))
    | WT.MPListCons(id, head, tail) ->
      let (context1, head') = toPT context head
      let (finalContext, tail') = toPT context1 tail
      (finalContext, PT.MPListCons(id, head', tail'))
    | WT.MPOr(id, pats) ->
      let (finalContext, convertedPats) =
        pats
        |> NEList.toList
        |> List.fold
          (fun (ctx, acc) pat ->
            let (newCtx, pat') = toPT ctx pat
            (newCtx, pat' :: acc))
          (context, [])
        |> fun (ctx, acc) -> (ctx, List.rev acc)
      (finalContext,
       PT.MPOr(id, NEList.ofListUnsafe "MatchPattern.toPT" [] convertedPats))


module Expr =
  let resolveTypeName
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (names : List<string>)
    (caseName : string) // used for errors
    : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
    match names with
    | [] -> Ply(Error(NRE.InvalidName [ caseName ]))
    | head :: tail ->
      let name = NEList.ofList head tail |> WT.Unresolved
      NR.resolveTypeName pm onMissing currentModule name

  let rec toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (context : Context)
    (e : WT.Expr)
    : Ply<PT.Expr> =
    let toPT ctx = toPT builtins pm onMissing currentModule ctx
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
            (stringSegmentToPT builtins pm onMissing currentModule context)
            segments
        return PT.EString(id, segments)
      | WT.EFloat(id, sign, whole, fraction) ->
        return PT.EFloat(id, sign, whole, fraction)
      | WT.EBool(id, b) -> return PT.EBool(id, b)
      | WT.EUnit id -> return PT.EUnit id
      | WT.EVariable(id, var) ->
        // Check if this variable is a function argument first
        match Map.tryFind var context.argMap with
        | Some index -> return PT.EArg(id, index)
        | None ->
          let! value =
            NR.resolveValueName
              (builtins.values |> Map.keys |> Set)
              pm
              NR.OnMissing.Allow
              currentModule
              (WT.Unresolved(NEList.singleton var))
          match value with
          | Ok _ as name -> return PT.EValue(id, name)
          | Error _ -> return PT.EVariable(id, var)
      | WT.ERecordFieldAccess(id, obj, fieldname) ->
        let! obj = toPT context obj
        return PT.ERecordFieldAccess(id, obj, fieldname)
      | WT.EApply(id, (WT.EFnName(_, name)), [], { head = WT.EPlaceHolder }) ->
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
            NR.resolveValueName
              (builtins.values |> Map.keys |> Set)
              pm
              onMissing
              currentModule
              name
          return PT.EValue(id, name)
      | WT.EApply(id, (WT.EFnName(_, name) as fnName), typeArgs, args) ->
        let! processedTypeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            typeArgs
        let! processedArgs = Ply.NEList.mapSequentially (toPT context) args

        // Handle function calls with arguments, check for variable shadowing first
        match name with
        | WT.Unresolved { head = varName; tail = [] } ->
          match context.currentFnName with
          | Some currentFnName ->
            let varQualifiedName = currentModule @ [ varName ]
            if varQualifiedName = currentFnName then
              return PT.EApply(id, PT.ESelf(id), processedTypeArgs, processedArgs)
            else
              let! fnName =
                NR.resolveFnName
                  (builtins.fns |> Map.keys |> Set)
                  pm
                  NR.OnMissing.Allow
                  currentModule
                  name
              let expr =
                match fnName with
                | Ok resolvedName -> PT.EFnName(id, Ok resolvedName)
                | Error _ ->
                  match Map.tryFind varName context.argMap with
                  | Some index -> PT.EArg(id, index)
                  | None -> PT.EVariable(id, varName)
              return PT.EApply(id, expr, processedTypeArgs, processedArgs)
          | None when context.isInFunction ->
            // Inside a function, prioritize variables for unqualified calls (allows shadowing)
            let varExpr =
              match Map.tryFind varName context.argMap with
              | Some index -> PT.EArg(id, index)
              | None -> PT.EVariable(id, varName)
            return PT.EApply(id, varExpr, processedTypeArgs, processedArgs)
          | None ->
            // Global context, try to resolve as function name first, fall back to variable
            let! fnName =
              NR.resolveFnName
                (builtins.fns |> Map.keys |> Set)
                pm
                NR.OnMissing.Allow
                currentModule
                name
            let expr =
              match fnName with
              | Ok resolvedName -> PT.EFnName(id, Ok resolvedName)
              | Error _ ->
                match Map.tryFind varName context.argMap with
                | Some index -> PT.EArg(id, index)
                | None -> PT.EVariable(id, varName)
            return PT.EApply(id, expr, processedTypeArgs, processedArgs)
        | _ ->
          // For qualified names, process as normal function name
          let! expr = toPT context fnName
          return PT.EApply(id, expr, processedTypeArgs, processedArgs)
      | WT.EApply(id, name, typeArgs, args) ->
        let! name = toPT context name
        let! typeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            typeArgs
        let! args = Ply.NEList.mapSequentially (toPT context) args

        return PT.EApply(id, name, typeArgs, args)
      | WT.EFnName(id, name) ->
        let! fnName =
          NR.resolveFnName
            (builtins.fns |> Map.keys |> Set)
            pm
            NR.OnMissing.Allow
            currentModule
            name
        return PT.EFnName(id, fnName)
      | WT.ELambda(id, pats, body) ->
        // Start with a clean argMap to prevent lambda params from being converted to EArg
        let lambdaContext = { context with argMap = Map.empty }
        let (finalContext, ptPats) =
          pats
          |> NEList.toList
          |> List.fold
            (fun (ctx, acc) pat ->
              let (newCtx, ptPat) = LetPattern.toPT ctx pat
              (newCtx, acc @ [ ptPat ]))
            (lambdaContext, [])
        let! body = toPT finalContext body
        return
          PT.ELambda(
            id,
            NEList.ofListUnsafe "Lambda patterns cannot be empty" [] ptPats,
            body
          )
      | WT.ELet(id, pat, rhs, body) ->
        let! rhs = toPT context rhs
        let (newContext, ptPat) = LetPattern.toPT context pat
        let! body = toPT newContext body
        return PT.ELet(id, ptPat, rhs, body)
      | WT.EIf(id, cond, thenExpr, elseExpr) ->
        let! cond = toPT context cond
        let! thenExpr = toPT context thenExpr
        let! elseExpr =
          uply {
            match elseExpr with
            | Some value ->
              let! newValue = toPT context value
              return Some newValue
            | None -> return None
          }
        return PT.EIf(id, cond, thenExpr, elseExpr)
      | WT.EList(id, exprs) ->
        let! exprs = Ply.List.mapSequentially (toPT context) exprs
        return PT.EList(id, exprs)
      | WT.ETuple(id, first, second, theRest) ->
        let! first = toPT context first
        let! second = toPT context second
        let! theRest = Ply.List.mapSequentially (toPT context) theRest
        return PT.ETuple(id, first, second, theRest)
      | WT.ERecord(id, typeName, fields) ->
        let! typeName = NR.resolveTypeName pm onMissing currentModule typeName
        let! fields =
          Ply.List.mapSequentially
            (fun (fieldName, fieldExpr) ->
              uply {
                let! fieldExpr = toPT context fieldExpr
                return (fieldName, fieldExpr)
              })
            fields
        let typeArgs = [] // TODO
        return PT.ERecord(id, typeName, typeArgs, fields)
      | WT.ERecordUpdate(id, record, updates) ->
        let! record = toPT context record
        let! updates =
          updates
          |> Ply.NEList.mapSequentially (fun (name, expr) ->
            uply {
              let! expr = toPT context expr
              return (name, expr)
            })
        return PT.ERecordUpdate(id, record, updates)
      | WT.EPipe(pipeID, expr1, rest) ->
        let! expr1 = toPT context expr1
        let! rest =
          Ply.List.mapSequentially
            (pipeExprToPT builtins pm onMissing currentModule context)
            rest
        return PT.EPipe(pipeID, expr1, rest)
      | WT.EEnum(id, typeName, caseName, exprs) ->
        let! typeName = resolveTypeName pm onMissing currentModule typeName caseName
        let! exprs = Ply.List.mapSequentially (toPT context) exprs
        let typeArgs = [] // TODO
        return PT.EEnum(id, typeName, typeArgs, caseName, exprs)
      | WT.EMatch(id, mexpr, cases) ->
        let! mexpr = toPT context mexpr
        let! cases =
          Ply.List.mapSequentially
            (fun (case : WT.MatchCase) ->
              uply {
                let (patternContext, mp) = MatchPattern.toPT context case.pat
                let! whenCondition =
                  uply {
                    match case.whenCondition with
                    | Some whenExpr ->
                      let! whenExpr = toPT patternContext whenExpr
                      return Some whenExpr
                    | None -> return None
                  }
                let! expr = toPT patternContext case.rhs
                let result : PT.MatchCase =
                  { pat = mp; whenCondition = whenCondition; rhs = expr }
                return result
              })
            cases

        return PT.EMatch(id, mexpr, cases)
      | WT.EInfix(id, infix, arg1, arg2) ->
        let! arg1 = toPT context arg1
        let! arg2 = toPT context arg2
        return PT.EInfix(id, Infix.toPT infix, arg1, arg2)
      | WT.EDict(id, pairs) ->
        let! pairs =
          Ply.List.mapSequentially
            (fun (key, value) ->
              uply {
                let! value = toPT context value
                return (key, value)
              })
            pairs
        return PT.EDict(id, pairs)

      | WT.EStatement(id, first, next) ->
        let! first = toPT context first
        let! next = toPT context next
        return PT.EStatement(id, first, next)

      | WT.EPlaceHolder ->
        return Exception.raiseInternal "Invalid parse - placeholder not removed" []
    }

  and stringSegmentToPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (context : Context)
    (segment : WT.StringSegment)
    : Ply<PT.StringSegment> =
    match segment with
    | WT.StringText text -> Ply(PT.StringText text)
    | WT.StringInterpolation expr ->
      toPT builtins pm onMissing currentModule context expr
      |> Ply.map (fun interpolated -> PT.StringInterpolation interpolated)

  and pipeExprToPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (context : Context)
    (pipeExpr : WT.PipeExpr)
    : Ply<PT.PipeExpr> =
    let toPT ctx = toPT builtins pm onMissing currentModule ctx

    uply {
      match pipeExpr with
      | WT.EPipeVariableOrFnCall(id, name) ->
        match context.currentFnName with
        | Some currentFnName ->
          let functionName = List.tryLast currentFnName
          match functionName with
          | Some fnName when name = fnName ->
            // This is a recursive call - resolve as function
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
          | _ ->
            // Not a self-reference - try function resolution first, fall back to variable
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
        | None when context.isInFunction ->
          // When inside a function with no self context, prioritize variables to allow shadowing
          return PT.EPipeVariable(id, name, [])
        | None ->
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
        // Start with a clean argMap to prevent lambda params from being converted to EArg
        let lambdaContext = { context with argMap = Map.empty }
        let (finalContext, ptPats) =
          pats
          |> NEList.toList
          |> List.fold
            (fun (ctx, acc) pat ->
              let (newCtx, ptPat) = LetPattern.toPT ctx pat
              (newCtx, acc @ [ ptPat ]))
            (lambdaContext, [])
        let! body = toPT finalContext body
        return
          PT.EPipeLambda(
            id,
            NEList.ofListUnsafe "Pipe lambda patterns cannot be empty" [] ptPats,
            body
          )

      | WT.EPipeInfix(id, infix, first) ->
        let! first = toPT context first
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
        let! args = Ply.List.mapSequentially (toPT context) args
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
        let! args = Ply.List.mapSequentially (toPT context) args
        return PT.EPipeFnCall(id, fnName, typeArgs, args)

      | WT.EPipeEnum(id, typeName, caseName, fields) ->
        let! typeName = resolveTypeName pm onMissing currentModule typeName caseName
        let! fields = Ply.List.mapSequentially (toPT context) fields
        return PT.EPipeEnum(id, typeName, caseName, fields)
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
          description = pt.description
          declaration = declaration
          deprecated = PT.NotDeprecated }
    }

module PackageValue =
  let toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (c : WT.PackageValue.PackageValue)
    : Ply<PT.PackageValue.PackageValue> =
    uply {
      let context =
        { currentFnName = None; isInFunction = false; argMap = Map.empty }
      let! body = Expr.toPT builtins pm onMissing currentModule context c.body
      return
        { id = PackageIDs.Value.idForName c.name.owner c.name.modules c.name.name
          description = c.description
          deprecated = PT.NotDeprecated
          body = body }
    }


module PackageFn =
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
      let argMap =
        fn.parameters
        |> NEList.toList
        |> List.mapi (fun i param -> param.name, i)
        |> Map.ofList
      let context =
        { currentFnName = Some(currentModule @ [ fn.name.name ])
          isInFunction = true
          argMap = argMap }
      let! body = Expr.toPT builtins pm onMissing currentModule context fn.body

      return
        { id = PackageIDs.Fn.idForName fn.name.owner fn.name.modules fn.name.name
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
      let context =
        { currentFnName = None; isInFunction = false; argMap = Map.empty }
      let! ast = Expr.toPT builtins pm onMissing currentModule context h.ast
      return { tlid = gid (); ast = ast; spec = Spec.toPT h.spec }
    }
