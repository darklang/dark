/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude

// Used for conversion functions
module RT = RuntimeTypes
module VT = RT.ValueType
module PT = ProgramTypes

// module FQTypeName =
//   module Package =
//     let toRT (p : PT.FQTypeName.Package) : RT.FQTypeName.Package = p

//     let fromRT (p : RT.FQTypeName.Package) : PT.FQTypeName.Package = p

//   let toRT (fqtn : PT.FQTypeName.FQTypeName) : RT.FQTypeName.FQTypeName =
//     match fqtn with
//     | PT.FQTypeName.Package p -> RT.FQTypeName.Package(Package.toRT p)

//   let fromRT (fqtn : RT.FQTypeName.FQTypeName) : Option<PT.FQTypeName.FQTypeName> =
//     match fqtn with
//     | RT.FQTypeName.Package p -> PT.FQTypeName.Package(Package.fromRT p) |> Some


// module FQConstantName =
//   module Builtin =
//     let toRT (c : PT.FQConstantName.Builtin) : RT.FQConstantName.Builtin =
//       { name = c.name; version = c.version }

//     let fromRT (c : RT.FQConstantName.Builtin) : PT.FQConstantName.Builtin =
//       { name = c.name; version = c.version }

//   module Package =
//     let toRT (c : PT.FQConstantName.Package) : RT.FQConstantName.Package = c

//     let fromRT (c : RT.FQConstantName.Package) : PT.FQConstantName.Package = c

//   let toRT
//     (name : PT.FQConstantName.FQConstantName)
//     : RT.FQConstantName.FQConstantName =
//     match name with
//     | PT.FQConstantName.Builtin s -> RT.FQConstantName.Builtin(Builtin.toRT s)
//     | PT.FQConstantName.Package p -> RT.FQConstantName.Package(Package.toRT p)


module FQFnName =
  module Builtin =
    let toRT (s : PT.FQFnName.Builtin) : RT.FQFnName.Builtin =
      { name = s.name; version = s.version }

    let fromRT (s : RT.FQFnName.Builtin) : PT.FQFnName.Builtin =
      { name = s.name; version = s.version }

  module Package =
    let toRT (p : PT.FQFnName.Package) : RT.FQFnName.Package = p

    let fromRT (p : RT.FQFnName.Package) : PT.FQFnName.Package = p

  let toRT (fqfn : PT.FQFnName.FQFnName) : RT.FQFnName.FQFnName =
    match fqfn with
    | PT.FQFnName.Builtin s -> RT.FQFnName.Builtin(Builtin.toRT s)
    | PT.FQFnName.Package p -> RT.FQFnName.Package(Package.toRT p)


module NameResolution =
  let toRT (f : 'a -> 'b) (nr : PT.NameResolution<'a>) : RT.NameResolution<'b> =
    match nr with
    | Ok x -> Ok(f x)
    | Error e -> Error(NameResolutionError.RTE.toRuntimeError e)


module TypeReference =
  let rec toRT (t : PT.TypeReference) : RT.TypeReference =
    match t with
    | PT.TUnit -> RT.TUnit

    | PT.TBool -> RT.TBool

    | PT.TInt8 -> RT.TInt8
    | PT.TUInt8 -> RT.TUInt8
    | PT.TInt16 -> RT.TInt16
    | PT.TUInt16 -> RT.TUInt16
    | PT.TInt32 -> RT.TInt32
    | PT.TUInt32 -> RT.TUInt32
    | PT.TInt64 -> RT.TInt64
    | PT.TUInt64 -> RT.TUInt64
    | PT.TInt128 -> RT.TInt128
    | PT.TUInt128 -> RT.TUInt128

    | PT.TFloat -> RT.TFloat

    | PT.TChar -> RT.TChar
    | PT.TString -> RT.TString

    | PT.TList inner -> RT.TList(toRT inner)
    | PT.TTuple(first, second, theRest) ->
      RT.TTuple(toRT first, toRT second, theRest |> List.map toRT)
    | PT.TDict typ -> RT.TDict(toRT typ)

    | PT.TDateTime -> RT.TDateTime
    | PT.TUuid -> RT.TUuid
// | PT.TCustomType(typeName, typeArgs) ->
//   RT.TCustomType(
//     NameResolution.toRT FQTypeName.toRT typeName,
//     List.map toRT typeArgs
//   )
// | PT.TVariable(name) -> RT.TVariable(name)
// | PT.TFn(paramTypes, returnType) ->
//   RT.TFn(NEList.map toRT paramTypes, toRT returnType)
//| PT.TDB typ -> RT.TDB(toRT typ)


// module InfixFnName =
//   let toFnName (name : PT.InfixFnName) : (string * int) =
//     match name with
//     | PT.ArithmeticPlus -> ("int64Add", 0)
//     | PT.ArithmeticMinus -> ("int64Subtract", 0)
//     | PT.ArithmeticMultiply -> ("int64Multiply", 0)
//     | PT.ArithmeticDivide -> ("floatDivide", 0)
//     | PT.ArithmeticModulo -> ("int64Mod", 0)
//     | PT.ArithmeticPower -> ("int64Power", 0)
//     | PT.ComparisonGreaterThan -> ("int64GreaterThan", 0)
//     | PT.ComparisonGreaterThanOrEqual -> ("int64GreaterThanOrEqualTo", 0)
//     | PT.ComparisonLessThan -> ("int64LessThan", 0)
//     | PT.ComparisonLessThanOrEqual -> ("int64LessThanOrEqualTo", 0)
//     | PT.StringConcat -> ("stringAppend", 0)
//     | PT.ComparisonEquals -> ("equals", 0)
//     | PT.ComparisonNotEquals -> ("notEquals", 0)


module LetPattern =
  let rec toRT
    (rc : int)
    (pat : PT.LetPattern)
    (rhsReg : RT.Register) // what we're binding to
    : (int * RT.Instructions) =
    match pat with
    // No binding needed for unit pattern
    // (would also be the case if we have a `_ignore` pattern later)
    | PT.LPUnit _ -> (rc, [])

    | PT.LPTuple(_id, first, second, theRest) ->
      // reserve the first two registers
      let firstReg, secondReg, rc = rc, rc + 1, rc + 2

      let (rcAfterFirst, firstInstrs) = toRT rc first firstReg
      let (rcAfterSecond, secondInstrs) = toRT rcAfterFirst second secondReg

      let (finalRc, restInstrs, restRegs) =
        theRest
        |> List.fold
          (fun (currentRc, instrs, regs) restPattern ->
            let restReg = currentRc
            let (rcAfterPat, patternInstrs) =
              toRT (currentRc + 1) restPattern restReg
            (rcAfterPat, instrs @ patternInstrs, regs @ [ restReg ]))
          (rcAfterSecond, [], [])

      let extractInstructions =
        [ RT.ExtractTupleItems(rhsReg, firstReg, secondReg, restRegs) ]

      (finalRc, extractInstructions @ firstInstrs @ secondInstrs @ restInstrs)

    | PT.LPVariable(_id, varName) -> (rc, [ RT.SetVar(varName, rhsReg) ])





// module MatchPattern =
//   let rec toRT (p : PT.MatchPattern) : RT.MatchPattern =
//     match p with
//     | PT.MPVariable(id, str) -> RT.MPVariable(id, str)
//     | PT.MPEnum(id, caseName, fieldPats) ->
//       RT.MPEnum(id, caseName, List.map toRT fieldPats)
//     | PT.MPInt64(id, i) -> RT.MPInt64(id, i)
//     | PT.MPUInt64(id, i) -> RT.MPUInt64(id, i)
//     | PT.MPInt8(id, i) -> RT.MPInt8(id, i)
//     | PT.MPUInt8(id, i) -> RT.MPUInt8(id, i)
//     | PT.MPInt16(id, i) -> RT.MPInt16(id, i)
//     | PT.MPUInt16(id, i) -> RT.MPUInt16(id, i)
//     | PT.MPInt32(id, i) -> RT.MPInt32(id, i)
//     | PT.MPUInt32(id, i) -> RT.MPUInt32(id, i)
//     | PT.MPInt128(id, i) -> RT.MPInt128(id, i)
//     | PT.MPUInt128(id, i) -> RT.MPUInt128(id, i)
//     | PT.MPBool(id, b) -> RT.MPBool(id, b)
//     | PT.MPChar(id, c) -> RT.MPChar(id, c)
//     | PT.MPString(id, s) -> RT.MPString(id, s)
//     | PT.MPFloat(id, s, w, f) ->
//       let w = if w = "" then "0" else w
//       RT.MPFloat(id, makeFloat s w f)
//     | PT.MPUnit id -> RT.MPUnit id
//     | PT.MPTuple(id, first, second, theRest) ->
//       RT.MPTuple(id, toRT first, toRT second, List.map toRT theRest)
//     | PT.MPList(id, pats) -> RT.MPList(id, List.map toRT pats)
//     | PT.MPListCons(id, head, tail) -> RT.MPListCons(id, toRT head, toRT tail)


module Expr =
  // CLEANUP clearly not the most efficient to do this, but probably fine for now
  let rec compileString
    (rc : int)
    (segments : List<PT.StringSegment>)
    : (int * RT.Instructions * RT.Register) =
    let stringReg = rc
    let init = (rc + 1, [ RT.LoadVal(stringReg, RT.DString "") ], stringReg)

    segments
    |> List.fold
      (fun (rc, instrs, _) segment ->
        match segment with
        | PT.StringText text ->
          let textReg = rc
          let newRc = rc + 1
          (newRc,
           instrs
           @ [ RT.LoadVal(textReg, RT.DString text)
               RT.AppendString(stringReg, textReg) ],
           stringReg)
        | PT.StringInterpolation expr ->
          let (newRc, exprInstrs, exprReg) = toRT rc expr
          (newRc,
           instrs @ exprInstrs @ [ RT.AppendString(stringReg, exprReg) ],
           stringReg))
      init

  and toRT (rc : int) (e : PT.Expr) : (int * RT.Instructions * RT.Register) =
    match e with
    | PT.EUnit _id -> (rc + 1, [ RT.LoadVal(rc, RT.DUnit) ], rc)

    | PT.EBool(_id, b) -> (rc + 1, [ RT.LoadVal(rc, RT.DBool b) ], rc)

    | PT.EInt8(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DInt8 num) ], rc)
    | PT.EInt16(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DInt16 num) ], rc)
    | PT.EInt32(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DInt32 num) ], rc)
    | PT.EInt64(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DInt64 num) ], rc)
    | PT.EInt128(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DInt128 num) ], rc)
    | PT.EUInt8(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DUInt8 num) ], rc)
    | PT.EUInt16(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DUInt16 num) ], rc)
    | PT.EUInt32(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DUInt32 num) ], rc)
    | PT.EUInt64(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DUInt64 num) ], rc)
    | PT.EUInt128(_id, num) -> (rc + 1, [ RT.LoadVal(rc, RT.DUInt128 num) ], rc)

    | PT.EFloat(_id, sign, whole, fraction) ->
      let whole = if whole = "" then "0" else whole
      let fraction = if fraction = "" then "0" else fraction
      (rc + 1, [ RT.LoadVal(rc, RT.DFloat(makeFloat sign whole fraction)) ], rc)

    | PT.EChar(_id, c) -> (rc + 1, [ RT.LoadVal(rc, RT.DChar c) ], rc)

    | PT.EString(_id, segments) -> compileString rc segments

    | PT.EList(_id, items) ->
      let listReg = rc
      let init = (rc + 1, [ RT.LoadVal(listReg, RT.DList(VT.unknown, [])) ])

      let (regCounter, instrs) =
        items
        |> List.fold
          (fun (rc, instrs) item ->
            let (newRc, itemInstrs, innerResultReg) = toRT rc item
            (newRc,
             instrs @ itemInstrs @ [ RT.AddItemToList(listReg, innerResultReg) ]))
          init

      (regCounter, instrs, listReg)

    | PT.EDict(_id, items) ->
      let dictReg = rc
      let init = (rc + 1, [ RT.LoadVal(dictReg, RT.DDict(VT.unknown, Map.empty)) ])

      let (regCounter, instrs) =
        items
        |> List.fold
          (fun (rc, instrs) (key, value) ->
            let (newRc, valueInstrs, valueReg) = toRT rc value
            (newRc,
             instrs @ valueInstrs @ [ RT.AddDictEntry(dictReg, key, valueReg) ]))
          init

      (regCounter, instrs, dictReg)

    | PT.ETuple(_id, first, second, theRest) ->
      // save the 'first' register for the result
      let tupleReg, rc = rc, rc + 1

      let (rcAfterFirst, firstInstrs, firstReg) = toRT rc first
      let (rcAfterSecond, secondInstrs, secondReg) = toRT rcAfterFirst second
      let (rcAfterAll, _rcsAfterTheRest, theRestInstrs, theRestRegs) =
        theRest
        |> List.fold
          (fun (rc, rcs, instrs, resultRegs) item ->
            let (rcAfterItem, itemInstrs, itemResultReg) = toRT rc item
            (rcAfterItem,
             rcs @ [ rcAfterItem ],
             instrs @ itemInstrs,
             resultRegs @ [ itemResultReg ]))
          (rcAfterSecond, [], [], [])

      let instrs =
        firstInstrs
        @ secondInstrs
        @ theRestInstrs
        @ [ RT.CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ]

      (rcAfterAll, instrs, tupleReg)


    // let x = 1
    | PT.ELet(_id, pat, expr, body) ->
      let (regCounter, exprInstrs, exprReg) = toRT rc expr
      let (regCounter, patInstrs) = LetPattern.toRT regCounter pat exprReg
      let (regCounter, bodyInstrs, bodyExprReg) = toRT regCounter body
      (regCounter, exprInstrs @ patInstrs @ bodyInstrs, bodyExprReg)


    | PT.EVariable(_id, varName) ->
      let reg = rc
      (rc + 1, [ RT.GetVar(reg, varName) ], reg)


    | PT.EIf(_id, cond, thenExpr, elseExpr) ->
      // We need a consistent result register,
      // so we'll create this, and copy to it at the end of each branch
      let resultReg, rc = rc, rc + 1

      let (rcAfterCond, condInstrs, condReg) = toRT rc cond
      let jumpIfCondFalse jumpBy = [ RT.JumpByIfFalse(jumpBy, condReg) ]

      let (rcAfterThen, thenInstrs, thenResultReg) = toRT rcAfterCond thenExpr
      let copyThenToResultInstr = [ RT.CopyVal(resultReg, thenResultReg) ]

      match elseExpr with
      | None ->
        let instrs =
          [ RT.LoadVal(resultReg, RT.DUnit) ] // if `cond` is `false`, the (default) result should probably be Unit
          @ condInstrs
          @ jumpIfCondFalse (
            // goto the first instruction past the `if`
            // (the 1 is for the copy instruction)
            List.length thenInstrs + 1
          )
          @ thenInstrs
          @ copyThenToResultInstr

        (rcAfterThen, instrs, resultReg)

      | Some elseExpr ->
        let (rcAfterElse, elseInstrs, elseResultReg) = toRT rcAfterThen elseExpr
        let copyToResultInstr = [ RT.CopyVal(resultReg, elseResultReg) ]

        let instrs =
          // cond -- if cond `false`, jump to start of 'else' block
          condInstrs
          @ jumpIfCondFalse (
            // goto the first instruction past the `if`
            // (first 1 is for the copy instruction)
            // (second 1 is for the jump instruction)
            List.length thenInstrs + 1 + 1
          )

          // then
          @ thenInstrs
          @ copyThenToResultInstr
          @ [ RT.JumpBy(List.length elseInstrs + 1) ]

          // else
          @ elseInstrs
          @ copyToResultInstr

        (rcAfterElse, instrs, resultReg)


    | PT.EFnName(_, Ok name) ->
      let reg = rc
      (rc + 1, [ RT.LoadVal(reg, RT.DFnVal(RT.NamedFn(FQFnName.toRT name))) ], reg)

    | PT.EFnName(_, Error _err) ->
      // TODO improve
      // hmm maybe we shouldn't fail yet here.
      // It's ok to _reference_ a bad name, so long as we don't try to `apply` it.
      // maybe the 'value' here is (still) some unresolved name?
      // (which should fail when we apply it)
      (rc, [ RT.Fail(RT.RuntimeError.oldError "Couldn't find fn") ], rc)

    | PT.EApply(_id, thingToApplyExpr, typeArgs, args) ->
      let (regCounter, thingToApplyInstrs, thingToApplyReg) =
        // (usually, a fn name)
        toRT rc thingToApplyExpr
      // TODO: maybe one or both of these lists should be an `NEList`?

      // CLEANUP find a way to get rid of silly NEList stuff
      let (regCounter, argInstrs, argRegs) =
        let init = (regCounter, [], [])

        args
        |> NEList.fold
          (fun (rc, instrs, argResultRegs) arg ->
            let (newRc, newInstrs, argResultReg) = toRT rc arg
            (newRc, instrs @ newInstrs, argResultRegs @ [ argResultReg ]))
          init

      let putResultIn = regCounter
      let callInstr =
        RT.Apply(
          putResultIn,
          thingToApplyReg,
          List.map TypeReference.toRT typeArgs,
          NEList.ofListUnsafe "" [] argRegs
        )

      (regCounter + 1, thingToApplyInstrs @ argInstrs @ [ callInstr ], putResultIn)


// let rec toRT (e : PT.Expr) : RT.Instructions =
//   match e with
//   // | PT.EConstant(id, Ok name) -> RT.EConstant(id, FQConstantName.toRT name)
//   // | PT.EConstant(id, Error err) ->
//   //   RT.EError(id, NameResolutionError.RTE.toRuntimeError err, [])

//   // | PT.EVariable(id, var) -> RT.EVariable(id, var)

//   // | PT.ERecordFieldAccess(id, obj, fieldname) -> RT.ERecordFieldAccess(id, toRT obj, fieldname)

//   | PT.EApply(id, fnName, typeArgs, args) ->
//     // RT.EApply(
//     //   id,
//     //   toRT fnName,
//     //   List.map TypeReference.toRT typeArgs,
//     //   NEList.map toRT args
//     // )
//     let fnInstr =
//       match fnName with
//       | PT.EFnName(_, Ok name) -> RT.Call(id, FQFnName.toRT name, List.map TypeReference.toRT typeArgs, NEList.map (fun a -> 0) args)
//       | _ -> failwith "Unsupported function name resolution"
//     fnInstr :: (args |> NEList.toList |> List.map toRT |> List.concat)

//   | PT.EFnName(id, Ok name) -> RT.EFnName(id, FQFnName.toRT name)
//   | PT.EFnName(id, Error err) ->
//     RT.EError(id, NameResolutionError.RTE.toRuntimeError err, [])

//   // // CLEANUP tidy infix stuff - extract to another fn?
//   // | PT.EInfix(id, PT.InfixFnCall fnName, left, right) ->
//   //   let (fn, version) = InfixFnName.toFnName fnName
//   //   let name = RT.FQFnName.Builtin({ name = fn; version = version })
//   //   RT.EApply(
//   //     id,
//   //     RT.EFnName(id, name),
//   //     [],
//   //     NEList.ofList (toRT left) [ toRT right ]
//   //   )
//   // | PT.EInfix(id, PT.BinOp PT.BinOpAnd, left, right) ->
//   //   RT.EAnd(id, toRT left, toRT right)
//   // | PT.EInfix(id, PT.BinOp PT.BinOpOr, left, right) ->
//   //   RT.EOr(id, toRT left, toRT right)

//   // | PT.ELambda(id, pats, body) ->
//   //   RT.ELambda(id, NEList.map LetPattern.toRT pats, toRT body)

//   // | PT.ERecord(id, Ok typeName, fields) ->
//   //   match fields with
//   //   | [] ->
//   //     let fields = fields |> List.map Tuple2.second |> List.map toRT
//   //     RT.EError(
//   //       id,
//   //       RT.RuntimeError.oldError "Record must have at least one field",
//   //       fields
//   //     )
//   //   | head :: tail ->
//   //     let fields =
//   //       NEList.ofList head tail
//   //       |> NEList.map (fun (name, expr) -> (name, toRT expr))
//   //     RT.ERecord(id, FQTypeName.toRT typeName, fields)
//   // | PT.ERecord(id, Error err, fields) ->
//   //   RT.EError(
//   //     id,
//   //     err |> NameResolutionError.RTE.toRuntimeError,
//   //     fields |> List.map Tuple2.second |> List.map toRT
//   //   )

//   // | PT.ERecordUpdate(id, record, updates) ->
//   //   RT.ERecordUpdate(
//   //     id,
//   //     toRT record,
//   //     updates |> NEList.map (fun (fieldName, update) -> (fieldName, toRT update))
//   //   )

//   // | PT.EPipe(pipeID, expr1, rest) ->
//   //   // Convert v |> fn1 a |> fn2 |> fn3 b c
//   //   // into fn3 (fn2 (fn1 v a)) b c
//   //   let folder (prev : RT.Expr) (next : PT.PipeExpr) : RT.Expr =
//   //     let applyFn (expr : RT.Expr) (args : List<RT.Expr>) =
//   //       let typeArgs = []
//   //       RT.EApply(pipeID, expr, typeArgs, NEList.ofList prev args)

//   //     match next with
//   //     | PT.EPipeFnCall(id, Error err, _typeArgs, exprs) ->
//   //       let err = NameResolutionError.RTE.toRuntimeError err
//   //       let addlExprs = List.map toRT exprs
//   //       RT.EError(id, err, prev :: addlExprs)
//   //     | PT.EPipeFnCall(id, Ok fnName, typeArgs, exprs) ->
//   //       RT.EApply(
//   //         id,
//   //         RT.EFnName(id, FQFnName.toRT fnName),
//   //         List.map TypeReference.toRT typeArgs,
//   //         exprs |> List.map toRT |> NEList.ofList prev
//   //       )
//   //     | PT.EPipeInfix(id, PT.InfixFnCall fnName, expr) ->
//   //       let (fn, version) = InfixFnName.toFnName fnName
//   //       let name = PT.FQFnName.Builtin({ name = fn; version = version })
//   //       RT.EApply(
//   //         id,
//   //         RT.EFnName(id, FQFnName.toRT name),
//   //         [],
//   //         NEList.doubleton prev (toRT expr)
//   //       )
//   //     // Binops work pretty naturally here
//   //     | PT.EPipeInfix(id, PT.BinOp op, expr) ->
//   //       match op with
//   //       | PT.BinOpAnd -> RT.EAnd(id, prev, toRT expr)
//   //       | PT.BinOpOr -> RT.EOr(id, prev, toRT expr)
//   //     | PT.EPipeEnum(id, Ok typeName, caseName, fields) ->
//   //       RT.EEnum(
//   //         id,
//   //         FQTypeName.toRT typeName,
//   //         caseName,
//   //         prev :: (List.map toRT fields)
//   //       )
//   //     | PT.EPipeEnum(id, Error err, _caseName, fields) ->
//   //       RT.EError(
//   //         id,
//   //         NameResolutionError.RTE.toRuntimeError err,
//   //         prev :: (List.map toRT fields)
//   //       )
//   //     | PT.EPipeVariable(id, name, exprs) ->
//   //       applyFn (RT.EVariable(id, name)) (List.map toRT exprs)
//   //     | PT.EPipeLambda(id, pats, body) ->
//   //       applyFn (RT.ELambda(id, NEList.map LetPattern.toRT pats, toRT body)) []

//   //   let init = toRT expr1
//   //   List.fold folder init rest

//   // | PT.EMatch(id, mexpr, cases) ->
//   //   match cases with
//   //   | [] ->
//   //     RT.EError(
//   //       id,
//   //       RT.RuntimeError.oldError "Match must have at least one case",
//   //       [ toRT mexpr ]
//   //     )
//   //   | head :: tail ->
//   //     let cases =
//   //       NEList.ofList head tail
//   //       |> NEList.map (fun case ->
//   //         let pattern = MatchPattern.toRT case.pat
//   //         let whenCondition = Option.map toRT case.whenCondition
//   //         let expr = toRT case.rhs
//   //         let result : RT.MatchCase =
//   //           { pat = pattern; whenCondition = whenCondition; rhs = expr }
//   //         result)

//   //     RT.EMatch(id, toRT mexpr, cases)

//   // | PT.EEnum(id, Ok typeName, caseName, fields) ->
//   //   RT.EEnum(id, FQTypeName.toRT typeName, caseName, List.map toRT fields)
//   // | PT.EEnum(id, Error err, _caseName, fields) ->
//   //   RT.EError(id, NameResolutionError.RTE.toRuntimeError err, List.map toRT fields)

//   // | PT.EDict(id, entries) ->
//   //   RT.EDict(id, entries |> List.map (Tuple2.mapSecond toRT))


// module Const =
//   let rec toRT (c : PT.Const) : RT.Const =
//     match c with
//     | PT.Const.CInt64 i -> RT.CInt64 i
//     | PT.Const.CUInt64 i -> RT.CUInt64 i
//     | PT.Const.CInt8 i -> RT.CInt8 i
//     | PT.Const.CUInt8 i -> RT.CUInt8 i
//     | PT.Const.CInt16 i -> RT.CInt16 i
//     | PT.Const.CUInt16 i -> RT.CUInt16 i
//     | PT.Const.CInt32 i -> RT.CInt32 i
//     | PT.Const.CUInt32 i -> RT.CUInt32 i
//     | PT.Const.CInt128 i -> RT.CInt128 i
//     | PT.Const.CUInt128 i -> RT.CUInt128 i
//     | PT.Const.CBool b -> RT.CBool b
//     | PT.Const.CString s -> RT.CString s
//     | PT.Const.CChar c -> RT.CChar c
//     | PT.Const.CFloat(sign, w, f) -> RT.CFloat(sign, w, f)
//     | PT.Const.CUnit -> RT.CUnit
//     | PT.Const.CTuple(first, second, rest) ->
//       RT.CTuple(toRT first, toRT second, List.map toRT rest)
//     | PT.Const.CEnum(typeName, caseName, fields) ->
//       RT.CEnum(
//         NameResolution.toRT FQTypeName.toRT typeName,
//         caseName,
//         List.map toRT fields
//       )
//     | PT.Const.CList items -> RT.CList(List.map toRT items)
//     | PT.Const.CDict entries -> RT.CDict(entries |> List.map (Tuple2.mapSecond toRT))


// module TypeDeclaration =
//   module RecordField =
//     let toRT (f : PT.TypeDeclaration.RecordField) : RT.TypeDeclaration.RecordField =
//       { name = f.name; typ = TypeReference.toRT f.typ }

//   module EnumField =
//     let toRT (f : PT.TypeDeclaration.EnumField) : RT.TypeReference =
//       TypeReference.toRT f.typ

//   module EnumCase =
//     let toRT (c : PT.TypeDeclaration.EnumCase) : RT.TypeDeclaration.EnumCase =
//       { name = c.name; fields = List.map EnumField.toRT c.fields }

//   module Definition =
//     let toRT (d : PT.TypeDeclaration.Definition) : RT.TypeDeclaration.Definition =
//       match d with
//       | PT.TypeDeclaration.Definition.Alias(typ) ->
//         RT.TypeDeclaration.Alias(TypeReference.toRT typ)

//       | PT.TypeDeclaration.Record fields ->
//         RT.TypeDeclaration.Record(NEList.map RecordField.toRT fields)

//       | PT.TypeDeclaration.Enum cases ->
//         RT.TypeDeclaration.Enum(NEList.map EnumCase.toRT cases)

//   let toRT (t : PT.TypeDeclaration.T) : RT.TypeDeclaration.T =
//     { typeParams = t.typeParams; definition = Definition.toRT t.definition }


// --
// Package stuff
// --
// module PackageType =
//   let toRT (t : PT.PackageType.PackageType) : RT.PackageType.PackageType =
//     { id = t.id; declaration = TypeDeclaration.toRT t.declaration }

// module PackageConstant =
//   let toRT
//     (c : PT.PackageConstant.PackageConstant)
//     : RT.PackageConstant.PackageConstant =
//     { id = c.id; body = Const.toRT c.body }

module PackageFn =
  module Parameter =
    let toRT (p : PT.PackageFn.Parameter) : RT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toRT p.typ }

  let toRT (f : PT.PackageFn.PackageFn) : RT.PackageFn.PackageFn =
    { id = f.id
      body =
        let initialRegCounter =
          // TODO: OK? depends if we try to 'inline' package fns or not...
          0
        Expr.toRT initialRegCounter f.body
      typeParams = f.typeParams
      parameters = f.parameters |> NEList.map Parameter.toRT
      returnType = f.returnType |> TypeReference.toRT }



// // --
// // User stuff
// // --
// module Handler =
//   module CronInterval =
//     let toRT (ci : PT.Handler.CronInterval) : RT.Handler.CronInterval =
//       match ci with
//       | PT.Handler.EveryDay -> RT.Handler.EveryDay
//       | PT.Handler.EveryWeek -> RT.Handler.EveryWeek
//       | PT.Handler.EveryFortnight -> RT.Handler.EveryFortnight
//       | PT.Handler.EveryHour -> RT.Handler.EveryHour
//       | PT.Handler.Every12Hours -> RT.Handler.Every12Hours
//       | PT.Handler.EveryMinute -> RT.Handler.EveryMinute

//   module Spec =
//     let toRT (s : PT.Handler.Spec) : RT.Handler.Spec =
//       match s with
//       | PT.Handler.HTTP(route, method) -> RT.Handler.HTTP(route, method)
//       | PT.Handler.Worker name -> RT.Handler.Worker name
//       | PT.Handler.Cron(name, interval) ->
//         RT.Handler.Cron(name, CronInterval.toRT interval)
//       | PT.Handler.REPL name -> RT.Handler.REPL name

//   let toRT (h : PT.Handler.T) : RT.Handler.T =
//     { tlid = h.tlid; ast = Expr.toRT h.ast; spec = Spec.toRT h.spec }

// module DB =
//   let toRT (db : PT.DB.T) : RT.DB.T =
//     { tlid = db.tlid
//       name = db.name
//       version = db.version
//       typ = TypeReference.toRT db.typ }

// module Secret =
//   let toRT (s : PT.Secret.T) : RT.Secret.T =
//     { name = s.name; value = s.value; version = s.version }



module PackageManager =
  let toRT (pm : PT.PackageManager) : RT.PackageManager =
    { //getType = fun id -> pm.getType id |> Ply.map (Option.map PackageType.toRT)
      //getConstant =
      //  fun id -> pm.getConstant id |> Ply.map (Option.map PackageConstant.toRT)
      getFn = fun id -> pm.getFn id |> Ply.map (Option.map PackageFn.toRT)

      init = pm.init }
