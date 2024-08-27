/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude

// Used for conversion functions
module RT = RuntimeTypes
module VT = ValueType
module PT = ProgramTypes

module FQTypeName =
  module Package =
    let toRT (p : PT.FQTypeName.Package) : RT.FQTypeName.Package = p

  //let fromRT (p : RT.FQTypeName.Package) : PT.FQTypeName.Package = p

  let toRT (fqtn : PT.FQTypeName.FQTypeName) : RT.FQTypeName.FQTypeName =
    match fqtn with
    | PT.FQTypeName.Package p -> RT.FQTypeName.Package(Package.toRT p)

// let fromRT (fqtn : RT.FQTypeName.FQTypeName) : Option<PT.FQTypeName.FQTypeName> =
//   match fqtn with
//   | RT.FQTypeName.Package p -> PT.FQTypeName.Package(Package.fromRT p) |> Some


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


module NameResolutionError =
  let toRT (e : PT.NameResolutionError) : RT.NameResolutionError =
    match e with
    | PT.NameResolutionError.NotFound names -> RT.NameResolutionError.NotFound names
    | PT.NameResolutionError.InvalidName names ->
      RT.NameResolutionError.InvalidName names

module NameResolution =
  let toRT (f : 'a -> 'b) (nr : PT.NameResolution<'a>) : RT.NameResolution<'b> =
    match nr with
    | Ok x -> Ok(f x)
    | Error e -> Error(NameResolutionError.toRT e)


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

    | PT.TDateTime -> RT.TDateTime
    | PT.TUuid -> RT.TUuid

    | PT.TList inner -> RT.TList(toRT inner)
    | PT.TTuple(first, second, theRest) ->
      RT.TTuple(toRT first, toRT second, theRest |> List.map toRT)
    | PT.TDict typ -> RT.TDict(toRT typ)

    | PT.TCustomType(typeName, typeArgs) ->
      RT.TCustomType(
        NameResolution.toRT FQTypeName.toRT typeName,
        List.map toRT typeArgs
      )

    | PT.TVariable(name) -> RT.TVariable(name)

    | PT.TFn(paramTypes, returnType) ->
      RT.TFn(NEList.map toRT paramTypes, toRT returnType)

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
  let rec toRT (p : PT.LetPattern) : RT.LetPattern =
    match p with
    | PT.LPUnit _ -> RT.LPUnit

    | PT.LPTuple(_, first, second, theRest) ->
      RT.LPTuple(toRT first, toRT second, List.map toRT theRest)

    | PT.LPVariable(_, name) -> RT.LPVariable name


  let toInstr (valueReg : RT.Register) (p : PT.LetPattern) : RT.Instruction =
    RT.CheckLetPatternAndExtractVars(valueReg, toRT p)



module MatchPattern =
  let rec toRT (p : PT.MatchPattern) : RT.MatchPattern =
    match p with
    | PT.MPUnit _ -> RT.MPUnit

    | PT.MPBool(_, b) -> RT.MPBool b

    | PT.MPInt8(_, i) -> RT.MPInt8 i
    | PT.MPUInt8(_, i) -> RT.MPUInt8 i
    | PT.MPInt16(_, i) -> RT.MPInt16 i
    | PT.MPUInt16(_, i) -> RT.MPUInt16 i
    | PT.MPInt32(_, i) -> RT.MPInt32 i
    | PT.MPUInt32(_, i) -> RT.MPUInt32 i
    | PT.MPInt64(_, i) -> RT.MPInt64 i
    | PT.MPUInt64(_, i) -> RT.MPUInt64 i
    | PT.MPInt128(_, i) -> RT.MPInt128 i
    | PT.MPUInt128(_, i) -> RT.MPUInt128 i

    | PT.MPFloat(_, sign, whole, frac) -> RT.MPFloat(makeFloat sign whole frac)

    | PT.MPChar(_, c) -> RT.MPChar c
    | PT.MPString(_, s) -> RT.MPString s

    | PT.MPList(_, pats) -> RT.MPList(List.map toRT pats)
    | PT.MPListCons(_, head, tail) -> RT.MPListCons(toRT head, toRT tail)

    | PT.MPTuple(_, first, second, theRest) ->
      RT.MPTuple(toRT first, toRT second, List.map toRT theRest)

    | PT.MPVariable(_, name) -> RT.MPVariable name


  let toMatchInstr
    (valueReg : RT.Register)
    (p : PT.MatchPattern)
    (jumpByFail : int)
    : RT.Instruction =
    RT.CheckMatchPatternAndExtractVars(valueReg, toRT p, jumpByFail)


module MatchCase =
  /// Compiling a MatchCase happens in two phases, because many instructions
  /// require knowing how many instructions to jump over, which we can't know
  /// until we know the basics of all the cases.
  ///
  /// This type holds all the information we gather as part of the first phase
  /// , in order of where the instrs should be at the end of the second phase.
  ///
  /// Note: not represented here, we'll also need an unconditional `JumpBy` instr
  /// , to get past all the cases. We can only determine how many instrs to jump
  /// after the first phases is complete, but it'll land at the end of these.
  type IntermediateValue =
    {
      /// jumpByFail -> instr
      /// `RT.MatchValue(valueReg, pat, jumpByFail)`
      /// (the `pat` and `valueReg` are known in the first phase)
      matchValueInstrFn : int -> RT.Instruction

      /// Evaluation of the `whenCondition` (if it exists -- might be empty)
      whenCondInstructions : RT.Instructions

      /// (jumpBy) -> instr
      /// `RT.JumpByIfFalse(jumpBy, whenCondResultReg)`
      /// (`whenCondResultReg` is known in the first phase)
      whenCondJump : Option<int -> RT.Instruction>

      /// Evaluation of the RHS
      ///
      /// Includes `CopyVal(resultReg, rhsResultReg)`
      rhsInstrs : RT.Instructions

      /// RC after all instructions
      ///
      /// Note: Different branches/cases will require different # of registers
      /// , so we'll end up taking the max of all the RCs
      rc : int
    }


module Expr =
  let rec toRT (rc : int) (e : PT.Expr) : (int * RT.Instructions * RT.Register) =
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

    | PT.EString(_id, segments) ->
      match segments with
      // if there's only one segment, just load it directly
      | [ PT.StringText text ] ->
        (rc + 1, [ RT.LoadVal(rc, RT.DString text) ], rc)

      // otherwise, handle each segment separately
      // and then create a string from the parts
      | segments ->
        let (rc, instrs, segments) =
          List.fold
            (fun (rc, instrs, segments) segment ->
              match segment with
              | PT.StringText text ->
                (rc, instrs, segments @ [ RT.StringSegment.Text text ])

              | PT.StringInterpolation expr ->
                let (rcAfterExpr, exprInstrs, exprReg) = toRT rc expr
                (rcAfterExpr,
                 instrs @ exprInstrs,
                 segments @ [ RT.Interpolated exprReg ]))
            (rc, [], [])
            segments

        (rc + 1, instrs @ [ RT.CreateString(rc, segments) ], rc)


    | PT.EList(_id, items) ->
      let listReg = rc
      let init = (rc + 1, [], [])

      let (regCounter, instrs, itemResultRegs) =
        items
        |> List.fold
          (fun (rc, instrs, itemResultRegs) item ->
            let (newRc, itemInstrs, innerResultReg) = toRT rc item
            (newRc, instrs @ itemInstrs, itemResultRegs @ [ innerResultReg ]))
          init

      (regCounter, instrs @ [ RT.CreateList(listReg, itemResultRegs) ], listReg)


    | PT.EDict(_id, items) ->
      let dictReg = rc
      let init = (rc + 1, [], [])

      let (regCounter, instrs, entryPairs) =
        items
        |> List.fold
          (fun (rc, instrs, entryPairs) (key, value) ->
            let (newRc, valueInstrs, valueReg) = toRT rc value
            (newRc, instrs @ valueInstrs, entryPairs @ [ (key, valueReg) ]))
          init

      (regCounter, instrs @ [ RT.CreateDict(dictReg, entryPairs) ], dictReg)


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
      let patInstr = LetPattern.toInstr exprReg pat
      let (regCounter, bodyInstrs, bodyExprReg) = toRT regCounter body
      (regCounter, exprInstrs @ [ patInstr ] @ bodyInstrs, bodyExprReg)


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

    | PT.EFnName(_, Error nre) ->
      // TODO improve
      // hmm maybe we shouldn't fail yet here.
      // It's ok to _reference_ a bad name, so long as we don't try to `apply` it.
      // maybe the 'value' here is (still) some unresolved name?
      // (which should fail when we apply it)
      (rc, [ RT.RaiseNRE(NameResolutionError.toRT nre) ], rc)


    // | PT.EApply(_id, thingToApplyExpr, typeArgs, args) ->
    //   let (regCounter, thingToApplyInstrs, thingToApplyReg) =
    //     // (usually, a fn name)
    //     toRT rc thingToApplyExpr
    //   // TODO: maybe one or both of these lists should be an `NEList`?

    //   // CLEANUP find a way to get rid of silly NEList stuff
    //   let (regCounter, argInstrs, argRegs) =
    //     let init = (regCounter, [], [])

    //     args
    //     |> NEList.fold
    //       (fun (rc, instrs, argResultRegs) arg ->
    //         let (newRc, newInstrs, argResultReg) = toRT rc arg
    //         (newRc, instrs @ newInstrs, argResultRegs @ [ argResultReg ]))
    //       init

    //   let putResultIn = regCounter
    //   let callInstr =
    //     RT.Apply(
    //       putResultIn,
    //       thingToApplyReg,
    //       List.map TypeReference.toRT typeArgs,
    //       NEList.ofListUnsafe "" [] argRegs
    //     )

    //   (regCounter + 1, thingToApplyInstrs @ argInstrs @ [ callInstr ], putResultIn)


    | PT.EMatch(_id, expr, cases) ->
      // first, the easy part - compile the expression we're `match`ing against.
      let (rcAfterExpr, exprInstrs, exprResultReg) = toRT rc expr

      // Shortly, we'll compile each of the cases.
      // We'll use this `resultReg` to store the final result of the match
      // , so we have a consistent place to look for it.
      // (similar to how we handle `EIf` -- refer to that for a simpler example)
      let resultReg, rcAfterResult = rcAfterExpr, rcAfterExpr + 1

      // We compile each `case` in two phases, because some instrs require knowing
      // how many instrs to jump over, which we can't know until we know the basics
      // of all the cases.
      //
      // See `MatchCase.IntermediateValue` for more info.
      let casesAfterFirstPhase : List<MatchCase.IntermediateValue> =
        cases
        |> List.map (fun c ->
          // compile the `when` condition, if it exists, as much as we can
          let rcAfterWhenCond, whenCondInstrs, whenCondJump =
            match c.whenCondition with
            | None -> (rcAfterResult, [], None)
            | Some whenCond ->
              let (rcAfterWhenCond, whenCondInstrs, whenCondReg) =
                toRT rcAfterResult whenCond
              (rcAfterWhenCond,
               whenCondInstrs,
               Some(fun jumpBy -> RT.JumpByIfFalse(jumpBy, whenCondReg)))

          // compile the `rhs` of the case
          let rcAfterRhs, rhsInstrs, rhsResultReg = toRT rcAfterWhenCond c.rhs

          // return the intermediate results, as far along as they are
          { matchValueInstrFn = MatchPattern.toMatchInstr exprResultReg c.pat
            whenCondInstructions = whenCondInstrs
            whenCondJump = whenCondJump
            rhsInstrs = rhsInstrs @ [ RT.CopyVal(resultReg, rhsResultReg) ]
            rc = rcAfterRhs })

      let countInstrsForCase (c : MatchCase.IntermediateValue) : int =
        1 // for the `MatchValue` instruction
        + List.length c.whenCondInstructions
        + (match c.whenCondJump with
           | Some _ -> 1
           | None -> 0)
        + List.length c.rhsInstrs
        + 1 // for the `JumpBy` instruction

      let (cases, _) : List<MatchCase.IntermediateValue * int> * int =
        casesAfterFirstPhase
        |> List.map (fun c ->
          let instrCount = countInstrsForCase c
          (c, instrCount))
        |> List.foldRight
          // CLEANUP this works, but hurts the brain a bit.
          (fun (acc, runningTotal) (c, instrCount) ->
            let newTotal = runningTotal + instrCount
            (acc @ [ c, runningTotal ], newTotal))
          ([], 0)
      let cases = List.rev cases

      let caseInstrs =
        cases
        |> List.fold
          (fun instrs (c, instrsAfterThisCaseUntilEndOfMatch) ->
            // note: `instrsAfterThisCaseUntilEndOfMatch` does not include
            // the final MatchUnmatched instruction

            let caseInstrs =
              [ c.matchValueInstrFn (
                  countInstrsForCase c
                  // because we can skip over the MatchValue instr
                  - 1
                ) ]
              @ c.whenCondInstructions
              @ (match c.whenCondJump with
                 // jump to next case if the when condition is false
                 | Some jump -> [ jump (List.length c.rhsInstrs + 1) ]
                 | None -> [])
              @ c.rhsInstrs
              @ [ RT.JumpBy(instrsAfterThisCaseUntilEndOfMatch + 1) ]

            instrs @ caseInstrs)
          []

      let instrs = exprInstrs @ caseInstrs @ [ RT.MatchUnmatched ]

      let rcAtEnd = casesAfterFirstPhase |> List.map _.rc |> List.max

      (rcAtEnd, instrs, resultReg)


    // -- Records --
    | PT.ERecord(_id, Error nre, _typeArgs, _fields) ->
      let returnReg = 0 // TODO - not sure what to do here
      (rc, [ RT.RaiseNRE(NameResolutionError.toRT nre) ], returnReg)

    | PT.ERecord(_id, Ok typeName, typeArgs, fields) ->
      // fields : List<string * Expr>
      let recordReg, rc = rc, rc + 1

      // CLEANUP: complain if there are no fields -- or maybe that should happen during interpretation?
      // - actually- is there anything _wrong_ with a fieldless record?
      let (rcAfterFields, instrs, fields) =
        fields
        |> List.fold
          (fun (rc, instrs, fieldRegs) (fieldName, fieldExpr) ->
            let (newRc, newInstrs, fieldReg) = toRT rc fieldExpr
            (newRc, instrs @ newInstrs, fieldRegs @ [ (fieldName, fieldReg) ]))
          (rc, [], [])

      (rcAfterFields,
       instrs
       @ [ RT.CreateRecord(
             recordReg,
             FQTypeName.toRT typeName,
             List.map TypeReference.toRT typeArgs,
             fields
           ) ],
       recordReg)

    // | PT.ERecordUpdate(_id, expr, updates) ->
    //   let (rcAfterOriginalRecord, originalRecordInstrs, originalRecordReg) =
    //     toRT rc expr

    //   let (rcAfterUpdates, updatesInstrs, updates) =
    //     updates
    //     |> NEList.fold
    //       (fun (rc, instrs, regs) (fieldName, fieldExpr) ->
    //         let (newRc, newInstrs, newReg) = toRT rc fieldExpr
    //         (newRc, instrs @ newInstrs, regs @ [ (fieldName, newReg) ]))
    //       (rcAfterOriginalRecord, [], [])

    //   let targetReg, rc = rcAfterUpdates, rcAfterUpdates + 1
    //   let instrs =
    //     originalRecordInstrs
    //     @ updatesInstrs
    //     @ [ RT.CloneRecordWithUpdates(targetReg, originalRecordReg, updates) ]

    //   (rc, instrs, targetReg)

    | PT.ERecordFieldAccess(_id, expr, fieldName) ->
      let (rcAfterExpr, exprInstrs, exprReg) = toRT rc expr
      (rcAfterExpr + 1,
       exprInstrs @ [ RT.GetRecordField(rcAfterExpr, exprReg, fieldName) ],
       rcAfterExpr)


    // -- Enums --
    | PT.EEnum(_id, Error nre, _caseName, _typeArgs, _fields) ->
      let returnReg = 0 // TODO - not sure what to do here
      (rc, [ RT.RaiseNRE(NameResolutionError.toRT nre) ], returnReg)

    | PT.EEnum(_id, Ok typeName, typeArgs, caseName, fields) ->
      // fields : List<string * Expr>
      let enumReg, rc = rc, rc + 1

      let (rcAfterFields, instrs, fields) =
        fields
        |> List.fold
          (fun (rc, instrs, fieldRegs) fieldExpr ->
            let (newRc, newInstrs, fieldReg) = toRT rc fieldExpr
            (newRc, instrs @ newInstrs, fieldRegs @ [ fieldReg ]))
          (rc, [], [])

      (rcAfterFields,
       instrs
       @ [ RT.CreateEnum(
             enumReg,
             FQTypeName.toRT typeName,
             List.map TypeReference.toRT typeArgs,
             caseName,
             fields
           ) ],
       enumReg)



module Const =
  let rec toRT (c : PT.Const) : RT.Const =
    match c with
    | PT.Const.CUnit -> RT.CUnit

    | PT.Const.CBool b -> RT.CBool b

    | PT.Const.CInt8 i -> RT.CInt8 i
    | PT.Const.CUInt8 i -> RT.CUInt8 i
    | PT.Const.CInt16 i -> RT.CInt16 i
    | PT.Const.CUInt16 i -> RT.CUInt16 i
    | PT.Const.CInt32 i -> RT.CInt32 i
    | PT.Const.CUInt32 i -> RT.CUInt32 i
    | PT.Const.CInt64 i -> RT.CInt64 i
    | PT.Const.CUInt64 i -> RT.CUInt64 i
    | PT.Const.CInt128 i -> RT.CInt128 i
    | PT.Const.CUInt128 i -> RT.CUInt128 i

    | PT.Const.CFloat(sign, w, f) -> RT.CFloat(sign, w, f)

    | PT.Const.CChar c -> RT.CChar c
    | PT.Const.CString s -> RT.CString s

    | PT.Const.CTuple(first, second, rest) ->
      RT.CTuple(toRT first, toRT second, List.map toRT rest)
    | PT.Const.CList items -> RT.CList(List.map toRT items)
    | PT.Const.CDict entries -> RT.CDict(entries |> List.map (Tuple2.mapSecond toRT))

    | PT.Const.CEnum(typeName, caseName, fields) ->
      RT.CEnum(
        NameResolution.toRT FQTypeName.toRT typeName,
        caseName,
        List.map toRT fields
      )


module TypeDeclaration =
  module RecordField =
    let toRT (f : PT.TypeDeclaration.RecordField) : RT.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toRT f.typ }

  // module EnumField =
  //   let toRT (f : PT.TypeDeclaration.EnumField) : RT.TypeReference =
  //     TypeReference.toRT f.typ

  // module EnumCase =
  //   let toRT (c : PT.TypeDeclaration.EnumCase) : RT.TypeDeclaration.EnumCase =
  //     { name = c.name; fields = List.map EnumField.toRT c.fields }

  module Definition =
    let toRT (d : PT.TypeDeclaration.Definition) : RT.TypeDeclaration.Definition =
      match d with
      | PT.TypeDeclaration.Definition.Alias(typ) ->
        RT.TypeDeclaration.Alias(TypeReference.toRT typ)

      | PT.TypeDeclaration.Record fields ->
        RT.TypeDeclaration.Record(NEList.map RecordField.toRT fields)

  // | PT.TypeDeclaration.Enum cases ->
  //   RT.TypeDeclaration.Enum(NEList.map EnumCase.toRT cases)

  let toRT (t : PT.TypeDeclaration.T) : RT.TypeDeclaration.T =
    { typeParams = t.typeParams; definition = Definition.toRT t.definition }


// --
// Package stuff
// --
module PackageType =
  let toRT (t : PT.PackageType.PackageType) : RT.PackageType.PackageType =
    { id = t.id; declaration = TypeDeclaration.toRT t.declaration }

module PackageConstant =
  let toRT
    (c : PT.PackageConstant.PackageConstant)
    : RT.PackageConstant.PackageConstant =
    { id = c.id; body = Const.toRT c.body }

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
    { getType = fun id -> pm.getType id |> Ply.map (Option.map PackageType.toRT)
      getConstant =
        fun id -> pm.getConstant id |> Ply.map (Option.map PackageConstant.toRT)
      getFn = fun id -> pm.getFn id |> Ply.map (Option.map PackageFn.toRT)

      init = pm.init }
