/// Interprets Dark instructions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open Prelude
open RuntimeTypes
module RTE = RuntimeError
module VT = ValueType


let rec checkAndExtractLetPattern
  (pat : LetPattern)
  (dv : Dval)
  : bool * List<Register * Dval> =
  let r = checkAndExtractLetPattern

  let rec rList pats items =
    match pats, items with
    | [], [] -> true, []
    | [], _ -> false, []
    | _, [] -> false, []
    | pat :: otherPats, item :: items ->
      let matches, vars = r pat item
      if matches then
        let matchesOtherPats, varsFromOtherParts = rList otherPats items
        if matchesOtherPats then true, vars @ varsFromOtherParts else false, []
      else
        false, []

  match pat, dv with
  | LPVariable extractTo, dv -> true, [ (extractTo, dv) ]
  | LPUnit, DUnit -> true, []
  | LPTuple(first, second, theRest), DTuple(firstVal, secondVal, theRestVal) ->
    match r first firstVal, r second secondVal with
    | (true, varsFirst), (true, varsSecond) ->
      match rList theRest theRestVal with
      | true, varsRest -> true, varsFirst @ varsSecond @ varsRest
      | false, _ -> false, []
    | _ -> false, []
  | _ -> false, []


let rec checkAndExtractMatchPattern
  (pat : MatchPattern)
  (dv : Dval)
  : bool * List<Register * Dval> =
  let r = checkAndExtractMatchPattern

  let rec rList pats items =
    match pats, items with
    | [], [] -> true, []
    | [], _ -> false, []
    | _, [] -> false, []
    | pat :: otherPats, item :: items ->
      let matches, vars = r pat item
      if matches then
        let matchesOtherPats, varsFromOtherPats = rList otherPats items
        if matchesOtherPats then true, vars @ varsFromOtherPats else false, []
      else
        false, []

  match pat, dv with
  | MPVariable reg, dv -> true, [ (reg, dv) ]

  | MPUnit, DUnit -> true, []
  | MPBool l, DBool r -> l = r, []
  | MPInt8 l, DInt8 r -> l = r, []
  | MPUInt8 l, DUInt8 r -> l = r, []
  | MPInt16 l, DInt16 r -> l = r, []
  | MPUInt16 l, DUInt16 r -> l = r, []
  | MPInt32 l, DInt32 r -> l = r, []
  | MPUInt32 l, DUInt32 r -> l = r, []
  | MPInt64 l, DInt64 r -> l = r, []
  | MPUInt64 l, DUInt64 r -> l = r, []
  | MPInt128 l, DInt128 r -> l = r, []
  | MPUInt128 l, DUInt128 r -> l = r, []
  | MPFloat l, DFloat r -> l = r, []
  | MPChar l, DChar r -> l = r, []
  | MPString l, DString r -> l = r, []

  | MPList pats, DList(_, items) -> rList pats items

  | MPListCons(head, tail), DList(vt, items) ->
    match items with
    | [] -> false, []
    | headItem :: tailItems ->
      let matchesHead, varsHead = r head headItem
      if matchesHead then
        let matchesTail, varsTail = r tail (DList(vt, tailItems))
        if matchesTail then true, varsHead @ varsTail else false, []
      else
        false, []

  | MPTuple(first, second, theRest), DTuple(firstVal, secondVal, theRestVal) ->
    match r first firstVal, r second secondVal with
    | (true, varsFirst), (true, varsSecond) ->
      match rList theRest theRestVal with
      | true, varsRest -> true, varsFirst @ varsSecond @ varsRest
      | false, _ -> false, []
    | _ -> false, []

  | MPEnum(caseName, fields), DEnum(_, _, _, caseNameActual, fieldsActual) ->
    if caseName = caseNameActual then rList fields fieldsActual else false, []

  // Dval didn't match the pattern even in a basic sense
  | MPVariable _, _
  | MPUnit, _
  | MPBool _, _
  | MPInt8 _, _
  | MPUInt8 _, _
  | MPInt16 _, _
  | MPUInt16 _, _
  | MPInt32 _, _
  | MPUInt32 _, _
  | MPInt64 _, _
  | MPUInt64 _, _
  | MPInt128 _, _
  | MPUInt128 _, _
  | MPFloat _, _
  | MPChar _, _
  | MPString _, _
  | MPTuple _, _
  | MPListCons _, _
  | MPList _, _
  | MPEnum _, _ -> false, []


// CLEANUP evaluate constants/values at dev-time, not interpreter time,
// and remove the Const structure
let rec evalConst
  (types : Types)
  (threadID : ThreadID)
  (tst : TypeSymbolTable)
  (c : Const)
  : Ply<Dval> =
  uply {
    let r = evalConst types threadID tst

    match c with
    | CUnit -> return DUnit
    | CBool b -> return DBool b

    | CInt8 i -> return DInt8 i
    | CUInt8 i -> return DUInt8 i
    | CInt16 i -> return DInt16 i
    | CUInt16 i -> return DUInt16 i
    | CInt32 i -> return DInt32 i
    | CUInt32 i -> return DUInt32 i
    | CInt64 i -> return DInt64 i
    | CUInt64 i -> return DUInt64 i
    | CInt128 i -> return DInt128 i
    | CUInt128 i -> return DUInt128 i

    | CFloat(sign, w, f) -> return DFloat(makeFloat sign w f)

    | CChar c -> return DChar c
    | CString s -> return DString s

    | CTuple(first, second, theRest) ->
      let! first = r first
      let! second = r second
      let! theRest = theRest |> Ply.List.mapSequentially r
      return DTuple(first, second, theRest)

    | CList items ->
      let! items = items |> Ply.List.mapSequentially r
      return TypeChecker.DvalCreator.list threadID ValueType.Unknown items

    | CDict entries ->
      let! entries =
        entries
        |> Ply.List.mapSequentially (fun (k, v) -> r v |> Ply.map (fun v -> k, v))
      return TypeChecker.DvalCreator.dict threadID ValueType.Unknown entries


    | CEnum(Ok typeName, caseName, fields) ->
      let typeArgs =
        // At this point, we don't support explicit type args for constant enums
        // once (CLEANUP) we make some big changes to how Constants work
        // (so they're eval'd at dev-time rather than run-time)
        // , we can support this.
        []

      let! fields = fields |> Ply.List.mapSequentially r
      let! enum =
        TypeChecker.DvalCreator.enum
          types
          threadID
          tst
          typeName
          typeArgs
          caseName
          fields

      return enum

    | CEnum(Error nre, _caseName, _fields) ->
      // CLEANUP ConstNotFound would be better
      return raiseRTE threadID (RuntimeError.ParseTimeNameResolution nre)
  }


let execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  uply {
    let raiseRTE rte = raiseRTE vm.threadID rte

    let mutable finalResult : Dval option = None

    while Map.containsKey vm.currentFrameID vm.callFrames do
      let currentFrame = Map.findUnsafe vm.currentFrameID vm.callFrames

      let mutable counter = currentFrame.programCounter
      let registers = currentFrame.registers


      let! instrData =
        match currentFrame.executionPoint with
        | Source -> Ply(snd vm.rootInstrData)

        | Lambda(parentContext, lambdaID) ->
          let lambda =
            (match Map.tryFind (parentContext, lambdaID) vm.lambdaInstrCache with
             | Some l -> l
             | None ->
               match Map.tryFind (Source, lambdaID) vm.lambdaInstrCache with
               | Some l -> l
               | None ->
                 Exception.raiseInternal
                   "lambda not found"
                   [ "lambdaID", lambdaID; "parentContext", parentContext ])
            |> _.instructions

          { instructions = List.toArray lambda.instructions
            resultReg = lambda.resultIn }
          |> Ply

        | Function(FQFnName.Builtin _) ->
          // we should error in some better way (CLEANUP)
          // , but the point is that callstacks shouldn't be created for builtin fn calls
          raiseRTE (RTE.FnNotFound(FQFnName.fqBuiltin "builtin" 0))

        | Function(FQFnName.Package fn) ->
          uply {
            match Map.find fn vm.packageFnInstrCache with
            | Some fn -> return fn
            | None ->
              match! exeState.fns.package fn with
              | Some fn ->
                let instrData =
                  { instructions = List.toArray fn.body.instructions
                    resultReg = fn.body.resultIn }
                vm.packageFnInstrCache <-
                  Map.add fn.id instrData vm.packageFnInstrCache
                return instrData

              | None -> return raiseRTE (RTE.FnNotFound(FQFnName.Package fn))
          }


      let mutable frameToPush = None

      while counter < instrData.instructions.Length && frameToPush = None do

        let inst = instrData.instructions[counter]
        match inst with

        // == Simple register operations ==
        | LoadVal(reg, value) -> registers[reg] <- value

        | CopyVal(copyTo, copyFrom) -> registers[copyTo] <- registers[copyFrom]

        | Or(createTo, left, right) ->
          match registers[left] with
          | DBool true -> registers[createTo] <- DBool true
          | DBool false ->
            match registers[right] with
            | DBool true -> registers[createTo] <- DBool true
            | DBool false -> registers[createTo] <- DBool false
            | r ->
              RTE.Bools.OrOnlySupportsBooleans(VT.bool, Dval.toValueType r)
              |> RTE.Bool
              |> raiseRTE
          | l ->
            let r = registers[right]
            RTE.Bools.OrOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
            |> RTE.Bool
            |> raiseRTE

        | And(createTo, left, right) ->
          match registers[left] with
          | DBool false -> registers[createTo] <- DBool false
          | DBool true ->
            match registers[right] with
            | DBool true -> registers[createTo] <- DBool true
            | DBool false -> registers[createTo] <- DBool false
            | r ->
              RTE.Bools.AndOnlySupportsBooleans(VT.bool, Dval.toValueType r)
              |> RTE.Bool
              |> raiseRTE
          | l ->
            let r = registers[right]
            RTE.Bools.AndOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
            |> RTE.Bool
            |> raiseRTE


        // == Working with Variables ==
        | CheckLetPatternAndExtractVars(valueReg, pat) ->
          let dv = registers[valueReg]
          let doesMatch, registersToAssign = checkAndExtractLetPattern pat dv

          if doesMatch then
            registersToAssign
            |> List.iter (fun (reg, value) -> registers[reg] <- value)
          else
            raiseRTE (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))


        // TODO References to DBs and Secrets should be resolved at parse-time
        // , not runtime. For consistency, safety, etc.
        // We should have specific
        // EReferenceSecret and EReferenceDB constructs that we respect,
        // all throughout WT, NR, PT, RT, PT2RT, etc.
        // I don't even think this would be that hard -- good to do sooner rather than later.
        | VarNotFound(targetRegIfSecretOrDB, varName) ->
          match exeState.program.dbs |> Map.get varName with
          | Some _foundDB -> registers[targetRegIfSecretOrDB] <- DDB varName
          | None ->
            match
              exeState.program.secrets |> List.find (fun s -> s.name = varName)
            with
            | Some found -> registers[targetRegIfSecretOrDB] <- DString found.value
            | None -> raiseRTE (RTE.VariableNotFound varName)



        // == Working with Basic Types ==
        | CreateString(targetReg, segments) ->
          let sb = new System.Text.StringBuilder()

          segments
          |> List.iter (fun seg ->
            match seg with
            | Text s -> sb.Append s |> ignore<System.Text.StringBuilder>
            | Interpolated reg ->
              match registers[reg] with
              | DString s -> sb.Append s |> ignore<System.Text.StringBuilder>
              | dv ->
                let vt = Dval.toValueType dv
                raiseRTE (
                  RTE.String(RTE.Strings.Error.NonStringInInterpolation(vt, dv))
                ))

          registers[targetReg] <- DString(sb.ToString())


        // == Flow Control ==
        // -- Jumps --
        | JumpBy jumpBy -> counter <- counter + jumpBy

        | JumpByIfFalse(jumpBy, condReg) ->
          match registers[condReg] with
          | DBool false -> counter <- counter + jumpBy
          | DBool true -> ()
          | dv ->
            raiseRTE (
              RTE.Bool(RTE.Bools.ConditionRequiresBool(Dval.toValueType dv, dv))
            )

        // -- Match --
        | CheckMatchPatternAndExtractVars(valueReg, pat, failJump) ->
          let doesMatch, registersToAssign =
            checkAndExtractMatchPattern pat registers[valueReg]

          if doesMatch then
            registersToAssign
            |> List.iter (fun (reg, value) -> registers[reg] <- value)
          else
            counter <- counter + failJump

        | MatchUnmatched -> raiseRTE (RTE.Match RTE.Matches.MatchUnmatched)


        // == Working with Collections ==
        | CreateList(listReg, itemsToAddRegs) ->
          let itemsToAdd = itemsToAddRegs |> List.map (fun r -> registers[r])
          registers[listReg] <-
            TypeChecker.DvalCreator.list vm.threadID VT.unknown itemsToAdd

        | CreateDict(dictReg, entries) ->
          let entries =
            entries |> List.map (fun (key, valueReg) -> (key, registers[valueReg]))
          registers[dictReg] <-
            TypeChecker.DvalCreator.dict vm.threadID VT.unknown entries

        | CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ->
          let first = registers[firstReg]
          let second = registers[secondReg]
          let theRest = theRestRegs |> List.map (fun r -> registers[r])
          registers[tupleReg] <- DTuple(first, second, theRest)


        // == Working with Custom Data ==
        // -- Records --
        | CreateRecord(recordReg, sourceTypeName, typeArgs, fields) ->
          let fields =
            fields |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

          let! typeArgs =
            typeArgs
            |> Ply.List.mapSequentially (
              TypeReference.toVT exeState.types currentFrame.typeSymbolTable
            )

          let! record =
            TypeChecker.DvalCreator.record
              exeState.types
              vm.threadID
              currentFrame.typeSymbolTable
              sourceTypeName
              typeArgs
              fields

          registers[recordReg] <- record


        | CloneRecordWithUpdates(targetReg, originalRecordReg, fieldUpdates) ->
          let originalRecord = registers[originalRecordReg]

          // CLEANUP maybe some of this logic should be in the typechecker
          match originalRecord with
          | DRecord(sourceTypeName, resolvedTypeName, typeArgs, originalFields) ->
            let fieldUpdates =
              fieldUpdates
              |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

            let! updatedRecord =
              TypeChecker.DvalCreator.recordUpdate
                exeState.types
                vm.threadID
                currentFrame.typeSymbolTable
                sourceTypeName
                resolvedTypeName
                typeArgs
                originalFields
                fieldUpdates

            registers[targetReg] <- updatedRecord

          | dv ->
            Dval.toValueType dv
            |> RTE.Records.UpdateNotRecord
            |> RTE.Record
            |> raiseRTE


        | GetRecordField(targetReg, recordReg, fieldName) ->
          match registers[recordReg] with
          | DRecord(_, _, _, fields) ->
            if fieldName = "" then
              RTE.Records.FieldAccessEmptyFieldName |> RTE.Record |> raiseRTE
            else
              match Map.find fieldName fields with
              | Some value -> registers[targetReg] <- value
              | None ->
                RTE.Records.FieldAccessFieldNotFound fieldName
                |> RTE.Record
                |> raiseRTE
          | dv ->
            RTE.Records.FieldAccessNotRecord(Dval.toValueType dv)
            |> RTE.Record
            |> raiseRTE


        // -- Enums --
        | CreateEnum(enumReg, typeName, typeArgs, caseName, fields) ->
          let fields = fields |> List.map (fun valueReg -> registers[valueReg])

          let tst = currentFrame.typeSymbolTable

          let! typeArgs =
            typeArgs
            |> Ply.List.mapSequentially (TypeReference.toVT exeState.types tst)

          let! newEnum =
            TypeChecker.DvalCreator.enum
              exeState.types
              vm.threadID
              tst
              typeName
              typeArgs
              caseName
              fields

          registers[enumReg] <- newEnum


        | LoadConstant(createTo, name) ->
          match name with
          | FQConstantName.Builtin builtin ->
            match Map.find builtin exeState.constants.builtIn with
            | Some c -> registers[createTo] <- c.body
            | None -> raiseRTE (RTE.ConstNotFound name)

          | FQConstantName.Package pkg ->
            match! exeState.constants.package pkg with
            | Some c ->
              let! dv =
                evalConst
                  exeState.types
                  vm.threadID
                  currentFrame.typeSymbolTable
                  c.body
              registers[createTo] <- dv
            | None -> raiseRTE (RTE.ConstNotFound name)


        | CreateLambda(lambdaReg, impl) ->
          vm.lambdaInstrCache <-
            vm.lambdaInstrCache
            |> Map.add (currentFrame.executionPoint, impl.exprId) impl
            // CLEANUP why do we need this? ask Ocean for a reminder. I really feel like we shouldn't.
            |> Map.add (Source, impl.exprId) impl

          registers[lambdaReg] <-
            { exprId = impl.exprId
              closedRegisters =
                impl.registersToCloseOver
                |> List.map (fun (parentReg, childReg) ->
                  childReg, registers[parentReg])
              typeSymbolTable = currentFrame.typeSymbolTable
              argsSoFar = [] }
            |> AppLambda
            |> DApplicable



        // == Working with things that Apply (fns, lambdas) ==
        // `add (increment 1L) (3L)` and store results in `putResultIn`
        | Apply(putResultIn, thingToCallReg, typeArgs, newArgRegs) ->
          // CLEANUP
          // only the first apply of an applicable should be allowed to provide type args

          let applicable =
            let thingToCall = registers[thingToCallReg]
            match thingToCall with
            | DApplicable applicable -> applicable
            | _ ->
              RTE.Applications.ExpectedApplicableButNot(
                Dval.toValueType thingToCall,
                thingToCall
              )
              |> RTE.Apply
              |> raiseRTE

          let newArgDvals =
            newArgRegs |> NEList.toList |> List.map (fun r -> registers[r])

          match applicable with
          | AppLambda appLambda ->
            let exprId = appLambda.exprId
            let foundLambda =
              match
                Map.tryFind (currentFrame.executionPoint, exprId) vm.lambdaInstrCache
              with
              | Some lambda -> lambda
              | None ->
                match Map.tryFind (Source, exprId) vm.lambdaInstrCache with
                | Some lambda -> lambda
                | None ->
                  Exception.raiseInternal "lambda not found" [ "exprId", exprId ]

            let allArgs = appLambda.argsSoFar @ newArgDvals

            let argCount = List.length allArgs
            let paramCount = NEList.length foundLambda.patterns

            if typeArgs <> [] then
              RTE.Applications.CannotApplyTypeArgsToLambda |> RTE.Apply |> raiseRTE

            if argCount = paramCount then
              let newFrame =
                { id = guuid ()
                  parent = Some(vm.currentFrameID, putResultIn, counter + 1)
                  programCounter = 0
                  registers =
                    let r = Array.zeroCreate foundLambda.instructions.registerCount

                    // extract and copy over the args
                    List.zip (NEList.toList foundLambda.patterns) allArgs
                    |> List.iter (fun (pat, arg) ->
                      let doesMatch, registersToAssign =
                        checkAndExtractLetPattern pat arg

                      if doesMatch then
                        registersToAssign
                        |> List.iter (fun (reg, value) -> r[reg] <- value)
                      else
                        raiseRTE (RTE.Let(RTE.Lets.PatternDoesNotMatch(arg, pat))))

                    // copy over closed registers
                    appLambda.closedRegisters
                    |> List.iter (fun (reg, value) -> r[reg] <- value)

                    r
                  typeSymbolTable =
                    // CLEANUP is this right? (I think yes...)
                    Map.mergeFavoringRight
                      appLambda.typeSymbolTable
                      currentFrame.typeSymbolTable
                  executionPoint = Lambda(currentFrame.executionPoint, exprId) }

              frameToPush <- Some newFrame

            else if argCount > paramCount then
              RTE.Applications.TooManyArgsForLambda(exprId, paramCount, argCount)
              |> RTE.Apply
              |> raiseRTE
            else
              registers[putResultIn] <-
                { appLambda with argsSoFar = allArgs } |> AppLambda |> DApplicable

          | AppNamedFn applicable ->
            // some helpers
            let name = applicable.name

            let handleTooManyArgs expected actual =
              RTE.Applications.TooManyArgsForFn(name, expected, actual)
              |> RTE.Apply
              |> raiseRTE

            let handleWrongTypeArgCount expected actual =
              RTE.Applications.WrongNumberOfTypeArgsForFn(name, expected, actual)
              |> RTE.Apply
              |> raiseRTE

            let typeCheckParam =
              TypeChecker.checkFnParam exeState.types applicable.name

            let mutable tst =
              Map.mergeFavoringRight
                currentFrame.typeSymbolTable
                applicable.typeSymbolTable

            let typeCheckParams pairs =
              pairs
              |> Ply.List.iterSequentially (fun ((pIndex, pName, pType), arg) ->
                uply {
                  match! typeCheckParam tst pIndex pName pType arg with
                  | Ok updatedTst ->
                    tst <- updatedTst
                    return ()
                  | Error rte -> return raiseRTE rte
                })

            let typeArgs =
              match applicable.typeArgs, typeArgs with
              | [], newTypeArgs -> newTypeArgs
              | oldTypeArgs, [] -> oldTypeArgs
              | _, _ ->
                RTE.Applications.CannotApplyTypeArgsMoreThanOnce
                |> RTE.Apply
                |> raiseRTE

            // TODO: reduce duplication between branches
            match applicable.name with
            | FQFnName.Builtin builtin ->
              match Map.find builtin exeState.fns.builtIn with
              | None -> return RTE.FnNotFound(FQFnName.Builtin builtin) |> raiseRTE
              | Some fn ->
                // Process type args
                // - there should be right #,
                // - we should update _some_ TST with new info
                let typeParamCount, typeArgCount =
                  (List.length fn.typeParams, List.length typeArgs)
                if typeArgCount <> typeParamCount then
                  return handleWrongTypeArgCount typeParamCount typeArgCount

                let! typeArgsVT =
                  typeArgs
                  |> Ply.List.mapSequentially (TypeReference.toVT exeState.types tst)
                tst <-
                  let newlyBound = List.zip fn.typeParams typeArgsVT |> Map
                  Map.mergeFavoringRight tst newlyBound

                // type-check new arguments against the corresponding parameters
                do!
                  List.zipUntilEitherEnds
                    (fn.parameters
                     |> List.mapi (fun i p -> i, p.name, p.typ)
                     |> List.skip (List.length applicable.argsSoFar))
                    newArgDvals
                  |> typeCheckParams

                let allArgs = applicable.argsSoFar @ newArgDvals

                let paramCount, argCount =
                  (List.length fn.parameters, List.length allArgs)

                let! result =
                  uply {
                    if argCount > paramCount then
                      return handleTooManyArgs paramCount argCount
                    else if argCount < paramCount then
                      return
                        // CLEANUP should the typeArgs here be of VTs, not TRs? check out usages, I suppose.
                        { applicable with
                            typeSymbolTable = tst
                            typeArgs = typeArgs
                            argsSoFar = allArgs }
                        |> AppNamedFn
                        |> DApplicable
                    else
                      let! result = fn.fn (exeState, vm, typeArgs, allArgs)

                      // now: type-check result against the return type
                      //let tst = currentFrame.typeSymbolTable
                      let expectedReturnType = fn.returnType
                      match!
                        TypeChecker.checkFnResult
                          exeState.types
                          (FQFnName.Builtin fn.name)
                          tst
                          expectedReturnType
                          result
                      with
                      | Ok _updatedTst ->
                        //currentFrame.typeSymbolTable <- updatedTst
                        // CLEANUP is this^ or something like it worthwhile?
                        // I think so, but let's wait until some relevant bug comes up...
                        ()
                      | Error rte -> raiseRTE rte

                      return result
                  }

                registers[putResultIn] <- result

            | FQFnName.Package pkg ->
              match! exeState.fns.package pkg with
              | None -> return RTE.FnNotFound(FQFnName.Package pkg) |> raiseRTE
              | Some fn ->
                // Process type args
                // - there should be right #,
                // - we should update _some_ TST with new info
                let typeParamCount, typeArgCount =
                  (List.length fn.typeParams, List.length typeArgs)
                if typeArgCount <> typeParamCount then
                  return handleWrongTypeArgCount typeParamCount typeArgCount

                let! typeArgsVT =
                  typeArgs
                  |> Ply.List.mapSequentially (TypeReference.toVT exeState.types tst)
                tst <-
                  let newlyBound = List.zip fn.typeParams typeArgsVT |> Map
                  Map.mergeFavoringRight tst newlyBound

                // type-check new arguments against the corresponding parameters
                do!
                  List.zipUntilEitherEnds
                    (fn.parameters
                     |> NEList.toList
                     |> List.mapi (fun i p -> i, p.name, p.typ)
                     |> List.skip (List.length applicable.argsSoFar))
                    newArgDvals
                  |> typeCheckParams

                let allArgs = applicable.argsSoFar @ newArgDvals

                let paramCount, argCount =
                  (NEList.length fn.parameters, List.length allArgs)

                if argCount > paramCount then
                  return handleTooManyArgs paramCount argCount
                else if argCount < paramCount then
                  registers[putResultIn] <-
                    { applicable with
                        typeArgs = typeArgs
                        argsSoFar = allArgs
                        typeSymbolTable = tst }
                    |> AppNamedFn
                    |> DApplicable
                else
                  // push a new frame to execute the function
                  // , and the interpreter will evaluate it shortly
                  frameToPush <-
                    { id = guuid ()
                      parent = Some(vm.currentFrameID, putResultIn, counter + 1)
                      programCounter = 0
                      registers =
                        let r = Array.zeroCreate fn.body.registerCount
                        allArgs |> List.iteri (fun i arg -> r[i] <- arg)
                        r
                      typeSymbolTable = currentFrame.typeSymbolTable // copy. probably also need to _extend_ here.
                      executionPoint = Function(FQFnName.Package fn.id) }
                    |> Some

        | RaiseNRE nre -> raiseRTE (RTE.ParseTimeNameResolution nre)

        | CheckIfFirstExprIsUnit reg ->
          match registers[reg] with
          | DUnit -> ()
          | dval ->
            RTE.Statements.FirstExpressionMustBeUnit(
              ValueType.Known KTUnit,
              Dval.toValueType dval,
              dval
            )
            |> RuntimeError.Statement
            |> raiseRTE

        counter <- counter + 1


      // exited loop -- either pushed a frame or finished the current frame

      match frameToPush with
      | Some newFrame ->
        // Something in this eval just pushed a frame -- don't do the "normal" processing
        vm.callFrames <- Map.add newFrame.id newFrame vm.callFrames
        vm.currentFrameID <- newFrame.id

      | None ->
        // We are at the end of the instructions of the current frame
        // Either we're done with the whole eval, or we need to return a value to the parent frame
        let resultOfFrame = registers[instrData.resultReg]

        match currentFrame.parent with
        | Some(parentID, regOfParentToPutResultInto, pcOfParent) ->
          // We just finished processing a frame, and we need to return a value to the parent frame

          // TODO this might be where the type-checking of a fn result needs to happen.
          // But when here, it's not always a fn call - could also be for a lambda.

          // Type-check results of fns
          match currentFrame.executionPoint with
          | Source -> ()
          | Lambda _ -> ()
          | Function fnName ->
            let! expectedReturnType =
              match fnName with
              | FQFnName.Package id ->
                uply {
                  let! fn = exeState.fns.package id
                  match fn with
                  | None -> return RTE.FnNotFound fnName |> raiseRTE
                  | Some fn -> return fn.returnType
                }

              | FQFnName.Builtin builtin ->
                let fn = Map.findUnsafe builtin exeState.fns.builtIn
                Ply fn.returnType

            let tst = currentFrame.typeSymbolTable
            match!
              TypeChecker.unify exeState.types tst expectedReturnType resultOfFrame
            with
            | Ok _updatedTst ->
              //currentFrame.typeSymbolTable <- updatedTst
              // CLEANUP is this^ or something like it worthwhile?
              ()
            | Error _path ->
              let! expectedVT =
                TypeReference.toVT exeState.types tst expectedReturnType
              return
                RuntimeError.Applications.FnResultNotExpectedType(
                  fnName,
                  expectedVT,
                  Dval.toValueType resultOfFrame,
                  resultOfFrame
                )
                |> RuntimeError.Apply
                |> raiseRTE

          // TODO think about if/when we should actually do this.
          //vm.callFrames <- Map.remove vm.currentFrameID vm.callFrames

          vm.currentFrameID <- parentID

          let parentFrame = Map.findUnsafe parentID vm.callFrames
          parentFrame.registers[regOfParentToPutResultInto] <- resultOfFrame
          parentFrame.programCounter <- pcOfParent

        | None ->
          vm.callFrames <- Map.remove vm.currentFrameID vm.callFrames
          finalResult <- Some resultOfFrame


    // If we've reached the end of the instructions, return the result
    match finalResult with
    | Some dv -> return dv
    | None -> return Exception.raiseInternal "No finalResult found" []
  }
