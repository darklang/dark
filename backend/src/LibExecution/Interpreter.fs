/// Interprets Dark instructions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open Prelude
open RuntimeTypes
module RTE = RuntimeError
module VT = ValueType


/// The free type-vars of a function's signature -- its "implicit" type parameters -- computed once per
/// function rather than per call.
///
/// Used to shadow inherited TST bindings of the same name when entering the fn's frame. Without
/// shadowing, an outer fn's `'a := KTString` would leak into a nested fn's param of type `value: 'a` and
/// force the nested arg to be a String even when the nested fn's own `'a` was meant to be something else.
///
/// Safe to memoise: it's a pure function of the signature, and a signature can't change under a running
/// process, since package fns are content-addressed and builtins are compiled in. Most functions have no
/// free type-vars at all, and `Set.empty` is a singleton, so the usual answer costs nothing.
///
/// Keyed by name/hash rather than held on the fn record because `BuiltInFn` and `PackageFn` are defined
/// well below here and shared with code that has no interest in this.
module private FreeTVars =
  let private builtins =
    System.Collections.Concurrent.ConcurrentDictionary<FQFnName.Builtin, Set<string>>()

  let private packages =
    System.Collections.Concurrent.ConcurrentDictionary<Hash, Set<string>>()

  let rec private collect (acc : Set<string>) (tr : TypeReference) : Set<string> =
    match tr with
    | TVariable name -> Set.add name acc
    | TList inner
    | TStream inner
    | TDict inner
    | TDB inner -> collect acc inner
    | TTuple(a, b, rest) ->
      let acc = collect acc a
      let acc = collect acc b
      rest |> List.fold collect acc
    | TCustomType(_, args) -> args |> List.fold collect acc
    | TFn(args, ret) ->
      let acc = NEList.toList args |> List.fold collect acc
      collect acc ret
    | _ -> acc

  let ofBuiltin (fn : BuiltInFn) : Set<string> =
    match builtins.TryGetValue fn.name with
    | true, v -> v
    | false, _ ->
      let v =
        fn.parameters
        |> List.fold (fun acc (p : BuiltInParam) -> collect acc p.typ) Set.empty
        |> fun acc -> collect acc fn.returnType
      builtins[fn.name] <- v
      v

  /// Same idea for the frame's `ExecutionPoint`. It's two allocations -- the `FQFnName.Package` and the
  /// `Function` around it -- built identically on every call to the same function.
  let private packageEntryPoints =
    System.Collections.Concurrent.ConcurrentDictionary<Hash, ExecutionPoint>()

  let packageExecutionPoint (hash : Hash) : ExecutionPoint =
    match packageEntryPoints.TryGetValue hash with
    | true, v -> v
    | false, _ ->
      let v = Function(FQFnName.Package hash)
      packageEntryPoints[hash] <- v
      v

  let ofPackage (fn : PackageFn.PackageFn) : Set<string> =
    match packages.TryGetValue fn.hash with
    | true, v -> v
    | false, _ ->
      let v =
        fn.parameters
        |> NEList.toList
        |> List.fold
          (fun acc (p : PackageFn.Parameter) -> collect acc p.typ)
          Set.empty
        |> fun acc -> collect acc fn.returnType
      packages[fn.hash] <- v
      v


/// Read a list of registers into their Dvals in a single pass.
///
/// Don't simplify this back to `regs |> NEList.toList |> List.map (fun r -> registers[r])`: that runs on
/// every Apply and allocates the intermediate list, the mapped result and a closure over `registers`.
/// Top-level rather than local so `registers` is a parameter and nothing is captured.
let rec private readRegs
  (registers : Registers)
  (regs : List<Register>)
  : List<Dval> =
  match regs with
  | [] -> []
  | r :: rest -> registers[r] :: readRegs registers rest

let private readRegsNE
  (registers : Registers)
  (regs : NEList<Register>)
  : List<Dval> =
  registers[regs.head] :: readRegs registers regs.tail


/// True iff the ValueType has no Unknown anywhere in its tree.
/// Empty-literal lists like `[]` are typed `Known (KTList Unknown)`;
/// binding a TVariable to that kind of half-Unknown shape would lock
/// in an over-tight constraint that downstream callers (which pass
/// concretely-typed values) would fail. We only infer from
/// fully-known shapes for that reason.
let rec private isFullyKnown (vt : ValueType) : bool =
  match vt with
  | ValueType.Unknown -> false
  | ValueType.Known kt ->
    match kt with
    | KTUnit
    | KTBool
    | KTInt8
    | KTUInt8
    | KTInt16
    | KTUInt16
    | KTInt32
    | KTUInt32
    | KTInt64
    | KTUInt64
    | KTInt128
    | KTUInt128
    | KTInt
    | KTFloat
    | KTChar
    | KTString
    | KTUuid
    | KTDateTime
    | KTBlob
    | KTDB _ -> true
    | KTList inner
    | KTStream inner
    | KTDict inner -> isFullyKnown inner
    | KTTuple(a, b, rest) ->
      isFullyKnown a && isFullyKnown b && List.forall isFullyKnown rest
    | KTCustomType(_, args) -> List.forall isFullyKnown args
    | KTFn(args, ret) ->
      NEList.toList args |> List.forall isFullyKnown && isFullyKnown ret


/// TODO richer error messages around type-variable resolution. When a
/// `FnParameterNotExpectedType` fires after inference has bound some
/// TVariables, the error currently shows the *resolved* expected type
/// (e.g. "expects List<(String * Int64)>") which is more accurate than
/// before but loses the fact that `Int64` came from inference and was
/// reasoned-back-from a sibling argument. A "(inferred from arg N's
/// VT)" annotation would help users understand why the type check
/// fired the way it did. Touches RuntimeError formatting and the
/// type-checker's binding trail. Not on fire today.
/// Walk a TypeReference and a ValueType in lockstep, collecting
/// `TVariable name -> VT` bindings. Used at function-call sites to
/// infer type-variable values from actual arguments — so a wrapper
/// of the shape `let f (x: List<'a>) : Stream<'a> = ...` works when
/// called as `f [1L; 2L]` without the caller passing explicit type
/// args. Inference is conservative:
///   - TVariable binds only when the corresponding ValueType is
///     fully Known (no Unknown anywhere). Half-Unknown shapes
///     (e.g. `Known (KTList Unknown)` from an empty literal `[]`)
///     would lock in an over-tight constraint for downstream calls.
///   - Pre-existing bindings in `acc` win — explicit type args from
///     the call site stay authoritative.
///   - On any shape mismatch (different head, mismatched arity), we
///     return `acc` unchanged and let typeCheckParams report the
///     real error.
/// Matches on `tr` and then on `vt`, rather than on the pair, and walks argument lists in lockstep
/// rather than zipping them. `Tuple<TypeReference, ValueType>` was the most-allocated type in the
/// interpreter's allocation-tick profile, and this function is where they came from: it runs per argument
/// per call and recurses through the type structure.
let rec private inferTVarsFromArg
  (acc : Map<string, ValueType>)
  (tr : TypeReference)
  (vt : ValueType)
  : Map<string, ValueType> =
  match tr with
  | TVariable name ->
    if isFullyKnown vt && not (Map.containsKey name acc) then
      Map.add name vt acc
    else
      acc

  | TList tr' ->
    match vt with
    | ValueType.Known(KTList vt') -> inferTVarsFromArg acc tr' vt'
    | _ -> acc
  | TStream tr' ->
    match vt with
    | ValueType.Known(KTStream vt') -> inferTVarsFromArg acc tr' vt'
    | _ -> acc
  | TDict tr' ->
    match vt with
    | ValueType.Known(KTDict vt') -> inferTVarsFromArg acc tr' vt'
    | _ -> acc

  | TTuple(a, b, rest) ->
    match vt with
    | ValueType.Known(KTTuple(a', b', rest')) ->
      let acc = inferTVarsFromArg acc a a'
      let acc = inferTVarsFromArg acc b b'
      inferTVarsFromArgs acc rest rest'
    | _ -> acc

  | TCustomType({ resolved = Ok _ }, typeArgs) ->
    match vt with
    | ValueType.Known(KTCustomType(_, vtArgs)) ->
      inferTVarsFromArgs acc typeArgs vtArgs
    | _ -> acc

  | _ -> acc

/// Lockstep over matched-arity type argument lists. Arity mismatch returns `acc` untouched, as the
/// zip-based version did -- `typeCheckParams` reports the real error.
and private inferTVarsFromArgs
  (acc : Map<string, ValueType>)
  (trs : List<TypeReference>)
  (vts : List<ValueType>)
  : Map<string, ValueType> =
  if List.length trs <> List.length vts then
    acc
  else
    let rec go acc (trs : List<TypeReference>) (vts : List<ValueType>) =
      match trs with
      | [] -> acc
      | tr :: trRest ->
        match vts with
        | [] -> acc
        | vt :: vtRest -> go (inferTVarsFromArg acc tr vt) trRest vtRest
    go acc trs vts


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
  | LPWildcard, _ -> true, []
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
  | MPInt l, DInt r -> l = DarkInt.toBigInt r, []
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

  | MPOr patterns, dv ->
    patterns
    |> NEList.toList
    |> List.map (fun p -> r p dv)
    |> List.tryFind (fun (matches, _) -> matches)
    |> Option.defaultValue (false, [])

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
  | MPInt _, _
  | MPFloat _, _
  | MPChar _, _
  | MPString _, _
  | MPTuple _, _
  | MPListCons _, _
  | MPList _, _
  | MPEnum _, _
  | MPOr _, _ -> false, []




/// Record bytes allocated in a synchronous region of the Apply path. `before` must come from a
/// `GC.GetAllocatedBytesForCurrentThread()` taken in the same synchronous stretch -- see the note on
/// `InterpreterStats.allocByStage` for why spanning an await would measure the wrong thing.
let inline private recordStage (vm : VMState) (stage : int) (before : int64) : unit =
  if vm.stats.enabled then
    let d = System.GC.GetAllocatedBytesForCurrentThread() - before
    if d > 0L then vm.stats.allocByStage[stage] <- vm.stats.allocByStage[stage] + d

/// Frame identity is internal to a VM: `callFrames`, `pendingCallArgs` and `framePushTimestamps` key on it,
/// and the tracer's `storeFrameEntry` ignores the argument entirely. It never reaches storage or the wire.
///
/// So it doesn't need to be random. `Guid.NewGuid()` draws from the cryptographic RNG, and a frame push
/// happens tens of thousands of times in a single command.
///
/// The counter lives on the VM, not in this module. A VM's interpreter loop is single-threaded so a
/// per-VM counter needs no synchronization, whereas a process-global `let mutable` does: tests run VMs in
/// parallel, and a non-atomic shared increment handed two frames the same id, dropping one from
/// `callFrames` and failing the parent lookup on return. Caught by
/// `Interpreter.Fns.Package.Recursion.addUpTo 30000`.
let inline private nextFrameId (vm : VMState) : uuid =
  vm.frameIdCounter <- vm.frameIdCounter + 1L
  let n = vm.frameIdCounter
  System.Guid(
    int (n &&& 0xFFFFFFFFL),
    int16 ((n >>> 32) &&& 0xFFFFL),
    0s,
    0uy,
    0uy,
    0uy,
    0uy,
    0uy,
    0uy,
    0uy,
    0uy
  )

/// `Map.remove` rebuilds nodes along the search path even when the key isn't there.
///
/// The type symbol table averages 1.23 entries (measured), and the names stripped from it are a function's
/// own type variables, which are usually *not* bound by the caller -- so most of these removes were
/// allocating a new tree to produce the same contents. `Map.containsKey` is a lookup and allocates nothing.
let inline private removeIfPresent
  (name : string)
  (m : TypeSymbolTable)
  : TypeSymbolTable =
  if Map.containsKey name m then Map.remove name m else m


/// Check as many arguments as can be answered without awaiting, returning where it stopped.
///
/// `TypeChecker.tryUnifySync` answers the ordinary cases outright, so this walks the parameter and
/// argument lists together for as long as that keeps working, and hands the remainder to the
/// computation-expression version. Getting all the way through, which is the usual case, allocates
/// nothing: top-level so nothing is captured, and a struct tuple so the return isn't an allocation.
let rec private checkBiParamsSync
  (i : int)
  (ps : List<BuiltInParam>)
  (args : List<Dval>)
  (tst : TypeSymbolTable)
  : struct (int * List<BuiltInParam> * List<Dval> * TypeSymbolTable) =
  match ps with
  | [] -> struct (i, ps, args, tst)
  | p :: pRest ->
    match args with
    | [] -> struct (i, ps, args, tst)
    | a :: aRest ->
      match TypeChecker.tryUnifySync tst p.typ a with
      | ValueSome updatedTst -> checkBiParamsSync (i + 1) pRest aRest updatedTst
      | ValueNone -> struct (i, ps, args, tst)

/// As [checkBiParamsSync], for package fns.
let rec private checkPkgParamsSync
  (i : int)
  (ps : List<PackageFn.Parameter>)
  (args : List<Dval>)
  (tst : TypeSymbolTable)
  : struct (int * List<PackageFn.Parameter> * List<Dval> * TypeSymbolTable) =
  match ps with
  | [] -> struct (i, ps, args, tst)
  | p :: pRest ->
    match args with
    | [] -> struct (i, ps, args, tst)
    | a :: aRest ->
      match TypeChecker.tryUnifySync tst p.typ a with
      | ValueSome updatedTst -> checkPkgParamsSync (i + 1) pRest aRest updatedTst
      | ValueNone -> struct (i, ps, args, tst)


let inline private allocNow (vm : VMState) : int64 =
  if vm.stats.enabled then System.GC.GetAllocatedBytesForCurrentThread() else 0L


/// Run consecutive instructions that need no `await`, without entering the interpreter's computation
/// expression at all. Returns the counter where it stopped: past the end of the block, or at one of the
/// five opcodes that must be handled on the async path.
///
/// Those five -- CreateRecord, CloneRecordWithUpdates, CreateEnum, LoadValue, Apply -- are the only ones
/// that await. The other eighteen are register moves, jumps, comparisons, match tests and container
/// construction, and they are the overwhelming majority of instructions executed.
let private runSyncInstructions
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (registers : Dval array)
  (instrData : InstrData)
  (startCounter : int)
  : int =
  let raiseRTE rte = raiseRTE vm.threadID rte
  let mutable counter = startCounter
  let mutable running = true

  while running && counter < instrData.instructions.Length do
    let inst = instrData.instructions[counter]

    match inst with
    | CreateRecord _
    | CloneRecordWithUpdates _
    | CreateEnum _
    | LoadValue _
    | Apply _ -> running <- false
    | _ ->
      if vm.stats.enabled then
        vm.stats.instructionCount <- vm.stats.instructionCount + 1L

      let allocBefore =
        if vm.stats.enabled then
          System.GC.GetAllocatedBytesForCurrentThread()
        else
          0L

      match inst with
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
        // Fast path for the common single-variable let binding
        match pat with
        | LPVariable extractTo -> registers[extractTo] <- dv
        | LPUnit ->
          match dv with
          | DUnit -> ()
          | _ -> raiseRTE (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))
        | _ ->
          let doesMatch, registersToAssign = checkAndExtractLetPattern pat dv
          if doesMatch then
            registersToAssign
            |> List.iter (fun (reg, value) -> registers[reg] <- value)
          else
            raiseRTE (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))


      // TODO References to DBs should be resolved at parse-time, not
      // runtime. For consistency, safety, etc. We should have a specific
      // EReferenceDB construct that we respect throughout WT, NR, PT, RT,
      // PT2RT, etc. I don't think this would be that hard.
      | VarNotFound(targetRegIfDB, varName) ->
        match exeState.program.dbs |> Map.get varName with
        | Some _foundDB -> registers[targetRegIfDB] <- DDB varName
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
        // Fast path for common single-variable match
        match pat with
        | MPVariable reg -> registers[reg] <- registers[valueReg]
        | _ ->
          let doesMatch, registersToAssign =
            checkAndExtractMatchPattern pat registers[valueReg]
          if doesMatch then
            registersToAssign
            |> List.iter (fun (reg, value) -> registers[reg] <- value)
          else
            counter <- counter + failJump
      | MatchUnmatched(valueReg) ->
        let unmatchedValue = registers[valueReg]
        raiseRTE (RTE.Match(RTE.Matches.MatchUnmatched unmatchedValue))


      // == Working with Collections ==
      | CreateList(listReg, itemsToAddRegs) ->
        let itemsToAdd = readRegs registers itemsToAddRegs
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
        let theRest = readRegs registers theRestRegs
        registers[tupleReg] <- DTuple(first, second, theRest)


      // == Working with Custom Data ==
      // -- Records --
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
      | CreateLambda(lambdaReg, impl) ->
        exeState.lambdaInstrCache[impl.exprId] <- impl

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
      | RaiseNRE(names, nre) -> raiseRTE (RTE.ParseTimeNameResolution(names, nre))

      // CLEANUP: consider renaming this to something like "RequireExprToReturnUnit"
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
      // Unreachable: these five were filtered out above, but the match must be exhaustive.
      | CreateRecord _
      | CloneRecordWithUpdates _
      | CreateEnum _
      | LoadValue _
      | Apply _ -> ()

      if vm.stats.enabled then
        let tag = Opcode.index inst
        if tag >= 0 && tag < vm.stats.allocByOpcode.Length then
          let delta = System.GC.GetAllocatedBytesForCurrentThread() - allocBefore
          if delta > 0L then
            vm.stats.allocByOpcode[tag] <- vm.stats.allocByOpcode[tag] + delta
          vm.stats.countByOpcode[tag] <- vm.stats.countByOpcode[tag] + 1L

      counter <- counter + 1

  counter



/// Write a pattern-match's register assignments into a frame's registers.
///
/// Top-level and fully parameterised so nothing is captured: the `List.iter` this replaces allocated a
/// closure over the register array, on a path that runs once per lambda parameter per lambda call.
let rec private assignRegisters
  (r : Dval array)
  (assignments : List<Register * Dval>)
  : unit =
  match assignments with
  | [] -> ()
  | (reg, value) :: rest ->
    r[reg] <- value
    assignRegisters r rest

/// Bind a lambda's parameters to its arguments, in lockstep.
///
/// The `List.zip |> List.iter` this replaces built a pair per parameter and two closures per call.
let rec private bindLambdaParams
  (vm : VMState)
  (r : Dval array)
  (pats : List<LetPattern>)
  (args : List<Dval>)
  : unit =
  match pats with
  | [] -> ()
  | pat :: patRest ->
    match args with
    | [] -> ()
    | arg :: argRest ->
      // One name bound to one register is the overwhelmingly common shape, and
      // `checkAndExtractLetPattern` costs four allocations to express it: the returned tuple, the cons,
      // the pair inside it, and the pair its own `match pat, dv with` builds. Per parameter, per call.
      // `CheckLetPattern` already short-circuits the same way.
      match pat with
      | LPVariable extractTo -> r[extractTo] <- arg
      | _ ->
        let doesMatch, registersToAssign = checkAndExtractLetPattern pat arg
        if doesMatch then
          assignRegisters r registersToAssign
        else
          RTE.Let(RTE.Lets.PatternDoesNotMatch(arg, pat)) |> raiseRTE vm.threadID
      bindLambdaParams vm r patRest argRest


/// The parts of an `Apply` that the call paths below all need.
///
/// Bundled so their call sites aren't a column of eight positional arguments, twice. A struct record, so
/// passing it around costs nothing; each function unpacks what it uses at the top rather than reading
/// through `ctx.` everywhere.
[<Struct>]
type private ApplyContext =
  {
    applicable : ApplicableNamedFn
    typeArgs : List<TypeReference>
    args : List<Dval>
    tst : TypeSymbolTable
    /// Register in the calling frame that the result goes in.
    putResultIn : Register
    /// Where to resume the calling frame once this call returns.
    returnPc : int
  }


/// Explicit type args, resolved against the OUTER symbol table so the wrapper-pass-through pattern works:
/// a wrapper body calling `Builtin.x<'a>` needs `'a` resolved against the wrapper's table, not the
/// post-shadow one.
///
/// `ValueNone` means it needs the package store. That is the one step that can block before any of the
/// call has happened, and it is rare -- most calls pass no type args at all -- so the callers keep it out
/// of line rather than awaiting on every call.
let private resolveTypeArgsSync
  (exeState : ExecutionState)
  (vm : VMState)
  (name : FQFnName.FQFnName)
  (typeParamCount : int)
  (ctx : ApplyContext)
  : List<ValueType> voption =
  match ctx.typeArgs with
  | [] -> ValueSome []
  | typeArgs ->
    // Separate lets, not `let a, b = (x, y)`: the tuple form allocates.
    let typeArgCount = List.length typeArgs
    if typeArgCount <> typeParamCount then
      RTE.Applications.WrongNumberOfTypeArgsForFn(name, typeParamCount, typeArgCount)
      |> RTE.Apply
      |> raiseRTE vm.threadID
    typeArgs
    |> Ply.List.mapSequentially (TypeReference.toVT exeState.types ctx.tst)
    |> Ply.trySync

/// As `resolveTypeArgsSync`, for when it does need the store. Cold path; the arity check already ran.
let private resolveTypeArgsAsync
  (exeState : ExecutionState)
  (ctx : ApplyContext)
  : Ply<List<ValueType>> =
  ctx.typeArgs
  |> Ply.List.mapSequentially (TypeReference.toVT exeState.types ctx.tst)



/// Everything from "we have the arguments and a checked symbol table" to "we have a checked result".
///
/// Shared by the synchronous path and the fallback below it, so there is one copy of the capability gate,
/// the stats bracket, the result check and the trace.
let private invokeBuiltin
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (fn : BuiltInFn)
  (tst : TypeSymbolTable)
  (typeArgs : List<TypeReference>)
  (allArgs : List<Dval>)
  : Ply<Dval> =
  let raiseRTE rte = raiseRTE vm.threadID rte
  // Resolve type variables in typeArgs before passing to builtin.
  // When a package function like Stdlib.Json.parse<Int64> calls
  // Builtin.jsonParse<'a>, the 'a needs to resolve to Int64.
  let resolvedTypeArgs =
    match typeArgs with
    | [] -> []
    | _ -> typeArgs |> List.map (TypeReference.resolveTypeVariables tst)
  let sw =
    if vm.stats.enabled then
      vm.stats.builtinCallCount <- vm.stats.builtinCallCount + 1L
      if vm.stats.detailedTiming then
        System.Diagnostics.Stopwatch.GetTimestamp()
      else
        0L
    else
      0L
  // capabilities gate: a builtin runs only if the instance's grant covers the DOMAIN
  // it declares it needs (`fn.capabilities`): a structural PRESENCE check, no name
  // matching. Nuanced builtins (http/file/exec/…) additionally enforce the SPECIFIC
  // target (URL/path/args) in their own body via `CapabilityCheck`. Default grant is
  // allCaps (no behavior change); a real instance narrows it (default NONE for `dark run`).
  // fast-path: pure builtins (the vast majority) all share the one `noCaps` instance,
  // so a reference check skips the structural scan entirely in the hot path. A false
  // negative (an all-empty need built fresh) just runs the full check, still correct.
  if not (System.Object.ReferenceEquals(fn.capabilities, Capabilities.noCaps)) then
    match Capabilities.coversStructurally exeState.grantedCaps fn.capabilities with
    | Capabilities.Denied what ->
      raiseRTE (
        RTE.UncaughtException(
          $"capability denied: `{fn.name.name}` needs {what}, which this instance doesn't grant. Grant it with `dark caps`.",
          []
        )
      )
    | Capabilities.Allowed -> ()

  let bodyAllocBefore =
    if vm.stats.enabled then System.GC.GetAllocatedBytesForCurrentThread() else 0L

  // Every builtin's signature is async because some of them have to be -- HTTP, the package store,
  // anything touching disk. Most aren't: `Int64.add` computes and returns.
  let body = fn.fn (exeState, vm, resolvedTypeArgs, allArgs)

  /// Stats, the result type-check and the trace. Runs whether or not the body had to wait.
  let finish (result : Dval) : Ply<Dval> =
    if vm.stats.enabled then
      let d = System.GC.GetAllocatedBytesForCurrentThread() - bodyAllocBefore
      if d > 0L then
        vm.stats.builtinBodyAlloc <- vm.stats.builtinBodyAlloc + d
        let n = fn.name.name
        match vm.stats.builtinAlloc.TryGetValue n with
        | true, v -> vm.stats.builtinAlloc[n] <- v + d
        | false, _ -> vm.stats.builtinAlloc[n] <- d
    // `sw = 0L` means the bracket never opened: timing was off when this call started and the call
    // itself turned it on (`interpreterStatsEnableDetailedTiming` is the case). Subtracting from zero
    // would record the raw tick count as a duration.
    if vm.stats.enabled && vm.stats.detailedTiming && sw <> 0L then
      let elapsed = System.Diagnostics.Stopwatch.GetTimestamp() - sw
      vm.stats.recordBuiltin (fn.name.name, elapsed)

    let trace (result : Dval) =
      if not exeState.tracing.skipTracing then
        let source : Tracing.Source = (currentFrame.executionPoint, None)
        let fnRecord : Tracing.FunctionRecord = (source, FQFnName.Builtin fn.name)
        exeState.tracing.storeFnResult
          fnRecord
          (NEList.ofListUnsafe "" [] allArgs)
          result
      result

    let biResAlloc = allocNow vm
    match TypeChecker.tryUnifySync tst fn.returnType result with
    | ValueSome _ ->
      recordStage vm ApplyStage.BiCheckResult biResAlloc
      Ply(trace result)
    | ValueNone ->
      uply {
        match!
          TypeChecker.checkFnResult
            exeState.types
            (FQFnName.Builtin fn.name)
            tst
            fn.returnType
            result
        with
        | Ok _ -> ()
        | Error rte -> raiseRTE rte
        recordStage vm ApplyStage.BiCheckResult biResAlloc
        return trace result
      }

  match Ply.trySync body with
  | ValueSome result -> finish result
  | ValueNone ->
    uply {
      let! result = body
      return! finish result
    }


/// Run a builtin call without entering the interpreter's computation expression.
///
/// Synchronous because almost nothing here ever waits: resolving a type arg is a cache hit, checking an
/// argument needs no lookup in the ordinary case, and a pure builtin hands back a `Ply` that is already
/// finished. Entering the builder for those costs a continuation closure each time, and this runs once
/// per call.
///
/// Two things can still need the package store, and each has an escape hatch that keeps a single
/// implementation rather than a fast copy and a slow copy that drift:
///
/// The three ways a builtin call ends, once the symbol table is settled: too many arguments, not enough
/// (so it stays a partial application), or exactly right.
///
/// Top-level rather than a local function, so it captures nothing. As a closure over the nine values it
/// needs, it was an allocation on every builtin call.
let private completeBuiltin
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (ctx : ApplyContext)
  (fn : BuiltInFn)
  (allArgs : List<Dval>)
  (argCount : int)
  (paramCount : int)
  (tst : TypeSymbolTable)
  : Ply<Dval> =
  if argCount > paramCount then
    RTE.Applications.TooManyArgsForFn(ctx.applicable.name, paramCount, argCount)
    |> RTE.Apply
    |> raiseRTE vm.threadID
  elif argCount < paramCount then
    // CLEANUP should the typeArgs here be of VTs, not TRs? check out usages, I suppose.
    { ctx.applicable with
        typeSymbolTable = tst
        typeArgs = ctx.typeArgs
        argsSoFar = allArgs }
    |> AppNamedFn
    |> DApplicable
    |> Ply
  else
    invokeBuiltin exeState vm currentFrame fn tst ctx.typeArgs allArgs


/// - Resolving explicit type args, which happens before anything else, is done by `callBuiltin` and
///   handed in here already resolved.
/// - Argument checking can need `Types.find` partway through, so it hands the *remainder* to a `uply`
///   and returns a `Called` that isn't finished yet.
let private callBuiltinResolved
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (ctx : ApplyContext)
  (fn : BuiltInFn)
  (resolvedTypeArgsVT : List<ValueType>)
  : Ply<Dval> =
  let raiseRTE rte = raiseRTE vm.threadID rte
  let applicable = ctx.applicable
  let newArgDvals = ctx.args

  let mutable tst = ctx.tst

  // Step 2: shadow this fn's free type-vars from the inherited TST. Mirrors the package-fn path;
  // without shadowing, an outer fn's `'a := X` would silently constrain a builtin's unrelated `'a`
  // (e.g. `==` with `'a` polluted to a tuple type from a parent List<(...)> context).
  let biShadowAlloc = allocNow vm
  let fnFreeTVars = FreeTVars.ofBuiltin fn
  // Only used to strip names from `tst`, so with nothing in `tst` there's nothing to strip and not even
  // the memo lookup is worth doing.
  let implicitTypeParams : Set<string> =
    if Map.isEmpty tst then Set.empty else fnFreeTVars
  tst <- implicitTypeParams |> Set.fold (fun m name -> removeIfPresent name m) tst

  // Step 3: bind the (already-resolved) explicit type args. If the caller omitted type args, leave the
  // typeParams unbound for inference to fill in.
  let explicitlyBound =
    if List.isEmpty resolvedTypeArgsVT then
      Map.empty
    else
      List.zip fn.typeParams resolvedTypeArgsVT |> Map
  tst <- Map.mergeFavoringRight tst explicitlyBound
  recordStage vm ApplyStage.BiTstShadow biShadowAlloc

  let biArgsAlloc = allocNow vm
  let allArgs =
    match applicable.argsSoFar with
    | [] -> newArgDvals
    | prev -> prev @ newArgDvals
  recordStage vm ApplyStage.BiArgs biArgsAlloc

  let paramCount = List.length fn.parameters
  let argCount = List.length allArgs

  // Step 4: infer type-variable bindings from arg ValueTypes for any TVariables in the param types not
  // bound by explicit type args. Same rule as the package-fn path. Nothing to infer for a fn with no
  // free type-vars, which most builtins are, and skipping saves a `Dval.toValueType` per argument.
  if argCount > 0 && not (Set.isEmpty fnFreeTVars) then
    // Lockstep: projecting the param types into their own list and zipping it against the args
    // allocates two lists and a pair per argument.
    let rec inferBi acc (ps : List<BuiltInParam>) (args : List<Dval>) =
      match ps with
      | [] -> acc
      | p :: pRest ->
        match args with
        | [] -> acc
        | a :: aRest ->
          inferBi (inferTVarsFromArg acc p.typ (Dval.toValueType a)) pRest aRest
    let inferredBound = inferBi explicitlyBound fn.parameters allArgs
    if not (Map.isEmpty inferredBound) then
      tst <- Map.mergeFavoringRight tst inferredBound

  // Step 5: type-check the new arguments against the corresponding parameters. Walk them in lockstep
  // rather than building an indexed triple list and zipping it against the args. `already` is the count
  // already applied, so the first parameter checked keeps its original index.
  let biTcRunAlloc = allocNow vm
  let already = List.length applicable.argsSoFar
  let struct (biNextI, biRestPs, biRestArgs, biTst) =
    checkBiParamsSync already (List.skip already fn.parameters) newArgDvals tst
  tst <- biTst
  recordStage vm ApplyStage.BiTypeCheckRun biTcRunAlloc

  match biRestPs, biRestArgs with
  | [], _
  | _, [] ->
    completeBuiltin exeState vm currentFrame ctx fn allArgs argCount paramCount tst
  | _ ->
    // Something in the remaining parameters needs the type store. Finish the check in a computation
    // expression and carry on from there -- still the one implementation, just resumed asynchronously.
    let typeCheckParam = TypeChecker.checkFnParam exeState.types applicable.name
    uply {
      let mutable tstRest = tst
      let rec checkRest i (ps : List<BuiltInParam>) (args : List<Dval>) =
        uply {
          match ps with
          | [] -> return ()
          | p :: pRest ->
            match args with
            | [] -> return ()
            | a :: aRest ->
              match! typeCheckParam tstRest i p.name p.typ a with
              | Ok updatedTst ->
                tstRest <- updatedTst
                return! checkRest (i + 1) pRest aRest
              | Error rte -> return raiseRTE rte
        }
      do! checkRest biNextI biRestPs biRestArgs
      return!
        completeBuiltin
          exeState
          vm
          currentFrame
          ctx
          fn
          allArgs
          argCount
          paramCount
          tstRest
    }



/// Run a builtin call, resolving its explicit type args first.
let private callBuiltin
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (ctx : ApplyContext)
  (fn : BuiltInFn)
  : Ply<Dval> =
  let run resolved = callBuiltinResolved exeState vm currentFrame ctx fn resolved
  match
    resolveTypeArgsSync
      exeState
      vm
      ctx.applicable.name
      (List.length fn.typeParams)
      ctx
  with
  | ValueSome resolved -> run resolved
  | ValueNone ->
    uply {
      let! resolved = resolveTypeArgsAsync exeState ctx
      return! run resolved
    }



/// What a package-fn call produces: either it wasn't fully applied, or there's a frame to run.
[<Struct>]
type private PackageOutcome =
  | PartiallyApplied of dval : Dval
  | PushFrame of frame : CallFrame


/// Too many arguments for a package fn, not enough (so it stays a partial application), or exactly right
/// -- in which case build the frame to run it in.
///
/// Top-level rather than a local function, for the same reason as `completeBuiltin`: as a closure over
/// the values it needs, it was an allocation on every package call.
let private completePackage
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (pendingCallArgs : System.Collections.Generic.Dictionary<uuid, Dval list>)
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  (implicitTypeParams : Set<string>)
  (newlyBound : TypeSymbolTable)
  (allArgs : List<Dval>)
  (argCount : int)
  (paramCount : int)
  (tst : TypeSymbolTable)
  : PackageOutcome =
  let raiseRTE rte = raiseRTE vm.threadID rte
  let applicable = ctx.applicable
  let typeArgs = ctx.typeArgs
  if argCount > paramCount then
    RTE.Applications.TooManyArgsForFn(applicable.name, paramCount, argCount)
    |> RTE.Apply
    |> raiseRTE
  elif argCount < paramCount then
    { applicable with
        typeArgs = typeArgs
        argsSoFar = allArgs
        typeSymbolTable = tst }
    |> AppNamedFn
    |> DApplicable
    |> PartiallyApplied
  else
    // Inherit the outer frame's TST but shadow this fn's own free type-vars first, so the inner fn's
    // `'a` is local to this call and not the outer's.
    let frameTst =
      let stripped =
        implicitTypeParams
        |> Set.fold
          (fun m name -> removeIfPresent name m)
          currentFrame.typeSymbolTable
      Map.mergeFavoringRight stripped newlyBound
    let pkgFrameAlloc = allocNow vm
    if vm.stats.enabled then
      let n = int64 (Map.count frameTst)
      vm.stats.tstSizeSum <- vm.stats.tstSizeSum + n
      if n > vm.stats.tstSizeMax then vm.stats.tstSizeMax <- n
    let newFrameId = nextFrameId vm
    if not exeState.tracing.skipTracing then pendingCallArgs[newFrameId] <- allArgs
    if vm.stats.enabled then
      vm.stats.packageCallCount <- vm.stats.packageCallCount + 1L
      vm.stats.framePushCount <- vm.stats.framePushCount + 1L
      if vm.stats.detailedTiming then
        vm.stats.framePushTimestamps[newFrameId] <-
          System.Diagnostics.Stopwatch.GetTimestamp()
    let pkgEp = FreeTVars.packageExecutionPoint fn.hash
    if not exeState.tracing.skipTracing then
      exeState.tracing.storeFrameEntry newFrameId pkgEp allArgs
    let frame =
      { id = newFrameId
        parent = ValueSome(struct (vm.currentFrameID, ctx.putResultIn, ctx.returnPc))
        programCounter = 0
        registers =
          if vm.stats.enabled then
            vm.stats.registersAllocated <-
              vm.stats.registersAllocated + int64 fn.body.registerCount
          let r = Array.zeroCreate fn.body.registerCount
          // A manual walk rather than `List.iteri`, whose closure over `r` is an allocation on a path
          // that runs once per call.
          let rec fill i (args : List<Dval>) =
            match args with
            | [] -> ()
            | a :: rest ->
              r[i] <- a
              fill (i + 1) rest
          fill 0 allArgs
          r
        typeSymbolTable = frameTst
        executionPoint = pkgEp
        expectedReturnType = ValueSome fn.returnType
        // We already hold the fn here, so the loop needn't fetch it.
        instrData =
          match exeState.packageFnInstrCache.TryGetValue fn.hash with
          | true, cached -> cached
          | false, _ ->
            let d : InstrData =
              { instructions = List.toArray fn.body.instructions
                resultReg = fn.body.resultIn }
            exeState.packageFnInstrCache[fn.hash] <- d
            d }
    recordStage vm ApplyStage.PkgFrame pkgFrameAlloc
    PushFrame frame


/// Everything after the explicit type args are resolved. See `callPackage`.
let private callPackageResolved
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (pendingCallArgs : System.Collections.Generic.Dictionary<uuid, Dval list>)
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  (resolvedExplicitTypeArgsVT : List<ValueType>)
  : Ply<PackageOutcome> =
  let raiseRTE rte = raiseRTE vm.threadID rte
  let applicable = ctx.applicable
  let newArgDvals = ctx.args
  let mutable tst = ctx.tst

  // Step 2: shadow this fn's free type-vars in the inherited TST. Each fn's TVariables are scoped to
  // that fn; without shadowing, an outer fn's `'a := X` would silently constrain a nested fn's
  // unrelated `'a`.
  let pkgShadowAlloc = allocNow vm
  // Used to strip names from `tst` here and from the parent's table when building `frameTst` below.
  // Both empty means both strips are no-ops, so don't compute it.
  let fnFreeTVars = FreeTVars.ofPackage fn
  let implicitTypeParams : Set<string> =
    if Map.isEmpty tst && Map.isEmpty currentFrame.typeSymbolTable then
      Set.empty
    else
      fnFreeTVars
  tst <- implicitTypeParams |> Set.fold (fun m name -> removeIfPresent name m) tst

  // Step 3: bind the (already-resolved) explicit type args into the freshly-shadowed tst. Explicit args
  // win over inferred bindings filled in below. If the caller omitted type args, leave the typeParams
  // unbound for inference.
  let explicitlyBound =
    if List.isEmpty resolvedExplicitTypeArgsVT then
      Map.empty
    else
      List.zip fn.typeParams resolvedExplicitTypeArgsVT |> Map
  if not (Map.isEmpty explicitlyBound) then
    tst <- Map.mergeFavoringRight tst explicitlyBound
  recordStage vm ApplyStage.PkgTstShadow pkgShadowAlloc

  // Pre-compute allArgs so inference runs BEFORE typeCheckParams. Otherwise the type check runs against
  // a TST that doesn't yet know `'a := whatever`.
  let allArgs =
    match applicable.argsSoFar with
    | [] -> newArgDvals
    | prev -> prev @ newArgDvals

  let paramCount = NEList.length fn.parameters
  let argCount = List.length allArgs

  // Step 4: infer type-variable bindings from arg ValueTypes for any TVariables in the param types not
  // bound by explicit type args. Lets wrappers of the shape
  //   let f (x: List<'a>) : Stream<'a> = Builtin.fromList<'a> x
  // work without callers passing explicit type args. See [inferTVarsFromArg] for the unification rules.
  let pkgInferAlloc = allocNow vm
  let newlyBound =
    // A fn with no free type-vars has nothing to infer, and this is the majority of them. Skipping the
    // walk skips a `Dval.toValueType` per argument, which builds a ValueType tree only to throw away.
    if argCount = 0 || Set.isEmpty fnFreeTVars then
      explicitlyBound
    else
      // Lockstep walk, no projected list and no zip pairs.
      let rec inferPkg acc (ps : List<PackageFn.Parameter>) (args : List<Dval>) =
        match ps with
        | [] -> acc
        | p :: pRest ->
          match args with
          | [] -> acc
          | a :: aRest ->
            inferPkg (inferTVarsFromArg acc p.typ (Dval.toValueType a)) pRest aRest
      inferPkg explicitlyBound (NEList.toList fn.parameters) allArgs
  if not (Map.isEmpty newlyBound) then
    tst <- Map.mergeFavoringRight tst newlyBound
  recordStage vm ApplyStage.PkgInfer pkgInferAlloc


  // Step 5: type-check params against the fully-resolved tst. Lockstep walk, no indexed-triple list and
  // no zip.
  let pkgTcAlloc = allocNow vm
  let alreadyApplied = List.length applicable.argsSoFar
  let pkgParams = fn.parameters |> NEList.toList |> List.skip alreadyApplied
  recordStage vm ApplyStage.PkgTypeCheckArgs pkgTcAlloc

  let pkgTcRunAlloc = allocNow vm
  let struct (pkgNextI, pkgRestPs, pkgRestArgs, pkgTst) =
    checkPkgParamsSync alreadyApplied pkgParams newArgDvals tst
  tst <- pkgTst
  recordStage vm ApplyStage.PkgTypeCheckRun pkgTcRunAlloc

  match pkgRestPs, pkgRestArgs with
  | [], _
  | _, [] ->
    Ply(
      completePackage
        exeState
        vm
        currentFrame
        pendingCallArgs
        ctx
        fn
        implicitTypeParams
        newlyBound
        allArgs
        argCount
        paramCount
        tst
    )
  | _ ->
    // Something in the remaining parameters needs the type store. Finish the check in a computation
    // expression and carry on from there -- still one implementation, just resumed asynchronously.
    let typeCheckParam = TypeChecker.checkFnParam exeState.types applicable.name
    uply {
      let mutable tstRest = tst
      let rec checkRest i (ps : List<PackageFn.Parameter>) (args : List<Dval>) =
        uply {
          match ps with
          | [] -> return ()
          | p :: pRest ->
            match args with
            | [] -> return ()
            | a :: aRest ->
              match! typeCheckParam tstRest i p.name p.typ a with
              | Ok updatedTst ->
                tstRest <- updatedTst
                return! checkRest (i + 1) pRest aRest
              | Error rte -> return raiseRTE rte
        }
      do! checkRest pkgNextI pkgRestPs pkgRestArgs
      return
        completePackage
          exeState
          vm
          currentFrame
          pendingCallArgs
          ctx
          fn
          implicitTypeParams
          newlyBound
          allArgs
          argCount
          paramCount
          tstRest
    }


/// Run a package-fn call, resolving its explicit type args first. Outside the computation expression for
/// the same reason as `callBuiltin`.
let private callPackage
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (pendingCallArgs : System.Collections.Generic.Dictionary<uuid, Dval list>)
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  : Ply<PackageOutcome> =
  let run resolved =
    callPackageResolved exeState vm currentFrame pendingCallArgs ctx fn resolved
  match
    resolveTypeArgsSync
      exeState
      vm
      ctx.applicable.name
      (List.length fn.typeParams)
      ctx
  with
  | ValueSome resolved -> run resolved
  | ValueNone ->
    uply {
      let! resolved = resolveTypeArgsAsync exeState ctx
      return! run resolved
    }




let rec private executeInner (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  uply {
    let raiseRTE rte = raiseRTE vm.threadID rte
    let pendingCallArgs = System.Collections.Generic.Dictionary<uuid, Dval list>()

    let mutable finalResult : Dval option = None

    while vm.callFrames.ContainsKey vm.currentFrameID do
      let currentFrame = vm.callFrames[vm.currentFrameID]

      let mutable counter = currentFrame.programCounter
      let registers = currentFrame.registers


      // Resolved when the frame was pushed. This used to be a cache lookup bound with `let!` on every
      // iteration of this loop, which in the Ply builder's dynamic path allocates a continuation closure
      // each time -- once per awaiting instruction executed, so tens of thousands per script.
      let instrData = currentFrame.instrData

      let mutable frameToPush = None

      while counter < instrData.instructions.Length && frameToPush = None do
        // Drain every instruction that doesn't need to await, outside the computation expression.
        counter <-
          runSyncInstructions exeState vm currentFrame registers instrData counter

        if counter < instrData.instructions.Length && frameToPush = None then
          if vm.stats.enabled then
            vm.stats.instructionCount <- vm.stats.instructionCount + 1L

          let inst = instrData.instructions[counter]
          let allocBefore =
            if vm.stats.enabled then
              System.GC.GetAllocatedBytesForCurrentThread()
            else
              0L

          match inst with
          | CreateRecord(recordReg, sourceTypeName, typeArgs, fields) ->
            let fields =
              fields
              |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

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
          | LoadValue(createTo, name) ->
            match name with
            | FQValueName.Builtin builtin ->
              match Map.find builtin exeState.values.builtIn with
              | Some v -> registers[createTo] <- v.body
              | None -> raiseRTE (RTE.ValueNotFound name)

            | FQValueName.Package pkg ->
              match! exeState.values.package pkg with
              | Some v ->
                // The Dval is already stored in the package value
                registers[createTo] <- v.body
              | None -> raiseRTE (RTE.ValueNotFound name)
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

            let applyArgsAlloc = allocNow vm
            let newArgDvals = readRegsNE registers newArgRegs
            recordStage vm ApplyStage.ApplyArgs applyArgsAlloc

            match applicable with
            | AppLambda appLambda ->
              let exprId = appLambda.exprId
              let foundLambda =
                match exeState.lambdaInstrCache.TryGetValue exprId with
                | true, lambda -> lambda
                | false, _ ->
                  Exception.raiseInternal "lambda not found" [ "exprId", exprId ]

              let allArgs =
                match appLambda.argsSoFar with
                | [] -> newArgDvals
                | prev -> prev @ newArgDvals

              let argCount = List.length allArgs
              let paramCount = NEList.length foundLambda.patterns

              if typeArgs <> [] then
                RTE.Applications.CannotApplyTypeArgsToLambda |> RTE.Apply |> raiseRTE

              if argCount = paramCount then
                let lambdaFrameAlloc = allocNow vm
                let newFrame =
                  { id = nextFrameId vm
                    parent =
                      ValueSome(struct (vm.currentFrameID, putResultIn, counter + 1))
                    programCounter = 0
                    registers =
                      if vm.stats.enabled then
                        vm.stats.registersAllocated <-
                          vm.stats.registersAllocated
                          + int64 foundLambda.instructions.registerCount
                      let r = Array.zeroCreate foundLambda.instructions.registerCount

                      // extract and copy over the args
                      bindLambdaParams
                        vm
                        r
                        (foundLambda.patterns.head :: foundLambda.patterns.tail)
                        allArgs

                      // copy over closed registers
                      assignRegisters r appLambda.closedRegisters

                      // Put the lambda itself in the self register so the body can
                      // call itself. If it already has no applied args, reuse it
                      // as-is.
                      match foundLambda.selfRegister with
                      | Some selfReg ->
                        r[selfReg] <-
                          if List.isEmpty appLambda.argsSoFar then
                            DApplicable(AppLambda appLambda)
                          else
                            DApplicable(AppLambda { appLambda with argsSoFar = [] })
                      | None -> ()

                      r
                    typeSymbolTable =
                      if Map.isEmpty appLambda.typeSymbolTable then
                        currentFrame.typeSymbolTable
                      else if Map.isEmpty currentFrame.typeSymbolTable then
                        appLambda.typeSymbolTable
                      else
                        Map.mergeFavoringRight
                          appLambda.typeSymbolTable
                          currentFrame.typeSymbolTable
                    executionPoint = Lambda(currentFrame.executionPoint, exprId)
                    expectedReturnType = ValueNone
                    // Resolved here so the loop never has to look it up. Same shared InstrData the
                    // per-VM cache holds; this is a reference to it, not a copy.
                    instrData =
                      match Map.tryFind exprId vm.lambdaInstrDataCache with
                      | Some cached -> cached
                      | None ->
                        let d : InstrData =
                          { instructions =
                              List.toArray foundLambda.instructions.instructions
                            resultReg = foundLambda.instructions.resultIn }
                        vm.lambdaInstrDataCache <-
                          Map.add exprId d vm.lambdaInstrDataCache
                        d }

                recordStage vm ApplyStage.LambdaFrame lambdaFrameAlloc
                if vm.stats.enabled then
                  vm.stats.framePushCount <- vm.stats.framePushCount + 1L
                if not exeState.tracing.skipTracing then
                  exeState.tracing.storeFrameEntry
                    newFrame.id
                    newFrame.executionPoint
                    allArgs
                frameToPush <- Some newFrame

              else if argCount > paramCount then
                RTE.Applications.TooManyArgsForLambda(exprId, paramCount, argCount)
                |> RTE.Apply
                |> raiseRTE
              else
                registers[putResultIn] <-
                  { appLambda with argsSoFar = allArgs } |> AppLambda |> DApplicable

            | AppNamedFn applicable ->
              // The symbol table the call starts from. `callBuiltin` and `callPackage` take it from
              // here and do the rest -- shadowing, inference, checking, invocation -- outside this
              // computation expression, which is where all of it used to live.
              let tst =
                if Map.isEmpty applicable.typeSymbolTable then
                  currentFrame.typeSymbolTable
                else if Map.isEmpty currentFrame.typeSymbolTable then
                  applicable.typeSymbolTable
                else
                  Map.mergeFavoringRight
                    currentFrame.typeSymbolTable
                    applicable.typeSymbolTable

              let typeArgs =
                match applicable.typeArgs, typeArgs with
                | [], newTypeArgs -> newTypeArgs
                | oldTypeArgs, [] -> oldTypeArgs
                | _, _ ->
                  RTE.Applications.CannotApplyTypeArgsMoreThanOnce
                  |> RTE.Apply
                  |> raiseRTE

              let ctx : ApplyContext =
                { applicable = applicable
                  typeArgs = typeArgs
                  args = newArgDvals
                  tst = tst
                  putResultIn = putResultIn
                  returnPc = counter + 1 }

              // CLEANUP the two branches below are near-identical in shape, and so are `callBuiltin`
              // and `callPackage` behind them: same five steps, different parameter and outcome types.
              // Unifying them needs `BuiltInParam` and `PackageFn.Parameter` to share an interface.
              match applicable.name with
              | FQFnName.Builtin builtin ->
                let biLookupAlloc = allocNow vm
                match Map.find builtin exeState.fns.builtIn with
                | None -> return RTE.FnNotFound(FQFnName.Builtin builtin) |> raiseRTE
                | Some fn ->
                  recordStage vm ApplyStage.BiFnLookup biLookupAlloc
                  let call = callBuiltin exeState vm currentFrame ctx fn
                  // Usually already finished, in which case there's no bind to pay for.
                  match Ply.trySync call with
                  | ValueSome dv -> registers[putResultIn] <- dv
                  | ValueNone ->
                    let! dv = call
                    registers[putResultIn] <- dv

              | FQFnName.Package pkg ->
                // Harmful-deprecation runtime halt.
                // Checked before even fetching the fn so the error is surfaced
                // whether or not the fn definition is still available.
                let isHarmful = exeState.fns.isHarmful pkg
                if isHarmful && not exeState.allowHarmful then
                  return RTE.DeprecatedItemHalted pkg |> raiseRTE
                let pkgFetchAlloc = allocNow vm
                // Warm cache after the first call, which for a script means all but a handful of these.
                let fetch = exeState.fns.package pkg
                let mutable fetched = None
                match Ply.trySync fetch with
                | ValueSome f -> fetched <- f
                | ValueNone ->
                  let! f = fetch
                  fetched <- f
                match fetched with
                | None -> return RTE.FnNotFound(FQFnName.Package pkg) |> raiseRTE
                | Some fn ->
                  recordStage vm ApplyStage.PkgFetch pkgFetchAlloc
                  let call =
                    callPackage exeState vm currentFrame pendingCallArgs ctx fn
                  // Overwritten on the next line either way; F# needs something to start from.
                  let mutable outcome = PartiallyApplied DUnit
                  match Ply.trySync call with
                  | ValueSome o -> outcome <- o
                  | ValueNone ->
                    let! o = call
                    outcome <- o
                  match outcome with
                  | PartiallyApplied dv -> registers[putResultIn] <- dv
                  | PushFrame frame -> frameToPush <- Some frame

          // Handled by `runSyncInstructions`; the match must still be exhaustive.
          | _ -> ()

          if vm.stats.enabled then
            let tag = Opcode.index inst
            if tag >= 0 && tag < vm.stats.allocByOpcode.Length then
              // Clamped at zero: these arms await, and this counter is per-thread, so a resume on another
              // thread makes the odd delta meaningless rather than merely noisy.
              let delta = System.GC.GetAllocatedBytesForCurrentThread() - allocBefore
              if delta > 0L then
                vm.stats.allocByOpcode[tag] <- vm.stats.allocByOpcode[tag] + delta
              vm.stats.countByOpcode[tag] <- vm.stats.countByOpcode[tag] + 1L

          counter <- counter + 1



      // exited loop -- either pushed a frame or finished the current frame

      match frameToPush with
      | Some newFrame ->
        // Something in this eval just pushed a frame -- don't do the "normal" processing
        vm.callFrames[newFrame.id] <- newFrame
        vm.currentFrameID <- newFrame.id

      | None ->
        // We are at the end of the instructions of the current frame
        // Either we're done with the whole eval, or we need to return a value to the parent frame
        let resultOfFrame = registers[instrData.resultReg]

        match currentFrame.parent with
        | ValueSome(parentID, regOfParentToPutResultInto, pcOfParent) ->
          // We just finished processing a frame, and we need to return a value to the parent frame

          // TODO this might be where the type-checking of a fn result needs to happen.
          // But when here, it's not always a fn call - could also be for a lambda.

          // Type-check results of fns
          let retTcAlloc = allocNow vm
          match currentFrame.executionPoint with
          | Source -> ()
          | Lambda _ -> ()
          | Function fnName ->
            // Recorded when the frame was pushed. Builtins never get a frame, so a Function frame always
            // carries one; the fallback keeps the match total rather than asserting.
            let expectedReturnType =
              match currentFrame.expectedReturnType with
              | ValueSome t -> t
              | ValueNone ->
                match fnName with
                | FQFnName.Builtin builtin ->
                  (Map.findUnsafe builtin exeState.fns.builtIn).returnType
                | FQFnName.Package _ -> RTE.FnNotFound fnName |> raiseRTE

            let tst = currentFrame.typeSymbolTable
            // Every frame return checks its result, so the same sync-first treatment as the argument
            // checks applies: skip the bind when the answer needs no type lookup.
            match TypeChecker.tryUnifySync tst expectedReturnType resultOfFrame with
            | ValueSome _ -> ()
            | ValueNone ->
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

          recordStage vm ApplyStage.FrameReturnTypeCheck retTcAlloc

          // Record per-package-fn timing on frame return
          if vm.stats.enabled && vm.stats.detailedTiming then
            match vm.stats.framePushTimestamps.TryGetValue(vm.currentFrameID) with
            | true, pushTs ->
              let elapsed = System.Diagnostics.Stopwatch.GetTimestamp() - pushTs
              match currentFrame.executionPoint with
              | Function(FQFnName.Package(Hash h)) ->
                vm.stats.recordPackageFn (h, elapsed)
              | _ -> ()
              vm.stats.framePushTimestamps.Remove(vm.currentFrameID) |> ignore<bool>
            | false, _ -> ()

          let framePopAlloc = allocNow vm
          vm.callFrames.Remove(vm.currentFrameID) |> ignore<bool>

          vm.currentFrameID <- parentID

          let parentFrame = vm.callFrames[parentID]

          // Trace package function call at frame return.
          // Lambda frames fire storeLambdaResult instead.
          if not exeState.tracing.skipTracing then
            match currentFrame.executionPoint with
            | Function fnName ->
              match pendingCallArgs.TryGetValue(currentFrame.id) with
              | true, args ->
                pendingCallArgs.Remove(currentFrame.id) |> ignore<bool>
                let source : Tracing.Source = (parentFrame.executionPoint, None)
                let fnRecord : Tracing.FunctionRecord = (source, fnName)
                exeState.tracing.storeFnResult
                  fnRecord
                  (NEList.ofListUnsafe "" [] args)
                  resultOfFrame
              | _ -> ()
            | Lambda _ ->
              pendingCallArgs.Remove(currentFrame.id) |> ignore<bool>
              exeState.tracing.storeLambdaResult currentFrame.id resultOfFrame
            | Source -> pendingCallArgs.Remove(currentFrame.id) |> ignore<bool>
          parentFrame.registers[regOfParentToPutResultInto] <- resultOfFrame
          parentFrame.programCounter <- pcOfParent
          recordStage vm ApplyStage.FramePop framePopAlloc

        | ValueNone ->
          vm.callFrames.Remove(vm.currentFrameID) |> ignore<bool>
          finalResult <- Some resultOfFrame


    // If we've reached the end of the instructions, return the result
    match finalResult with
    | Some dv -> return dv
    | None -> return Exception.raiseInternal "No finalResult found" []
  }

and execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  executeInner exeState vm
