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

  // `TryGetValue(key, &out)` rather than `match d.TryGetValue key with | true, v ->`. The tuple form
  // reads better but allocates a `Tuple<bool, 'v>` on every lookup, hit or miss -- measured at 35 bytes
  // a call, which on a per-call cache lookup is more than the thing being cached.
  let ofBuiltin (fn : BuiltInFn) : Set<string> =
    let mutable cached = Unchecked.defaultof<Set<string>>
    if builtins.TryGetValue(fn.name, &cached) then
      cached
    else
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
    let mutable cached = Unchecked.defaultof<ExecutionPoint>
    if packageEntryPoints.TryGetValue(hash, &cached) then
      cached
    else
      let v = Function(FQFnName.Package hash)
      packageEntryPoints[hash] <- v
      v

  /// The parameter list, as a list. `NEList.toList` conses the head onto the tail on every call, and
  /// a package fn's parameters can't change under a running process: it's content-addressed.
  let private packageParams =
    System.Collections.Concurrent.ConcurrentDictionary<Hash, List<PackageFn.Parameter>>()

  /// A lambda's parameter patterns, as a list. `head :: tail` conses on every lambda call, and a
  /// lambda's patterns can't change: the instruction that created it is fixed.
  let private lambdaPatterns =
    System.Collections.Concurrent.ConcurrentDictionary<id, List<LetPattern>>()

  let patternsOfLambda (exprId : id) (pats : NEList<LetPattern>) : List<LetPattern> =
    let mutable cached = Unchecked.defaultof<List<LetPattern>>
    if lambdaPatterns.TryGetValue(exprId, &cached) then
      cached
    else
      let v = NEList.toList pats
      lambdaPatterns[exprId] <- v
      v

  let paramsOfPackage (fn : PackageFn.PackageFn) : List<PackageFn.Parameter> =
    let mutable cached = Unchecked.defaultof<List<PackageFn.Parameter>>
    if packageParams.TryGetValue(fn.hash, &cached) then
      cached
    else
      let v = NEList.toList fn.parameters
      packageParams[fn.hash] <- v
      v

  let ofPackage (fn : PackageFn.PackageFn) : Set<string> =
    let mutable cached = Unchecked.defaultof<Set<string>>
    if packages.TryGetValue(fn.hash, &cached) then
      cached
    else
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


/// A call's arguments, without materialising them into a list.
///
/// `FSharpList<Dval>` is the largest single entry in the allocation profile: one cons per argument
/// per call, for a list whose uses on the package path are two lockstep walks and a copy into the
/// callee's registers, all of which can read the caller's register file directly.
///
/// The head is held separately from the tail rather than as one list, because the `Apply` instruction
/// carries its argument registers as an `NEList` and `head :: tail` would itself be the allocation
/// this is trying to remove. A first attempt did exactly that and measured worse.
///
/// `Prior` covers partial application without a second code path: the walks consume already-applied
/// arguments first, then the registers. Tracing, partial application and the builtin ABI still want a
/// real list, and `toList` builds one for them.
[<Struct>]
type private ArgSeq =
  {
    /// Arguments from earlier partial applications. Almost always empty.
    Prior : List<Dval>
    Regs : Registers
    /// The `NEList` head, until it has been consumed.
    Head : Register voption
    Tail : List<Register>
  }

module private ArgSeq =
  let inline ofNE (registers : Registers) (regs : NEList<Register>) : ArgSeq =
    { Prior = []; Regs = registers; Head = ValueSome regs.head; Tail = regs.tail }

  let inline withPrior (prior : List<Dval>) (a : ArgSeq) : ArgSeq =
    { a with Prior = prior }

  let inline count (a : ArgSeq) : int =
    List.length a.Prior + (if a.Head.IsSome then 1 else 0) + List.length a.Tail

  let inline isEmpty (a : ArgSeq) : bool =
    List.isEmpty a.Prior && a.Head.IsNone && List.isEmpty a.Tail

  /// The next argument and the rest. Allocation-free: structs all the way down.
  let inline uncons (a : ArgSeq) : struct (Dval * ArgSeq) voption =
    match a.Prior with
    | x :: rest -> ValueSome(struct (x, { a with Prior = rest }))
    | [] ->
      match a.Head with
      | ValueSome r -> ValueSome(struct (a.Regs[r], { a with Head = ValueNone }))
      | ValueNone ->
        match a.Tail with
        | r :: rest -> ValueSome(struct (a.Regs[r], { a with Tail = rest }))
        | [] -> ValueNone

  /// Only for the paths that genuinely need a list: tracing, partial application, and builtins.
  let toList (a : ArgSeq) : List<Dval> =
    let fromRegs =
      match a.Head with
      | ValueSome r -> a.Regs[r] :: readRegs a.Regs a.Tail
      | ValueNone -> readRegs a.Regs a.Tail
    match a.Prior with
    | [] -> fromRegs
    | prior -> prior @ fromRegs

  /// The arguments as an array, which is what a builtin takes. One allocation rather than a cons
  /// per argument; `count` is already known, so it's filled in place with no intermediate.
  /// The arguments as an array, reusing the calling frame's scratch buffer when the arity matches.
  /// See the note on `CallFrame.argBuf` for why a per-frame buffer needs no rent/return discipline.
  let toArrayFor (frame : CallFrame) (a : ArgSeq) : Dval[] =
    let n = count a
    let arr =
      if frame.argBuf.Length = n then
        frame.argBuf
      else
        let fresh = Array.zeroCreate n
        frame.argBuf <- fresh
        fresh
    let mutable i = 0
    let mutable rest = a
    while i < n do
      match uncons rest with
      | ValueSome(struct (dv, tail)) ->
        arr[i] <- dv
        rest <- tail
        i <- i + 1
      | ValueNone -> i <- n
    arr

  /// Copy positionally into a callee's register file, starting at 0.
  let rec private fillFrom (dest : Registers) (i : int) (a : ArgSeq) : unit =
    match uncons a with
    | ValueNone -> ()
    | ValueSome(struct (v, rest)) ->
      dest[i] <- v
      fillFrom dest (i + 1) rest

  let fill (dest : Registers) (a : ArgSeq) : unit = fillFrom dest 0 a


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
  (acc : TypeSymbolTable)
  (tr : TypeReference)
  (vt : ValueType)
  : TypeSymbolTable =
  match tr with
  | TVariable name ->
    if isFullyKnown vt && not (TST.containsKey name acc) then
      TST.add name vt acc
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
  (acc : TypeSymbolTable)
  (trs : List<TypeReference>)
  (vts : List<ValueType>)
  : TypeSymbolTable =
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


/// As `inferTVarsFromArg`, against the argument itself rather than its ValueType.
///
/// `Dval.toValueType` on a container builds `Known(KTList t)` -- two allocations -- and inference's next
/// move is to take it apart again to get at `t`. Scalars are cached singletons and cost nothing, so only
/// the shapes that already carry their element type are worth special-casing. Binding a type variable
/// needs the whole ValueType, so that still falls through.
///
/// Same trick as `TypeChecker.unifyDvalSync`, on the other walk over the same arguments.
let private inferTVarsFromDval
  (acc : TypeSymbolTable)
  (tr : TypeReference)
  (dv : Dval)
  : TypeSymbolTable =
  match tr with
  | TList tr' ->
    match dv with
    | DList(vt', _) -> inferTVarsFromArg acc tr' vt'
    | _ -> inferTVarsFromArg acc tr (Dval.toValueType dv)
  | TDict tr' ->
    match dv with
    | DDict(vt', _) -> inferTVarsFromArg acc tr' vt'
    | _ -> inferTVarsFromArg acc tr (Dval.toValueType dv)
  | _ -> inferTVarsFromArg acc tr (Dval.toValueType dv)


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


/// Try a match pattern against a value, appending any bindings it makes to `buf`.
///
/// Returns whether it matched. The bindings go in a caller-supplied buffer rather than a returned
/// list because a returned `List<Register * Dval>` cost a tuple and a cons per bound variable, on
/// every pattern *tried* rather than every pattern that matched: 11.6% of the interpreter's
/// allocation. The buffer is reused for the life of the VM.
///
/// The caller writes the registers only once the whole pattern has matched, so a pattern that fails
/// partway leaves the frame untouched. An or-pattern's failed alternative truncates the buffer back
/// to where that alternative started.
let rec private checkMatchPatternList
  (buf : ResizeArray<struct (Register * Dval)>)
  (pats : List<MatchPattern>)
  (items : List<Dval>)
  : bool =
  match pats with
  | [] -> List.isEmpty items
  | pat :: otherPats ->
    match items with
    | [] -> false
    | item :: otherItems ->
      checkAndExtractMatchPattern buf pat item
      && checkMatchPatternList buf otherPats otherItems

/// Nested matches throughout, not `match pat, dv with`. The tuple form reads better and allocates the
/// pair on every pattern tried; this runs once per arm of every `match` a script evaluates.
and checkAndExtractMatchPattern
  (buf : ResizeArray<struct (Register * Dval)>)
  (pat : MatchPattern)
  (dv : Dval)
  : bool =
  match pat with
  | MPVariable reg ->
    buf.Add(struct (reg, dv))
    true

  | MPUnit ->
    match dv with
    | DUnit -> true
    | _ -> false
  | MPBool l ->
    match dv with
    | DBool r -> l = r
    | _ -> false
  | MPInt8 l ->
    match dv with
    | DInt8 r -> l = r
    | _ -> false
  | MPUInt8 l ->
    match dv with
    | DUInt8 r -> l = r
    | _ -> false
  | MPInt16 l ->
    match dv with
    | DInt16 r -> l = r
    | _ -> false
  | MPUInt16 l ->
    match dv with
    | DUInt16 r -> l = r
    | _ -> false
  | MPInt32 l ->
    match dv with
    | DInt32 r -> l = r
    | _ -> false
  | MPUInt32 l ->
    match dv with
    | DUInt32 r -> l = r
    | _ -> false
  | MPInt64 l ->
    match dv with
    | DInt64 r -> l = r
    | _ -> false
  | MPUInt64 l ->
    match dv with
    | DUInt64 r -> l = r
    | _ -> false
  | MPInt128 l ->
    match dv with
    | DInt128 r -> l = r
    | _ -> false
  | MPUInt128 l ->
    match dv with
    | DUInt128 r -> l = r
    | _ -> false
  | MPInt l ->
    match dv with
    | DInt r -> l = DarkInt.toBigInt r
    | _ -> false
  | MPFloat l ->
    match dv with
    | DFloat r -> l = r
    | _ -> false
  | MPChar l ->
    match dv with
    | DChar r -> l = r
    | _ -> false
  | MPString l ->
    match dv with
    | DString r -> l = r
    | _ -> false

  | MPList pats ->
    match dv with
    | DList(_, items) -> checkMatchPatternList buf pats items
    | _ -> false

  | MPListCons(head, tail) ->
    match dv with
    | DList(vt, headItem :: tailItems) ->
      checkAndExtractMatchPattern buf head headItem
      && checkAndExtractMatchPattern buf tail (DList(vt, tailItems))
    | _ -> false

  | MPTuple(first, second, theRest) ->
    match dv with
    | DTuple(firstVal, secondVal, theRestVal) ->
      checkAndExtractMatchPattern buf first firstVal
      && checkAndExtractMatchPattern buf second secondVal
      && checkMatchPatternList buf theRest theRestVal
    | _ -> false

  | MPEnum(caseName, fields) ->
    match dv with
    | DEnum(_, _, _, caseNameActual, fieldsActual) when caseName = caseNameActual ->
      checkMatchPatternList buf fields fieldsActual
    | _ -> false

  | MPOr patterns ->
    // A walk rather than `List.map |> List.tryFind |> Option.defaultValue`, which built a list of
    // outcomes, a closure for each stage and an option, to answer a question the first hit settles.
    // Each failed alternative rolls the buffer back to where it started.
    let rec firstMatch (ps : List<MatchPattern>) =
      match ps with
      | [] -> false
      | p :: rest ->
        let mark = buf.Count
        if checkAndExtractMatchPattern buf p dv then
          true
        else
          buf.RemoveRange(mark, buf.Count - mark)
          firstMatch rest
    firstMatch (NEList.toList patterns)


/// Record bytes allocated in a synchronous region of the Apply path. `before` must come from a
/// `GC.GetAllocatedBytesForCurrentThread()` taken in the same synchronous stretch -- see the note on
/// `InterpreterStats.allocByStage` for why spanning an await would measure the wrong thing.
let inline private recordStage (vm : VMState) (stage : int) (before : int64) : unit =
  if vm.stats.enabled then
    let d = System.GC.GetAllocatedBytesForCurrentThread() - before
    vm.stats.countByStage[stage] <- vm.stats.countByStage[stage] + 1L
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
/// A frame with a register file of exactly this size, reused from the pool if one is free.
///
/// A frame and its registers are what pushing a call costs. Nothing holds either past the pop -- a
/// lambda copies the values it closes over, a partial application copies its args, the tracer is
/// handed lists, and the parent link carries a frame id rather than a reference -- so a popped frame
/// can be handed straight back out with every field overwritten.
///
/// The registers of a pooled frame are already cleared; `returnFrame` does it on the way in.
let inline private takeFrame
  (vm : VMState)
  (registerCount : int)
  (id : uuid)
  (parent : voption<struct (uuid * Register * int)>)
  (executionPoint : ExecutionPoint)
  (instrData : InstrData)
  (expectedReturnType : TypeReference voption)
  (typeSymbolTable : TypeSymbolTable)
  : CallFrame =
  let mutable free = Unchecked.defaultof<Stack<CallFrame>>
  if vm.framePool.TryGetValue(registerCount, &free) && free.Count > 0 then
    let f = free.Pop()
    f.id <- id
    f.parent <- parent
    f.executionPoint <- executionPoint
    f.instrData <- instrData
    f.expectedReturnType <- expectedReturnType
    f.programCounter <- 0
    f.typeSymbolTable <- typeSymbolTable
    f
  else
    { id = id
      parent = parent
      executionPoint = executionPoint
      instrData = instrData
      expectedReturnType = expectedReturnType
      programCounter = 0
      typeSymbolTable = typeSymbolTable
      registers = Array.zeroCreate registerCount
      argBuf = Array.empty }

/// Hand a popped frame back. Registers are cleared here rather than at reuse, so a pooled frame that
/// never gets reused isn't holding a call's worth of Dvals alive.
let inline private returnFrame (vm : VMState) (frame : CallFrame) : unit =
  let count = frame.registers.Length
  if count > 0 then System.Array.Clear(frame.registers, 0, count)
  frame.typeSymbolTable <- TST.empty
  let mutable free = Unchecked.defaultof<Stack<CallFrame>>
  if not (vm.framePool.TryGetValue(count, &free)) then
    free <- Stack()
    vm.framePool[count] <- free
  free.Push frame

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
  TST.removeIfPresent name m


/// Infer type-variable bindings from a builtin's arguments, walking parameters and arguments in
/// lockstep.
///
/// Top-level and taking the arguments as a parameter, not a local closing over them. As a local it
/// captured the argument array, which F# can't lambda-lift, so it allocated a closure on every
/// builtin call -- 57% of the recursion workload's profile, and it appeared the moment this stopped
/// taking the arguments as an argument.
let rec private inferBiParams
  (acc : TypeSymbolTable)
  (ps : List<BuiltInParam>)
  (args : Dval[])
  (idx : int)
  : TypeSymbolTable =
  match ps with
  | [] -> acc
  | p :: pRest ->
    if idx >= args.Length then
      acc
    else
      inferBiParams (inferTVarsFromDval acc p.typ args[idx]) pRest args (idx + 1)


/// Check as many arguments as can be answered without awaiting, returning where it stopped.
///
/// `TypeChecker.tryUnifySync` answers the ordinary cases outright, so this walks the parameter and
/// argument lists together for as long as that keeps working, and hands the remainder to the
/// computation-expression version. Getting all the way through, which is the usual case, allocates
/// nothing: top-level so nothing is captured, and a struct tuple so the return isn't an allocation.
let rec private checkBiParamsSync
  (i : int)
  (ps : List<BuiltInParam>)
  (args : Dval[])
  (argIdx : int)
  (tst : TypeSymbolTable)
  : struct (int * List<BuiltInParam> * int * TypeSymbolTable) =
  match ps with
  | [] -> struct (i, ps, argIdx, tst)
  | p :: pRest ->
    if argIdx >= args.Length then
      struct (i, ps, argIdx, tst)
    else
      match TypeChecker.tryUnifySync tst p.typ args[argIdx] with
      | ValueSome updatedTst ->
        checkBiParamsSync (i + 1) pRest args (argIdx + 1) updatedTst
      | ValueNone -> struct (i, ps, argIdx, tst)

/// As [checkBiParamsSync], for package fns.
let rec private checkPkgParamsSync
  (i : int)
  (ps : List<PackageFn.Parameter>)
  (args : ArgSeq)
  (tst : TypeSymbolTable)
  : struct (int * List<PackageFn.Parameter> * ArgSeq * TypeSymbolTable) =
  match ps with
  | [] -> struct (i, ps, args, tst)
  | p :: pRest ->
    match ArgSeq.uncons args with
    | ValueNone -> struct (i, ps, args, tst)
    | ValueSome(struct (a, aRest)) ->
      match TypeChecker.tryUnifySync tst p.typ a with
      | ValueSome updatedTst -> checkPkgParamsSync (i + 1) pRest aRest updatedTst
      | ValueNone -> struct (i, ps, args, tst)


let inline private allocNow (vm : VMState) : int64 =
  if vm.stats.enabled then System.GC.GetAllocatedBytesForCurrentThread() else 0L


/// Write a pattern-match's register assignments into a frame's registers.
///
/// Top-level and fully parameterised so nothing is captured: the `List.iter` this replaces allocated a
/// closure over the register array, on a path that runs once per lambda parameter per lambda call.
/// Copy a call's arguments into the callee's register file, positionally.
///
/// Top-level and taking the array, rather than a local `let rec` that closes over it: F# can't lift a
/// recursive local that captures, so that was a closure allocated on every package call. `fill@1157`
/// was 4.7% of the allocation profile.
let rec private fillRegisters
  (registers : Dval array)
  (i : int)
  (args : List<Dval>)
  : unit =
  match args with
  | [] -> ()
  | a :: rest ->
    registers[i] <- a
    fillRegisters registers (i + 1) rest


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
  (args : ArgSeq)
  : unit =
  match pats with
  | [] -> ()
  | pat :: patRest ->
    match ArgSeq.uncons args with
    | ValueNone -> ()
    | ValueSome(struct (arg, argRest)) ->
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
    args : ArgSeq
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



/// Record a builtin's result in the trace, and hand it back.
///
/// Top-level for the same reason as `finishBuiltin` below it: as a local it was captured by that
/// function's cold-path `uply`, so it was built on every builtin call.
let private traceBuiltinResult
  (exeState : ExecutionState)
  (currentFrame : CallFrame)
  (fn : BuiltInFn)
  (allArgs : Dval[])
  (result : Dval)
  : Dval =
  if not exeState.tracing.skipTracing then
    let source : Tracing.Source = (currentFrame.executionPoint, None)
    let fnRecord : Tracing.FunctionRecord = (source, FQFnName.Builtin fn.name)
    exeState.tracing.storeFnResult
      fnRecord
      (NEList.ofListUnsafe "" [] (List.ofArray allArgs))
      result
  result


/// Stats, the result type-check and the trace, once a builtin's body has produced a value. Runs
/// whether or not the body had to wait.
let private finishBuiltin
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (fn : BuiltInFn)
  (tst : TypeSymbolTable)
  (allArgs : Dval[])
  (sw : int64)
  (bodyAllocBefore : int64)
  (result : Dval)
  : Ply<Dval> =
  if vm.stats.enabled then
    let n = fn.name.name
    let mutable calls = 0L
    vm.stats.builtinCallsByName.TryGetValue(n, &calls) |> ignore<bool>
    vm.stats.builtinCallsByName[n] <- calls + 1L
    let d = System.GC.GetAllocatedBytesForCurrentThread() - bodyAllocBefore
    if d > 0L then
      vm.stats.builtinBodyAlloc <- vm.stats.builtinBodyAlloc + d
      let mutable prev = 0L
      vm.stats.builtinAlloc.TryGetValue(n, &prev) |> ignore<bool>
      vm.stats.builtinAlloc[n] <- prev + d
  // `sw = 0L` means the bracket never opened: timing was off when this call started and the call
  // itself turned it on (`interpreterStatsEnableDetailedTiming` is the case). Subtracting from zero
  // would record the raw tick count as a duration.
  if vm.stats.enabled && vm.stats.detailedTiming && sw <> 0L then
    let elapsed = System.Diagnostics.Stopwatch.GetTimestamp() - sw
    vm.stats.recordBuiltin (fn.name.name, elapsed)

  let biResAlloc = allocNow vm
  match TypeChecker.tryUnifySync tst fn.returnType result with
  | ValueSome _ ->
    recordStage vm ApplyStage.BiCheckResult biResAlloc
    Ply(traceBuiltinResult exeState currentFrame fn allArgs result)
  | ValueNone ->
    // Closed here rather than after the await: a bracket spanning a bind measures whatever nested
    // execution resumes inside it, not this region. The async answer isn't counted, which is the
    // right call -- it's rare, and counting it wrong is worse than not counting it.
    recordStage vm ApplyStage.BiCheckResult biResAlloc
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
      | Error rte -> raiseRTE vm.threadID rte
      return traceBuiltinResult exeState currentFrame fn allArgs result
    }


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
  (allArgs : Dval[])
  : Ply<Dval> =
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
      raiseRTE
        vm.threadID
        (RTE.UncaughtException(
          $"capability denied: `{fn.name.name}` needs {what}, which this instance doesn't grant. Grant it with `dark caps`.",
          []
        ))
    | Capabilities.Allowed -> ()

  let bodyAllocBefore =
    if vm.stats.enabled then System.GC.GetAllocatedBytesForCurrentThread() else 0L

  // Every builtin's signature is async because some of them have to be -- HTTP, the package store,
  // anything touching disk. Most aren't: `Int64.add` computes and returns.
  let body = fn.fn (struct (exeState, vm, resolvedTypeArgs, allArgs))

  // `finishBuiltin` is top-level rather than a local closing over the eight values it needs, for the
  // same reason `completeBuiltin` is: the fallback arm below is a `uply`, so a local would be captured
  // and built on every call.
  match Ply.trySync body with
  | ValueSome result ->
    finishBuiltin exeState vm currentFrame fn tst allArgs sw bodyAllocBefore result
  | ValueNone ->
    uply {
      let! result = body
      return!
        finishBuiltin
          exeState
          vm
          currentFrame
          fn
          tst
          allArgs
          sw
          bodyAllocBefore
          result
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
  (allArgs : Dval[])
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
        // `Applicable.argsSoFar` is a list because lambdas share it. Converting back costs a cons
        // per argument, but only on a partial application, which is rare and already not free.
        argsSoFar = List.ofArray allArgs }
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
  // No local `raiseRTE` alias: the cold-path `uply` below would capture it, and a local a
  // closure captures is an allocation on every call, not just the ones that need it.
  let applicable = ctx.applicable
  // Builtins take a `List<Dval>`, so this is the one path that still materialises one.
  let biArgListAlloc = allocNow vm
  let newArgDvals = ArgSeq.toArrayFor currentFrame ctx.args
  recordStage vm ApplyStage.BiArgList biArgListAlloc

  let mutable tst = ctx.tst

  // Step 2: shadow this fn's free type-vars from the inherited TST. Mirrors the package-fn path;
  // without shadowing, an outer fn's `'a := X` would silently constrain a builtin's unrelated `'a`
  // (e.g. `==` with `'a` polluted to a tuple type from a parent List<(...)> context).
  let biShadowAlloc = allocNow vm
  let fnFreeTVars = FreeTVars.ofBuiltin fn
  // Only used to strip names from `tst`, so with nothing in `tst` there's nothing to strip and not even
  // the memo lookup is worth doing.
  let implicitTypeParams : Set<string> =
    if TST.isEmpty tst then Set.empty else fnFreeTVars
  tst <- implicitTypeParams |> Set.fold (fun m name -> removeIfPresent name m) tst

  // Step 3: bind the (already-resolved) explicit type args. If the caller omitted type args, leave the
  // typeParams unbound for inference to fill in.
  let explicitlyBound =
    if List.isEmpty resolvedTypeArgsVT then
      TST.empty
    else
      List.zip fn.typeParams resolvedTypeArgsVT |> TST.ofList
  tst <- TST.mergeFavoringRight tst explicitlyBound
  recordStage vm ApplyStage.BiTstShadow biShadowAlloc

  let biArgsAlloc = allocNow vm
  let allArgs =
    match applicable.argsSoFar with
    | [] -> newArgDvals
    | prev -> Array.append (List.toArray prev) newArgDvals
  recordStage vm ApplyStage.BiArgs biArgsAlloc

  let paramCount = List.length fn.parameters
  let argCount = Array.length allArgs

  // Step 4: infer type-variable bindings from arg ValueTypes for any TVariables in the param types not
  // bound by explicit type args. Same rule as the package-fn path. Nothing to infer for a fn with no
  // free type-vars, which most builtins are, and skipping saves a `Dval.toValueType` per argument.
  if argCount > 0 && not (Set.isEmpty fnFreeTVars) then
    // Lockstep: projecting the param types into their own list and zipping it against the args
    // allocates two lists and a pair per argument.
    let inferredBound = inferBiParams explicitlyBound fn.parameters allArgs 0
    if not (TST.isEmpty inferredBound) then
      tst <- TST.mergeFavoringRight tst inferredBound

  // Step 5: type-check the new arguments against the corresponding parameters. Walk them in lockstep
  // rather than building an indexed triple list and zipping it against the args. `already` is the count
  // already applied, so the first parameter checked keeps its original index.
  let biTcRunAlloc = allocNow vm
  let already = List.length applicable.argsSoFar
  let struct (biNextI, biRestPs, biRestArgIdx, biTst) =
    checkBiParamsSync already (List.skip already fn.parameters) newArgDvals 0 tst
  tst <- biTst
  recordStage vm ApplyStage.BiTypeCheckRun biTcRunAlloc

  // `if` rather than `match biRestPs, biRestArgs with`: the tuple form allocates the pair, once per
  // call, to ask a question two `isEmpty` checks answer.
  if List.isEmpty biRestPs || biRestArgIdx >= newArgDvals.Length then
    completeBuiltin exeState vm currentFrame ctx fn allArgs argCount paramCount tst
  else
    // Something in the remaining parameters needs the type store. Finish the check in a computation
    // expression and carry on from there -- still the one implementation, just resumed asynchronously.
    let typeCheckParam = TypeChecker.checkFnParam exeState.types applicable.name
    // An immutable snapshot, so the `uply` below captures this instead of the mutable `tst`. A
    // mutable a closure captures becomes a heap ref cell, allocated on every call to serve a branch
    // that after the first call is never taken; `FSharpRef<FSharpMap<string, ValueType>>` was 1.1%
    // of the profile and this is both of the places it came from.
    let tstAtCheck = tst
    uply {
      let mutable tstRest = tstAtCheck
      let rec checkRest i (ps : List<BuiltInParam>) (idx : int) =
        uply {
          match ps with
          | [] -> return ()
          | p :: pRest ->
            if idx >= newArgDvals.Length then
              return ()
            else
              match! typeCheckParam tstRest i p.name p.typ newArgDvals[idx] with
              | Ok updatedTst ->
                tstRest <- updatedTst
                return! checkRest (i + 1) pRest (idx + 1)
              | Error rte -> return raiseRTE vm.threadID rte
        }
      do! checkRest biNextI biRestPs biRestArgIdx
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
  // Spelled out in both arms rather than shared as a local `run`, for the same reason as in
  // `callPackage`: the async arm would capture it, so the closure would be built on every call.
  match
    resolveTypeArgsSync
      exeState
      vm
      ctx.applicable.name
      (List.length fn.typeParams)
      ctx
  with
  | ValueSome resolved ->
    callBuiltinResolved exeState vm currentFrame ctx fn resolved
  | ValueNone ->
    uply {
      let! resolved = resolveTypeArgsAsync exeState ctx
      return! callBuiltinResolved exeState vm currentFrame ctx fn resolved
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
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  (implicitTypeParams : Set<string>)
  (newlyBound : TypeSymbolTable)
  (allArgs : ArgSeq)
  (argCount : int)
  (paramCount : int)
  (tst : TypeSymbolTable)
  : PackageOutcome =
  let applicable = ctx.applicable
  let typeArgs = ctx.typeArgs
  if argCount > paramCount then
    RTE.Applications.TooManyArgsForFn(applicable.name, paramCount, argCount)
    |> RTE.Apply
    |> raiseRTE vm.threadID
  elif argCount < paramCount then
    { applicable with
        typeArgs = typeArgs
        // Materialised only here: a partial application has to retain its arguments.
        argsSoFar = ArgSeq.toList allArgs
        typeSymbolTable = tst }
    |> AppNamedFn
    |> DApplicable
    |> PartiallyApplied
  else
    // Inherit the outer frame's TST but shadow this fn's own free type-vars first, so the inner fn's
    // `'a` is local to this call and not the outer's.
    let frameTstAlloc = allocNow vm
    let frameTst =
      let stripped =
        implicitTypeParams
        |> Set.fold
          (fun m name -> removeIfPresent name m)
          currentFrame.typeSymbolTable
      TST.mergeFavoringRight stripped newlyBound
    recordStage vm ApplyStage.PkgFrameTst frameTstAlloc
    let pkgFrameAlloc = allocNow vm
    if vm.stats.enabled then
      let n = int64 (TST.count frameTst)
      vm.stats.tstSizeSum <- vm.stats.tstSizeSum + n
      if n > vm.stats.tstSizeMax then vm.stats.tstSizeMax <- n
    let newFrameId = nextFrameId vm
    if not exeState.tracing.skipTracing then
      vm.pendingCallArgs[newFrameId] <- ArgSeq.toList allArgs
    if vm.stats.enabled then
      vm.stats.packageCallCount <- vm.stats.packageCallCount + 1L
      vm.stats.framePushCount <- vm.stats.framePushCount + 1L
      if vm.stats.detailedTiming then
        vm.stats.framePushTimestamps[newFrameId] <-
          System.Diagnostics.Stopwatch.GetTimestamp()
    let pkgEp = FreeTVars.packageExecutionPoint fn.hash
    if not exeState.tracing.skipTracing then
      exeState.tracing.storeFrameEntry newFrameId pkgEp (ArgSeq.toList allArgs)
    // We already hold the fn here, so the loop needn't fetch it.
    let instrData =
      let mutable cached = Unchecked.defaultof<InstrData>
      if exeState.packageFnInstrCache.TryGetValue(fn.hash, &cached) then
        cached
      else
        let d : InstrData =
          { instructions = List.toArray fn.body.instructions
            resultReg = fn.body.resultIn }
        exeState.packageFnInstrCache[fn.hash] <- d
        d
    if vm.stats.enabled then
      vm.stats.registersAllocated <-
        vm.stats.registersAllocated + int64 fn.body.registerCount
    let frame =
      takeFrame
        vm
        fn.body.registerCount
        newFrameId
        (ValueSome(struct (vm.currentFrameID, ctx.putResultIn, ctx.returnPc)))
        pkgEp
        instrData
        (ValueSome fn.returnType)
        frameTst
    ArgSeq.fill frame.registers allArgs
    recordStage vm ApplyStage.PkgFrame pkgFrameAlloc
    PushFrame frame


/// Everything after the explicit type args are resolved. See `callPackage`.
let private callPackageResolved
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  (resolvedExplicitTypeArgsVT : List<ValueType>)
  : Ply<PackageOutcome> =
  // No local `raiseRTE` alias: the cold-path `uply` below would capture it, and a local a
  // closure captures is an allocation on every call, not just the ones that need it.
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
    if TST.isEmpty tst && TST.isEmpty currentFrame.typeSymbolTable then
      Set.empty
    else
      fnFreeTVars
  tst <- implicitTypeParams |> Set.fold (fun m name -> removeIfPresent name m) tst

  // Step 3: bind the (already-resolved) explicit type args into the freshly-shadowed tst. Explicit args
  // win over inferred bindings filled in below. If the caller omitted type args, leave the typeParams
  // unbound for inference.
  let explicitlyBound =
    if List.isEmpty resolvedExplicitTypeArgsVT then
      TST.empty
    else
      List.zip fn.typeParams resolvedExplicitTypeArgsVT |> TST.ofList
  if not (TST.isEmpty explicitlyBound) then
    tst <- TST.mergeFavoringRight tst explicitlyBound
  recordStage vm ApplyStage.PkgTstShadow pkgShadowAlloc

  // Pre-compute allArgs so inference runs BEFORE typeCheckParams. Otherwise the type check runs against
  // a TST that doesn't yet know `'a := whatever`.
  let allArgs = ArgSeq.withPrior applicable.argsSoFar newArgDvals

  let paramCount = NEList.length fn.parameters
  let argCount = ArgSeq.count allArgs

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
      let rec inferPkg acc (ps : List<PackageFn.Parameter>) (args : ArgSeq) =
        match ps with
        | [] -> acc
        | p :: pRest ->
          match ArgSeq.uncons args with
          | ValueNone -> acc
          | ValueSome(struct (a, aRest)) ->
            inferPkg (inferTVarsFromDval acc p.typ a) pRest aRest
      inferPkg explicitlyBound (FreeTVars.paramsOfPackage fn) allArgs
  if not (TST.isEmpty newlyBound) then
    tst <- TST.mergeFavoringRight tst newlyBound
  recordStage vm ApplyStage.PkgInfer pkgInferAlloc


  // Step 5: type-check params against the fully-resolved tst. Lockstep walk, no indexed-triple list and
  // no zip.
  let pkgTcAlloc = allocNow vm
  let alreadyApplied = List.length applicable.argsSoFar
  let pkgParams = FreeTVars.paramsOfPackage fn |> List.skip alreadyApplied
  recordStage vm ApplyStage.PkgTypeCheckArgs pkgTcAlloc

  let pkgTcRunAlloc = allocNow vm
  let struct (pkgNextI, pkgRestPs, pkgRestArgs, pkgTst) =
    checkPkgParamsSync alreadyApplied pkgParams newArgDvals tst
  tst <- pkgTst
  recordStage vm ApplyStage.PkgTypeCheckRun pkgTcRunAlloc

  // Same as in `callBuiltinResolved`: two `isEmpty` checks, no pair.
  if List.isEmpty pkgRestPs || ArgSeq.isEmpty pkgRestArgs then
    Ply(
      completePackage
        exeState
        vm
        currentFrame
        ctx
        fn
        implicitTypeParams
        newlyBound
        allArgs
        argCount
        paramCount
        tst
    )
  else
    // Something in the remaining parameters needs the type store. Finish the check in a computation
    // expression and carry on from there -- still one implementation, just resumed asynchronously.
    let typeCheckParam = TypeChecker.checkFnParam exeState.types applicable.name
    // An immutable snapshot, so the `uply` below captures this instead of the mutable `tst`. A
    // mutable a closure captures becomes a heap ref cell, allocated on every call to serve a branch
    // that after the first call is never taken; `FSharpRef<FSharpMap<string, ValueType>>` was 1.1%
    // of the profile and this is both of the places it came from.
    let tstAtCheck = tst
    uply {
      let mutable tstRest = tstAtCheck
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
              | Error rte -> return raiseRTE vm.threadID rte
        }
      // The cold path materialises: it already awaits per parameter, so a list is not the cost.
      do! checkRest pkgNextI pkgRestPs (ArgSeq.toList pkgRestArgs)
      return
        completePackage
          exeState
          vm
          currentFrame
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
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  : Ply<PackageOutcome> =
  // Both arms spell out the call rather than sharing a local `run`. The async arm would capture that
  // local into its closure, and a local a closure captures can't be lambda-lifted away, so the six
  // values it closes over are an allocation on every package call whether or not the async arm runs.
  match
    resolveTypeArgsSync
      exeState
      vm
      ctx.applicable.name
      (List.length fn.typeParams)
      ctx
  with
  | ValueSome resolved ->
    callPackageResolved exeState vm currentFrame ctx fn resolved
  | ValueNone ->
    uply {
      let! resolved = resolveTypeArgsAsync exeState ctx
      return! callPackageResolved exeState vm currentFrame ctx fn resolved
    }




/// What an `Apply` still needs, after everything that could be done synchronously has been.
[<Struct>]
type private ApplyOutcome =
  /// Finished. Nothing to await.
  | ApplyDone
  /// A builtin that had to wait. Its result goes in this register.
  | AwaitBuiltin of bCall : Ply<Dval> * bReg : Register
  /// A package call that had to wait. Its outcome is a value for this register, or a frame to push.
  | AwaitPackage of pCall : Ply<PackageOutcome> * pReg : Register

  /// Spelled out rather than compared with `=`: a `Ply` doesn't support equality, so neither does this.
  member this.IsDone =
    match this with
    | ApplyDone -> true
    | _ -> false


/// One `Apply` instruction, run without entering the interpreter's computation expression.
///
/// Returns what, if anything, still has to be awaited. Almost nothing does: resolving a type arg is a
/// cache hit, checking an argument needs no store lookup in the ordinary case, and a pure builtin hands
/// back a `Ply` that is already finished. Those cases finish here and the caller never touches the
/// builder.
///
/// `outcome` is a plain mutable local, not a captured one: there is no computation expression in this
/// function, so it lives in a slot rather than a ref cell. Keeping it meant every existing branch could
/// stay unit-typed, which made this a move rather than a rewrite.
let private applyInstruction
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (registers : Dval array)
  (putResultIn : Register)
  (thingToCallReg : Register)
  (typeArgs : List<TypeReference>)
  (newArgRegs : NEList<Register>)
  : ApplyOutcome =
  let mutable outcome = ApplyDone
  let applyTotalAlloc = allocNow vm
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
      |> raiseRTE vm.threadID

  // Deliberately not read into a list here. The package path walks the caller's registers directly,
  // and only the lambda and builtin paths below materialise one.
  let applyArgsAlloc = allocNow vm
  recordStage vm ApplyStage.ApplyArgs applyArgsAlloc

  match applicable with
  | AppLambda appLambda ->
    let lambdaTotalAlloc = allocNow vm
    let exprId = appLambda.exprId
    let foundLambda =
      let mutable cached = Unchecked.defaultof<_>
      if exeState.lambdaInstrCache.TryGetValue(exprId, &cached) then
        cached
      else
        Exception.raiseInternal "lambda not found" [ "exprId", exprId ]

    let allArgs =
      ArgSeq.withPrior appLambda.argsSoFar (ArgSeq.ofNE registers newArgRegs)

    let argCount = ArgSeq.count allArgs
    let paramCount = NEList.length foundLambda.patterns

    if typeArgs <> [] then
      RTE.Applications.CannotApplyTypeArgsToLambda
      |> RTE.Apply
      |> raiseRTE vm.threadID

    if argCount = paramCount then
      let lambdaFrameAlloc = allocNow vm
      // Hoisted out of the record expression so each piece can be bracketed separately;
      // `lambda.frame` was the largest Apply stage and most of it was unaccounted for.
      let lambdaTstAlloc = allocNow vm
      let lambdaTst =
        if TST.isEmpty appLambda.typeSymbolTable then
          currentFrame.typeSymbolTable
        else if TST.isEmpty currentFrame.typeSymbolTable then
          appLambda.typeSymbolTable
        else
          TST.mergeFavoringRight
            appLambda.typeSymbolTable
            currentFrame.typeSymbolTable
      recordStage vm ApplyStage.LambdaTst lambdaTstAlloc

      let lambdaEpAlloc = allocNow vm
      let parentEp = currentFrame.executionPoint
      // The `ExecutionPoint` a lambda body runs under is a pure function of (calling frame's
      // execution point, lambda's expression id), and both repeat: a lambda in a loop is called
      // from the same function over and over. So it's memoized rather than rebuilt per call, which
      // was 88% of everything a lambda application allocated.
      //
      // Keyed on the expression id, holding the parent it was derived from. A single last-value
      // slot is not enough -- `List.map` alternates between its own recursion and the caller's
      // lambda, so two expression ids interleave and a one-entry cache misses every time. This
      // cost an hour to find; the counter barely moved and the reason was thrashing, not a bug.
      let lambdaEp =
        let mutable hit =
          Unchecked.defaultof<struct (ExecutionPoint * ExecutionPoint)>
        if
          vm.lambdaEpCache.TryGetValue(exprId, &hit)
          && (let struct (cachedParent, _) = hit
              System.Object.ReferenceEquals(cachedParent, parentEp))
        then
          let struct (_, ep) = hit
          ep
        else
          let ep = Lambda(parentEp, exprId)
          vm.lambdaEpCache[exprId] <- struct (parentEp, ep)
          ep
      recordStage vm ApplyStage.LambdaExecPoint lambdaEpAlloc

      // Resolved here so the loop never has to look it up. Same shared InstrData the
      // per-VM cache holds; this is a reference to it, not a copy.
      let lambdaInstrData =
        // `TryGetValue` rather than `Map.tryFind`, which allocates a `Some` on every hit.
        let mutable hit = Unchecked.defaultof<InstrData>
        if vm.lambdaInstrDataCache.TryGetValue(exprId, &hit) then
          hit
        else
          let d : InstrData =
            { instructions = List.toArray foundLambda.instructions.instructions
              resultReg = foundLambda.instructions.resultIn }
          vm.lambdaInstrDataCache[exprId] <- d
          d

      let newFrame =
        takeFrame
          vm
          foundLambda.instructions.registerCount
          (nextFrameId vm)
          (ValueSome(
            struct (vm.currentFrameID, putResultIn, currentFrame.programCounter + 1)
          ))
          lambdaEp
          lambdaInstrData
          ValueNone
          lambdaTst

      let lambdaRegsAlloc = allocNow vm
      if vm.stats.enabled then
        vm.stats.registersAllocated <-
          vm.stats.registersAllocated + int64 foundLambda.instructions.registerCount
      let r = newFrame.registers

      // extract and copy over the args
      bindLambdaParams
        vm
        r
        (FreeTVars.patternsOfLambda exprId foundLambda.patterns)
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
      recordStage vm ApplyStage.LambdaRegisters lambdaRegsAlloc

      recordStage vm ApplyStage.LambdaFrame lambdaFrameAlloc
      if vm.stats.enabled then
        vm.stats.framePushCount <- vm.stats.framePushCount + 1L
      if not exeState.tracing.skipTracing then
        exeState.tracing.storeFrameEntry
          newFrame.id
          newFrame.executionPoint
          (ArgSeq.toList allArgs)
      vm.frameToPush <- ValueSome newFrame

    else if argCount > paramCount then
      RTE.Applications.TooManyArgsForLambda(exprId, paramCount, argCount)
      |> RTE.Apply
      |> raiseRTE vm.threadID
    else
      registers[putResultIn] <-
        // Materialised only here: a partial application has to retain its arguments.
        { appLambda with argsSoFar = ArgSeq.toList allArgs }
        |> AppLambda
        |> DApplicable

    recordStage vm ApplyStage.LambdaTotal lambdaTotalAlloc

  | AppNamedFn applicable ->
    // The symbol table the call starts from. `callBuiltin` and `callPackage` take it from
    // here and do the rest -- shadowing, inference, checking, invocation -- outside this
    // computation expression, which is where all of it used to live.
    let tst =
      if TST.isEmpty applicable.typeSymbolTable then
        currentFrame.typeSymbolTable
      else if TST.isEmpty currentFrame.typeSymbolTable then
        applicable.typeSymbolTable
      else
        TST.mergeFavoringRight
          currentFrame.typeSymbolTable
          applicable.typeSymbolTable

    let typeArgs =
      match applicable.typeArgs with
      | [] -> typeArgs
      | oldTypeArgs ->
        match typeArgs with
        | [] -> oldTypeArgs
        | _ ->
          RTE.Applications.CannotApplyTypeArgsMoreThanOnce
          |> RTE.Apply
          |> raiseRTE vm.threadID

    let ctx : ApplyContext =
      { applicable = applicable
        typeArgs = typeArgs
        args = ArgSeq.ofNE registers newArgRegs
        tst = tst
        putResultIn = putResultIn
        returnPc = currentFrame.programCounter + 1 }

    // CLEANUP the two branches below are near-identical in shape, and so are `callBuiltin`
    // and `callPackage` behind them: same five steps, different parameter and outcome types.
    // Unifying them needs `BuiltInParam` and `PackageFn.Parameter` to share an interface.
    match applicable.name with
    | FQFnName.Builtin builtin ->
      let biTotalAlloc = allocNow vm
      let biLookupAlloc = allocNow vm
      // `TryGetValue` rather than `Map.find`, which allocates a `Some` on every hit --
      // `FSharpOption<BuiltInFn>` was 0.9% of the profile, and this is where it came from.
      // F#'s Map implements IDictionary, so the byref overload is available here too.
      let mutable found = Unchecked.defaultof<BuiltInFn>
      if not (exeState.fns.builtIn.TryGetValue(builtin, &found)) then
        RTE.FnNotFound(FQFnName.Builtin builtin) |> raiseRTE vm.threadID
      else
        let fn = found
        recordStage vm ApplyStage.BiFnLookup biLookupAlloc
        let call = callBuiltin exeState vm currentFrame ctx fn
        // Usually already finished, in which case there's no bind to pay for.
        match Ply.trySync call with
        | ValueSome dv -> registers[putResultIn] <- dv
        | ValueNone -> outcome <- AwaitBuiltin(call, putResultIn)
        recordStage vm ApplyStage.BiTotal biTotalAlloc

    | FQFnName.Package pkg ->
      // Harmful-deprecation runtime halt.
      // Checked before even fetching the fn so the error is surfaced
      // whether or not the fn definition is still available.
      let isHarmful = exeState.fns.isHarmful pkg
      if isHarmful && not exeState.allowHarmful then
        RTE.DeprecatedItemHalted pkg |> raiseRTE vm.threadID
      let pkgFetchAlloc = allocNow vm
      // Warm cache after the first call, which for a script means all but a handful of these.
      //
      // The `let!` for the cold path used to sit here, in the loop's computation expression,
      // and cost a continuation closure on every call whether or not it was reached -- the
      // same thing that made the two call paths expensive before they were extracted. Now the
      // miss builds its own `uply` and the hit never touches the builder.
      let fetchOnlyAlloc = allocNow vm
      let fetch = exeState.fns.package pkg
      let fetched = Ply.trySync fetch
      recordStage vm ApplyStage.PkgFetchOnly fetchOnlyAlloc
      let call =
        match fetched with
        | ValueSome(Some fn) -> callPackage exeState vm currentFrame ctx fn
        | ValueSome None ->
          RTE.FnNotFound(FQFnName.Package pkg) |> raiseRTE vm.threadID
        | ValueNone ->
          uply {
            match! fetch with
            | Some fn -> return! callPackage exeState vm currentFrame ctx fn
            | None ->
              return RTE.FnNotFound(FQFnName.Package pkg) |> raiseRTE vm.threadID
          }
      recordStage vm ApplyStage.PkgFetch pkgFetchAlloc
      // Overwritten on the next line either way; F# needs something to start from.
      // No `let mutable` spanning the bind. A mutable a continuation captures becomes a
      // heap ref cell, allocated whether or not the branch that needs it is taken; measured
      // at 4.4 MB for this one. Duplicating two lines is cheaper than the cell.
      match Ply.trySync call with
      | ValueSome(PartiallyApplied dv) -> registers[putResultIn] <- dv
      | ValueSome(PushFrame frame) -> vm.frameToPush <- ValueSome frame
      | ValueNone -> outcome <- AwaitPackage(call, putResultIn)

  recordStage vm ApplyStage.ApplyTotal applyTotalAlloc
  outcome


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
  : struct (int * ApplyOutcome) =
  // No local `raiseRTE` alias: F# doesn't lift it out of the loop below, so it's a closure over
  // the VM allocated on every call. `raiseRTE@614` was 2.9% of the profile.
  let mutable counter = startCounter
  let mutable running = true
  // Set only if an `Apply` below has to wait for something. A struct, so carrying it costs nothing.
  let mutable pending = ApplyDone

  while running && counter < instrData.instructions.Length do
    let inst = instrData.instructions[counter]

    match inst with
    | CreateRecord _
    | CloneRecordWithUpdates _
    | CreateEnum _
    | LoadValue _ -> running <- false

    // `Apply` is 114,202 of the 114,274 instructions that used to stop this drain and hand control
    // to the computation expression, which builds a continuation per iteration. It almost never has
    // to wait, so it runs here, and only a genuine await stops the drain.
    | Apply(putResultIn, thingToCallReg, typeArgs, newArgRegs) ->
      if vm.stats.enabled then
        vm.stats.instructionCount <- vm.stats.instructionCount + 1L
      let allocBefore =
        if vm.stats.enabled then
          System.GC.GetAllocatedBytesForCurrentThread()
        else
          0L

      // The frame this pushes records `currentFrame.programCounter + 1` as where to resume the
      // caller, and this loop tracks the counter in a local, so the field has to be caught up first
      // or the callee returns to the wrong instruction.
      currentFrame.programCounter <- counter

      pending <-
        applyInstruction
          exeState
          vm
          currentFrame
          registers
          putResultIn
          thingToCallReg
          typeArgs
          newArgRegs

      if vm.stats.enabled then
        let tag = Opcode.index inst
        if tag >= 0 && tag < vm.stats.allocByOpcode.Length then
          let delta = System.GC.GetAllocatedBytesForCurrentThread() - allocBefore
          if delta > 0L then
            vm.stats.allocByOpcode[tag] <- vm.stats.allocByOpcode[tag] + delta
          vm.stats.countByOpcode[tag] <- vm.stats.countByOpcode[tag] + 1L

      match pending with
      // Nothing to wait for. Step past it, and stop only if it pushed a frame for the outer loop.
      | ApplyDone ->
        counter <- counter + 1
        if vm.frameToPush.IsSome then running <- false
      // Hand the wait back to the caller, with the counter still on this instruction. The caller
      // steps past it once the result is in its register.
      | _ -> running <- false
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
            |> raiseRTE vm.threadID
        | l ->
          let r = registers[right]
          RTE.Bools.OrOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
          |> RTE.Bool
          |> raiseRTE vm.threadID
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
            |> raiseRTE vm.threadID
        | l ->
          let r = registers[right]
          RTE.Bools.AndOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
          |> RTE.Bool
          |> raiseRTE vm.threadID


      // == Working with Variables ==
      | CheckLetPatternAndExtractVars(valueReg, pat) ->
        let dv = registers[valueReg]
        // Fast path for the common single-variable let binding
        match pat with
        | LPVariable extractTo -> registers[extractTo] <- dv
        | LPUnit ->
          match dv with
          | DUnit -> ()
          | _ ->
            raiseRTE vm.threadID (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))
        | _ ->
          let doesMatch, registersToAssign = checkAndExtractLetPattern pat dv
          if doesMatch then
            registersToAssign
            |> List.iter (fun (reg, value) -> registers[reg] <- value)
          else
            raiseRTE vm.threadID (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))


      // TODO References to DBs should be resolved at parse-time, not
      // runtime. For consistency, safety, etc. We should have a specific
      // EReferenceDB construct that we respect throughout WT, NR, PT, RT,
      // PT2RT, etc. I don't think this would be that hard.
      | VarNotFound(targetRegIfDB, varName) ->
        match exeState.program.dbs |> Map.get varName with
        | Some _foundDB -> registers[targetRegIfDB] <- DDB varName
        | None -> raiseRTE vm.threadID (RTE.VariableNotFound varName)



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
              raiseRTE
                vm.threadID
                (RTE.String(RTE.Strings.Error.NonStringInInterpolation(vt, dv))))

        registers[targetReg] <- DString(sb.ToString())


      // == Flow Control ==
      // -- Jumps --
      | JumpBy jumpBy -> counter <- counter + jumpBy
      | JumpByIfFalse(jumpBy, condReg) ->
        match registers[condReg] with
        | DBool false -> counter <- counter + jumpBy
        | DBool true -> ()
        | dv ->
          raiseRTE
            vm.threadID
            (RTE.Bool(RTE.Bools.ConditionRequiresBool(Dval.toValueType dv, dv)))

      // -- Match --
      | CheckMatchPatternAndExtractVars(valueReg, pat, failJump) ->
        // Fast path for common single-variable match
        match pat with
        | MPVariable reg -> registers[reg] <- registers[valueReg]
        | _ ->
          let buf = vm.matchBindings
          buf.Clear()
          if checkAndExtractMatchPattern buf pat registers[valueReg] then
            // Written only now that the whole pattern has matched, so a pattern that failed partway
            // leaves the frame untouched. An index loop, not `for x in buf`, which boxes the
            // enumerator.
            for i in 0 .. buf.Count - 1 do
              let struct (reg, value) = buf[i]
              registers[reg] <- value
          else
            counter <- counter + failJump
      | MatchUnmatched(valueReg) ->
        let unmatchedValue = registers[valueReg]
        raiseRTE vm.threadID (RTE.Match(RTE.Matches.MatchUnmatched unmatchedValue))


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
            RTE.Records.FieldAccessEmptyFieldName
            |> RTE.Record
            |> raiseRTE vm.threadID
          else
            match Map.find fieldName fields with
            | Some value -> registers[targetReg] <- value
            | None ->
              RTE.Records.FieldAccessFieldNotFound fieldName
              |> RTE.Record
              |> raiseRTE vm.threadID
        | dv ->
          RTE.Records.FieldAccessNotRecord(Dval.toValueType dv)
          |> RTE.Record
          |> raiseRTE vm.threadID


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
      | RaiseNRE(names, nre) ->
        raiseRTE vm.threadID (RTE.ParseTimeNameResolution(names, nre))

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
          |> raiseRTE vm.threadID
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

  struct (counter, pending)




/// Why the synchronous run of a frame's instructions stopped.
[<Struct>]
type private FrameStep =
  /// The block ended, or an `Apply` pushed a frame. Either way the caller looks at `vm.frameToPush`.
  | FrameBlockEnded
  /// A builtin that had to wait. Its result goes in this register.
  | FrameAwaitBuiltin of fbCall : Ply<Dval> * fbReg : Register
  /// A package call that had to wait.
  | FrameAwaitPackage of fpCall : Ply<PackageOutcome> * fpReg : Register
  /// The counter is sitting on one of the four opcodes the caller still runs itself.
  | FrameRareOpcode

  member this.IsBlockEnded =
    match this with
    | FrameBlockEnded -> true
    | _ -> false


/// Run a frame's instructions until something needs the caller: an await, one of the four rare
/// opcodes, a pushed frame, or the end of the block.
///
/// This is the loop that used to live inside the computation expression, where Ply built a
/// continuation for its body on every iteration -- once per frame activation, about 122,000 times in
/// the reference workload. Here it's an ordinary `while`, and the caller enters the builder only for
/// the cases above, which are rare.
let private runFrame
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (registers : Dval array)
  (instrData : InstrData)
  : FrameStep =
  let mutable step = FrameBlockEnded
  let mutable running = true

  while running
        && currentFrame.programCounter < instrData.instructions.Length
        && vm.frameToPush.IsNone do
    let struct (drainedTo, pendingApply) =
      runSyncInstructions
        exeState
        vm
        currentFrame
        registers
        instrData
        currentFrame.programCounter
    currentFrame.programCounter <- drainedTo

    match pendingApply with
    | AwaitBuiltin(call, reg) ->
      step <- FrameAwaitBuiltin(call, reg)
      running <- false
    | AwaitPackage(call, reg) ->
      step <- FrameAwaitPackage(call, reg)
      running <- false
    | ApplyDone ->
      if
        currentFrame.programCounter < instrData.instructions.Length
        && vm.frameToPush.IsNone
      then
        step <- FrameRareOpcode
        running <- false

  step


/// Make a freshly built frame the current one.
let inline private pushFrame (vm : VMState) (frame : CallFrame) : unit =
  vm.callFrames[frame.id] <- frame
  vm.currentFrameID <- frame.id


/// EXPERIMENT: this loop's computation expression is F#'s built-in `task`, not Ply's `uply`.
///
/// Ply is continuation-based and predates F# 6's resumable code. A micro-benchmark says a `uply`
/// loop allocates per iteration in proportion to the size of its body -- 32 bytes an iteration for a
/// ten-statement body, with or without a bind in it -- while the same loop under `task` allocates
/// nothing. This body is far bigger than ten statements and runs about 122,000 times.
/// A `Task<unit>` that is already finished, allocated once. `Task.CompletedTask` is the untyped
/// `Task`, and `Task.FromResult ()` would allocate on every call that has nothing to await.
let private completedUnit : System.Threading.Tasks.Task<unit> =
  System.Threading.Tasks.Task.FromResult()


/// The part of a frame's return check that actually needs the type store. Rare: only when the
/// return type can't be settled without a lookup.
///
/// Its own small `task`, with its binds at statement position, so it reduces to a static state
/// machine.
let private frameReturnTypeCheckAsync
  (exeState : ExecutionState)
  (vm : VMState)
  (tst : TypeSymbolTable)
  (expectedReturnType : TypeReference)
  (resultOfFrame : Dval)
  (fnName : FQFnName.FQFnName)
  : System.Threading.Tasks.Task<unit> =
  task {
    match!
      Ply.toTask (
        TypeChecker.unify exeState.types tst expectedReturnType resultOfFrame
      )
    with
    | Ok _updatedTst ->
      //currentFrame.typeSymbolTable <- updatedTst
      // CLEANUP is this^ or something like it worthwhile?
      ()
    | Error _path ->
      let! expectedVT =
        Ply.toTask (TypeReference.toVT exeState.types tst expectedReturnType)
      RuntimeError.Applications.FnResultNotExpectedType(
        fnName,
        expectedVT,
        Dval.toValueType resultOfFrame,
        resultOfFrame
      )
      |> RuntimeError.Apply
      |> raiseRTE vm.threadID
  }


/// Type-check what a frame is returning, if it's a function frame.
///
/// Deliberately *not* a computation expression: it returns an already-completed task in the ordinary
/// case and hands back the rare async one otherwise. A `match!` sitting inside a nested match arm is
/// what F#'s resumable code cannot reduce (FS3511), and an unreduced state machine silently falls
/// back to the dynamic, allocating implementation -- exactly what moving off Ply was meant to escape.
/// It also breaks the Release build, where that warning is an error.
let private checkFrameReturnType
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (resultOfFrame : Dval)
  : System.Threading.Tasks.Task<unit> =
  let retTcAlloc = allocNow vm
  match currentFrame.executionPoint with
  | Source
  | Lambda _ -> completedUnit
  | Function fnName ->
    // Recorded when the frame was pushed. Builtins never get a frame, so a Function frame always
    // carries one; the fallback keeps the match total rather than asserting.
    let expectedReturnType =
      match currentFrame.expectedReturnType with
      | ValueSome t -> t
      | ValueNone ->
        match fnName with
        | FQFnName.Builtin builtin -> exeState.fns.builtIn[builtin].returnType
        | FQFnName.Package _ -> RTE.FnNotFound fnName |> raiseRTE vm.threadID

    let tst = currentFrame.typeSymbolTable
    // Every frame return checks its result, so the same sync-first treatment as the argument checks
    // applies: skip the bind when the answer needs no type lookup.
    match TypeChecker.tryUnifySync tst expectedReturnType resultOfFrame with
    | ValueSome _ ->
      recordStage vm ApplyStage.FrameReturnTypeCheck retTcAlloc
      completedUnit
    | ValueNone ->
      recordStage vm ApplyStage.FrameReturnTypeCheck retTcAlloc
      frameReturnTypeCheckAsync
        exeState
        vm
        tst
        expectedReturnType
        resultOfFrame
        fnName


/// The four opcodes that can still need the package store: CreateRecord, CloneRecordWithUpdates,
/// CreateEnum and LoadValue.
///
/// Its own `task` so the interpreter loop's state machine stays statically compilable: six binds
/// nested two matches deep inside the loop stopped F#'s resumable code reducing it (FS3511), which
/// downgrades the whole loop to the dynamic implementation.
let private runRareOpcode
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (registers : Dval array)
  (inst : Instruction)
  : System.Threading.Tasks.Task<unit> =
  task {
    match inst with
    | CreateRecord(recordReg, sourceTypeName, typeArgs, fields) ->
      let fields =
        fields |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

      let! typeArgs =
        Ply.toTask (
          typeArgs
          |> Ply.List.mapSequentially (
            TypeReference.toVT exeState.types currentFrame.typeSymbolTable
          )
        )

      let! record =
        Ply.toTask (
          TypeChecker.DvalCreator.record
            exeState.types
            vm.threadID
            currentFrame.typeSymbolTable
            sourceTypeName
            typeArgs
            fields
        )

      registers[recordReg] <- record
    | CloneRecordWithUpdates(targetReg, originalRecordReg, fieldUpdates) ->
      let originalRecord = registers[originalRecordReg]

      match originalRecord with
      | DRecord(sourceTypeName, resolvedTypeName, typeArgs, originalFields) ->
        let fieldUpdates =
          fieldUpdates
          |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

        let! updatedRecord =
          Ply.toTask (
            TypeChecker.DvalCreator.recordUpdate
              exeState.types
              vm.threadID
              currentFrame.typeSymbolTable
              sourceTypeName
              resolvedTypeName
              typeArgs
              originalFields
              fieldUpdates
          )

        registers[targetReg] <- updatedRecord

      | dv ->
        Dval.toValueType dv
        |> RTE.Records.UpdateNotRecord
        |> RTE.Record
        |> raiseRTE vm.threadID
    | CreateEnum(enumReg, typeName, typeArgs, caseName, fields) ->
      let fields = fields |> List.map (fun valueReg -> registers[valueReg])

      let tst = currentFrame.typeSymbolTable

      let! typeArgs =
        Ply.toTask (
          typeArgs
          |> Ply.List.mapSequentially (TypeReference.toVT exeState.types tst)
        )

      let! newEnum =
        Ply.toTask (
          TypeChecker.DvalCreator.enum
            exeState.types
            vm.threadID
            tst
            typeName
            typeArgs
            caseName
            fields
        )

      registers[enumReg] <- newEnum
    | LoadValue(createTo, name) ->
      match name with
      | FQValueName.Builtin builtin ->
        match exeState.values.builtIn.TryGetValue builtin with
        | true, v -> registers[createTo] <- v.body
        | false, _ -> raiseRTE vm.threadID (RTE.ValueNotFound name)

      | FQValueName.Package pkg ->
        match! Ply.toTask (exeState.values.package pkg) with
        | Some v ->
          // The Dval is already stored in the package value
          registers[createTo] <- v.body
        | None -> raiseRTE vm.threadID (RTE.ValueNotFound name)
    // `Apply` never arrives here: `runSyncInstructions` runs it, and `runFrame` only reports
    // `FrameRareOpcode` for the four above. Loud rather than silent if that ever stops holding.
    | Apply _ ->
      Exception.raiseInternal
        "Apply reached the interpreter's async instruction path"
        []

    // Handled by `runSyncInstructions`; the match must still be exhaustive.
    | _ -> ()
  }



/// Everything the interpreter loop does once `runFrame` hands control back: the awaits, the four
/// rare opcodes, and the frame push or pop.
///
/// All of it lives here so the loop's body contains a single `do!` at statement position. F#'s
/// resumable code could not reduce a state machine with these binds sitting in match arms inside a
/// `while` (FS3511), and an unreduced machine falls back to the dynamic, allocating implementation --
/// exactly what moving off Ply was meant to escape.
let private handleFrameStep
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (registers : Dval array)
  (instrData : InstrData)
  (step : FrameStep)
  : System.Threading.Tasks.Task<unit> =
  task {
    match step with
    | FrameBlockEnded
    | FrameRareOpcode -> ()
    | FrameAwaitBuiltin(call, reg) ->
      let! dv = Ply.toTask call
      registers[reg] <- dv
      currentFrame.programCounter <- currentFrame.programCounter + 1
    | FrameAwaitPackage(call, reg) ->
      let! o = Ply.toTask call
      currentFrame.programCounter <- currentFrame.programCounter + 1
      match o with
      | PartiallyApplied dv -> registers[reg] <- dv
      // Pushed here rather than left in `vm.frameToPush`, which the next turn of this loop clears.
      | PushFrame frame -> pushFrame vm frame

    match step with
    | FrameRareOpcode ->
      if vm.stats.enabled then
        vm.stats.instructionCount <- vm.stats.instructionCount + 1L

      let inst = instrData.instructions[currentFrame.programCounter]
      let allocBefore =
        if vm.stats.enabled then
          System.GC.GetAllocatedBytesForCurrentThread()
        else
          0L

      do! runRareOpcode exeState vm currentFrame registers inst

      if vm.stats.enabled then
        let tag = Opcode.index inst
        if tag >= 0 && tag < vm.stats.allocByOpcode.Length then
          // Clamped at zero: these arms await, and this counter is per-thread, so a resume on another
          // thread makes the odd delta meaningless rather than merely noisy.
          let delta = System.GC.GetAllocatedBytesForCurrentThread() - allocBefore
          if delta > 0L then
            vm.stats.allocByOpcode[tag] <- vm.stats.allocByOpcode[tag] + delta
          vm.stats.countByOpcode[tag] <- vm.stats.countByOpcode[tag] + 1L

      currentFrame.programCounter <- currentFrame.programCounter + 1

    | FrameBlockEnded
    | FrameAwaitBuiltin _
    | FrameAwaitPackage _ -> ()

    // Either a frame was pushed, or this one finished. An await or a rare opcode just comes round
    // again, since the frame it was running is still the current one.
    // Only when the frame's block actually ended. An await or a rare opcode leaves the frame
    // part-run, and the next turn of this loop picks it up where it left off.
    if step.IsBlockEnded then
      match vm.frameToPush with
      | ValueSome newFrame ->
        // Something in this eval just pushed a frame -- don't do the "normal" processing
        vm.callFrames[newFrame.id] <- newFrame
        vm.currentFrameID <- newFrame.id

      | ValueNone ->
        // We are at the end of the instructions of the current frame
        // Either we're done with the whole eval, or we need to return a value to the parent frame
        let resultOfFrame = registers[instrData.resultReg]

        match currentFrame.parent with
        | ValueSome(parentID, regOfParentToPutResultInto, pcOfParent) ->
          // We just finished processing a frame, and we need to return a value to the parent frame

          // TODO this might be where the type-checking of a fn result needs to happen.
          // But when here, it's not always a fn call - could also be for a lambda.

          // A single `do!` at statement position, so the loop's state machine stays statically
          // compilable. `checkFrameReturnType` answers synchronously in the ordinary case.
          do! checkFrameReturnType exeState vm currentFrame resultOfFrame
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
              match vm.pendingCallArgs.TryGetValue(currentFrame.id) with
              | true, args ->
                vm.pendingCallArgs.Remove(currentFrame.id) |> ignore<bool>
                let source : Tracing.Source = (parentFrame.executionPoint, None)
                let fnRecord : Tracing.FunctionRecord = (source, fnName)
                exeState.tracing.storeFnResult
                  fnRecord
                  (NEList.ofListUnsafe "" [] args)
                  resultOfFrame
              | _ -> ()
            | Lambda _ ->
              vm.pendingCallArgs.Remove(currentFrame.id) |> ignore<bool>
              exeState.tracing.storeLambdaResult currentFrame.id resultOfFrame
            | Source -> vm.pendingCallArgs.Remove(currentFrame.id) |> ignore<bool>
          parentFrame.registers[regOfParentToPutResultInto] <- resultOfFrame
          parentFrame.programCounter <- pcOfParent
          // Last, after everything above that still reads the popped frame. `resultOfFrame` came out
          // of its registers before the pop and is now in the parent's.
          returnFrame vm currentFrame
          recordStage vm ApplyStage.FramePop framePopAlloc

        | ValueNone ->
          vm.callFrames.Remove(vm.currentFrameID) |> ignore<bool>
          vm.finalResult <- ValueSome resultOfFrame
  }



let private executeInnerTask
  (exeState : ExecutionState)
  (vm : VMState)
  : System.Threading.Tasks.Task<Dval> =
  task {
    // No local `raiseRTE` alias: every continuation the builder makes for the loop body would
    // capture it, so it's a field in each of them.

    while vm.callFrames.ContainsKey vm.currentFrameID do
      let currentFrame = vm.callFrames[vm.currentFrameID]

      let registers = currentFrame.registers


      // Resolved when the frame was pushed. This used to be a cache lookup bound with `let!` on every
      // iteration of this loop, which in the Ply builder's dynamic path allocates a continuation closure
      // each time -- once per awaiting instruction executed, so tens of thousands per script.
      let instrData = currentFrame.instrData

      vm.frameToPush <- ValueNone

      // The whole of a frame's instruction stream runs in `runFrame`, outside this computation
      // expression. It comes back only for an await, one of the four rare opcodes, a pushed frame or
      // the end of the block, and this loop then comes round again for the rest -- so the builder
      // makes a continuation per *interruption* rather than per iteration.
      let step = runFrame exeState vm currentFrame registers instrData

      do! handleFrameStep exeState vm currentFrame registers instrData step

    // If we've reached the end of the instructions, return the result
    match vm.finalResult with
    | ValueSome dv -> return dv
    | ValueNone -> return Exception.raiseInternal "No finalResult found" []
  }

let private executeInner (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  uply { return! executeInnerTask exeState vm }

let execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  executeInner exeState vm
