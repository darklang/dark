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
  /// Keyed by name, and so relying on a builtin's *signature* being determined by its name whatever
  /// execution state built it. That holds: `Builtins.Http.Client.builtins` is the only builtin set
  /// taking a construction parameter, and its `Configuration` reaches only the bodies, never
  /// `parameters`, `returnType` or `typeParams`.
  ///
  /// Worth stating because the same shape, keyed the same way, was a real bug once: the wrapper cache
  /// held a resolved `BuiltInFn` and handed one execution state's HTTP configuration to another's
  /// callers. A name-keyed cache of anything the *body* decides is wrong; of what the signature
  /// decides, right.
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

/// A call's arguments, without materialising them into a list.
///
/// Materialising costs a cons per argument per call, and the package path does not need it: its uses
/// are two lockstep walks and a copy into the callee's registers, all of which can read the caller's
/// register file directly.
///
/// The head is held separately from the tail rather than as one list, because the `Apply` instruction
/// carries its argument registers as an `NEList`, and `head :: tail` would be exactly the allocation
/// this exists to avoid.
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

  /// The arguments as an array, which is what a builtin takes, filled into the calling frame's
  /// scratch buffer for that arity. `count` is known up front, so there's no intermediate.
  /// See the note on `CallFrame.argBufs` for why a per-frame buffer needs no rent/return discipline.
  let toArrayFor (frame : CallFrame) (a : ArgSeq) : Dval[] =
    let n = count a
    if frame.argBufs.Length <= n then
      let grown : Dval[][] = Array.zeroCreate (n + 1)
      System.Array.Copy(frame.argBufs, grown, frame.argBufs.Length)
      frame.argBufs <- grown
    let arr =
      match frame.argBufs[n] with
      | null ->
        let fresh = Array.zeroCreate n
        frame.argBufs[n] <- fresh
        fresh
      | existing -> existing
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


/// Bind a let pattern's variables into `registers`, returning whether it matched.
///
/// Assigns as it walks rather than collecting `(Register * Dval)` pairs to assign afterwards, which
/// cost a tuple and a cons per bound variable plus the returned pair. A let is not a match: a pattern
/// that fails is fatal, and the frame raises rather than carrying on, so the partial writes a failed
/// walk leaves behind are never read.
let rec private assignLetPattern
  (registers : Registers)
  (pat : LetPattern)
  (dv : Dval)
  : bool =
  match pat with
  | LPVariable extractTo ->
    registers[extractTo] <- dv
    true
  | LPWildcard -> true
  | LPUnit ->
    match dv with
    | DUnit -> true
    | _ -> false
  | LPTuple(first, second, theRest) ->
    match dv with
    | DTuple(firstVal, secondVal, theRestVal) ->
      assignLetPattern registers first firstVal
      && assignLetPattern registers second secondVal
      && assignLetPatterns registers theRest theRestVal
    | _ -> false

and private assignLetPatterns
  (registers : Registers)
  (pats : List<LetPattern>)
  (dvs : List<Dval>)
  : bool =
  // Nested, not `match pats, dvs with`, which allocates the pair. Every tuple destructure reaches
  // here, including the pairs that make up nearly all of them: `assignLetPattern` handles the first
  // two elements itself and calls this with the rest, which for a pair is two empty lists -- so the
  // allocation happened on the way to answering `true` about nothing.
  match pats with
  | [] -> List.isEmpty dvs
  | pat :: patRest ->
    match dvs with
    | dv :: dvRest ->
      assignLetPattern registers pat dv && assignLetPatterns registers patRest dvRest
    | [] -> false


/// Try a match pattern against a value, appending any bindings it makes to `buf`.
///
/// Returns whether it matched. The bindings go in a caller-supplied buffer, reused for the life of
/// the VM, rather than a returned `List<Register * Dval>`: returning one costs a tuple and a cons
/// per bound variable on every pattern *tried*, not just on the one that matches.
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
      argBufs = Array.empty }

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
/// captured the argument array, which F# cannot lambda-lift, so it allocated a closure on every
/// builtin call -- enough to dominate the profile, and it appeared the moment this stopped taking
/// the arguments as an argument.
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


/// Copy a call's arguments into the callee's register file, positionally.
///
/// Top-level and taking the array, rather than a local `let rec` that closes over it: F# cannot lift
/// a recursive local that captures, so a local here means a closure on every package call.
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
      // One name bound to one register is the overwhelmingly common shape, so it skips the walk.
      match pat with
      | LPVariable extractTo -> r[extractTo] <- arg
      | _ ->
        if not (assignLetPattern r pat arg) then
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
/// Top-level for the same reason as `finishBuiltin` below it: a local here is captured by that
/// function's cold-path `uply`, and so gets built on every builtin call, hot path included.
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
/// The arithmetic and comparison operators, evaluated in the interpreter when both operands are
/// `Int`.
///
/// 65% of the builtin calls in a workbench view are these -- 7,049 of 10,881, `add` alone 1,562 --
/// and each was going through the whole call path: an `Apply`, the elided wrapper, the argument
/// array, unification of two type variables, the result check, a `Ply`. About 1.3 us to add two
/// integers.
///
/// Only two `Int`s. Every other numeric type, and every mixed or non-numeric pair, goes the ordinary
/// way and gets the ordinary error. The results here are the same expressions the builtins compute
/// for that case, and nothing else about them is reimplemented.
///
/// Not a compiler change: the `Apply` still happens, so a real opcode emitted by `PT2RT` would win
/// more again. This is the part that needed no new instruction.
module private IntOps =
  let add = 0
  let subtract = 1
  let lessThan = 2
  let lessThanOrEqualTo = 3
  let greaterThan = 4
  let greaterThanOrEqualTo = 5
  let equals = 6
  let notEquals = 7
  let max = 8
  let min = 9

  /// The operator itself, given a tag from `byName` and two `Int`s.
  let eval (tag : int) (a : DarkInt) (b : DarkInt) : Dval voption =
    if tag = add then
      ValueSome(Dval.dint (DarkInt.add a b))
    elif tag = subtract then
      ValueSome(Dval.dint (DarkInt.subtract a b))
    elif tag = lessThan then
      ValueSome(Dval.bool (DarkInt.compare a b < 0))
    elif tag = lessThanOrEqualTo then
      ValueSome(Dval.bool (DarkInt.compare a b <= 0))
    elif tag = greaterThan then
      ValueSome(Dval.bool (DarkInt.compare a b > 0))
    elif tag = greaterThanOrEqualTo then
      ValueSome(Dval.bool (DarkInt.compare a b >= 0))
    // Structurally, which is what `equals` does for this case: `DarkInt` is `Finite` whenever the
    // value fits an int64, so equal values have equal representations.
    elif tag = equals then
      ValueSome(Dval.bool (a = b))
    elif tag = notEquals then
      ValueSome(Dval.bool (a <> b))
    elif tag = max then
      ValueSome(if DarkInt.compare a b > 0 then DInt a else DInt b)
    elif tag = min then
      ValueSome(if DarkInt.compare a b < 0 then DInt a else DInt b)
    else
      ValueNone

  /// Looked up by name once per call rather than matched as a string: `FQFnName.Builtin` is a small
  /// record and this is a single probe of a table with ten entries in it.
  let byName : Dictionary<FQFnName.Builtin, int> =
    let d = Dictionary<FQFnName.Builtin, int>()
    let put (name : string) (tag : int) = d[{ name = name; version = 0 }] <- tag
    put "add" add
    put "subtract" subtract
    put "lessThan" lessThan
    put "lessThanOrEqualTo" lessThanOrEqualTo
    put "greaterThan" greaterThan
    put "greaterThanOrEqualTo" greaterThanOrEqualTo
    put "equals" equals
    put "notEquals" notEquals
    put "intMax" max
    put "intMin" min
    d


/// The result of an `Int` operator, or `ValueNone` to take the ordinary path.
///
/// Declines while tracing is on: a builtin call is recorded with its arguments and result when it
/// returns, and a fast path that skipped that would quietly drop every arithmetic operation from the
/// trace. Tracing is off in the CLI, which is what this is for.
let private tryIntOp
  (exeState : ExecutionState)
  (fn : BuiltInFn)
  (ctx : ApplyContext)
  : Dval voption =
  if
    not exeState.tracing.skipTracing || not (List.isEmpty ctx.applicable.argsSoFar)
  then
    ValueNone
  else
    let mutable tag = 0
    if not (IntOps.byName.TryGetValue(fn.name, &tag)) then
      ValueNone
    else
      match ArgSeq.uncons ctx.args with
      | ValueSome(struct (DInt a, rest)) ->
        match ArgSeq.uncons rest with
        | ValueSome(struct (DInt b, tail)) when ArgSeq.isEmpty tail ->
          IntOps.eval tag a b
        | _ -> ValueNone
      | _ -> ValueNone


/// The same operators as `tryIntOp`, reached straight from `Apply` before an `ApplyContext` exists.
///
/// `tryIntOp` covers the ones that arrive through an elided package wrapper; this covers the ones
/// compiled as a direct builtin call, which is what `a + b` is.
let private tryIntOpDirect
  (exeState : ExecutionState)
  (registers : Dval array)
  (applicable : ApplicableNamedFn)
  (typeArgs : List<TypeReference>)
  (argRegs : NEList<Register>)
  : Dval voption =
  match argRegs.tail with
  | [ secondReg ] when
    exeState.tracing.skipTracing
    && List.isEmpty typeArgs
    && List.isEmpty applicable.argsSoFar
    ->
    match applicable.name with
    | FQFnName.Builtin b ->
      let mutable tag = 0
      if IntOps.byName.TryGetValue(b, &tag) then
        // Nested, not `match a, b with`, which allocates the pair -- on every two-argument `Apply`,
        // Int operator or not. It measured 4.4% on the gate and 10% on a view build.
        match registers[argRegs.head] with
        | DInt x ->
          match registers[secondReg] with
          | DInt y -> IntOps.eval tag x y
          | _ -> ValueNone
        | _ -> ValueNone
      else
        ValueNone
    | _ -> ValueNone
  | _ -> ValueNone


let rec private callBuiltinResolved
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (ctx : ApplyContext)
  (fn : BuiltInFn)
  (resolvedTypeArgsVT : List<ValueType>)
  : Ply<Dval> =
  match tryIntOp exeState fn ctx with
  | ValueSome result ->
    // Counted, so `builtinCalls` still says how many builtin calls the program made.
    if vm.stats.enabled then
      vm.stats.builtinCallCount <- vm.stats.builtinCallCount + 1L
    Ply result
  | ValueNone ->
    callBuiltinResolvedSlow exeState vm currentFrame ctx fn resolvedTypeArgsVT

and private callBuiltinResolvedSlow
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
  // Guarded, as the package path already guards it: with no explicit type args there is nothing to
  // merge, and that is nearly every call.
  if not (TST.isEmpty explicitlyBound) then
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
    // that after the first call is never taken.
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
  /// A thin wrapper ran its builtin directly, with no frame. See `thinWrapperOf`.
  | Completed of result : Dval


/// Answers by hash, which content-addressing makes permanent.
///
/// The *name* is cached, not the resolved `BuiltInFn`. Caching the resolved one assumed the builtin
/// table is fixed for the process, and it is not: `Builtins.Http.Client.builtins` takes a
/// `Configuration`, so two execution states can hold two different `BuiltInFn` values under one name
/// -- one of which blocks localhost, private ranges and non-http schemes, and one of which does not.
/// A process-wide cache of the resolved fn hands whichever was seen first to everyone afterwards.
/// Resolving per call is one dictionary lookup against the table the caller is actually using.
let private thinWrapperCache =
  System.Collections.Concurrent.ConcurrentDictionary<Hash, FQFnName.Builtin voption>()

/// Structural type equality that ignores the name a type was written as.
///
/// `TCustomType` carries the source name beside the resolved hash, and the two sides of a forwarder
/// spell the same type differently: a builtin's `TypeReference.option varA` has no original name,
/// where the Dark signature wrapping it says `Option<'a>`. Comparing whole `TypeReference`s rejected
/// every forwarder whose signature mentions a custom type -- `Dict.get` among them, at 292 calls in
/// one workbench view.
let rec private sameType (a : TypeReference) (b : TypeReference) : bool =
  match a, b with
  | TCustomType(aName, aArgs), TCustomType(bName, bArgs) ->
    aName.resolved = bName.resolved && sameTypes aArgs bArgs
  | TStream x, TStream y
  | TList x, TList y
  | TDict x, TDict y
  | TDB x, TDB y -> sameType x y
  | TTuple(a1, a2, aRest), TTuple(b1, b2, bRest) ->
    sameType a1 b1 && sameType a2 b2 && sameTypes aRest bRest
  | TFn(aArgs, aRet), TFn(bArgs, bRet) ->
    sameTypes (NEList.toList aArgs) (NEList.toList bArgs) && sameType aRet bRet
  // Primitives and `TVariable`, where the case itself is the whole of the type.
  | _ -> a = b

and private sameTypes (a : List<TypeReference>) (b : List<TypeReference>) : bool =
  List.length a = List.length b && List.forall2 sameType a b


let private detectThinWrapper
  (exeState : ExecutionState)
  (fn : PackageFn.PackageFn)
  : FQFnName.Builtin voption =
  // The builtin's own type params are left to inference, which is what running the wrapper's body
  // would have done: that body applies the builtin with no explicit type args, which the match below
  // insists on.
  // Never a builtin that needs capabilities. Eliding `Stdlib.HttpClient.request`, a bare forwarder
  // like any other, changed what `request "put" "file:///etc/passwd"` does: 36 testfiles that assert
  // an unsupported-protocol error instead saw a real request attempted. The effectful builtins reach
  // for more of the calling context than a pure one does, and the wrapper's frame is part of that
  // context. Pure builtins are the whole of the win here anyway -- `Dict.get`, `Option`, `Tuple2` --
  // so this costs nothing worth having.
  let sameSignature (bi : BuiltInFn) =
    sameType fn.returnType bi.returnType
    && List.length bi.parameters = NEList.length fn.parameters
    && List.forall2
      (fun (p : PackageFn.Parameter) (bp : BuiltInParam) -> sameType p.typ bp.typ)
      (NEList.toList fn.parameters)
      bi.parameters

  match fn.body.instructions with
  | [ LoadVal(loadTo, DApplicable(AppNamedFn named))
      Apply(createTo, thingToApply, [], args) ] when
    loadTo = thingToApply
    && createTo = fn.body.resultIn
    && List.isEmpty named.argsSoFar
    && List.isEmpty named.typeArgs
    // The arguments must be the parameters, all of them, in order.
    && NEList.toList args = [ 0 .. NEList.length fn.parameters - 1 ]
    ->
    match named.name with
    | FQFnName.Builtin b ->
      match exeState.fns.builtIn.TryGetValue b with
      | true, bi when sameSignature bi -> ValueSome b
      | _ -> ValueNone
    | _ -> ValueNone
  | _ -> ValueNone

/// The builtin a package fn is a bare forwarder for, if it is one.
///
/// Most of the stdlib is `let f a b = Builtin.g a b`, which compiles to exactly two instructions: load
/// the builtin, apply it to the parameters in order. Calling such a fn the ordinary way pushes a frame
/// to run those two instructions, and that frame is most of what the call costs -- a wrapper is +4.0us
/// over baseline where the builtin it wraps is +1.5. Half the package calls in a view build are
/// forwarders of this shape.
///
/// Only forwarders whose signature is *identical* to the builtin's qualify. A wrapper that narrows
/// `List<'a>` to `List<TraceSummary>` is doing real work: its param types decide what is accepted, and
/// its return type is what `checkFrameReturnType` applies to the result. With the signatures equal,
/// running the builtin directly checks the same things against the same types.
/// The builtin a package hash is a known forwarder for, from the cache only.
///
/// `thinWrapperOf` needs the `PackageFn` because it may have to *detect*. This one answers from what
/// has already been detected, so `Apply` can ask before fetching the fn at all -- and a forwarder is
/// the one case where fetching it was pointless, since nothing but its hash gets used.
///
/// Resolved against this execution state's table, never a remembered `BuiltInFn`; see the note on
/// `thinWrapperCache` for what that mistake cost.
let private thinWrapperCachedFor
  (exeState : ExecutionState)
  (hash : Hash)
  : BuiltInFn voption =
  let mutable cached = ValueNone
  if not (thinWrapperCache.TryGetValue(hash, &cached)) then
    ValueNone
  else
    match cached with
    | ValueNone -> ValueNone
    | ValueSome b ->
      let mutable bi = Unchecked.defaultof<BuiltInFn>
      if exeState.fns.builtIn.TryGetValue(b, &bi) then ValueSome bi else ValueNone


let private thinWrapperOf
  (exeState : ExecutionState)
  (fn : PackageFn.PackageFn)
  : BuiltInFn voption =
  // Explicit byref, not `match ... with | true, v`, which allocates the tuple: this runs on every
  // package call.
  let mutable cached = ValueNone
  let name =
    if thinWrapperCache.TryGetValue(fn.hash, &cached) then
      cached
    else
      let found = detectThinWrapper exeState fn
      thinWrapperCache[fn.hash] <- found
      found

  match name with
  | ValueNone -> ValueNone
  | ValueSome b ->
    // Against this execution state's table, never a remembered `BuiltInFn`.
    let mutable bi = Unchecked.defaultof<BuiltInFn>
    if exeState.fns.builtIn.TryGetValue(b, &bi) then ValueSome bi else ValueNone


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
let private callPackageViaFrame
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
    // that after the first call is never taken.
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


/// Run a package-fn call whose explicit type args are resolved.
let private callPackageResolved
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (ctx : ApplyContext)
  (fn : PackageFn.PackageFn)
  (resolvedExplicitTypeArgsVT : List<ValueType>)
  : Ply<PackageOutcome> =
  // A bare forwarder to a builtin of the same signature runs that builtin here, instead of pushing a
  // frame whose whole job is to run the two instructions that would. Partial applications go the
  // ordinary way: the point is to skip the frame, and a partial application doesn't push one anyway.
  match thinWrapperOf exeState fn with
  | ValueSome biFn when
    List.isEmpty ctx.applicable.argsSoFar
    && List.isEmpty resolvedExplicitTypeArgsVT
    && ArgSeq.count ctx.args = NEList.length fn.parameters
    ->
    // Counted as a package call, since one happened. `framePushCount` is deliberately not bumped:
    // the gap between the two counters is what elision saves.
    if vm.stats.enabled then
      vm.stats.packageCallCount <- vm.stats.packageCallCount + 1L
    let call = callBuiltinResolved exeState vm currentFrame ctx biFn []
    // Builtins answer synchronously unless they do I/O, and a wrapper of one this thin rarely does.
    match Ply.trySync call with
    | ValueSome dv -> Ply(Completed dv)
    | ValueNone ->
      uply {
        let! dv = call
        return Completed dv
      }
  | _ ->
    callPackageViaFrame exeState vm currentFrame ctx fn resolvedExplicitTypeArgsVT


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
      // Hoisted out of the record expression so each piece can be bracketed separately: as one
      // expression, `lambda.frame` reports a single total with no way to attribute it.
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
      // from the same function over and over. Rebuilding it per call is nearly everything a lambda
      // application allocates, so it is memoized.
      //
      // Keyed on the expression id, holding the parent it was derived from. A single last-value slot
      // is not enough: `List.map` alternates between its own recursion and the caller's lambda, so
      // two expression ids interleave and a one-entry cache misses every time. A memo that thrashes
      // looks like a memo that does not help -- check the hit rate, not just the total.
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
    // The symbol table the call starts from. `callBuiltin` and `callPackage` take it from here and
    // do the rest -- shadowing, inference, checking, invocation -- outside this computation
    // expression, so the common case never enters the builder.
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

    // An `Int` operator called directly, taken before any of the call machinery: no `ApplyContext`,
    // no builtin-table lookup, no `ArgSeq`. `tryIntOp` further down catches the same operators
    // arriving through an elided package wrapper -- `Stdlib.Int.max` never reaches here.
    //
    // A function, not a `let mutable` here: the rest of this body has `uply` blocks in it, and a
    // mutable a continuation captures becomes a heap ref cell allocated on every `Apply`, taken
    // branch or not. Written that way first, it cost the reference workload 4% and a view build 10%.
    match tryIntOpDirect exeState registers applicable typeArgs newArgRegs with
    | ValueSome result ->
      if vm.stats.enabled then
        vm.stats.builtinCallCount <- vm.stats.builtinCallCount + 1L
      registers[putResultIn] <- result
    | ValueNone ->

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
        // `TryGetValue` rather than `Map.find`, which allocates a `Some` on every hit. F#'s Map
        // implements IDictionary, so the byref overload is available here too.
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
        // A forwarder whose builtin is already known needs neither the fn nor the package call
        // path: the fetch, `resolveTypeArgs`, `callPackage` and `callPackageResolved` all exist to
        // reach the same `callBuiltinResolved` this reaches directly. The first call to any given fn
        // still goes the long way -- that is what detects it and fills the cache.
        //
        // Same guards as the elision in `callPackageResolved`: no explicit type args, nothing
        // applied already, every parameter supplied. Signature equality is what put the fn in the
        // cache, so the builtin's parameter count is the package fn's.
        let earlyWrapper =
          if List.isEmpty typeArgs && List.isEmpty applicable.argsSoFar then
            thinWrapperCachedFor exeState pkg
          else
            ValueNone

        match earlyWrapper with
        | ValueSome biFn when NEList.length newArgRegs = List.length biFn.parameters ->
          let ctx : ApplyContext =
            { applicable = applicable
              typeArgs = typeArgs
              args = ArgSeq.ofNE registers newArgRegs
              tst = tst
              putResultIn = putResultIn
              returnPc = currentFrame.programCounter + 1 }

          let call = callBuiltinResolved exeState vm currentFrame ctx biFn []

          match Ply.trySync call with
          | ValueSome dv -> registers[putResultIn] <- dv
          | ValueNone -> outcome <- AwaitBuiltin(call, putResultIn)

        | _ ->

          let pkgFetchAlloc = allocNow vm
          // Warm cache after the first call, which for a script means all but a handful of these.
          //
          // The miss builds its own `uply` and the hit never touches the builder. A `let!` here instead
          // would sit in the loop's computation expression and cost a continuation closure on every
          // call, reached or not.
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
          // No `let mutable` spanning the bind: a mutable a continuation captures becomes a heap ref
          // cell, allocated whether or not the branch needing it is taken. Duplicating two lines is
          // cheaper than the cell.
          match Ply.trySync call with
          | ValueSome(PartiallyApplied dv)
          | ValueSome(Completed dv) -> registers[putResultIn] <- dv
          | ValueSome(PushFrame frame) -> vm.frameToPush <- ValueSome frame
          | ValueNone -> outcome <- AwaitPackage(call, putResultIn)

  recordStage vm ApplyStage.ApplyTotal applyTotalAlloc
  outcome


/// `TypeReference.toVT` over a list, without awaiting. `ValueNone` if any element needs the store.
///
/// The empty list is the overwhelmingly common case and costs nothing here.
let rec private toVTsSync
  (types : Types)
  (tst : TypeSymbolTable)
  (acc : List<ValueType>)
  (typeArgs : List<TypeReference>)
  : List<ValueType> voption =
  match typeArgs with
  | [] -> ValueSome(List.rev acc)
  | t :: rest ->
    match Ply.trySync (TypeReference.toVT types tst t) with
    | ValueSome vt -> toVTsSync types tst (vt :: acc) rest
    | ValueNone -> ValueNone


/// Build a record, an enum or a record update without entering a computation expression.
///
/// These three opcodes are async only because their builders can need the type store. That store is
/// cached, so after the first reference to a type the `Ply` they hand back is already complete, and
/// the drain can take the value straight out of it.
///
/// Returns false when the store is genuinely needed. Nothing has been written to a register in that
/// case, and the caller leaves the counter where it is so `runRareOpcode` runs the same instruction
/// properly. That does mean the builder runs twice on a miss: the discarded `Ply` finishes on its own
/// and its result is dropped. These builders only read the type store and construct a value, so a
/// repeat is wasted work rather than a second effect, and a miss happens about once per type.
let private tryBuildSync
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (registers : Dval array)
  (inst : Instruction)
  : bool =
  let tst = currentFrame.typeSymbolTable

  match inst with
  | CreateRecord(recordReg, sourceTypeName, typeArgs, fields) ->
    match toVTsSync exeState.types tst [] typeArgs with
    | ValueNone -> false
    | ValueSome typeArgs ->
      let fields =
        fields |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

      match
        Ply.trySync (
          TypeChecker.DvalCreator.record
            exeState.types
            vm.threadID
            tst
            sourceTypeName
            typeArgs
            fields
        )
      with
      | ValueSome record ->
        registers[recordReg] <- record
        true
      | ValueNone -> false

  | CreateEnum(enumReg, typeName, typeArgs, caseName, fields) ->
    match toVTsSync exeState.types tst [] typeArgs with
    | ValueNone -> false
    | ValueSome typeArgs ->
      let fields = fields |> List.map (fun valueReg -> registers[valueReg])

      match
        Ply.trySync (
          TypeChecker.DvalCreator.enum
            exeState.types
            vm.threadID
            tst
            typeName
            typeArgs
            caseName
            fields
        )
      with
      | ValueSome newEnum ->
        registers[enumReg] <- newEnum
        true
      | ValueNone -> false

  | CloneRecordWithUpdates(targetReg, originalRecordReg, fieldUpdates) ->
    // A non-record here is a runtime error, which the async path raises. Declining keeps the error
    // construction in one place rather than duplicating it.
    match registers[originalRecordReg] with
    | DRecord(sourceTypeName, resolvedTypeName, typeArgs, originalFields) ->
      let fieldUpdates =
        fieldUpdates
        |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

      match
        Ply.trySync (
          TypeChecker.DvalCreator.recordUpdate
            exeState.types
            vm.threadID
            tst
            sourceTypeName
            resolvedTypeName
            typeArgs
            originalFields
            fieldUpdates
        )
      with
      | ValueSome updated ->
        registers[targetReg] <- updated
        true
      | ValueNone -> false
    | _ -> false

  | _ ->
    // Only the three above are offered here. Anything else reaching this is a routing mistake, and
    // silently declining would turn it into a hang or a wrong answer.
    Exception.raiseInternal
      "tryBuildSync given an opcode it does not handle"
      [ "opcode", Opcode.index inst ]


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
  // No local `raiseRTE` alias: F# doesn't lift it out of the loop below, so it becomes a closure
  // over the VM, allocated on every call.
  let mutable counter = startCounter
  let mutable running = true
  // Set only if an `Apply` below has to wait for something. A struct, so carrying it costs nothing.
  let mutable pending = ApplyDone

  while running && counter < instrData.instructions.Length do
    let inst = instrData.instructions[counter]

    match inst with
    // Records, enums and record updates are built here when their type is already resolved, which
    // after the first reference to it is always. Only a genuine store miss stops the drain.
    | CreateRecord _
    | CloneRecordWithUpdates _
    | CreateEnum _ ->
      if vm.stats.enabled then
        vm.stats.instructionCount <- vm.stats.instructionCount + 1L

      let allocBefore =
        if vm.stats.enabled then
          System.GC.GetAllocatedBytesForCurrentThread()
        else
          0L

      let handled = tryBuildSync exeState vm currentFrame registers inst

      if vm.stats.enabled then
        let tag = Opcode.index inst
        if tag >= 0 && tag < vm.stats.allocByOpcode.Length then
          let delta = System.GC.GetAllocatedBytesForCurrentThread() - allocBefore
          if delta > 0L then
            vm.stats.allocByOpcode[tag] <- vm.stats.allocByOpcode[tag] + delta
          vm.stats.countByOpcode[tag] <- vm.stats.countByOpcode[tag] + 1L
          if handled then
            vm.stats.syncHitByOpcode[tag] <- vm.stats.syncHitByOpcode[tag] + 1L
          else
            vm.stats.syncMissByOpcode[tag] <- vm.stats.syncMissByOpcode[tag] + 1L

      if handled then counter <- counter + 1 else running <- false

    | LoadValue _ -> running <- false

    // `Apply` is all but a handful of the instructions that could stop this drain, and it almost
    // never has to wait. So it runs here rather than handing control to the computation expression,
    // which would build a continuation per iteration; only a genuine await stops the drain.
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
          if not (assignLetPattern registers pat dv) then
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
            // `TryGetValue`, not `Map.find`, which allocates a `Some` on every hit. This is the
            // most-executed opcode in a workbench view after `Apply` -- 7,713 reads to draw one
            // screen -- so that `Some` was 185 KB per view.
            let mutable value = Unchecked.defaultof<Dval>
            if fields.TryGetValue(fieldName, &value) then
              registers[targetReg] <- value
            else
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
/// An ordinary `while`, deliberately: inside a computation expression, Ply builds a continuation
/// for the body on every iteration. The caller enters the builder only for the cases above, which
/// are rare.
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
        Some expectedReturnType,
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
/// Hand a finished frame's result to its parent, or record it as the whole run's result.
///
/// The trickiest bookkeeping in the interpreter -- the pop, the trace records, the parent's register
/// and program counter -- and it is synchronous. Extracted so the task loop and the synchronous loop
/// beside it share one copy rather than drifting apart.
///
/// The caller runs `checkFrameReturnType` first where there is a parent; it is separate because it is
/// the one part of this that can await.
let private returnFromFrame
  (exeState : ExecutionState)
  (vm : VMState)
  (currentFrame : CallFrame)
  (resultOfFrame : Dval)
  : unit =
  match currentFrame.parent with
  | ValueSome(parentID, regOfParentToPutResultInto, pcOfParent) ->
    // Record per-package-fn timing on frame return
    if vm.stats.enabled && vm.stats.detailedTiming then
      match vm.stats.framePushTimestamps.TryGetValue(vm.currentFrameID) with
      | true, pushTs ->
        let elapsed = System.Diagnostics.Stopwatch.GetTimestamp() - pushTs
        match currentFrame.executionPoint with
        | Function(FQFnName.Package(Hash h)) -> vm.stats.recordPackageFn (h, elapsed)
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
      | PartiallyApplied dv
      | Completed dv -> registers[reg] <- dv
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

    // Only when the frame's block actually ended: either a frame was pushed or this one finished.
    // An await or a rare opcode leaves the frame part-run and comes round again, since the frame it
    // was running is still the current one.
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
        | ValueSome _ ->
          // A single `do!` at statement position, so the loop's state machine stays statically
          // compilable. `checkFrameReturnType` answers synchronously in the ordinary case.
          do! checkFrameReturnType exeState vm currentFrame resultOfFrame
          returnFromFrame exeState vm currentFrame resultOfFrame
        | ValueNone -> returnFromFrame exeState vm currentFrame resultOfFrame
  }



/// The outermost interpreter loop.
///
/// `task`, not Ply's `uply`. Ply is continuation-based and predates F# 6's resumable code, so a
/// `uply` loop allocates on every iteration in proportion to the size of its body, bind or no bind;
/// the same loop under `task` allocates nothing. This body is large and runs once per frame
/// activation, so that difference dominated the interpreter's allocation.
/// What the synchronous loop could not finish, and how the task loop should pick it up.
[<Struct>]
type private SyncOutcome =
  /// The whole run finished without ever awaiting.
  | SyncDone of result : Dval
  /// A step whose await has not been started. `handleFrameStep` takes it from here.
  | SyncBailStep of step : FrameStep
  /// A return-type check already in flight; the frame returns once it completes.
  | SyncBailReturnCheck of
    check : System.Threading.Tasks.Task<unit> *
    checkedResult : Dval


/// The interpreter loop, for as long as nothing actually awaits.
///
/// The loop below is a `task`, and a `task` that completes synchronously still allocates the `Task`
/// it returns. That is one allocation per *entry*, which is nothing for a script and a great deal
/// for a builtin folding a list: `executeApplicable` enters once per element, and the `Task` was
/// half of everything that path allocated.
///
/// Nearly every lambda a builtin applies is arithmetic, a comparison or a push, and never awaits at
/// all. So run the same loop with no builder for as long as that holds, and hand over the moment it
/// stops. The two share `runFrame` and `returnFromFrame`, which is where the real work is; what is
/// duplicated here is the dispatch around them.
let private executeSync (exeState : ExecutionState) (vm : VMState) : SyncOutcome =
  let mutable bail = ValueNone

  // `TryGetValue`, not `ContainsKey` and then the indexer: the key is a `uuid`, so that was two
  // hashes and two probes per turn of the loop for one frame.
  let mutable currentFrame = Unchecked.defaultof<CallFrame>

  while ValueOption.isNone bail
        && vm.callFrames.TryGetValue(vm.currentFrameID, &currentFrame) do
    let registers = currentFrame.registers
    let instrData = currentFrame.instrData

    vm.frameToPush <- ValueNone

    let step = runFrame exeState vm currentFrame registers instrData

    match step with
    | FrameBlockEnded -> ()
    // Rare by construction, and running one can await, so it is handed over rather than tried. The
    // step is untouched, so `handleFrameStep` does the whole of it.
    | FrameRareOpcode -> bail <- ValueSome(SyncBailStep step)
    | FrameAwaitBuiltin(call, reg) ->
      match Ply.trySync call with
      | ValueSome dv ->
        registers[reg] <- dv
        currentFrame.programCounter <- currentFrame.programCounter + 1
      | ValueNone -> bail <- ValueSome(SyncBailStep step)
    | FrameAwaitPackage(call, reg) ->
      match Ply.trySync call with
      | ValueSome outcome ->
        currentFrame.programCounter <- currentFrame.programCounter + 1
        match outcome with
        | PartiallyApplied dv
        | Completed dv -> registers[reg] <- dv
        | PushFrame frame -> pushFrame vm frame
      | ValueNone -> bail <- ValueSome(SyncBailStep step)

    if ValueOption.isNone bail && step.IsBlockEnded then
      match vm.frameToPush with
      | ValueSome newFrame ->
        vm.callFrames[newFrame.id] <- newFrame
        vm.currentFrameID <- newFrame.id
      | ValueNone ->
        let resultOfFrame = registers[instrData.resultReg]

        match currentFrame.parent with
        | ValueSome _ ->
          // A `Task`, so asked with `IsCompletedSuccessfully` rather than `Ply.trySync`. It answers
          // synchronously in the ordinary case: the awaiting one is a type that needs the store.
          let check = checkFrameReturnType exeState vm currentFrame resultOfFrame
          if check.IsCompletedSuccessfully then
            returnFromFrame exeState vm currentFrame resultOfFrame
          else
            bail <- ValueSome(SyncBailReturnCheck(check, resultOfFrame))
        | ValueNone -> returnFromFrame exeState vm currentFrame resultOfFrame

  match bail with
  | ValueSome outcome -> outcome
  | ValueNone ->
    match vm.finalResult with
    | ValueSome dv -> SyncDone dv
    | ValueNone -> Exception.raiseInternal "No finalResult found" []


let private executeInnerTask
  (exeState : ExecutionState)
  (vm : VMState)
  (resumeFrom : SyncOutcome)
  : System.Threading.Tasks.Task<Dval> =
  task {
    // No local `raiseRTE` alias: every continuation the builder makes for the loop body would
    // capture it, so it's a field in each of them.

    // Whatever `executeSync` could not finish, before the loop proper.
    match resumeFrom with
    | SyncDone _ -> ()
    | SyncBailStep step ->
      let frame = vm.callFrames[vm.currentFrameID]
      do! handleFrameStep exeState vm frame frame.registers frame.instrData step
    | SyncBailReturnCheck(check, checkedResult) ->
      let frame = vm.callFrames[vm.currentFrameID]
      do! check
      returnFromFrame exeState vm frame checkedResult

    // See `executeSync`: one lookup per turn, not two.
    let mutable currentFrame = Unchecked.defaultof<CallFrame>

    while vm.callFrames.TryGetValue(vm.currentFrameID, &currentFrame) do

      let registers = currentFrame.registers


      // Resolved once, when the frame was pushed. Looking it up here instead would mean a `let!` on
      // every iteration of this loop, and in the Ply builder's dynamic path that allocates a
      // continuation closure each time -- once per awaiting instruction, so tens of thousands of
      // them across a script.
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

let execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  match executeSync exeState vm with
  | SyncDone dv -> Ply dv
  | bailed ->

    // Unwrapped by hand rather than `uply { return! ... }`, which builds a state machine per call.
    let running = executeInnerTask exeState vm bailed
    if running.IsCompletedSuccessfully then
      Ply running.Result
    else
      // The task already started; awaiting `running` continues it. Calling `executeInner` here would
      // start a second run of the same VM.
      uply { return! running }
