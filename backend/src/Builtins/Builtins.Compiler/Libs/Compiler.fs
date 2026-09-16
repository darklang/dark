module Builtins.Compiler.Libs.Compiler

// The compiler extension's builtin surface. These call the airlifted
// CompilerLibrary in-process (no shelling out to a `dark` binary). This is the
// interim, text-fed path (plan §5): callers hand over compiler-syntax source;
// the real PT->compiler-AST bridge (plan §6) lands next and replaces the text
// hop for entities pulled from the package tree.

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module Bridge = Builtins.Compiler.Bridge

/// buildStdlib parses + compiles the ~30 bundled .dark stdlib files; it's heavy,
/// so build it once and reuse. (Retires with the bundled stdlib once the bridge
/// consumes the main-repo tree.)
let private stdlibResult : Lazy<Result<CompilerLibrary.StdlibResult, string>> =
  lazy (CompilerLibrary.buildStdlib ())

/// Compile compiler-syntax source to a native binary, in-process.
let private compileSource (source : string) : Result<byte array, string> =
  match stdlibResult.Value with
  | Error e -> Error $"buildStdlib: {e}"
  | Ok stdlib ->
    let request : CompilerLibrary.CompileRequest =
      { Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.CompileMode.FullProgram
        SourceSyntax = CompilerLibrary.InterpreterSyntax
        Source = source
        SourceFile = ""
        AllowInternal = false
        Verbosity = 0
        Options = CompilerLibrary.defaultOptions
        PassTimingRecorder = None }
    (CompilerLibrary.compile request).Result

/// Compile a pre-built compiler AST program (the PT->AST bridge path, §6),
/// skipping the text parser entirely.
let private compileAst (program : AST.Program) : Result<byte array, string> =
  match stdlibResult.Value with
  | Error e -> Error $"buildStdlib: {e}"
  | Ok stdlib ->
    let request : CompilerLibrary.CompileRequest =
      { Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.CompileMode.FullProgram
        SourceSyntax = CompilerLibrary.InterpreterSyntax
        Source = ""
        SourceFile = ""
        AllowInternal = false
        Verbosity = 0
        Options = CompilerLibrary.defaultOptions
        PassTimingRecorder = None }
    (CompilerLibrary.compileAstProgram request program).Result

let private allOk (rs : List<Result<'a, string>>) : Result<List<'a>, string> =
  (Ok [], rs)
  ||> List.fold (fun acc r ->
    match acc, r with
    | Error e, _ -> Error e
    | Ok _, Error e -> Error e
    | Ok xs, Ok x -> Ok(xs @ [ x ]))

/// Fetch a package fn's transitive call graph (PT fns only — no bridging yet,
/// since bridging needs the type env which we build afterwards).
let rec private fetchFnClosure
  (visited : Set<string>)
  (acc : List<string * PT.PackageFn.PackageFn>)
  (worklist : List<string>)
  : Ply<Result<List<string * PT.PackageFn.PackageFn>, string>> =
  uply {
    match worklist with
    | [] -> return Ok acc
    | h :: rest ->
      if Set.contains h visited then
        return! fetchFnClosure visited acc rest
      else
        let! fnOpt = LibDB.PackageManager.pt.getFn (PT.FQFnName.package h)
        match fnOpt with
        | None -> return Error $"missing dependency fn {h}"
        | Some ptFn ->
          let deps = Bridge.referencedPackageFns ptFn.body
          return! fetchFnClosure (Set.add h visited) (acc @ [ (h, ptFn) ]) (rest @ deps)
  }

/// Fetch the transitive closure of custom types referenced by the seeds.
let rec private fetchTypeClosure
  (visited : Set<string>)
  (acc : List<string * PT.PackageType.PackageType>)
  (worklist : List<string>)
  : Ply<Result<List<string * PT.PackageType.PackageType>, string>> =
  uply {
    match worklist with
    | [] -> return Ok acc
    | h :: rest ->
      if Set.contains h visited then
        return! fetchTypeClosure visited acc rest
      else
        let! tOpt = LibDB.PackageManager.pt.getType (PT.FQTypeName.package h)
        match tOpt with
        // A hash that doesn't resolve to a type (e.g. an over-collected fn hash,
        // or a genuinely absent type) is skipped rather than failing the whole
        // fn. If it was a real type the fn needs, bridgeType hard-fails on it
        // cleanly (TCustomType not in env); if it was noise, no harm.
        | None -> return! fetchTypeClosure (Set.add h visited) acc rest
        | Some pt ->
          let deps = Bridge.typeRefsInTypeDef pt
          return! fetchTypeClosure (Set.add h visited) (acc @ [ (h, pt) ]) (rest @ deps)
  }

/// Fetch the transitive closure of package constants referenced by the seeds.
let rec private fetchValueClosure
  (visited : Set<string>)
  (acc : List<string * PT.PackageValue.PackageValue>)
  (worklist : List<string>)
  : Ply<Result<List<string * PT.PackageValue.PackageValue>, string>> =
  uply {
    match worklist with
    | [] -> return Ok acc
    | h :: rest ->
      if Set.contains h visited then
        return! fetchValueClosure visited acc rest
      else
        let! vOpt = LibDB.PackageManager.pt.getValue (PT.FQValueName.package h)
        match vOpt with
        | None -> return! fetchValueClosure (Set.add h visited) acc rest
        | Some pv ->
          let deps = Bridge.valueRefsInExpr pv.body
          return! fetchValueClosure (Set.add h visited) (acc @ [ (h, pv) ]) (rest @ deps)
  }

/// A builtin's RUNTIME return type -> the marshalable compiler AST.Type it decodes
/// to, or None if it can't be recursively unmarshaled. Handles primitives and
/// Option/Result/List of marshalable types (Option/Result recognized by hash);
/// custom-enum payloads (which need the type-def env) return None.
let rec private rtToMarshalable (t : TypeReference) : AST.Type option =
  match t with
  | TString -> Some AST.TString
  | TInt64 -> Some AST.TInt64
  | TInt -> Some AST.TInt64
  | TBool -> Some AST.TBool
  | TUnit -> Some AST.TUnit
  // Sized ints marshal as decimal strings on the wire: the compiled side encodes via
  // Stdlib.<T>.toString (marshalTyped) and the daemon decodes with .NET's full-range
  // parse (wireToDval). UInt64 is round-trip-safe this way (decimal string, not the
  // Int64 wire that would overflow >=2^63). This gates UInt-arg builtins into the seam
  // (intFromUInt64, uint64ToFloat, ...); UInt *returns* also need an unmarshalTyped case.
  | TInt8 -> Some AST.TInt8
  | TInt16 -> Some AST.TInt16
  | TInt32 -> Some AST.TInt32
  | TUInt8 -> Some AST.TUInt8
  | TUInt16 -> Some AST.TUInt16
  | TUInt32 -> Some AST.TUInt32
  | TUInt64 -> Some AST.TUInt64
  // A Blob marshals as its byte values (marshalTypedSeen's TBytes / dvalToWire's
  // DBlob), so it can cross the seam in either direction. Without this case, any
  // Blob-taking builtin was unroutable — which is why stringFromBlobWithReplacement
  // (the UTF-8 DECODE, and the single largest remaining blocker at 121 fns) never
  // compiled. Routing it to the real builtin is the sound answer: its semantics are
  // Encoding.UTF8.GetString, which replaces invalid sequences with U+FFFD per maximal
  // subpart. A hand-written decoder would match on valid input, and the sampler never
  // generates invalid UTF-8 — so it would "prove" correct and be silently wrong on
  // exactly the input the "WithReplacement" in its name is about.
  | TBlob -> Some AST.TBytes
  | TList inner -> rtToMarshalable inner |> Option.map AST.TList
  // Tuples: every other layer already handled them — marshalTypedSeen encodes them,
  // wireToDvalTyped decodes them daemon-side, unmarshalTypedSeen decodes them in compiled
  // code. Only this function, which decides whether a builtin is routable AT ALL, had no
  // case, so any builtin with a tuple anywhere in its params was silently unroutable.
  // That's what blocked httpClientRequest (headersType = List<(String, String)>), the
  // single biggest category in the current histogram at 164 fns.
  | TTuple(a, b, rest) ->
    (a :: b :: rest)
    |> List.map rtToMarshalable
    |> fun bs ->
         if List.forall Option.isSome bs then Some(AST.TTuple(bs |> List.map Option.get))
         else None
  | TCustomType(nr, args) ->
    match nr.resolved with
    | Ok(FQTypeName.Package(Hash h)) ->
      if h = Bridge.optionTypeHash then
        match args with
        | [ inner ] ->
          rtToMarshalable inner
          |> Option.map (fun a -> AST.TSum("Stdlib.Option.Option", [ a ]))
        | _ -> None
      elif h = Bridge.resultTypeHash then
        match args with
        | [ a; b ] ->
          match rtToMarshalable a, rtToMarshalable b with
          | Some ab, Some bb -> Some(AST.TSum("Stdlib.Result.Result", [ ab; bb ]))
          | _ -> None
        | _ -> None
      else
        // ANY other custom type: name it by hash and let marshalTyped/wireToDvalTyped
        // dispatch on its DEFINITION (this map is built from the runtime and has no
        // type env, so it can't tell record from sum — the def can). This is what
        // lets a Json enum, a PosixError, etc. cross the seam.
        args
        |> List.map rtToMarshalable
        |> fun bs ->
             if List.forall Option.isSome bs then
               Some(AST.TSum(Bridge.nameForType h, bs |> List.map Option.get))
             else None
    | _ -> None
  | _ -> None

/// Fetch + bridge the whole graph (fns and their non-generic custom types),
/// returning the compiler TypeDefs, FunctionDefs, and the root's compiled name.
/// Effectful builtins routable through the host RPC seam, from the live runtime:
/// name -> returns-String (true) / returns-Unit (false); included only when all
/// params are String (the wire-marshalable subset in v1).
let private buildEffectfulMap
  (exeState : ExecutionState)
  : Map<string, Bridge.WireArg list * Bridge.WireRet> =
  exeState.fns.builtIn
  |> Map.toList
  |> List.choose (fun (name, bfn) ->
    let argWires =
      bfn.parameters
      |> List.map (fun p ->
        match p.typ with
        | TString -> Some Bridge.WAString
        | TInt64 -> Some Bridge.WAInt
        | TInt -> Some Bridge.WAInt
        | TBool -> Some Bridge.WABool
        | TFloat -> Some Bridge.WAFloat
        | TUnit -> Some Bridge.WAUnit
        | TChar -> Some Bridge.WAString // a Char is a single grapheme, sent as a string
        // Host-opaque types travel as their canonical string (see bridgeType).
        // Uuid round-trips cleanly (Guid parse); DateTime args are omitted until
        // wire->DateTime ISO parsing is in (return-only for now).
        | TUuid -> Some Bridge.WAString
        // Containers and custom types: marshalled with the same encoding the daemon
        // decodes (wireToDvalTyped). Previously ANY non-scalar arg made a builtin
        // unroutable, which is why altJsonFormat (arg: a Json enum) and ~300 others
        // never compiled.
        | other -> rtToMarshalable other |> Option.map Bridge.WATyped)
    let wireRet =
      match bfn.returnType with
      | TUnit -> Some Bridge.WRUnit
      | TString -> Some Bridge.WRString
      | TInt64 -> Some Bridge.WRInt
      | TInt -> Some Bridge.WRInt
      | TBool -> Some Bridge.WRBool
      | TUuid -> Some Bridge.WRString
      | TDateTime -> Some Bridge.WRString
      | other ->
        // General container returns (Option/Result/List of marshalable types); the
        // recursive unmarshaller decodes them.
        //
        // NO LONGER gated on isMarshalableType. That gate existed because
        // unmarshalTyped ended in `| _ -> src`, so an undecodable return silently
        // yielded the raw wire string AS the value — and this map is built from the
        // RUNTIME, with no type env, so it cannot itself tell whether a custom type
        // decodes. Now that the decoder is TOTAL, the bridge (which does have the defs)
        // reports a precise blocker instead, so being optimistic here is safe: the worst
        // case is `unsupported-builtin: <name>: unmarshalable-return: <type>`, not a
        // wrong value. That's what lets custom-type returns (altJsonParse ->
        // Result<Json, ParseError>) route at all.
        rtToMarshalable other |> Option.map Bridge.WRTyped
    match wireRet with
    | Some ret when not (List.exists Option.isNone argWires) ->
      Some(name.name, (argWires |> List.map Option.get, ret))
    | _ -> None)
  |> Map.ofList

// ---------------------------------------------------------------------------
// Package values as SHARED nullary fns.
//
// A package value is a CONSTANT, and the runtime already evaluates and caches it
// (package_values.rt_dval — all 417 are populated), so we can ask the RT package
// manager for its Dval and emit the value ONCE as `def __val.<hash>() : T = <lit>`,
// referenced by a call. That replaces inlining the value's bridged body at every
// EValue, which compounded multiplicatively down value chains (a value's body
// contains its dependencies' bodies) and hit 55GB on a 50-fn batch.
//
// Taking the type from the Dval is the point: PT.PackageValue carries no declared
// type, which is what blocked emitting a nullary FunctionDef (ReturnType is required).
// ---------------------------------------------------------------------------

let private typeNameHash (t : FQTypeName.FQTypeName) : Result<string, string> =
  match t with
  | FQTypeName.Package(Hash h) -> Ok h

/// A runtime ValueType -> the compiler type it denotes. `fresh` supplies a type-var
/// name for an Unknown: an EMPTY container (`let empty = []`) genuinely has no
/// element type, and guessing one would mis-type every use site. Emitting a TVar
/// instead makes the value fn GENERIC (`def __val.<h><a>() : List<a> = []`) so each
/// call site infers it — which is exactly what an empty literal should do.
let rec private valueTypeToAst (fresh : unit -> string) (vt : ValueType) : Result<AST.Type, string> =
  let valueTypeToAst = valueTypeToAst fresh
  match vt with
  | ValueType.Unknown -> Ok(AST.TVar(fresh ()))
  | ValueType.Known kt ->
    match kt with
    | KTUnit -> Ok AST.TUnit
    | KTBool -> Ok AST.TBool
    | KTInt8 -> Ok AST.TInt8
    | KTUInt8 -> Ok AST.TUInt8
    | KTInt16 -> Ok AST.TInt16
    | KTUInt16 -> Ok AST.TUInt16
    | KTInt32 -> Ok AST.TInt32
    | KTUInt32 -> Ok AST.TUInt32
    | KTInt64 -> Ok AST.TInt64
    | KTUInt64 -> Ok AST.TUInt64
    | KTInt128 -> Ok AST.TInt128
    | KTUInt128 -> Ok AST.TUInt128
    | KTInt -> Ok AST.TInt64 // Dark's Int bridges to Int64, same as PT.TInt
    | KTFloat -> Ok AST.TFloat64
    | KTChar -> Ok AST.TChar
    | KTString -> Ok AST.TString
    // Match the conventions bridgeType already set for the same types, so a package
    // VALUE of these types bridges the same way a param/return of them does. Their
    // absence here was the 2nd and 3rd biggest slices of the `unsupported-value` bucket
    // (KTBlob 25, KTUuid 14) — nothing deep, just cases nobody had needed yet.
    | KTBlob -> Ok AST.TBytes // bridgeType: PT.TBlob -> AST.TBytes
    | KTUuid -> Ok AST.TString // bridgeType: PT.TUuid -> AST.TString (canonical string)
    | KTList inner -> valueTypeToAst inner |> Result.map AST.TList
    | KTDict v -> valueTypeToAst v |> Result.map (fun v' -> AST.TDict(AST.TString, v'))
    | KTTuple(a, b, rest) ->
      (a :: b :: rest) |> List.map valueTypeToAst |> allOk |> Result.map AST.TTuple
    | KTCustomType(name, targs) ->
      typeNameHash name
      |> Result.bind (fun h ->
        targs
        |> List.map valueTypeToAst
        |> allOk
        |> Result.map (fun args ->
          // Enums are TSum, records TRecord; Option/Result map to the native names.
          if Bridge.isNativeType h then AST.TSum(Bridge.compilerTypeName h, args)
          else AST.TRecord(Bridge.nameForType h, args)))
    | other -> Error $"value-type: {other}"

/// A package value's evaluated Dval -> a compiler literal.
let rec private dvalToAst (d : Dval) : Result<AST.Expr, string> =
  match d with
  | DUnit -> Ok AST.UnitLiteral
  | DBool b -> Ok(AST.BoolLiteral b)
  // There is no Bytes literal in the AST, so rebuild the blob from its byte values via
  // the real allocator -- the same shape marshalTypedSeen uses for TBytes. Only Ephemeral
  // is reachable here (a sampled/So-far-computed blob); a Persistent one is a content
  // hash needing IO, so it stays an honest Error rather than a guess.
  | DBlob(Ephemeral eph) ->
    Ok(
      AST.Call(
        "Stdlib.Bytes.fromList",
        AST.NonEmptyList.singleton (
          AST.ListLiteral(eph.bytes |> Array.toList |> List.map (int64 >> AST.Int64Literal)))))
  // A Uuid travels as its canonical string, exactly as bridgeType (PT.TUuid -> TString)
  // and dvalToWire (DUuid g -> string g) already treat it. Keep the three in step.
  | DUuid g -> Ok(AST.StringLiteral(string g))
  | DInt8 n -> Ok(AST.Int8Literal n)
  | DUInt8 n -> Ok(AST.UInt8Literal n)
  | DInt16 n -> Ok(AST.Int16Literal n)
  | DUInt16 n -> Ok(AST.UInt16Literal n)
  | DInt32 n -> Ok(AST.Int32Literal n)
  | DUInt32 n -> Ok(AST.UInt32Literal n)
  | DInt64 n -> Ok(AST.Int64Literal n)
  | DUInt64 n -> Ok(AST.UInt64Literal n)
  | DInt128 n -> Ok(AST.Int128Literal n)
  | DUInt128 n -> Ok(AST.UInt128Literal n)
  | DInt n -> Ok(AST.Int64Literal(int64 (DarkInt.toBigInt n)))
  | DFloat f -> Ok(AST.FloatLiteral f)
  | DChar c -> Ok(AST.CharLiteral c)
  | DString s -> Ok(AST.StringLiteral s)
  | DList(_, xs) -> xs |> List.map dvalToAst |> allOk |> Result.map AST.ListLiteral
  | DTuple(a, b, rest) ->
    (a :: b :: rest) |> List.map dvalToAst |> allOk |> Result.map AST.TupleLiteral
  | DDict(_, entries) ->
    // No dict literal in the compiler AST; Dict.fromList over (key, value) tuples.
    entries
    |> Map.toList
    |> List.map (fun (k, v) ->
      dvalToAst v |> Result.map (fun v' -> AST.TupleLiteral [ AST.StringLiteral k; v' ]))
    |> allOk
    |> Result.map (fun es ->
      AST.Call("Stdlib.Dict.fromList", AST.NonEmptyList.singleton (AST.ListLiteral es)))
  | DRecord(_, rtName, _, fields) ->
    typeNameHash rtName
    |> Result.bind (fun h ->
      fields
      |> Map.toList
      |> List.mapi (fun i (n, v) -> dvalToAst v |> Result.map (fun v' -> (n, $"__dr{i}_", v')))
      |> allOk
      |> Result.map (fun fs ->
        // Bind fields to Lets ONLY for the shape that miscompiles: a record holding BOTH
        // a non-empty List and a >=2-byte Blob SIGSEGVs when both are built inline in the
        // record literal (task #26 — proven minimal; hoisting is what diagnosed it).
        //
        // Gated, because hoisting costs TYPE PROPAGATION: the record-field position pushes
        // the field's declared type in, a Let doesn't, so a hoisted field infers on its own
        // and its fresh tvar stops unifying. Measured: an ungated hoist here regressed
        // Cli.Packages.Deprecate.parseArgs ("expected (String, List<t>), got (String,
        // List<String>)"). Same tvar trap the bridge's ERecord hit.
        let hasList = fields |> Map.exists (fun _ v -> match v with DList _ -> true | _ -> false)
        let hasBlob =
          fields
          |> Map.exists (fun _ v ->
            match v with
            | DBlob(Ephemeral e) -> e.bytes.Length >= 2
            | _ -> false)
        if hasList && hasBlob then
          let rec2 = AST.RecordLiteral(Bridge.nameForType h, fs |> List.map (fun (n, vn, _) -> (n, AST.Var vn)))
          fs |> List.rev |> List.fold (fun acc (_, vn, e) -> AST.Let(vn, e, acc)) rec2
        else
          AST.RecordLiteral(Bridge.nameForType h, fs |> List.map (fun (n, _, e) -> (n, e)))))
  | DEnum(_, rtName, _, caseName, fields) ->
    typeNameHash rtName
    |> Result.bind (fun h ->
      fields
      |> List.map dvalToAst
      |> allOk
      |> Result.map (fun fs ->
        AST.Constructor(Bridge.compilerTypeName h, caseName, Bridge.tuplePayloadPublic fs)))
  | other -> Error $"value-dval: {other.GetType().Name}"

/// Fetch a package value's evaluated Dval and lower it to a shared nullary fn:
/// `def __val.<hash>(__unit: Unit) : T = <literal>`. A Unit param is how the
/// compiler spells a nullary fn (normalizeNullaryCallArgs drops it at the call
/// site), since FunctionDef.Params is a NonEmptyList.
/// Resolve every PERSISTENT blob in a Dval into an Ephemeral one, fetching its bytes.
///
/// A package value lives in the DB, so its blobs are Persistent — a content hash into
/// package_blobs — while dvalToAst can only lower Ephemeral (it's pure; resolving a hash
/// is IO). That mismatch was the whole `value-dval: DBlob` bucket: the TYPE bridged fine
/// (KTBlob -> TBytes) and then the VALUE couldn't. Materialize here, where we're already
/// in Ply and can do the lookup, and dvalToAst stays pure.
let rec private materializeBlobs (d : Dval) : Ply<Dval> =
  uply {
    match d with
    | DBlob(Persistent(h, _)) ->
      let! bytes = LibDB.RuntimeTypes.Blob.get h
      match bytes with
      | Some bs -> return LibExecution.Blob.newEphemeral bs
      | None -> return d // missing row: leave it, dvalToAst reports it honestly
    | DList(vt, xs) ->
      let mutable acc = []
      for x in xs do
        let! x' = materializeBlobs x
        acc <- acc @ [ x' ]
      return DList(vt, acc)
    | DTuple(a, b, rest) ->
      let! a' = materializeBlobs a
      let! b' = materializeBlobs b
      let mutable acc = []
      for r in rest do
        let! r' = materializeBlobs r
        acc <- acc @ [ r' ]
      return DTuple(a', b', acc)
    | DRecord(o, t, targs, fields) ->
      let mutable acc = []
      for (k, v) in Map.toList fields do
        let! v' = materializeBlobs v
        acc <- acc @ [ (k, v') ]
      return DRecord(o, t, targs, Map.ofList acc)
    | DEnum(o, t, targs, c, fields) ->
      let mutable acc = []
      for v in fields do
        let! v' = materializeBlobs v
        acc <- acc @ [ v' ]
      return DEnum(o, t, targs, c, acc)
    | other -> return other
  }

let private valueFnDef (hash : string) : Ply<Result<AST.FunctionDef, string>> =
  uply {
    let! v0 = LibDB.PackageManager.rt.getValue (FQValueName.package hash)
    let! v =
      match v0 with
      | None -> Ply None
      | Some pv ->
        uply {
          let! body = materializeBlobs pv.body
          return Some { pv with body = body }
        }
    match v with
    | None -> return Error "package value has no evaluated rt_dval"
    | Some pv ->
      let mutable counter = 0
      let fresh () =
        counter <- counter + 1
        $"vt{counter}"
      match valueTypeToAst fresh (Dval.toValueType pv.body), dvalToAst pv.body with
      | Error e, _ -> return Error e
      | _, Error e -> return Error e
      | Ok t, Ok body ->
        return
          Ok(
            { Name = Bridge.valueFnName hash
              // Any tvar we introduced for an empty container must be declared.
              TypeParams = Bridge.freeTVars t |> Set.toList
              Params = AST.NonEmptyList.singleton ("__unit", AST.TUnit)
              ReturnType = t
              Body = body } : AST.FunctionDef
          )
  }

let private buildPieces
  (effectful : Map<string, Bridge.WireArg list * Bridge.WireRet>)
  (rootHash : string)
  : Ply<Result<List<AST.TypeDef> * List<AST.FunctionDef> * string, string>> =
  uply {
    let! fnClosure = fetchFnClosure Set.empty [] [ rootHash ]
    match fnClosure with
    | Error e -> return Error e
    | Ok fns ->
      let valueSeeds =
        fns |> List.collect (fun (_, fn) -> Bridge.valueRefsInExpr fn.body) |> List.distinct
      let! valueClosure = fetchValueClosure Set.empty [] valueSeeds
      let values =
        match valueClosure with
        | Ok vs -> vs
        | Error _ -> []
      let typeSeeds =
        ((fns |> List.collect (fun (_, fn) -> Bridge.typeRefsInFn fn))
         @ (values |> List.collect (fun (_, pv) -> Bridge.typeRefsInExpr pv.body)))
        |> List.distinct
      let! typeClosure = fetchTypeClosure Set.empty [] typeSeeds
      match typeClosure with
      | Error e -> return Error e
      | Ok typesRaw ->
        // Option/Result are defined natively by the compiler stdlib and bridged to
        // those native types (Bridge.isNativeType). Drop them from the fetched
        // closure so we don't ALSO emit T_<hash> defs — that would double-define
        // Some/None/Ok/Error and make every use ambiguous.
        let types = typesRaw |> List.filter (fun (h, _) -> not (Bridge.isNativeType h))
        // The type env: non-generic record (false) / enum (true) types only.
        // Generic + alias types are omitted, so any fn using one hard-fails.
        // Records/enums emit a TypeDef and lower to a named TRecord/TSum.
        let baseEnv =
          types
          |> List.choose (fun (h, pt) ->
            match pt.declaration.definition with
            | PT.TypeDeclaration.Record _ -> Some(h, Bridge.TERecord)
            | PT.TypeDeclaration.Enum _ -> Some(h, Bridge.TESum)
            | PT.TypeDeclaration.Alias _ -> None)
          |> Map.ofList
        // Non-generic aliases inline to their (bridged) target. Resolved against
        // baseEnv, so an alias of a record/enum works; alias-of-alias is skipped
        // (its users then hard-fail cleanly on TCustomType).
        let typeEnv =
          (baseEnv, types)
          ||> List.fold (fun env (h, pt) ->
            match pt.declaration.definition with
            | PT.TypeDeclaration.Alias t when List.isEmpty pt.declaration.typeParams ->
              match Bridge.bridgeType baseEnv t with
              | Ok bt -> Map.add h (Bridge.TEAlias bt) env
              | Error _ -> env
            | _ -> env)
        let typeDefs =
          types
          |> List.filter (fun (h, _) -> Map.containsKey h baseEnv)
          |> List.map (fun (h, pt) -> Bridge.bridgeTypeDef typeEnv (Bridge.nameForType h) pt)
          |> allOk
        // Bridge each package constant's body (no params/self) into an inline
        // expression. A value can reference another value; the fetch order isn't
        // dependency order, so iterate to a fixpoint — each pass bridges any value
        // whose referenced values are now in the map — rather than dropping a
        // forward reference on a single ordered pass.
        // Each package value becomes ONE shared nullary fn, typed and literal-ised
        // from its evaluated Dval (rt_dval). No fixpoint is needed any more: a value
        // that references other values does so through their Dvals, which are already
        // evaluated, so there's nothing to order. And no inlining, so value chains
        // can't compound.
        let mutable valueDefs : List<AST.FunctionDef> = []
        let mutable valueOk : Set<string> = Set.empty
        let mutable valueErrs : List<string * string> = []
        for (h, _) in values do
          let! r = valueFnDef h
          match r with
          | Ok fd ->
            valueDefs <- valueDefs @ [ fd ]
            valueOk <- Set.add h valueOk
          | Error e -> valueErrs <- valueErrs @ [ (h, e) ]
        let valuesMap = valueOk
        let valueErrors = Map.ofList valueErrs
        // The emitted type defs, so a record/enum ARG can be marshalled into the
        // wire format (marshalTyped dispatches on the def).
        let emittedDefs =
          match typeDefs with
          | Ok tds ->
            tds
            |> List.choose (fun td ->
              match td with
              | AST.RecordDef(n, _, _) -> Some(n, td)
              | AST.SumTypeDef(n, _, _) -> Some(n, td)
              | AST.TypeAlias(n, _, _) -> Some(n, td))
            |> Map.ofList
          | Error _ -> Map.empty
        let fnDefs =
          fns
          |> List.map (fun (h, fn) ->
            Bridge.bridgeFn typeEnv emittedDefs valuesMap valueErrors effectful (Bridge.nameFor h) fn)
          |> allOk
        match typeDefs, fnDefs with
        | Error e, _ -> return Error e
        | _, Error e -> return Error e
        // The value fns go in alongside the bridged fns — one definition each,
        // shared by every reference.
        | Ok tds, Ok fds ->
          // One marshaller fn per non-generic type, so a record/enum ARG (and a
          // RECURSIVE type like Json) can cross the seam: recursion becomes a call.
          // Only the marshallers actually referenced by the bridged fns (+ whatever
          // those transitively call). Emitting one per type polluted every program.
          let seed = fds |> List.map (fun f -> Bridge.marshalCallsInExpr f.Body) |> Set.unionMany
          let marshallers = Bridge.marshalFnDefs emittedDefs seed
          // Same story in the decode direction: a builtin returning a custom type calls
          // __unmarshal.T, so emit those too -- reachable ones only, transitively closed.
          let useed = fds |> List.map (fun f -> Bridge.unmarshalCallsInExpr f.Body) |> Set.unionMany
          let unmarshallers = Bridge.unmarshalFnDefs emittedDefs useed
          return Ok(tds, marshallers @ unmarshallers @ valueDefs @ fds, Bridge.nameFor rootHash)
  }

/// Substitute type variables (used to instantiate a generic root fn's type
/// params to a concrete type so it has a monomorphic entry point).
let rec private substTVars (m : Map<string, AST.Type>) (t : AST.Type) : AST.Type =
  let s = substTVars m
  match t with
  | AST.TVar v -> Map.tryFind v m |> Option.defaultValue t
  | AST.TRecord(n, args) -> AST.TRecord(n, List.map s args)
  | AST.TSum(n, args) -> AST.TSum(n, List.map s args)
  | AST.TList inner -> AST.TList(s inner)
  | AST.TTuple ts -> AST.TTuple(List.map s ts)
  | AST.TDict(k, v) -> AST.TDict(s k, s v)
  | AST.TFunction(args, ret) -> AST.TFunction(List.map s args, s ret)
  | other -> other

/// The entry call to the root fn. A generic root is instantiated at Int64 (so
/// it monomorphizes to a concrete entry point) via TypeApp.
let private mainCall (rootFd : AST.FunctionDef) (args : List<AST.Expr>) : AST.Expr =
  let nParams = List.length rootFd.TypeParams
  if nParams = 0 then
    AST.Call(rootFd.Name, AST.NonEmptyList.fromList args)
  else
    AST.TypeApp(
      rootFd.Name,
      List.replicate nParams AST.TInt64,
      AST.NonEmptyList.fromList args)

/// Concrete param types of the root, with its generic params instantiated at
/// Int64 (matching mainCall) — so zero args can be synthesized.
let private rootParamTypes (rootFd : AST.FunctionDef) : List<AST.Type> =
  let subst = rootFd.TypeParams |> List.map (fun tp -> (tp, AST.TInt64)) |> Map.ofList
  rootFd.Params |> AST.NonEmptyList.toList |> List.map (snd >> substTVars subst)

/// Build the program (type defs + all bridged fns + the entry call) and compile.
let private compileClosure
  (typeDefs : List<AST.TypeDef>)
  (fds : List<AST.FunctionDef>)
  (mainExpr : AST.Expr)
  : Result<byte array, string> =
  let program : AST.Program =
    AST.Program(
      (typeDefs |> List.map AST.TypeDef)
      @ (fds |> List.map AST.FunctionDef)
      @ [ AST.Expression mainExpr ])
  compileAst program

/// A zero/default literal for a compiler type, so a function can be made
/// reachable (called) to check that it actually compiles, without real args.
let rec private zeroLit (t : AST.Type) : Result<AST.Expr, string> =
  match t with
  | AST.TInt64 -> Ok(AST.Int64Literal 0L)
  | AST.TInt8 -> Ok(AST.Int8Literal 0y)
  | AST.TInt16 -> Ok(AST.Int16Literal 0s)
  | AST.TInt32 -> Ok(AST.Int32Literal 0l)
  | AST.TInt128 -> Ok(AST.Int128Literal System.Int128.Zero)
  | AST.TUInt8 -> Ok(AST.UInt8Literal 0uy)
  | AST.TUInt16 -> Ok(AST.UInt16Literal 0us)
  | AST.TUInt32 -> Ok(AST.UInt32Literal 0ul)
  | AST.TUInt64 -> Ok(AST.UInt64Literal 0UL)
  | AST.TUInt128 -> Ok(AST.UInt128Literal System.UInt128.Zero)
  | AST.TBool -> Ok(AST.BoolLiteral false)
  | AST.TString -> Ok(AST.StringLiteral "")
  | AST.TFloat64 -> Ok(AST.FloatLiteral 0.0)
  | AST.TChar -> Ok(AST.CharLiteral "a")
  | AST.TUnit -> Ok AST.UnitLiteral
  | other -> Error $"no zero literal for {other}"

/// A zero value for any bridged type — including records/enums, built from the
/// bridged type defs — so compileFnCheck can make a fn taking custom-type params
/// reachable. `seen` guards against infinite recursion on recursive types.
/// Build a type-var substitution for a generic type def instantiated with `args`:
/// each declared type param maps to its positional arg, or Int64 when the type is
/// referenced with fewer args than params (the harness only needs *a* concrete
/// monomorphization to check compilability).
let private defSubst (typeParams : string list) (args : AST.Type list) : Map<string, AST.Type> =
  typeParams
  |> List.mapi (fun i tp -> (tp, List.tryItem i args |> Option.defaultValue AST.TInt64))
  |> Map.ofList

let rec private zeroValue
  (defs : Map<string, AST.TypeDef>)
  (seen : Set<string>)
  (t : AST.Type)
  : Result<AST.Expr, string> =
  match t with
  | AST.TRecord(name, args) ->
    if Set.contains name seen then Error $"recursive type {name}"
    else
      match Map.tryFind name defs with
      | Some(AST.RecordDef(_, tps, fields)) ->
        // Instantiate the generic def's type params with the reference's args, so
        // a field typed `T` becomes its concrete type before we synthesize a zero.
        let subst = defSubst tps args
        fields
        |> List.map (fun (fname, ft) ->
          zeroValue defs (Set.add name seen) (substTVars subst ft)
          |> Result.map (fun v -> (fname, v)))
        |> allOk
        |> Result.map (fun fs -> AST.RecordLiteral(name, fs))
      | _ -> Error $"no record def {name}"
  // Native Option/Result have no emitted TypeDef (the compiler stdlib defines
  // them), so synthesize their zeros directly: None for Option, Ok<zero> for
  // Result (falling back to Error<zero> if the Ok type can't be zeroed).
  | AST.TSum("Stdlib.Option.Option", _) ->
    Ok(AST.Constructor("Stdlib.Option.Option", "None", None))
  | AST.TSum("Stdlib.Result.Result", args) ->
    let okT = List.tryItem 0 args |> Option.defaultValue AST.TInt64
    let errT = List.tryItem 1 args |> Option.defaultValue AST.TString
    match zeroValue defs seen okT with
    | Ok v -> Ok(AST.Constructor("Stdlib.Result.Result", "Ok", Some v))
    | Error _ ->
      zeroValue defs seen errT
      |> Result.map (fun v -> AST.Constructor("Stdlib.Result.Result", "Error", Some v))
  | AST.TSum(name, args) ->
    if Set.contains name seen then Error $"recursive type {name}"
    else
      match Map.tryFind name defs with
      | Some(AST.SumTypeDef(_, tps, variants)) when not (List.isEmpty variants) ->
        let subst = defSubst tps args
        // Try variants in order; a recursive type (e.g. `Nil | Cons of ...`) is
        // zeroable via its non-recursive base case, so take the first that
        // terminates rather than forcing the first-declared variant.
        let zeroed =
          variants
          |> List.tryPick (fun variant ->
            match variant.Payload with
            | None -> Some(AST.Constructor(name, variant.Name, None))
            | Some pt ->
              match zeroValue defs (Set.add name seen) (substTVars subst pt) with
              | Ok pv -> Some(AST.Constructor(name, variant.Name, Some pv))
              | Error _ -> None)
        match zeroed with
        | Some c -> Ok c
        | None -> Error $"recursive type {name}"
      | _ -> Error $"no sum def {name}"
  | AST.TTuple ts ->
    ts |> List.map (zeroValue defs seen) |> allOk |> Result.map AST.TupleLiteral
  | AST.TList _ -> Ok(AST.ListLiteral [])
  // Dict.empty is nullary AND generic, so give it explicit type args (we know k/v
  // here) rather than relying on inference from a call site that has none. The Unit
  // arg is how the compiler spells a nullary call (normalizeNullaryCallArgs drops it).
  | AST.TDict(k, v) ->
    Ok(AST.TypeApp("Stdlib.Dict.empty", [ k; v ], AST.NonEmptyList.singleton AST.UnitLiteral))
  | AST.TFunction(argTypes, retType) ->
    // A function param: synthesize a dummy lambda `(a0, a1, …) => <zero of ret>`
    // that ignores its args, so a higher-order fn can be made reachable. Param
    // types are TVar so the compiler infers them at the call site.
    zeroValue defs seen retType
    |> Result.map (fun body ->
      let ps =
        argTypes
        |> List.mapi (fun i _ -> ($"__zlam{i}", AST.TVar $"__zlamt{i}"))
      match ps with
      | [] -> AST.Lambda(AST.NonEmptyList.fromList [ ("__zlam0", AST.TVar "__zlamt0") ], body)
      | _ -> AST.Lambda(AST.NonEmptyList.fromList ps, body))
  | AST.TVar _ -> zeroLit AST.TInt64 // an unmapped signature tvar: monomorphize at Int64
  | AST.TBytes ->
    // No bytes literal exists in the AST, so call the real allocator for an empty blob.
    //
    // This used to be `Stdlib.__int64_to_bytes(0)` and it NEVER WORKED: the intrinsic is
    // registered and matched under its BARE name (Stdlib.fs:139, 2_AST_to_ANF.fs:291), so
    // the qualified form resolved to nothing and every Bytes-taking fn died with "There
    // is no variable named: Stdlib.__int64_to_bytes" — 35 fns, the 7th-biggest blocker,
    // and a HARNESS bug wearing a compiler bug's clothes. (It also cast 0 into a NULL
    // Bytes pointer, which the "compile-check only, never run" comment was covering for.)
    // Bytes.create(0) is a real empty blob and is safe even if it does get run.
    Ok(AST.Call("Stdlib.Bytes.create", AST.NonEmptyList.fromList [ AST.Int64Literal 0L ]))
  | prim -> zeroLit prim

/// Compile-check one package fn by hash (bridge its call-graph, make it
/// reachable with zero-valued args, compile — don't run). Returns (ok, detail).
/// Wrapped so an unexpected crash in one fn can't abort a batch sweep.
let private checkOne (effectful : Map<string, Bridge.WireArg list * Bridge.WireRet>) (hash : string) : Ply<bool * string> =
  uply {
    try
      let! pieces = buildPieces effectful hash
      match pieces with
      | Error e -> return (false, e)
      | Ok(typeDefs, fds, rootName) ->
        match fds |> List.tryFind (fun fd -> fd.Name = rootName) with
        | None -> return (false, "root fn not bridged")
        | Some rootFd ->
          let defsByName =
            typeDefs
            |> List.choose (fun td ->
              match td with
              | AST.RecordDef(n, _, _) -> Some(n, td)
              | AST.SumTypeDef(n, _, _) -> Some(n, td)
              | AST.TypeAlias(n, _, _) -> Some(n, td))
            |> Map.ofList
          let dummy =
            rootParamTypes rootFd |> List.map (zeroValue defsByName Set.empty)
          match List.tryPick (function Error e -> Some e | Ok _ -> None) dummy with
          | Some e -> return (false, $"arg-synthesis: {e}")
          | None ->
            let args = dummy |> List.choose (function Ok x -> Some x | Error _ -> None)
            match compileClosure typeDefs fds (mainCall rootFd args) with
            | Ok binary -> return (true, $"{binary.Length} bytes")
            | Error e -> return (false, e)
    with e ->
      return (false, $"exn: {e.Message}")
  }

/// Return a (Bool ok, String detail) tuple as a Dark value.
let private outcome (ok : bool) (detail : string) : Dval =
  DTuple(DBool ok, DString detail, [])

// ---------------------------------------------------------------------------
// Runtime seam: an in-process F# daemon that services builtin-call requests
// from compiled native code and dispatches them to the REAL builtins in the
// live ExecutionState. Channel = the FIFO/file prototype (Stdlib.hostRpc).
// v1 marshals scalars only; container types are TODO. (BUILTINS-ARCHITECTURE.md)
// ---------------------------------------------------------------------------

/// Opaque-handle table: complex values (List/Dict/…) the compiler can't represent
/// natively on x64 (the finger-tree DEEP-node runtime bug) live here as real Dvals;
/// compiled code holds only an Int64 handle and routes every operation on them back
/// through the daemon to the REAL builtins. Daemon requests are serviced on one
/// thread, so a plain Dictionary is safe.
let private handleTable = System.Collections.Generic.Dictionary<int64, Dval>()
let mutable private handleCounter = 0L


let private storeHandle (d : Dval) : int64 =
  handleCounter <- handleCounter + 1L
  handleTable[handleCounter] <- d
  handleCounter

let private getHandle (id : int64) : Dval option =
  match handleTable.TryGetValue id with
  | true, d -> Some d
  | _ -> None

/// Escape a child's encoding so a raw newline only ever separates children
/// (matches Stdlib.hostRpcEscape on the compiled side); composes to any depth.
let private escWire (s : string) : string =
  s.Replace("\\", "\\\\").Replace("\n", "\\n")

/// Inverse of escWire. The daemon never needed this before: it only ever ESCAPED
/// (encoding a return), because every arg was a scalar. Custom-type args arrive
/// encoded, so they have to be decoded — left-to-right, so "\\\\n" is a literal
/// backslash followed by n, not a newline.
let private unescWire (s : string) : string =
  let sb = System.Text.StringBuilder()
  let mutable i = 0
  while i < s.Length do
    if s[i] = '\\' && i + 1 < s.Length then
      let appended =
        match s[i + 1] with
        | 'n' -> sb.Append('\n')
        | '\\' -> sb.Append('\\')
        | c -> sb.Append(c)
      appended |> ignore<System.Text.StringBuilder>
      i <- i + 2
    else
      sb.Append(s[i]) |> ignore<System.Text.StringBuilder>
      i <- i + 1
  sb.ToString()

/// Dval -> wire string. Scalars are literal; enums (Option/Result) recurse; a
/// list becomes an opaque handle id (see handleTable) since the compiler can't
/// build a native multi-element list on x64.
let rec private dvalToWire (d : Dval) : string =
  match d with
  | DEnum(_, _, _, caseName, []) -> caseName
  | DEnum(_, _, _, caseName, fields) ->
    caseName + "\n" + (fields |> List.map (dvalToWire >> escWire) |> String.concat "\n")
  // A list marshals as its escaped elements joined by "\n" (same shape as DTuple),
  // which is exactly what Bridge.unmarshalTyped's TList decoder expects: split on
  // "\n", unescape + decode each, empty string -> []. It used to store an opaque
  // HANDLE here (a leftover from the handle-model detour, which the finger-tree
  // misdiagnosis motivated and native lists made unnecessary). Since TList
  // marshalling was re-enabled, that silently corrupted EVERY effectful builtin
  // returning a List: the daemon sent the handle id and the decoder read it back
  // as a one-element list, so `Directory.list` returned ["1"] instead of the
  // actual entries. Found by the differential run-sweep, not by a compile check.
  // The __list* handle primitives call storeHandle directly, so they're unaffected.
  | DList(_, xs) -> xs |> List.map (dvalToWire >> escWire) |> String.concat "\n"
  | DTuple(a, b, rest) ->
    (a :: b :: rest) |> List.map (dvalToWire >> escWire) |> String.concat "\n"
  // Records had NO case here and fell through to "?unmarshalable:DRecord", so any
  // record-returning fn was unprovable. Fields go in NAME order (Map.toList is
  // sorted) -- Bridge.marshalTyped sorts to match, and the two encodings must stay
  // byte-identical or every record comparison is a false DIFF.
  | DRecord(_, _, _, fields) ->
    fields |> Map.toList |> List.map (snd >> dvalToWire >> escWire) |> String.concat "\n"
  | DString s -> s
  | DInt n -> string (DarkInt.toBigInt n)
  | DInt8 n -> string n
  | DInt16 n -> string n
  | DInt32 n -> string n
  | DInt64 n -> string n
  | DInt128 n -> string n
  | DUInt8 n -> string n
  | DUInt16 n -> string n
  | DUInt32 n -> string n
  | DUInt64 n -> string n
  | DUInt128 n -> string n
  | DBool b -> if b then "true" else "false"
  | DUnit -> "unit"
  // A Float travels as its IEEE-754 bit pattern in two 32-bit halves, "hi:lo" -- the
  // mirror of Bridge's TFloat64 / WAFloat encoders (see there for why halves).
  //
  // `string f` rendered 5.0 as "5" against the compiler's "5.0" and reported
  // Stdlib.Int.toFloat as a DIFF when both engines held the same value. Dark's own
  // floatToString isn't the answer either: it is G12-lossy (so distinct floats render
  // ALIKE and a real diff could hide behind the rendering), and it appends ".0" to
  // exponent form, emitting the malformed "1e+13.0". Bits are exact and total.
  | DFloat f ->
    let b = System.BitConverter.DoubleToUInt64Bits f
    string (b >>> 32) + ":" + string (b &&& 0xFFFFFFFFUL)
  | DChar c -> c
  // A Blob marshals as its byte VALUES, one per line -- the same shape as a
  // List<Int64>, which is exactly what the compiled side emits via
  // Stdlib.Bytes.toList. Empty blob -> "" -> [], matching the DList encoding.
  //
  // Only Ephemeral is encodable here: a Persistent blob is a content hash that
  // resolves through state.blobs, which is IO, and dvalToWire is pure. Persistent
  // therefore falls through to the ?unmarshalable marker below and is reported as
  // `unprovable` rather than guessed at.
  | DBlob(Ephemeral eph) ->
    eph.bytes |> Array.toList |> List.map (string >> escWire) |> String.concat "\n"
  // Host-opaque types -> canonical string (see Bridge.bridgeType for soundness).
  | DUuid g -> string g
  | DDateTime dt -> LibExecution.DarkDateTime.toIsoString dt
  | other -> $"?unmarshalable:{other.GetType().Name}"

/// wire string -> scalar Dval, per the builtin param's declared type.
let private wireToDval (t : TypeReference) (s : string) : Dval =
  match t with
  | TString -> DString s
  | TInt64 -> DInt64(int64 s)
  // Dark's TInt (arbitrary-precision) is NOT TInt64, and buildEffectfulMap happily
  // routes a TInt param (as WAInt) -- but this fell through to `| _ -> DString s`,
  // so a TInt-taking builtin silently received its numbers as STRINGS. intRandom(5,5)
  // returned a fixed 3744625903238887673 instead of 5, whatever the bounds. Same
  // shape as the DList-as-handle bug: the builtin runs, returns garbage, and only a
  // value comparison catches it -- a compile check never would.
  | TInt -> DInt(DarkInt.ofBigInt (System.Numerics.BigInteger.Parse s))
  | TInt8 -> DInt8(sbyte s)
  | TInt16 -> DInt16(int16 s)
  | TInt32 -> DInt32(int32 s)
  | TInt128 -> DInt128(System.Int128.Parse s)
  | TUInt8 -> DUInt8(byte s)
  | TUInt16 -> DUInt16(uint16 s)
  | TUInt32 -> DUInt32(uint32 s)
  | TUInt64 -> DUInt64(uint64 s)
  | TUInt128 -> DUInt128(System.UInt128.Parse s)
  | TBool -> DBool(s = "true")
  | TUnit -> DUnit
  // Byte values, one per line -- the inverse of dvalToWire's DBlob case. Always
  // Ephemeral: a Persistent blob is a content hash in the package store, and a value
  // arriving over the wire has no such identity.
  | TBlob ->
    let bytes =
      if s = "" then [||] else s.Split('\n') |> Array.map System.Byte.Parse
    LibExecution.Blob.newEphemeral bytes
  // "hi:lo" bit halves -- the inverse of Bridge's WAFloat encoder.
  | TFloat ->
    match s.Split(':') with
    | [| hi; lo |] ->
      let b = (System.UInt64.Parse hi <<< 32) ||| System.UInt64.Parse lo
      DFloat(System.BitConverter.UInt64BitsToDouble b)
    | _ -> DFloat(float s) // pre-bits wire; shouldn't happen, but don't invent a value
  | TChar -> DChar s
  | TUuid -> DUuid(System.Guid.Parse s)
  // A list/dict arg arrives as an opaque handle id -> resolve to the real Dval.
  | TList _
  | TDict _ ->
    match System.Int64.TryParse s with
    | true, id ->
      match getHandle id with
      | Some d -> d
      | None -> DString s
    | _ -> DString s
  | _ -> DString s

/// wire string -> Dval for ANY marshalable type, including records and enums — the
/// daemon-side mirror of Bridge.marshalTyped, and the inverse of dvalToWire.
///
/// This is what lets compiled code pass a CUSTOM TYPE into a builtin. Until now the
/// seam only carried scalars in the arg direction, which is why `altJsonFormat`
/// (whose arg is a Json enum), cliExecute, fileRead and ~300 others couldn't route at
/// all. Async because it fetches type definitions.
///
/// The encoding must match dvalToWire exactly: fields/elements escaped and joined by
/// "\n", records in NAME order, enums as `Case` or `Case\n<escaped fields>`.
let rec private wireToDvalTyped (t : TypeReference) (s : string) : Ply<Dval> =
  uply {
    let split (str : string) = if str = "" then [] else str.Split('\n') |> Array.toList
    match t with
    | TList inner ->
      let mutable acc = []
      for part in split s do
        let! v = wireToDvalTyped inner (unescWire part)
        acc <- acc @ [ v ]
      let vt = acc |> List.tryHead |> Option.map Dval.toValueType |> Option.defaultValue ValueType.Unknown
      return DList(vt, acc)
    | TTuple(a, b, rest) ->
      let parts = split s
      let ts = a :: b :: rest
      let mutable acc = []
      for (ty, part) in List.zip (ts |> List.truncate (List.length parts)) (parts |> List.truncate (List.length ts)) do
        let! v = wireToDvalTyped ty (unescWire part)
        acc <- acc @ [ v ]
      match acc with
      | x :: y :: r -> return DTuple(x, y, r)
      | _ -> return DString s
    | TCustomType(nr, targs) ->
      match nr.resolved with
      | Error _ -> return DString s
      | Ok(FQTypeName.Package(Hash h)) ->
        // Option/Result are the compiler's native sums; they encode by case name.
        if h = Bridge.optionTypeHash then
          match targs with
          | [ inner ] ->
            if s = "None" then
              return DEnum(FQTypeName.Package(Hash h), FQTypeName.Package(Hash h), [], "None", [])
            else
              let payload = if s.StartsWith "Some\n" then s.Substring 5 else ""
              let! v = wireToDvalTyped inner (unescWire payload)
              return DEnum(FQTypeName.Package(Hash h), FQTypeName.Package(Hash h), [], "Some", [ v ])
          | _ -> return DString s
        elif h = Bridge.resultTypeHash then
          match targs with
          | [ okT; errT ] ->
            let isOk = s.StartsWith "Ok\n"
            let payload = if isOk then s.Substring 3 else (if s.StartsWith "Error\n" then s.Substring 6 else "")
            let! v = wireToDvalTyped (if isOk then okT else errT) (unescWire payload)
            return
              DEnum(
                FQTypeName.Package(Hash h),
                FQTypeName.Package(Hash h),
                [],
                (if isOk then "Ok" else "Error"),
                [ v ]
              )
          | _ -> return DString s
        else
          let! tOpt = LibDB.PackageManager.rt.getType (FQTypeName.package h)
          match tOpt with
          | None -> return DString s
          | Some pt ->
            let name = FQTypeName.Package(Hash h)
            match pt.declaration.definition with
            | TypeDeclaration.Alias inner -> return! wireToDvalTyped inner s
            | TypeDeclaration.Record fields ->
              // NAME order — dvalToWire emits a DvalMap, which iterates sorted.
              let fs = NEList.toList fields |> List.sortBy (fun f -> f.name)
              let parts = split s
              let mutable acc = []
              for (f, part) in List.zip (fs |> List.truncate (List.length parts)) (parts |> List.truncate (List.length fs)) do
                let! v = wireToDvalTyped f.typ (unescWire part)
                acc <- acc @ [ (f.name, v) ]
              return DRecord(name, name, [], Map.ofList acc)
            | TypeDeclaration.Enum cases ->
              let lines = split s
              match lines with
              | [] -> return DString s
              | caseName :: fieldParts ->
                match NEList.toList cases |> List.tryFind (fun c -> c.name = caseName) with
                | None -> return DString s
                | Some c ->
                  let mutable acc = []
                  for (ft, part) in
                    List.zip
                      (c.fields |> List.truncate (List.length fieldParts))
                      (fieldParts |> List.truncate (List.length c.fields)) do
                    let! v = wireToDvalTyped ft (unescWire part)
                    acc <- acc @ [ v ]
                  return DEnum(name, name, [], caseName, acc)
    | scalar -> return wireToDval scalar s
  }

/// Dispatch one request ("name\narg1\narg2…") to the real builtin, return the
/// wire response.
let private dispatchBuiltin
  (exeState : ExecutionState)
  (vm : VMState)
  (request : string)
  : Ply<string> =
  uply {
    let parts = request.Split('\n')
    let name = parts[0]
    // Args arrive ESCAPED (see Bridge's request construction) so that an arg containing
    // a newline survives this split instead of masquerading as extra args. Unescape is
    // a no-op for anything without a backslash or newline, which is why the hand-built
    // self-test requests still round-trip unchanged.
    let wireArgs = parts[1..] |> Array.toList |> List.map unescWire
    // Handle-based list primitives: build/walk a real Dval list held in the daemon,
    // so compiled code can work with multi-element lists (which the native x64
    // finger-tree can't) without ever constructing one. Element payloads are
    // String for now (the common case); typed elements come later.
    let listOf (hStr : string) : List<Dval> option =
      match System.Int64.TryParse hStr with
      | true, id ->
        match getHandle id with
        | Some(DList(_, xs)) -> Some xs
        | _ -> None
      | _ -> None
    let tryPrimitive () : string option =
      match name, wireArgs with
      | "__listEmpty", _ -> Some(string (storeHandle (DList(ValueType.Unknown, []))))
      | "__listConsStr", [ h; elem ] ->
        listOf h
        |> Option.map (fun xs ->
          string (storeHandle (DList(ValueType.Unknown, DString elem :: xs))))
      // Typed cons: prepend a wire element decoded per its type tag, so lists of
      // Int/Bool/Float (not just String) build correctly via handles.
      | "__listCons", [ h; tag; elem ] ->
        listOf h
        |> Option.map (fun xs ->
          let dv =
            match tag with
            | "i" -> DInt64(int64 elem)
            | "b" -> DBool(elem = "true")
            | "f" -> DFloat(float elem)
            | _ -> DString elem
          string (storeHandle (DList(ValueType.Unknown, dv :: xs))))
      | "__listLen", [ h ] -> listOf h |> Option.map (List.length >> string)
      | "__listIsEmpty", [ h ] ->
        listOf h |> Option.map (fun xs -> if List.isEmpty xs then "true" else "false")
      | "__listHeadStr", [ h ] ->
        listOf h
        |> Option.map (fun xs ->
          match xs with
          | DString s :: _ -> s
          | d :: _ -> dvalToWire d
          | [] -> "")
      | "__listTail", [ h ] ->
        listOf h
        |> Option.map (fun xs ->
          let t =
            match xs with
            | _ :: t -> t
            | [] -> []
          string (storeHandle (DList(ValueType.Unknown, t))))
      | _ -> None
    match tryPrimitive () with
    | Some resp -> return resp
    | None ->
    match Map.tryFind (FQFnName.builtin name 0) exeState.fns.builtIn with
    | None -> return $"ERR:no-builtin:{name}"
    | Some bfn ->
      let n = min bfn.parameters.Length wireArgs.Length
      // wireToDvalTyped, not wireToDval: an arg can now be a container or a custom
      // type (see WATyped), which needs the recursive decoder + type defs. The old
      // scalar-only path silently produced `DString s` for anything else — that is
      // exactly how every TInt arg became a string and intRandom returned garbage.
      let mutable dvalArgs = []
      for (p : BuiltInParam, a : string) in
        List.zip (List.truncate n bfn.parameters) (List.truncate n wireArgs) do
        let! v = wireToDvalTyped p.typ a
        dvalArgs <- dvalArgs @ [ v ]
      // A builtin that raises (e.g. a capability check, a bad arg) must still
      // produce a response, or the compiled program polls the response file
      // forever. Return an error marker instead of letting it propagate.
      try
        let! result = bfn.fn (exeState, vm, [], dvalArgs)
        return dvalToWire result
      with e ->
        return $"ERR:exn:{e.Message}"
  }

let private rpcReqFifo = "/tmp/dark-rpc-req"
let private rpcRespFile = "/tmp/dark-rpc-resp"
let private rpcShutdown = "__DARK_RPC_SHUTDOWN__"

/// Set up the channel and service ONE builtin request in a background task
/// (used by the daemon self-test).
let private serveOneRequest (exeState : ExecutionState) (vm : VMState) : System.Threading.Tasks.Task =
  for f in [ rpcRespFile; rpcReqFifo ] do
    if System.IO.File.Exists f then System.IO.File.Delete f
  let psi = System.Diagnostics.ProcessStartInfo("mkfifo", rpcReqFifo)
  psi.UseShellExecute <- false
  (System.Diagnostics.Process.Start psi).WaitForExit()
  System.Threading.Tasks.Task.Run(fun () ->
    let req = System.IO.File.ReadAllText rpcReqFifo
    let resp = (dispatchBuiltin exeState vm req |> Ply.toTask).Result
    System.IO.File.WriteAllText(rpcRespFile + ".tmp", resp)
    System.IO.File.Move(rpcRespFile + ".tmp", rpcRespFile, true))

/// Start a looping daemon that services builtin requests until shutdown. Each
/// request: read from the FIFO (blocks), dispatch, write the response file (the
/// compiled hostRpc deletes it after reading, so the next poll waits for a fresh
/// one). Returns the task + a shutdown fn (unblocks the FIFO read with a sentinel).
let private startDaemon
  (exeState : ExecutionState)
  (vm : VMState)
  : System.Threading.Tasks.Task * (unit -> unit) =
  for f in [ rpcRespFile; rpcReqFifo ] do
    if System.IO.File.Exists f then System.IO.File.Delete f
  let psi = System.Diagnostics.ProcessStartInfo("mkfifo", rpcReqFifo)
  psi.UseShellExecute <- false
  (System.Diagnostics.Process.Start psi).WaitForExit()
  let task =
    System.Threading.Tasks.Task.Run(fun () ->
      let mutable running = true
      while running do
        let req = System.IO.File.ReadAllText rpcReqFifo // blocks until a writer
        if req = rpcShutdown || req = "" then
          running <- false
        else
          let resp = (dispatchBuiltin exeState vm req |> Ply.toTask).Result
          System.IO.File.WriteAllText(rpcRespFile + ".tmp", resp)
          System.IO.File.Move(rpcRespFile + ".tmp", rpcRespFile, true))
  let shutdown () =
    try
      System.IO.File.WriteAllText(rpcReqFifo, rpcShutdown)
    with _ -> ()
  (task, shutdown)

/// Compile a fn by hash, make it reachable with zero args, RUN it (daemon up, so
/// effectful builtins are serviced) with a timeout, and classify the outcome:
/// "cf|<blocker>" compile-fail, "hang" (timed out — e.g. multi-element list),
/// "crash|<code>", or "ran|<stdout>". This is the runnable-vs-compilable measure.
/// sampleArg, extended to CUSTOM TYPES by reading their definitions. 191 of the 222
/// fns the harness couldn't check took a record or enum, so this is the difference
/// between proving a fifth of the tree and proving most of it. Records get every
/// field sampled; enums take their first non-recursive case (like zeroValue, so a
/// recursive type terminates via its base case). `seen` guards recursion.
let rec private sampleArg (t : PT.TypeReference) : Option<Dval> =
  match t with
  | PT.TUnit -> Some DUnit
  | PT.TBool -> Some(DBool true)
  // A Blob sample: without this every Blob-taking fn was `noargs` — compiled but
  // unprovable. Non-ASCII on purpose ("hé" = 68 c3 a9): a pure-ASCII sample can't tell a
  // byte-correct implementation from a char-based one, which is exactly how the
  // String.length / toUppercase routing bugs hid. Short, because the interpreter runs it
  // twice per check.
  | PT.TBlob -> Some(LibExecution.Blob.newEphemeral(System.Text.Encoding.UTF8.GetBytes "hé"))
  // Small on purpose: a sample arg must be cheap as well as realistic. 42 hung the
  // whole sweep on the first exponential fn it met (fib(42) interpreted is ~2000x
  // fib(27), which already takes 21s), and every chunk timed out with zero output.
  | PT.TInt64 -> Some(DInt64 5L)
  | PT.TInt8 -> Some(DInt8 7y)
  | PT.TInt16 -> Some(DInt16 7s)
  | PT.TInt32 -> Some(DInt32 7l)
  | PT.TUInt8 -> Some(DUInt8 7uy)
  | PT.TUInt16 -> Some(DUInt16 7us)
  | PT.TUInt32 -> Some(DUInt32 7ul)
  | PT.TUInt64 -> Some(DUInt64 7UL)
  | PT.TInt -> Some(DInt(DarkInt.ofBigInt (bigint 5)))
  | PT.TFloat -> Some(DFloat 2.5)
  | PT.TChar -> Some(DChar "a")
  | PT.TString -> Some(DString "hello")
  | PT.TVariable _ -> Some(DInt64 5L) // monomorphize a type param at Int64
  | PT.TList inner ->
    sampleArg inner
    |> Option.map (fun v -> DList(Dval.toValueType v, [ v; v; v ]))
  | PT.TTuple(a, b, rest) ->
    match sampleArg a, sampleArg b, rest |> List.map sampleArg |> List.fold (fun acc o -> match acc, o with Some xs, Some x -> Some(xs @ [ x ]) | _ -> None) (Some []) with
    | Some a', Some b', Some rest' -> Some(DTuple(a', b', rest'))
    | _ -> None
  | _ -> None

/// PROVE that a compiled fn agrees with the interpreter, for real arguments.
///
/// The old differential compared STDOUT, which the compiler renders as empty for
/// records/nested containers — so most fns ran but could never be checked (434 of
/// them). Instead, make both sides emit the same canonical wire encoding and compare
/// THAT: the interpreter's Dval through dvalToWire, the compiled program through
/// Bridge.marshalTyped (its mirror). Same bytes on both sides or it's a real diff.
///
/// Returns "match", "DIFF|c=…|i=…", or a reason it couldn't be proven either way.
/// Substitute bound type variables. A generic custom type's field/case types are
/// written in terms of the type's own params (`Some('a)`); to sample `Option<String>`
/// we must replace `'a` with `String` before descending, or the sampled value has the
/// wrong element type and the harness's call fails to type-check (the whole
/// `Expected Option<String>, got Option<Int64>` bucket, ~20% of the known-good set).
let rec private substituteType
  (subst : Map<string, PT.TypeReference>)
  (t : PT.TypeReference)
  : PT.TypeReference =
  if Map.isEmpty subst then t
  else
    match t with
    | PT.TVariable name ->
      match Map.tryFind name subst with
      | Some replacement -> replacement
      | None -> t
    | PT.TList inner -> PT.TList(substituteType subst inner)
    | PT.TStream inner -> PT.TStream(substituteType subst inner)
    | PT.TDict inner -> PT.TDict(substituteType subst inner)
    | PT.TDB inner -> PT.TDB(substituteType subst inner)
    | PT.TTuple(a, b, rest) ->
      PT.TTuple(substituteType subst a, substituteType subst b, List.map (substituteType subst) rest)
    | PT.TCustomType(nr, targs) -> PT.TCustomType(nr, List.map (substituteType subst) targs)
    | PT.TFn(args, ret) -> PT.TFn(NEList.map (substituteType subst) args, substituteType subst ret)
    | other -> other

/// Total sampled-node budget + depth cap. A record whose fields are records (fan-out N)
/// nesting records explodes as N^depth, and the interpreter runs the arg twice per proof,
/// so wide nested-record fns produce args that make the proof crawl. Bounds keep sampled
/// values reasonable; genuinely slow-to-EXECUTE fns are handled by the sweep's per-fn
/// timeout (they drop as harness-drop, they don't hang the sweep) — no interpreter
/// cancellation needed.
let private maxSampleDepth = 5
let private sampleNodeBudget = 256

let rec private sampleArgDeepAt
  (budget : int ref)
  (depth : int)
  (seen : Set<string>)
  (subst : Map<string, PT.TypeReference>)
  (t : PT.TypeReference)
  : Ply<Option<Dval>> =
  uply {
    // Resolve any type vars bound by an enclosing generic type before matching.
    let t = substituteType subst t
    if depth > maxSampleDepth || budget.Value <= 0 then return None
    else
    budget.Value <- budget.Value - 1
    match t with
    | PT.TList inner ->
      let! v = sampleArgDeepAt budget (depth + 1) seen subst inner
      // 3 elements at the top to catch multi-element bugs (e.g. #26), 1 when nested
      // so the sample size stays bounded instead of growing as N^depth.
      let n = if depth = 0 then 3 else 1
      return v |> Option.map (fun v -> DList(Dval.toValueType v, List.replicate n v))
    | PT.TTuple(a, b, rest) ->
      let! a' = sampleArgDeepAt budget (depth + 1) seen subst a
      let! b' = sampleArgDeepAt budget (depth + 1) seen subst b
      let mutable restVs = []
      let mutable ok = true
      for r in rest do
        let! rv = sampleArgDeepAt budget (depth + 1) seen subst r
        match rv with
        | Some v -> restVs <- restVs @ [ v ]
        | None -> ok <- false
      match a', b', ok with
      | Some a'', Some b'', true -> return Some(DTuple(a'', b'', restVs))
      | _ -> return None
    | PT.TCustomType(nr, targs) ->
      match nr.resolved with
      | Error _ -> return None
      | Ok resolved ->
        match resolved.name with
        | PT.FQTypeName.Package(PT.Hash h) ->
          if Set.contains h seen then
            return None // recursive: let the caller fall back
          else
            let! tOpt = LibDB.PackageManager.pt.getType (PT.FQTypeName.package h)
            match tOpt with
            | None -> return None
            | Some pt ->
              let seen' = Set.add h seen
              let rtName = FQTypeName.Package(Hash h)
              // Bind this type's params to the (already-substituted) type args. The
              // inner definition uses its OWN param names, a fresh scope — so field/case
              // types below are sampled under subst', not the outer subst.
              let tps = pt.declaration.typeParams
              let n = min (List.length tps) (List.length targs)
              let subst' =
                List.zip (List.truncate n tps) (List.truncate n targs) |> Map.ofList
              match pt.declaration.definition with
              // Alias is transparent — don't spend a depth level on it.
              | PT.TypeDeclaration.Alias inner -> return! sampleArgDeepAt budget depth seen' subst' inner
              | PT.TypeDeclaration.Record fields ->
                let fs = NEList.toList fields
                let mutable acc = []
                let mutable ok = true
                for (f : PT.TypeDeclaration.RecordField) in fs do
                  let! v = sampleArgDeepAt budget (depth + 1) seen' subst' f.typ
                  match v with
                  | Some v' -> acc <- acc @ [ (f.name, v') ]
                  | None -> ok <- false
                if ok then return Some(DRecord(rtName, rtName, [], Map.ofList acc))
                else return None
              | PT.TypeDeclaration.Enum cases ->
                // First case whose payload we can sample — a recursive enum
                // terminates through its non-recursive base case.
                let mutable result = None
                for (c : PT.TypeDeclaration.EnumCase) in NEList.toList cases do
                  if Option.isNone result then
                    let mutable acc = []
                    let mutable ok = true
                    for (f : PT.TypeDeclaration.EnumField) in c.fields do
                      let! v = sampleArgDeepAt budget (depth + 1) seen' subst' f.typ
                      match v with
                      | Some v' -> acc <- acc @ [ v' ]
                      | None -> ok <- false
                    if ok then result <- Some(DEnum(rtName, rtName, [], c.name, acc))
                return result
    | scalar -> return sampleArg scalar
  }

/// Sample a realistic value for a type, descending into custom types and binding
/// generic type args. Depth- and node-budget-bounded so nested/wide records don't
/// build an arg that makes the proof crawl.
let private sampleArgDeep
  (seen : Set<string>)
  (t : PT.TypeReference)
  : Ply<Option<Dval>> =
  sampleArgDeepAt (ref sampleNodeBudget) 0 seen Map.empty t

/// Bind a generic fn's type params by matching its DECLARED param types against the
/// types of the actual arguments. `List.head` declares `List<'a> -> Option<'a>`, so
/// without this its return type still mentions 'a and can't be serialized (nor can a
/// concrete entry point be emitted). Structural, first-binding-wins — the args are
/// real values, so there's nothing to solve.
let rec private unifyType
  (declared : AST.Type)
  (actual : AST.Type)
  (acc : Map<string, AST.Type>)
  : Map<string, AST.Type> =
  let both xs ys m =
    if List.length xs = List.length ys then List.fold2 (fun m x y -> unifyType x y m) m xs ys
    else m
  match declared, actual with
  | AST.TVar v, _ -> if Map.containsKey v acc then acc else Map.add v actual acc
  | AST.TList a, AST.TList b -> unifyType a b acc
  | AST.TTuple xs, AST.TTuple ys -> both xs ys acc
  | AST.TSum(_, xs), AST.TSum(_, ys) -> both xs ys acc
  | AST.TRecord(_, xs), AST.TRecord(_, ys) -> both xs ys acc
  | AST.TDict(k1, v1), AST.TDict(k2, v2) -> unifyType v1 v2 (unifyType k1 k2 acc)
  | AST.TFunction(pa, ra), AST.TFunction(pb, rb) -> unifyType ra rb (both pa pb acc)
  | _ -> acc

/// A REALISTIC sample argument for a declared param type — 42, "hello", [1;2;3] —
/// not the zero/empty values the compile-check synthesizes. Zeros make fns take
/// degenerate paths (div-by-zero, unwrap-of-None, empty-list early returns), which is
/// why the old sweep produced so many crashes and interpreter errors that proved
/// nothing. A type var monomorphizes at Int64. Returns None for types we can't
/// sensibly sample (custom types, Dict, fns), so the caller reports "no sample args"
/// instead of pretending.
let private equivOne
  (exeState : ExecutionState)
  (vm : VMState)
  (effectful : Map<string, Bridge.WireArg list * Bridge.WireRet>)
  (timeoutMs : int)
  (hash : string)
  (args : List<Dval>)
  : Ply<string> =
  uply {
    try
      let! pieces = buildPieces effectful hash
      match pieces with
      | Error e -> return "cf|" + e
      | Ok(typeDefs, fds, rootName) ->
        match fds |> List.tryFind (fun fd -> fd.Name = rootName) with
        | None -> return "cf|root fn not bridged"
        | Some rootFd ->
          // The args come in as real Dvals, so reuse the value-literal machinery to
          // hand the compiled side EXACTLY the same inputs the interpreter gets.
          let argLits = args |> List.map dvalToAst
          match argLits |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
          | Some e -> return "skip|arg-literal: " + e
          | None ->
            let argExprs = argLits |> List.map (function Ok x -> x | Error _ -> AST.UnitLiteral)
            // Instantiate the root's type params from the ACTUAL argument types, so a
            // generic fn gets a concrete entry point AND a serializable return type
            // (mainCall's default is to guess Int64, which is wrong for real args).
            let mutable c = 0
            let fresh () =
              c <- c + 1
              $"at{c}"
            let actualTypes =
              args |> List.map (fun a -> valueTypeToAst fresh (Dval.toValueType a))
            let declaredTypes = rootFd.Params |> AST.NonEmptyList.toList |> List.map snd
            // zip, not List.zip: arity can legitimately differ (a Dark nullary fn
            // declares one Unit param but is called with no args), and List.zip
            // throws "the lists had different lengths" on that.
            let binding =
              (Map.empty,
               List.zip
                 (declaredTypes |> List.truncate (List.length actualTypes))
                 (actualTypes
                  |> List.truncate (List.length declaredTypes)
                  |> List.map (Result.defaultValue (AST.TVar "?"))))
              ||> List.fold (fun m (d, a) -> unifyType d a m)
            let retType = substTVars binding rootFd.ReturnType
            // A nullary call is spelled with a Unit arg (normalizeNullaryCallArgs
            // drops it); NonEmptyList.fromList [] would just throw.
            let callArgs =
              match argExprs with
              | [] -> [ AST.UnitLiteral ]
              | xs -> xs
            let entry =
              if List.isEmpty rootFd.TypeParams then
                AST.Call(rootFd.Name, AST.NonEmptyList.fromList callArgs)
              else
                let tas =
                  rootFd.TypeParams
                  |> List.map (fun tp -> Map.tryFind tp binding |> Option.defaultValue AST.TInt64)
                AST.TypeApp(rootFd.Name, tas, AST.NonEmptyList.fromList callArgs)
            let defsByName =
              typeDefs
              |> List.choose (fun td ->
                match td with
                | AST.RecordDef(n, _, _) -> Some(n, td)
                | AST.SumTypeDef(n, _, _) -> Some(n, td)
                | AST.TypeAlias(n, _, _) -> Some(n, td))
              |> Map.ofList
            match Bridge.serializableReason defsByName retType with
            // Be explicit: this fn is NOT proven, rather than silently "ran" — and say
            // WHICH part of the type we can't encode, not just the whole type.
            | Error why -> return $"unprovable|{why}"
            | Ok() ->
              match Bridge.marshalTyped defsByName 0 retType entry with
              | Error e -> return "unprovable|" + e
              | Ok marshalled ->
                // buildPieces seeds marshaller reachability from the bridged fn
                // BODIES, but this return-marshalling expr is built afterwards and
                // calls __marshal.X for the return type -- so those defs were never
                // emitted and the program failed with "no variable named
                // __marshal.T.<hash>". That masqueraded as a compile failure for fns
                // that do compile (the coverage sweep counts them True), so the
                // harness under-reported what it could prove. Close over `marshalled`
                // too, dropping anything buildPieces already emitted (a duplicate def
                // is itself a compile error).
                let have = fds |> List.map (fun f -> f.Name) |> Set.ofList
                let extraMarshallers =
                  Bridge.marshalFnDefs defsByName (Bridge.marshalCallsInExpr marshalled)
                  |> List.filter (fun d -> not (Set.contains d.Name have))
                match compileClosure typeDefs (fds @ extraMarshallers) marshalled with
                | Error e -> return "cf|compile: " + e
                | Ok binary ->
                  // Run the interpreter with the SAME bound the compiled binary got.
                  // Previously this was a bare `.Result` — UNBOUNDED — so the per-fn
                  // timeout only ever fenced the compiled side. An exponential fn
                  // (fib 35 interpreted) ran for minutes here, TWICE (the nondet
                  // check), defeating the whole per-fn budget: this was the "79
                  // interpreter timeouts" that stalled the proof sweeps. Task.Wait(ms)
                  // gives a real deadline; a miss is reported as `itimeout`, distinct
                  // from a genuine interpreter error (`ierr`), so it's an honest
                  // "couldn't prove in time", not a false DIFF and not a crash.
                  let runInterp () : Result<string, string> =
                    try
                      let argsNE =
                        match args with
                        | [] -> NEList.singleton DUnit
                        | _ -> NEList.ofList args.Head args.Tail
                      let task =
                        LibExecution.Execution.executeFunction
                          exeState (FQFnName.fqPackage hash) [] argsNE
                      if task.Wait(timeoutMs) then
                        match task.Result with
                        // Materialize PERSISTENT blobs before wiring: dvalToWire is
                        // pure and only encodes Ephemeral, so a package blob in the
                        // result printed "?unmarshalable:DBlob" and read as a false
                        // DIFF (the compiled side was right).
                        | Ok v -> Ok(dvalToWire (Ply.toTask (materializeBlobs v)).Result)
                        | Error(_, e) -> Error(string e)
                      else Error("__timeout__")
                    with e -> Error("exn: " + e.Message)
                  // Run the interpreter TWICE **before** executing the compiled binary.
                  // For a STATE-MUTATING fn the order is load-bearing: Posix.symlink run
                  // by the binary FIRST creates the link, then both interpreter runs see
                  // the post-state (EEXIST) and AGREE, so the nondet check misses it and
                  // it's reported as a false DIFF (c=Ok vs i=Error 17). Running the two
                  // interpreter passes first, symlink gives Ok then EEXIST — they DISAGREE
                  // -> `nondet` -> honestly skipped, and the binary never runs to muddy the
                  // filesystem. (A pure fn's two runs still agree, so nothing changes for
                  // the provable set.)
                  let interp = runInterp ()
                  let interp2 = runInterp ()
                  match interp, interp2 with
                  | Error "__timeout__", _ | _, Error "__timeout__" -> return "itimeout"
                  | Error e, _ -> return "ierr|" + e
                  | Ok i, Ok i2 when i <> i2 -> return "nondet"
                  | Ok i, _ ->
                    let (daemon, shutdown) = startDaemon exeState vm
                    let out = CompilerLibrary.execute 0 timeoutMs binary
                    shutdown ()
                    let _ = (try daemon.Wait() with _ -> ())
                    if out.ExitCode <> 0 then
                      return $"crash|{out.ExitCode}"
                    else
                      // Strip exactly ONE trailing newline -- the one Print appends --
                      // not all of them. TrimEnd('\n') ate newlines belonging to the
                      // VALUE and reported two false DIFFs (`/// hello` vs `/// hello\n`)
                      // on strings that legitimately end in one.
                      let raw = out.Stdout
                      let compiled =
                        if raw.EndsWith "\n" then raw.Substring(0, raw.Length - 1) else raw
                      if compiled = i then return "match"
                      else
                        let esc (s : string) = s.Replace("\n", "\\n")
                        return $"DIFF|c={esc compiled}|i={esc i}"
    with e -> return "harness-exn|" + e.Message
  }

let private runOne
  (exeState : ExecutionState)
  (vm : VMState)
  (effectful : Map<string, Bridge.WireArg list * Bridge.WireRet>)
  (timeoutMs : int)
  (hash : string)
  : Ply<string> =
  uply {
    try
      let! pieces = buildPieces effectful hash
      match pieces with
      | Error e -> return "cf|" + e
      | Ok(typeDefs, fds, rootName) ->
        match fds |> List.tryFind (fun fd -> fd.Name = rootName) with
        | None -> return "cf|root not bridged"
        | Some rootFd ->
          let defsByName =
            typeDefs
            |> List.choose (fun td ->
              match td with
              | AST.RecordDef(n, _, _) -> Some(n, td)
              | AST.SumTypeDef(n, _, _) -> Some(n, td)
              | AST.TypeAlias(n, _, _) -> Some(n, td))
            |> Map.ofList
          let dummy = rootParamTypes rootFd |> List.map (zeroValue defsByName Set.empty)
          match List.tryPick (function Error e -> Some e | Ok _ -> None) dummy with
          | Some e -> return "cf|arg-synth: " + e
          | None ->
            let args = dummy |> List.choose (function Ok x -> Some x | Error _ -> None)
            match compileClosure typeDefs fds (mainCall rootFd args) with
            | Error e -> return "cf|" + e
            | Ok binary ->
              let (daemon, shutdown) = startDaemon exeState vm
              let out =
                try CompilerLibrary.execute 0 timeoutMs binary
                with e -> { ExitCode = -1; Stdout = ""; Stderr = e.Message; RuntimeTime = System.TimeSpan.Zero }
              shutdown ()
              let _ = (try daemon.Wait() with _ -> ())
              if out.ExitCode = -999 then return "hang"
              elif out.ExitCode <> 0 then return $"crash|{out.ExitCode}"
              else
                let compiledOut = out.Stdout.Trim()
                // Differential check: run the SAME fn in the interpreter with the
                // same zero args and compare. Restricted to non-generic fns whose
                // params are scalars (so the Dval zeros exactly match the compiled
                // zeroValue and the printed formats align) — otherwise just "ran".
                let! ptFnOpt = LibDB.PackageManager.pt.getFn (PT.FQFnName.package hash)
                match ptFnOpt with
                | Some ptFn when List.isEmpty ptFn.typeParams ->
                  let paramList = NEList.toList ptFn.parameters
                  // Fetch the param types' closure so record/enum params can be
                  // zeroed the same way the compiled zeroValue does.
                  let typeSeeds =
                    paramList
                    |> List.collect (fun (p : PT.PackageFn.Parameter) -> Bridge.typeRefsInType p.typ)
                    |> List.distinct
                  let! typeClosure = fetchTypeClosure Set.empty [] typeSeeds
                  let declMap =
                    match typeClosure with
                    | Ok ts -> ts |> List.map (fun (h, pt) -> (h, pt.declaration)) |> Map.ofList
                    | Error _ -> Map.empty
                  // Interpreter-side zero, mirroring zeroValue. Conservative: None
                  // (skip the diff) for generics/aliases/functions/dicts, so a
                  // synthesis mismatch can never masquerade as a miscompile.
                  let rec zeroDvalPT (seen : Set<string>) (t : PT.TypeReference) : Dval option =
                    match t with
                    | PT.TInt64 | PT.TInt -> Some(DInt64 0L)
                    | PT.TString -> Some(DString "")
                    | PT.TBool -> Some(DBool false)
                    | PT.TChar -> Some(DChar "a")
                    | PT.TUnit -> Some DUnit
                    | PT.TFloat -> Some(DFloat 0.0)
                    | PT.TList _ -> Some(DList(ValueType.Unknown, []))
                    | PT.TTuple(a, b, rest) ->
                      let zs = (a :: b :: rest) |> List.map (zeroDvalPT seen)
                      if List.exists Option.isNone zs then None
                      else
                        match zs |> List.choose (fun x -> x) with
                        | x1 :: x2 :: r -> Some(DTuple(x1, x2, r))
                        | _ -> None
                    | PT.TCustomType(nr, _) ->
                      match nr.resolved with
                      | Ok resolved ->
                        match resolved.name with
                        | PT.FQTypeName.Package(PT.Hash h) ->
                          let tn = FQTypeName.fqPackage h
                          if h = Bridge.optionTypeHash then
                            Some(DEnum(tn, tn, [ ValueType.Unknown ], "None", []))
                          elif Set.contains h seen then None
                          else
                            match Map.tryFind h declMap with
                            | Some(decl : PT.TypeDeclaration.T) when List.isEmpty decl.typeParams ->
                              match decl.definition with
                              | PT.TypeDeclaration.Record fields ->
                                let fzs =
                                  NEList.toList fields
                                  |> List.map (fun (f : PT.TypeDeclaration.RecordField) ->
                                    (f.name, zeroDvalPT (Set.add h seen) f.typ))
                                if fzs |> List.exists (snd >> Option.isNone) then None
                                else
                                  Some(
                                    DRecord(
                                      tn, tn, [],
                                      fzs |> List.map (fun (n, v) -> (n, Option.get v)) |> Map.ofList))
                              | PT.TypeDeclaration.Enum cases ->
                                NEList.toList cases
                                |> List.tryPick (fun (c : PT.TypeDeclaration.EnumCase) ->
                                  let pzs =
                                    c.fields
                                    |> List.map (fun (ef : PT.TypeDeclaration.EnumField) ->
                                      zeroDvalPT (Set.add h seen) ef.typ)
                                  if pzs |> List.exists Option.isNone then None
                                  else Some(DEnum(tn, tn, [], c.name, pzs |> List.choose (fun x -> x))))
                              | PT.TypeDeclaration.Alias _ -> None
                            | _ -> None
                      | Error _ -> None
                    | _ -> None
                  let zeros : List<Dval option> =
                    paramList |> List.map (fun (p : PT.PackageFn.Parameter) -> zeroDvalPT Set.empty p.typ)
                  if List.exists Option.isNone zeros then
                    return "ran|" + compiledOut.Replace("\n", "\\n")
                  else
                    let dvalArgs : List<Dval> = zeros |> List.choose (fun x -> x)
                    let argsNE =
                      match dvalArgs with
                      | [] -> NEList.singleton DUnit
                      | args -> NEList.ofListUnsafe "runOne diff args" [] args
                    let runInterp () =
                      try
                        match (LibExecution.Execution.executeFunction
                                 exeState (FQFnName.fqPackage hash) [] argsNE).Result with
                        | Ok v -> Some v
                        | Error _ -> None
                      with _ -> None
                    // Repr matching the compiler's own print format so outputs can be
                    // compared: strings raw at top level but quoted inside a list,
                    // list as [a, b], Unit as "". None => not comparable (skip).
                    let rec reprNested (d : Dval) : string option =
                      match d with
                      | DString s -> Some("\"" + s + "\"")
                      | _ -> scalarRepr d
                    and scalarRepr (d : Dval) : string option =
                      match d with
                      | DInt64 n -> Some(string n)
                      | DInt n -> Some(string (DarkInt.toBigInt n))
                      | DString s -> Some s
                      | DBool b -> Some(if b then "true" else "false")
                      | DFloat f -> Some(string f)
                      | DChar c -> Some c
                      | DUnit -> Some ""
                      | DList(_, xs) ->
                        let parts = xs |> List.map reprNested
                        if List.exists Option.isNone parts then None
                        else
                          let strs = parts |> List.choose (fun (x : string option) -> x)
                          Some("[" + String.concat ", " strs + "]")
                      | _ -> None
                    let iv1, iv2 = runInterp (), runInterp ()
                    match iv1, iv2 with
                    | Some a, Some b when (scalarRepr a) <> (scalarRepr b) ->
                      // interpreter itself gave two answers -> effectful/non-deterministic; don't compare
                      return "nondet|" + compiledOut.Replace("\n", "\\n")
                    | None, _ | _, None -> return "ierr|" + compiledOut.Replace("\n", "\\n")
                    | Some iv, _ ->
                      match scalarRepr iv with
                      | None -> return "ran|" + compiledOut.Replace("\n", "\\n")
                      | Some istr ->
                        let norm (s : string) =
                          let t = (s.Trim().Trim('"')).Replace("\n", "\\n")
                          if t = "()" || t = "[]" then "" else t
                        let ca, ia = norm compiledOut, norm istr
                        // numeric equality handles the compiler's "0.0" vs the interp's "0"
                        let numEq =
                          match System.Double.TryParse ca, System.Double.TryParse ia with
                          | (true, x), (true, y) -> x = y
                          | _ -> false
                        if ca = ia || numEq then return "match|" + ia
                        else return $"diff|c={ca}|i={ia}"
                | _ -> return "ran|" + compiledOut.Replace("\n", "\\n")
    with e -> return "cf|exn: " + e.Message
  }

/// Benchmark one Int64->Int64 package fn at arg n: compile it (bridge -> native
/// binary), run compiled vs interpreted (in-process), and return a pretty report.
let private perfBench (exeState : ExecutionState) (hash : string) (n : int64) : Ply<string> =
  uply {
    let effectful = buildEffectfulMap exeState
    let! pieces = buildPieces effectful hash
    match pieces with
    | Error e -> return "  ✗ does not compile: " + e
    | Ok(typeDefs, fds, rootName) ->
      let program : AST.Program =
        AST.Program(
          (typeDefs |> List.map AST.TypeDef)
          @ (fds |> List.map AST.FunctionDef)
          @ [ AST.Expression(AST.Call(rootName, AST.NonEmptyList.singleton (AST.Int64Literal n))) ])
      match compileAst program with
      | Error e -> return "  ✗ compile error: " + e
      | Ok binary ->
        let _warm = CompilerLibrary.execute 0 120000 binary
        let swC = System.Diagnostics.Stopwatch.StartNew()
        let outC = CompilerLibrary.execute 0 120000 binary
        swC.Stop()
        let args = NEList.singleton (DInt64 n)
        let swI = System.Diagnostics.Stopwatch.StartNew()
        let interp =
          try
            match (LibExecution.Execution.executeFunction exeState (FQFnName.fqPackage hash) [] args).Result with
            | Ok v -> dvalToWire v
            | Error _ -> "<interp error>"
          with e -> "<interp exn: " + e.Message + ">"
        swI.Stop()
        let cOut = outC.Stdout.Trim()
        let cMs = swC.Elapsed.TotalMilliseconds
        let iMs = swI.Elapsed.TotalMilliseconds
        let agree = if cOut = interp then "✓ match" else "✗ DIFFER"
        let speedup = if cMs > 0.0 then System.Math.Round(iMs / cMs, 0) else 0.0
        let bytes = binary.Length
        return
          "  arg           n = " + string n + "\n"
          + "  result        " + cOut + "   (compiled vs interpreted: " + agree + ")\n"
          + "  native size   " + string bytes + " bytes\n"
          + "  interpreted   " + string (System.Math.Round(iMs, 2)) + " ms\n"
          + "  compiled      " + string (System.Math.Round(cMs, 2)) + " ms  (incl. process launch)\n"
          + "  speedup       " + string speedup + "x  faster (compiled)"
  }

let fns () : List<BuiltInFn> =
  [ { name = fn "compilerInfo" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Reports that the native compiler extension is linked and available."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let opts = CompilerLibrary.defaultOptions
          DString
            $"native compiler linked (DisableFreeList={opts.DisableFreeList}, DisableTCO={opts.DisableTCO})"
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerCompile" 0
      typeParams = []
      parameters = [ Param.make "source" TString "compiler-syntax (interpreter-dialect) Dark source" ]
      returnType = TTuple(TBool, TString, [])
      description =
        "Compiles <source> to a native binary in-process via the airlifted compiler. Returns (true, \"<n> bytes\") on success or (false, <error>) on failure. Does not execute the binary."
      fn =
        (function
        | _, _, _, [ DString source ] ->
          match compileSource source with
          | Ok binary -> outcome true $"{binary.Length} bytes" |> Ply
          | Error e -> outcome false e |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerCompileAndRun" 0
      typeParams = []
      parameters = [ Param.make "source" TString "compiler-syntax (interpreter-dialect) Dark source" ]
      returnType = TTuple(TBool, TString, [])
      description =
        "Compiles <source> to a native binary and executes it in-process. Returns (true, <stdout>) on success or (false, <error>) on failure. NOTE: runs native code; x86-64 refcounting is disabled, so long/allocation-heavy programs may leak (fine for short pure-core leaves)."
      fn =
        (function
        | _, _, _, [ DString source ] ->
          match compileSource source with
          | Error e -> outcome false $"compile: {e}" |> Ply
          | Ok binary ->
            let out = CompilerLibrary.execute 0 10000 binary
            if out.ExitCode = 0 then outcome true out.Stdout |> Ply
            else outcome false $"exit {out.ExitCode}: {out.Stderr}" |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerDaemonSelfTest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Proves the runtime seam dispatches to a REAL builtin: compiles a program calling Stdlib.hostRpc(\"stringLength\\nhello\"); an in-process F# daemon reads the request, invokes the real stringLength builtin (DString hello -> DInt64 5), writes the response; the native binary reads it back. Returns the binary's stdout (expect 5)."
      fn =
        (function
        | exeState, vm, _, [ DUnit ] ->
          uply {
            let daemon = serveOneRequest exeState vm
            let program : AST.Program =
              AST.Program
                [ AST.Expression(
                    AST.Call(
                      "Stdlib.hostRpc",
                      AST.NonEmptyList.singleton (AST.StringLiteral "stringLength\nhello"))) ]
            match compileAst program with
            | Error e ->
              daemon.Wait()
              return DString $"compile-error: {e}"
            | Ok binary ->
              let out = CompilerLibrary.execute 0 10000 binary
              daemon.Wait()
              return DString $"binary stdout={out.Stdout.Trim()} (expected 5)"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerMarshalSelfTest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Proves the general container marshaller round-trips at runtime: compiles a program that calls the REAL environmentGet builtin through the daemon (returns Option<String>), decodes the wire response via Bridge.unmarshalTyped over Option<String>, and Option.withDefault-extracts it. Returns the binary stdout (expect the value of $HOME)."
      fn =
        (function
        | exeState, vm, _, [ DUnit ] ->
          uply {
            let daemon = serveOneRequest exeState vm
            // Option<String>: environmentGet "HOME" through the daemon -> decode -> withDefault
            let optStrTy = AST.TSum("Stdlib.Option.Option", [ AST.TString ])
            let rpcCall =
              AST.Call(
                "Stdlib.hostRpc",
                AST.NonEmptyList.singleton (AST.StringLiteral "environmentGet\nHOME"))
            match Bridge.unmarshalTypedR 0 optStrTy rpcCall with
            | Error e ->
              daemon.Wait()
              return DString $"decode-error: {e}"
            | Ok decoded ->
            let program : AST.Program =
              AST.Program
                [ AST.Expression(
                    AST.TypeApp(
                      "Stdlib.Option.withDefault",
                      [ AST.TString ],
                      AST.NonEmptyList.fromList [ decoded; AST.StringLiteral "NONE" ])) ]
            match compileAst program with
            | Error e ->
              daemon.Wait()
              return DString $"compile-error: {e}"
            | Ok binary ->
              let out = CompilerLibrary.execute 0 10000 binary
              daemon.Wait()
              return DString(out.Stdout.Trim())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerBridgeSelfTest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TTuple(TBool, TString, [])
      description =
        "Proves the ProgramTypes->compiler-AST bridge (§6) end to end: builds a PT function `(a,b) => a + b` in F#, lowers it via Bridge.bridgeFn to compiler AST, synthesizes `bridgedFn(20, 22)`, compiles that AST directly (no text/parser), runs it, and returns (true, \"42\") on success. This is the durable path the text builtins will be replaced by."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          // A pure-Int leaf, constructed as ProgramTypes: (a: Int64) (b: Int64): Int64 = a + b
          let ptFn : PT.PackageFn.PackageFn =
            { hash = PT.FQFnName.package "compiler-merge-selftest-add"
              body =
                PT.EInfix(
                  gid (),
                  PT.InfixFnCall PT.ArithmeticPlus,
                  PT.EArg(gid (), 0),
                  PT.EArg(gid (), 1))
              typeParams = []
              parameters =
                NEList.ofList
                  ({ name = "a"; typ = PT.TInt64; description = "" } : PT.PackageFn.Parameter)
                  [ { name = "b"; typ = PT.TInt64; description = "" } ]
              returnType = PT.TInt64
              description = "" }
          match Bridge.bridgeFn Map.empty Map.empty Set.empty Map.empty Map.empty "bridgedFn" ptFn with
          | Error e -> outcome false $"bridge: {e}" |> Ply
          | Ok fd ->
            let program : AST.Program =
              AST.Program
                [ AST.FunctionDef fd
                  AST.Expression(
                    AST.Call(
                      "bridgedFn",
                      AST.NonEmptyList.fromList [ AST.Int64Literal 20L; AST.Int64Literal 22L ])) ]
            match compileAst program with
            | Error e -> outcome false $"compile: {e}" |> Ply
            | Ok binary ->
              let out = CompilerLibrary.execute 0 10000 binary
              if out.ExitCode = 0 then outcome true out.Stdout |> Ply
              else outcome false $"exit {out.ExitCode}: {out.Stderr}" |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerHandleSelfTest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Proves the opaque-handle model end to end: compiles a program that (1) calls the REAL stringSplit builtin through the daemon (returns a List -> stored as a handle id), then (2) passes that handle to the REAL listLength builtin. The compiled code only ever touches strings/ints (no native finger-tree), yet a list flows between two builtins via the daemon. Returns stdout (expect 3)."
      fn =
        (function
        | exeState, vm, _, [ DUnit ] ->
          uply {
            let (daemon, shutdown) = startDaemon exeState vm
            // let h = hostRpc("stringSplit\na,b,c\n,") in hostRpc("listLength\n" ++ h)
            let rpc req = AST.Call("Stdlib.hostRpc", AST.NonEmptyList.singleton req)
            let program : AST.Program =
              AST.Program
                [ AST.Expression(
                    AST.Let(
                      "h",
                      rpc (AST.StringLiteral "stringSplit\na,b,c\n,"),
                      rpc (AST.BinOp(AST.StringConcat, AST.StringLiteral "listLength\n", AST.Var "h")))) ]
            match compileAst program with
            | Error e ->
              shutdown ()
              daemon.Wait()
              return DString $"compile-error: {e}"
            | Ok binary ->
              let out = CompilerLibrary.execute 0 10000 binary
              shutdown ()
              daemon.Wait()
              return DString(out.Stdout.Trim())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerListHandleSelfTest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Proves the compiled side can BUILD and query a multi-element list via handles (the native x64 finger-tree cannot): conses [a,b,c] through the daemon (__listEmpty/__listConsStr) then __listLen -> 3. Every value the compiled code holds is a String/Int handle."
      fn =
        (function
        | exeState, vm, _, [ DUnit ] ->
          uply {
            let (daemon, shutdown) = startDaemon exeState vm
            let rpc req = AST.Call("Stdlib.hostRpc", AST.NonEmptyList.singleton req)
            let cat a b = AST.BinOp(AST.StringConcat, a, b)
            let consReq hVar elem =
              cat (cat (AST.StringLiteral "__listConsStr\n") (AST.Var hVar)) (AST.StringLiteral ("\n" + elem))
            let program : AST.Program =
              AST.Program
                [ AST.Expression(
                    AST.Let("h0", rpc (AST.StringLiteral "__listEmpty"),
                      AST.Let("h1", rpc (consReq "h0" "c"),
                        AST.Let("h2", rpc (consReq "h1" "b"),
                          AST.Let("h3", rpc (consReq "h2" "a"),
                            rpc (cat (AST.StringLiteral "__listLen\n") (AST.Var "h3"))))))) ]
            match compileAst program with
            | Error e ->
              shutdown ()
              daemon.Wait()
              return DString $"compile-error: {e}"
            | Ok binary ->
              let out = CompilerLibrary.execute 0 10000 binary
              shutdown ()
              daemon.Wait()
              return DString(out.Stdout.Trim())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compileFnByHash" 0
      typeParams = []
      parameters =
        [ Param.make "hash" TString "content hash of a package function"
          Param.make "args" (TList TInt64) "Int64 arguments to call the function with" ]
      returnType = TTuple(TBool, TString, [])
      description =
        "Fetches a package function's ProgramTypes by hash, lowers it to compiler AST via the §6 bridge, compiles a call to it with <args>, runs the native binary, and returns (true, <stdout>) or (false, <reason>). Pure-core leaves only; anything the bridge can't lower hard-fails with an `unsupported-*` reason. (Dark resolves a name to its hash; this takes the hash.)"
      fn =
        (function
        | exeState, vm, _, [ DString hash; DList(_, argDvals) ] ->
          uply {
            let effectful = buildEffectfulMap exeState
            let argLits =
              argDvals
              |> List.choose (fun d ->
                match d with
                | DInt64 n -> Some(AST.Int64Literal n)
                | _ -> None)
            if List.length argLits <> List.length argDvals then
              return outcome false "only Int64 args are supported"
            elif List.isEmpty argLits then
              return outcome false "need >= 1 arg (nullary calls not yet supported)"
            else
              let! pieces = buildPieces effectful hash
              match pieces with
              | Error e -> return outcome false e
              | Ok(typeDefs, fds, rootName) ->
                match fds |> List.tryFind (fun fd -> fd.Name = rootName) with
                | None -> return outcome false "root fn not bridged"
                | Some rootFd ->
                match compileClosure typeDefs fds (mainCall rootFd argLits) with
                | Error e -> return outcome false $"compile: {e}"
                | Ok binary ->
                  // Start the host-RPC daemon so any effectful builtins the fn
                  // calls are serviced by the real runtime, then run the binary.
                  let (daemon, shutdown) = startDaemon exeState vm
                  let out = CompilerLibrary.execute 0 10000 binary
                  shutdown ()
                  daemon.Wait()
                  return
                    (if out.ExitCode = 0 then outcome true out.Stdout
                     else outcome false $"exit {out.ExitCode}: {out.Stderr}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compileFnCheck" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "content hash of a package function" ]
      returnType = TTuple(TBool, TString, [])
      description =
        "Checks whether a package function compiles via the §6 bridge, WITHOUT running it: fetches its ProgramTypes by hash, lowers to compiler AST, makes it reachable with zero-valued args, and compiles. Returns (true, \"<n> bytes\") if it compiles or (false, \"<unsupported-*>\") with the first blocker. This is the `dark <fn> compile` semantic (compilability, not execution)."
      fn =
        (function
        | exeState, _, _, [ DString hash ] ->
          uply {
            let! (ok, detail) = checkOne (buildEffectfulMap exeState) hash
            return outcome ok detail
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerCoverageSweep" 0
      typeParams = []
      parameters =
        [ Param.make "hashes" (TList TString) "package fn content hashes to compile-check" ]
      returnType = TString
      description =
        "The §4 bring-up loop, in ONE process (stdlib built once): compile-checks every fn hash and returns a newline-joined report, one `<ok>|<detail>` line per input hash in order (ok = true/false). Feed it every fn hash to get a whole-tree coverage number + ranked blockers without the per-process stdlib rebuild."
      fn =
        (function
        | exeState, _, _, [ DList(_, hashes) ] ->
          uply {
            let effectful = buildEffectfulMap exeState
            let mutable lines : List<string> = []
            for hd in hashes do
              match hd with
              | DString hash ->
                let! (ok, detail) = checkOne effectful hash
                lines <- lines @ [ $"{ok}|{detail}" ]
              | _ -> lines <- lines @ [ "false|non-string-hash" ]
            return DString(String.concat "\n" lines)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerNativeListTest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Debug: does a native multi-element list [7,8,9] match head work at runtime (no daemon)? Returns 7, or hangs/crashes if the finger-tree is broken."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            // Decode a wire list "alpha\\nbeta\\ngamma" via unmarshalTyped(List<String>), no daemon -> length 3
            match Bridge.unmarshalTypedR 0 (AST.TList AST.TString) (AST.StringLiteral "alpha\nbeta\ngamma") with
            | Error e -> return DString ("decode: " + e)
            | Ok decoded ->
            let len = AST.TypeApp("Stdlib.List.length", [ AST.TString ], AST.NonEmptyList.singleton decoded)
            let program = AST.Program [ AST.Expression(AST.Call("Stdlib.Int64.toString", AST.NonEmptyList.singleton len)) ]
            match compileAst program with
            | Error e -> return DString ("compile: " + e)
            | Ok binary ->
              let out = CompilerLibrary.execute 0 5000 binary
              return DString (if out.ExitCode = -999 then "HANG" elif out.ExitCode <> 0 then $"crash {out.ExitCode}" else out.Stdout.Trim())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerShowFn" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "package fn content hash" ]
      returnType = TString
      description = "Debug: fetch a package fn by hash and return its name + F# structural repr of params and body."
      fn =
        (function
        | _, _, _, [ DString hash ] ->
          uply {
            let! fnOpt = LibDB.PackageManager.pt.getFn (PT.FQFnName.package hash)
            match fnOpt with
            | None -> return DString "not found"
            | Some(f : PT.PackageFn.PackageFn) ->
              let ps =
                NEList.toList f.parameters
                |> List.map (fun (p : PT.PackageFn.Parameter) -> $"{p.name}: {p.typ}")
                |> String.concat ", "
              return DString $"ret={f.returnType}\ntypeParams={f.typeParams}\nparams=({ps})\nbody={f.body}"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerPerfCompare" 0
      typeParams = []
      parameters =
        [ Param.make "hash" TString "package fn hash (Int64 -> Int64)"
          Param.make "n" TInt64 "the Int64 argument" ]
      returnType = TString
      description = "Benchmarks one Int64->Int64 fn at arg n: runs it compiled (native binary) and interpreted (executeFunction), times each with a Stopwatch, and reports both wall times + the result. For a fair compute comparison use a heavy fn (e.g. naive fib)."
      fn =
        (function
        | exeState, _, _, [ DString hash; DInt64 n ] ->
          uply {
            let! report = perfBench exeState hash n
            return DString report
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "perfCompareEval" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs "fn" (TFn(NEList.singleton TInt64, TInt64)) "an Int64 -> Int64 fn" [ "n" ]
          Param.make "n" TInt64 "the Int64 argument" ]
      returnType = TString
      description = "The user-facing perf comparison: pass a fn value (e.g. Stdlib.PerfDemo.fib) and an arg; compiles it, runs compiled-native vs interpreted in-process, and returns a pretty report with both times, the speedup, and whether the results match."
      fn =
        (function
        | exeState, _, _, [ DApplicable(AppNamedFn nf); DInt64 n ] ->
          uply {
            match nf.name with
            | FQFnName.Package(Hash h) ->
              let! report = perfBench exeState h n
              return DString ("\n" + report + "\n")
            | _ -> return DString "perfCompareEval: not a package fn"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerEquivCheck" 0
      typeParams = []
      parameters =
        [ Param.make "hash" TString "package fn content hash"
          Param.make "args" (TVariable "a") "the fn's arguments: a tuple for 2+, the bare value for 1, () for none" ]
      returnType = TString
      description =
        "PROVES a compiled fn agrees with the interpreter on real arguments. Both sides emit the same canonical wire encoding (the interpreter's Dval via dvalToWire; the compiled program via the mirror, Bridge.marshalTyped) and the bytes are compared — unlike the stdout comparison, which can't see records/nested containers and left most fns unprovable. Args are passed as a tuple (2+), a bare value (1), or () for none, since a Dark list can't hold mixed types. Returns match / DIFF|c=..|i=.. / unprovable|<why> / cf|<blocker> / ierr|<err> / crash|<code>."
      fn =
        (function
        | exeState, vm, _, [ DString hash; argsDval ] ->
          uply {
            let args =
              match argsDval with
              | DUnit -> [] // nullary
              | DTuple(a, b, rest) -> a :: b :: rest // 2+ args
              | single -> [ single ] // exactly 1
            let! r = equivOne exeState vm (buildEffectfulMap exeState) 10000 hash args
            return DString r
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerEquivSweep" 0
      typeParams = []
      parameters = [ Param.make "hashes" (TList TString) "package fn content hashes" ]
      returnType = TString
      description =
        "Equivalence sweep: for each fn, synthesize REALISTIC args from its declared param types (42, \"hello\", [1;2;3] — not the zeros the compile-check uses, which push fns down degenerate paths), then prove compiled == interpreted by comparing the canonical wire encoding both sides emit. One `<hash>\\t<result>` line each: match / DIFF|c=..|i=.. / unprovable|<why> / noargs|<type> / cf|<blocker> / ierr / crash. This is the number that matters — compile coverage is not correctness."
      fn =
        (function
        | exeState, vm, _, [ DList(_, hashes) ] ->
          uply {
            let effectful = buildEffectfulMap exeState
            let mutable lines : List<string> = []
            for hd in hashes do
              match hd with
              | DString hash ->
                let! fnOpt = LibDB.PackageManager.pt.getFn (PT.FQFnName.package hash)
                match fnOpt with
                | None -> lines <- lines @ [ hash + "\tcf|not found" ]
                | Some f ->
                  let ps = NEList.toList f.parameters
                  // A single Unit param is Dark's nullary; call with no args.
                  let! sampled =
                    uply {
                      match ps with
                      | [ p ] when p.typ = PT.TUnit -> return Some []
                      | _ ->
                        let mutable acc = []
                        let mutable ok = true
                        for (p : PT.PackageFn.Parameter) in ps do
                          let! v = sampleArgDeep Set.empty p.typ
                          match v with
                          | Some v' -> acc <- acc @ [ v' ]
                          | None -> ok <- false
                        return (if ok then Some acc else None)
                    }
                  match sampled with
                  | None ->
                    let ts = ps |> List.map (fun p -> string p.typ) |> String.concat ", "
                    lines <- lines @ [ hash + "\tnoargs|" + ts ]
                  | Some args ->
                    let! r = equivOne exeState vm effectful 10000 hash args
                    lines <- lines @ [ hash + "\t" + r ]
              | _ -> lines <- lines @ [ "?\tcf|non-string-hash" ]
            return DString(String.concat "\n" lines)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "compilerRunSweep" 0
      typeParams = []
      parameters =
        [ Param.make "hashes" (TList TString) "package fn content hashes to compile AND run" ]
      returnType = TString
      description =
        "Runnable-vs-compilable sweep: for each fn hash, compiles it, makes it reachable with zero args, and RUNS the binary (daemon up) with a 2s timeout. One line per hash: cf|<blocker> (didn't compile), hang (timed out — e.g. the multi-element list bug), crash|<code>, or ran|<stdout>. Turns the compile-coverage number into an actually-executes number and surfaces runtime hangs as failures."
      fn =
        (function
        | exeState, vm, _, [ DList(_, hashes) ] ->
          uply {
            let effectful = buildEffectfulMap exeState
            let mutable lines : List<string> = []
            for hd in hashes do
              match hd with
              | DString hash ->
                let! r = runOne exeState vm effectful 2000 hash
                lines <- lines @ [ hash + "\t" + r ]
              | _ -> lines <- lines @ [ "?\tcf|non-string-hash" ]
            return DString(String.concat "\n" lines)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
