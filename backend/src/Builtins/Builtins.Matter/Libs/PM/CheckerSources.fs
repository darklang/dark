/// The F# source files the at-rest checker runs, out of the assemblies a reload's cache leaves out
/// of its build hash (`TraitCalls.outsideTheChecker`). A reload keys its cache on their content
/// (`HostProcess.sourcesHash`), so an edit to one of them misses and an edit to any other file
/// in those assemblies keeps the answers.
module Builtins.Matter.Libs.PM.CheckerSources

open System.Reflection
open System.Reflection.Emit

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTAst = LibExecution.ProgramTypesAst
module PackageRefs = LibExecution.PackageRefs
module NumericTraits = LibExecution.NumericTraits

/// What running a body can run next.
type private Reach =
  | Fn of PT.Hash
  | Value of PT.Hash
  | Builtin of RT.FQFnName.Builtin
  /// A trait method left to the run, which can be any implementation of it.
  | Method of trait_ : PT.Hash * method_ : string
  /// `x.name` where `x` has no field `name`: the method of that name, of any trait.
  | Receiver of string

/// What <param expr> names, and the trait methods it leaves to the run.
let rec private reachIn (expr : PT.Expr) : List<Reach> =
  let choice trait_ method_ (c : PT.FQFnName.ImplChoice) =
    match c with
    | PT.FQFnName.Chosen r -> Fn r.name
    | PT.FQFnName.FromTypeParam _
    | PT.FQFnName.Unknown -> Method(trait_, method_)
  let named
    (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
    (bounds : List<PT.FQFnName.BoundImpl>)
    =
    let own =
      match nr.resolved with
      // As it runs, so `-x` is the `Negate` method rather than the builtin it is stored as.
      | Ok r ->
        match PT2RT.FQFnName.toRT r.name with
        | RT.FQFnName.Builtin b -> [ Builtin b ]
        | RT.FQFnName.Package(RT.Hash h) -> [ Fn(PT.Hash h) ]
        | RT.FQFnName.TraitMethod tm ->
          match tm.implFn with
          | RT.FQFnName.Chosen(RT.Hash h) -> [ Fn(PT.Hash h) ]
          | _ ->
            let (RT.Hash t) = tm.trait_
            [ Method(PT.Hash t, tm.method_) ]
      | Error _ -> []
    own @ (bounds |> List.map (fun b -> choice b.trait_ b.method_ b.choice))
  let infix (i : PT.Infix) (implFn : PT.FQFnName.ImplChoice) =
    match i with
    | PT.BinOp _ -> []
    | PT.InfixFnCall name ->
      match NumericTraits.ofInfix name with
      | Some(t, method_) -> [ choice (PT.Hash t) method_ implFn ]
      | None -> [ Builtin { name = PT.InfixFnName.toBuiltinName name; version = 0 } ]
  let own =
    match expr with
    | PT.EFnName(_, nr, bounds, _) -> named nr bounds
    | PT.EInfix(_, i, _, _, implFn) -> infix i implFn
    | PT.EValue(_, { resolved = Ok { name = PT.FQValueName.Package h } }) ->
      [ Value h ]
    | PT.ERecordFieldAccess(_, _, field) -> [ Receiver field ]
    | PT.EPipe(_, _, parts) ->
      parts
      |> List.collect (fun part ->
        match part with
        | PT.EPipeFnCall(_, nr, _, _, bounds) -> named nr bounds
        | PT.EPipeInfix(_, i, _, implFn) -> infix i implFn
        | _ -> [])
    | _ -> []
  own @ (PTAst.subExprs expr |> List.collect reachIn)

/// The Dark that builtins run of their own accord, which no body names.
///
/// One is left out: a check that fails inside `atRestCheckGuarded` comes back as its message,
/// which the pretty printer writes (`Execution.applyCatching`). The message goes into a warning,
/// and the cache keeps resolutions only, so nothing the printer does reaches an answer.
let private runByBuiltin (b : RT.FQFnName.Builtin) : List<Reach> =
  match b.name with
  // A list of a type with `Compare` sorts by its `lessThan`.
  | "listSort" ->
    [ Method(PT.Hash(PackageRefs.Trait.Stdlib.Traits.compare ()), "lessThan") ]
  | _ -> []

/// Every builtin fn the checker can call: what its entry point names, and what that names, down
/// to builtins. A trait method left to the run counts every implementation of it. Builtin values
/// are not followed: the key holds each one whole (`TraitCalls.builtinSignatures`).
let private builtinsReached
  (exeState : RT.ExecutionState)
  (pm : PT.PackageManager)
  : Ply<Set<RT.FQFnName.Builtin>> =
  let implsOf (candidates : List<RT.ImplCandidate>) (method_ : string) =
    candidates
    |> List.choose (fun c -> Map.tryFind method_ c.methods)
    |> List.map (fun (RT.Hash h) -> Fn(PT.Hash h))
  let next (r : Reach) : Ply<List<Reach>> =
    uply {
      match r with
      | Fn h ->
        let! fn = pm.getFn h
        return fn |> Option.map (fun fn -> reachIn fn.body) |> Option.defaultValue []
      | Value h ->
        let! value = pm.getValue h
        return
          value |> Option.map (fun v -> reachIn v.body) |> Option.defaultValue []
      | Builtin b -> return runByBuiltin b
      | Method(PT.Hash t, method_) ->
        let! candidates = exeState.fns.implCandidates exeState.branchId (RT.Hash t)
        return implsOf candidates method_
      | Receiver method_ ->
        let! candidates =
          exeState.fns.implCandidatesByMethod exeState.branchId method_
        return implsOf candidates method_
    }
  // A level at a time, so the recursion is as deep as the call graph, not as wide.
  let rec walk (seen : Set<Reach>) (level : List<Reach>) : Ply<Set<Reach>> =
    uply {
      let fresh =
        level |> List.distinct |> List.filter (fun r -> not (seen.Contains r))
      if List.isEmpty fresh then
        return seen
      else
        let! after = Ply.List.mapSequentially next fresh
        return! walk (Set.union seen (Set.ofList fresh)) (List.concat after)
    }
  let checker = PackageRefs.Fn.LanguageTools.AtRestTypeChecker.checkPackageOpsOn ()
  walk Set.empty [ Fn(PT.Hash checker) ]
  |> Ply.map (
    Set.toList
    >> List.choose (function
      | Builtin b -> Some b
      | _ -> None)
    >> Set.ofList
  )


// Code to files.
// --------------------

/// The F# module a compiled type belongs to: its outermost type, or for a module's static
/// initializer (`<StartupCode$Builtins-Pure>.$Builtins.Pure.Libs.Dict`) the module it initializes.
let private moduleOf (t : System.Type) : string =
  let rec outermost (t : System.Type) =
    if isNull t.DeclaringType then t else outermost t.DeclaringType
  let t = outermost t
  if t.FullName.StartsWith "<StartupCode$" then
    t.FullName.Substring(t.FullName.IndexOf ".$" + 2)
  else
    t.FullName

/// One-byte opcodes by their value, two-byte ones by 0xFE00 and their second byte.
let private opCodes : Map<int, OpCode> =
  typeof<OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.map (fun f -> f.GetValue null :?> OpCode)
  |> Array.map (fun op -> (int (uint16 op.Value), op))
  |> Map.ofArray

/// The types whose members <param m>'s IL calls, reads, writes or constructs.
let private typesUsedBy (m : MethodBase) : List<System.Type> =
  let body =
    try
      m.GetMethodBody()
    with _ ->
      null
  if isNull body then
    []
  else
    let il = body.GetILAsByteArray()
    let typeArgs =
      if m.DeclaringType.IsGenericType then
        m.DeclaringType.GetGenericArguments()
      else
        null
    let methodArgs = if m.IsGenericMethod then m.GetGenericArguments() else null
    // The type a module defines: `Foo<int>` is `Foo<_>`, `Foo[]` is `Foo`, and a type
    // parameter is in no module.
    let rec defined (t : System.Type) : Option<System.Type> =
      if isNull t || t.IsGenericParameter then None
      elif t.HasElementType then defined (t.GetElementType())
      elif t.IsGenericType then Some(t.GetGenericTypeDefinition())
      else Some t
    let resolve (token : int) : Option<System.Type> =
      try
        match m.Module.ResolveMember(token, typeArgs, methodArgs) with
        | :? System.Type as t -> defined t
        | other -> defined other.DeclaringType
      with _ ->
        None
    let rec go (at : int) (found : List<System.Type>) =
      if at >= il.Length then
        found
      else
        let code, at =
          if il[at] = 0xFEuy then
            (0xFE00 ||| int il[at + 1], at + 2)
          else
            (int il[at], at + 1)
        let op = opCodes[code]
        let size, found =
          match op.OperandType with
          | OperandType.InlineNone -> 0, found
          | OperandType.ShortInlineBrTarget
          | OperandType.ShortInlineI
          | OperandType.ShortInlineVar -> 1, found
          | OperandType.InlineVar -> 2, found
          | OperandType.InlineI8
          | OperandType.InlineR -> 8, found
          | OperandType.InlineSwitch ->
            4 + 4 * System.BitConverter.ToInt32(il, at), found
          | OperandType.InlineMethod
          | OperandType.InlineField
          | OperandType.InlineType
          | OperandType.InlineTok ->
            4,
            (match resolve (System.BitConverter.ToInt32(il, at)) with
             | Some t -> t :: found
             | None -> found)
          | _ -> 4, found
        go (at + size) found
    go 0 []

/// Where a module of <param assembly> is, in the repository:
/// `Builtins.Pure.Libs.Dict` is `backend/src/Builtins/Builtins.Pure/Libs/Dict.fs`.
let private fileOf (assembly : string) (moduleName : string) : string =
  let dir =
    if assembly = "TestUtils" then "backend/tests/TestUtils"
    elif assembly.StartsWith "Builtins." then $"backend/src/Builtins/{assembly}"
    else $"backend/src/{assembly}"
  let inAssembly =
    if moduleName.StartsWith(assembly + ".") then
      moduleName.Substring(assembly.Length + 1)
    else
      moduleName
  $"""{dir}/{inAssembly.Replace('.', '/')}.fs"""

/// The files whose code runs when <param builtins> do, out of <param assemblies>: each builtin's
/// own module, and every module of those assemblies that its compiled code uses, transitively.
///
/// Read off the IL, so two things are not seen: a function another module inlines at compile time
/// (none of these assemblies inline across files), and a module of <param assemblies> reached
/// from a builtin's module elsewhere only through a third module, elsewhere too. The one
/// assembly elsewhere that uses one of <param assemblies>, `Builtins.Language`, uses `LibParser`
/// from its builtins' own module (`Parser.fs`).
let private filesRunBy
  (assemblies : Set<string>)
  (builtins : List<RT.BuiltInFn>)
  : Set<string> =
  let assemblyOf (t : System.Type) = t.Assembly.GetName().Name
  let moduleKey (t : System.Type) = (assemblyOf t, moduleOf t)
  let starts = builtins |> List.map (fun b -> moduleKey (b.fn.GetType()))
  let scanned = Set.union assemblies (starts |> List.map fst |> Set.ofList)
  let typesOf =
    System.AppDomain.CurrentDomain.GetAssemblies()
    |> Array.filter (fun a -> scanned.Contains(a.GetName().Name))
    |> Array.collect (fun a ->
      try
        a.GetTypes()
      with :? ReflectionTypeLoadException as e ->
        e.Types |> Array.filter (not << isNull))
    |> List.ofArray
    |> List.groupBy moduleKey
  let flags =
    BindingFlags.Public
    ||| BindingFlags.NonPublic
    ||| BindingFlags.Static
    ||| BindingFlags.Instance
    ||| BindingFlags.DeclaredOnly
  let used (m : string * string) : List<string * string> =
    Map.tryFind m typesOf
    |> Option.defaultValue []
    |> List.collect (fun t ->
      Seq.append
        (t.GetMethods flags |> Seq.cast<MethodBase>)
        (t.GetConstructors flags |> Seq.cast<MethodBase>)
      |> Seq.collect typesUsedBy
      |> List.ofSeq)
    |> List.filter (fun t -> assemblies.Contains(assemblyOf t))
    |> List.map moduleKey
  let rec walk (seen : Set<string * string>) (level : List<string * string>) =
    let fresh =
      level |> List.distinct |> List.filter (fun m -> not (seen.Contains m))
    if List.isEmpty fresh then
      seen
    else
      walk (Set.union seen (Set.ofList fresh)) (List.collect used fresh)
  walk Set.empty starts
  |> Set.filter (fun (a, _) -> assemblies.Contains a)
  |> Set.map (fun (a, m) -> fileOf a m)


/// The source files, in the repository, of every builtin the at-rest checker can call and of the
/// code they call, out of <param assemblies>. Computed on each reload from the checker as it is
/// stored, so a change to the checker that reaches another builtin changes the set as well.
let checkerFiles
  (assemblies : Set<string>)
  (exeState : RT.ExecutionState)
  (pm : PT.PackageManager)
  : Ply<Set<string>> =
  uply {
    let! reached = builtinsReached exeState pm
    let fns =
      reached
      |> Set.toList
      |> List.choose (fun b ->
        match exeState.fns.builtIn.TryGetValue b with
        | true, fn -> Some fn
        | _ -> None)
    return filesRunBy assemblies fns
  }

/// Every builtin's file, out of <param assemblies>, for the test that each one is where
/// `checkerFiles` would look.
let allFiles
  (assemblies : Set<string>)
  (exeState : RT.ExecutionState)
  : Set<string> =
  exeState.fns.builtIn.Values |> List.ofSeq |> filesRunBy assemblies
