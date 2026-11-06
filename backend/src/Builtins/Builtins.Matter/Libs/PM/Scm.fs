/// Typed reads over the SCM tables, for the Dark SCM code.
///
/// Each builtin takes a query enum and answers one family of questions, so Dark asks for data and
/// never writes SQL against the store. The queries are in `LibDB`, one module per family
/// (`LibDB.ScmDeps`, `LibDB.ScmBindings`, `LibDB.ScmBranches`, `LibDB.ScmCommits`,
/// `LibDB.ScmConflicts`, `LibDB.ScmOps`). Writes are whole actions, in `ScmActions`.
module Builtins.Matter.Libs.PM.Scm

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Deps = LibDB.ScmDeps
module Bindings = LibDB.ScmBindings
module Branches = LibDB.ScmBranches
module Commits = LibDB.ScmCommits
module PT = LibExecution.ProgramTypes
module Conflicts = LibDB.ScmConflicts
module Ops = LibDB.ScmOps
module PT2DT = LibExecution.ProgramTypesToDarkTypes


let private depQueryTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.depQuery ())

let private depEdgeTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.depEdge ())

let private strings (items : List<Dval>) : List<string> =
  items
  |> List.map (function
    | DString s -> s
    | other -> Exception.raiseInternal "expected a String" [ "got", other ])

let private depQueryFromDT (q : Dval) : Deps.Query =
  match q with
  | DEnum(_, _, _, "Of", [ DString h ]) -> Deps.Of h
  | DEnum(_, _, _, "Closure", [ DList(_, hs) ]) -> Deps.Closure(strings hs)
  | DEnum(_, _, _, "Referenced", [ DString h ]) -> Deps.Referenced h
  | other -> Exception.raiseInternal "unknown DepQuery" [ "got", other ]

let private depEdgeToDT (e : Deps.Edge) : Dval =
  let tn = depEdgeTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "itemHash", DString e.itemHash
        "dependsOnHash", DString e.dependsOnHash
        "dependsOnType", DString e.dependsOnType ]
  )


let private bindingFilterTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.bindingFilter ())

let private bindingRowTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.bindingRow ())

let private bindingName (o : Dval) (m : Dval) (n : Dval) : Bindings.Name =
  match o, m, n with
  | DString o, DString m, DString n -> { owner = o; modules = m; name = n }
  | _ -> Exception.raiseInternal "expected a name" [ "got", DTuple(o, m, [ n ]) ]

let private bindingNames (items : List<Dval>) : List<Bindings.Name> =
  items
  |> List.map (function
    | DTuple(o, m, [ n ]) -> bindingName o m n
    | other -> Exception.raiseInternal "expected a name tuple" [ "got", other ])

let private bindingFilterFromDT (f : Dval) : Bindings.Query =
  let br g = PT.BranchId.Id g
  match f with
  | DEnum(_, _, _, "LocationOf", [ DUuid g; DString h ]) ->
    Bindings.LocationOf(br g, h)
  | DEnum(_, _, _, "ByBareNames", [ DUuid g; DList(_, ns) ]) ->
    Bindings.ByBareNames(br g, strings ns)
  | DEnum(_, _, _, "At", [ DUuid g; DList(_, ns) ]) ->
    Bindings.At(br g, bindingNames ns)
  | DEnum(_, _, _, "CommittedAt", [ DUuid g; DList(_, ns) ]) ->
    Bindings.CommittedAt(br g, bindingNames ns)
  | DEnum(_, _, _, "Propagated", [ DUuid g ]) -> Bindings.Propagated(br g)
  | DEnum(_, _, _, "AuthoringOrder", [ DUuid g; o; m; n ]) ->
    Bindings.AuthoringOrder(br g, bindingName o m n)
  | DEnum(_, _, _, "EverBound", [ DUuid g; DList(_, keys) ]) ->
    let keys =
      keys
      |> List.map (function
        | DTuple(o, m, [ n; DString k ]) -> (bindingName o m n, k)
        | other ->
          Exception.raiseInternal "expected a (name, kind) tuple" [ "got", other ])
    Bindings.EverBound(br g, keys)
  | DEnum(_, _, _, "ByHash", [ DUuid g; DString h; DString k ]) ->
    Bindings.ByHash(br g, h, k)
  | DEnum(_, _, _, "ByHashes", [ DUuid g; DList(_, hs) ]) ->
    Bindings.ByHashes(br g, strings hs)
  | DEnum(_, _, _, "Own", [ DUuid g ]) -> Bindings.Own(br g)
  | DEnum(_, _, _, "BranchHistory", [ DUuid g ]) -> Bindings.BranchHistory(br g)
  | DEnum(_, _, _, "AboveBases", [ DUuid g ]) -> Bindings.AboveBases(br g)
  | other -> Exception.raiseInternal "unknown BindingFilter" [ "got", other ]

let private bindingRowToDT (b : Bindings.Binding) : Dval =
  let tn = bindingRowTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "owner", DString b.owner
        "modules", DString b.modules
        "name", DString b.name
        "itemType", DString b.itemType
        "hash", DString b.hash
        "originTs", DString b.originTs
        "previous", Dval.option KTString (Option.map DString b.previous) ]
  )

let private branchFilterTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.branchFilter ())

let private branchInfoTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.branchInfo ())

let private nameBaseTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.nameBase ())

let private branchFilterFromDT (f : Dval) : Branches.Query =
  match f with
  | DEnum(_, _, _, "All", []) -> Branches.All
  | DEnum(_, _, _, "ById", [ DUuid id ]) -> Branches.ById(string id)
  | DEnum(_, _, _, "ByName", [ DString name ]) -> Branches.ByName name
  | DEnum(_, _, _, "ChildrenOf", [ DUuid id ]) -> Branches.ChildrenOf(string id)
  | DEnum(_, _, _, "NameBasesOf", [ DUuid id ]) -> Branches.NameBasesOf(string id)
  | other -> Exception.raiseInternal "unknown BranchFilter" [ "got", other ]

let private nameBaseToDT (b : Branches.NameBase) : Dval =
  let tn = nameBaseTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "owner", DString b.owner
        "modules", DString b.modules
        "name", DString b.name
        "baseHash", DString b.baseHash ]
  )

let private branchInfoToDT (i : Branches.Info) : Dval =
  let tn = branchInfoTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "id", DString i.id
        "name", DString i.name
        "parentId", DString i.parentId
        "merged", DBool i.merged
        "ops", DInt64 i.ops
        "nameBases",
        i.nameBases
        |> List.map nameBaseToDT
        |> Dval.list (KTCustomType(nameBaseTypeName (), [])) ]
  )

let private commitFilterTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.commitFilter ())

let private commitPageTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.commitPage ())

let private commitTypeName () = FQTypeName.fqPackage (PackageRefs.Type.SCM.commit ())

let private commitFilterFromDT (f : Dval) : Commits.Filter =
  match f with
  | DEnum(_, _, _, "Recent", []) -> Commits.Recent
  | DEnum(_, _, _, "OnBranch", [ DUuid b ]) -> Commits.OnBranch(PT.BranchId.Id b)
  | DEnum(_, _, _, "HashPrefix", [ DString p ]) -> Commits.HashPrefix p
  | DEnum(_, _, _, "ChildrenOf", [ DString h ]) -> Commits.ChildrenOf h
  | other -> Exception.raiseInternal "unknown CommitFilter" [ "got", other ]

let private commitToDT (c : Commits.Commit) : Dval =
  let tn = commitTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "hash", DString c.hash
        "message", DString c.message
        "originTs", DString c.originTs
        "createdAt", DString c.createdAt
        "parent", DString c.parent
        "housekeeping", DBool c.housekeeping ]
  )

let private commitPageToDT (p : Commits.Page) : Dval =
  let tn = commitPageTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "total", DInt64 p.total
        "commits",
        p.commits
        |> List.map commitToDT
        |> Dval.list (KTCustomType(commitTypeName (), [])) ]
  )

let private conflictFilterTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.conflictFilter ())

let private conflictRowTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.conflictRow ())

let private conflictFilterFromDT (f : Dval) : Conflicts.Query =
  match f with
  | DEnum(_, _, _, "Pending", [ DUuid b ]) -> Conflicts.Pending b
  | DEnum(_, _, _, "ByRef", [ DString r; DUuid b ]) -> Conflicts.ByRef(r, b)
  | DEnum(_, _, _, "AckedConstraints", []) -> Conflicts.AckedConstraints
  | DEnum(_, _, _, "AnyOverridden", []) -> Conflicts.AnyOverridden
  | other -> Exception.raiseInternal "unknown ConflictFilter" [ "got", other ]

let private conflictRowToDT (r : Conflicts.Row) : Dval =
  let tn = conflictRowTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "id", DString r.id
        "owner", DString r.owner
        "modules", DString r.modules
        "name", DString r.name
        "itemType", DString r.itemType
        "part", DString r.part
        "kind", DString r.kind
        "candidates", DString r.candidates
        "autoResolvedTo", DString r.autoResolvedTo
        "reason", DString r.reason
        "status", DString r.status
        "resolvedBy", DString r.resolvedBy ]
  )

let private opFilterTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.SCM.opFilter ())

let private opPageTypeName () = FQTypeName.fqPackage (PackageRefs.Type.SCM.opPage ())

let private opRowTypeName () = FQTypeName.fqPackage (PackageRefs.Type.SCM.opRow ())

let private packageOpKT () =
  KTCustomType(
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.ProgramTypes.packageOp ()),
    []
  )

let private opFilterFromDT (f : Dval) : Ops.Filter =
  let b g = PT.BranchId.Id g
  match f with
  | DEnum(_, _, _, "Recent", []) -> Ops.Recent
  | DEnum(_, _, _, "IdPrefix", [ DString p ]) -> Ops.IdPrefix p
  | DEnum(_, _, _, "InCommit", [ DString h ]) -> Ops.InCommit h
  | DEnum(_, _, _, "Visible", [ DUuid g ]) -> Ops.Visible(b g)
  | DEnum(_, _, _, "LandedSince", [ DUuid g; DString since; DBool andAtSince ]) ->
    Ops.LandedSince(b g, since, andAtSince)
  | DEnum(_, _, _, "LandedAt", [ DString at ]) -> Ops.LandedAt at
  | DEnum(_, _, _, "Ids", [ DList(_, ids) ]) -> Ops.Ids(strings ids)
  | DEnum(_, _, _, "Draft", [ DUuid g ]) -> Ops.Draft(b g)
  | DEnum(_, _, _, "DraftIds", [ DUuid g ]) -> Ops.DraftIds(b g)
  | DEnum(_, _, _, "Own", [ DUuid g ]) -> Ops.Own(b g)
  | DEnum(_, _, _, "MergedFrom", [ DUuid g ]) -> Ops.MergedFrom(b g)
  | DEnum(_, _, _, "Main", []) -> Ops.Main
  | DEnum(_, _, _, "NewestLanded", []) -> Ops.NewestLanded
  | DEnum(_, _, _, "StoreNow", []) -> Ops.StoreNow
  | DEnum(_, _, _, "NextStamp", []) -> Ops.NextStamp
  | other -> Exception.raiseInternal "unknown OpFilter" [ "got", other ]

let private opRowToDT (r : Ops.Row) : Dval =
  let tn = opRowTypeName ()
  let op =
    match r.op with
    | Some op -> Dval.optionSome (packageOpKT ()) (PT2DT.PackageOp.toDT op)
    | None -> Dval.optionNone (packageOpKT ())
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "id", DString r.id
        "ts", DString r.ts
        "sizeBytes", DInt64 r.sizeBytes
        "source", DString r.source
        "op", op ]
  )

let private opPageToDT (p : Ops.Page) : Dval =
  let tn = opPageTypeName ()
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "total", DInt64 p.total
        "ops",
        p.ops |> List.map opRowToDT |> Dval.list (KTCustomType(opRowTypeName (), [])) ]
  )

let fns () : List<BuiltInFn> =
  [ { name = fn "scmDeps" 0
      typeParams = []
      parameters =
        [ Param.make
            "query"
            (TCustomType(NR.ok (depQueryTypeName ()), []))
            "what to ask of the recorded dependency edges" ]
      returnType = TList(TCustomType(NR.ok (depEdgeTypeName ()), []))
      description =
        "Reads the recorded dependency edges (who references whom) for <param query>, "
        + "by hash."
      fn =
        (function
        | _, _, _, [| q |] ->
          uply {
            let! edges = Deps.query (depQueryFromDT q)
            return
              edges
              |> List.map depEdgeToDT
              |> Dval.list (KTCustomType(depEdgeTypeName (), []))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "scmBindings" 0
      typeParams = []
      parameters =
        [ Param.make
            "filter"
            (TCustomType(NR.ok (bindingFilterTypeName ()), []))
            "which name-to-hash bindings to read" ]
      returnType = TList(TCustomType(NR.ok (bindingRowTypeName ()), []))
      description =
        "Reads the name-to-hash bindings recorded in the store for <param filter>. Every case "
        + "answers about main; the branch chain is the caller's to lay over it."
      fn =
        (function
        | _, _, _, [| f |] ->
          uply {
            let! rows = Bindings.query (bindingFilterFromDT f)
            return
              rows
              |> List.map bindingRowToDT
              |> Dval.list (KTCustomType(bindingRowTypeName (), []))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "scmBranches" 0
      typeParams = []
      parameters =
        [ Param.make
            "filter"
            (TCustomType(NR.ok (branchFilterTypeName ()), []))
            "which branches to read" ]
      returnType = TList(TCustomType(NR.ok (branchInfoTypeName ()), []))
      description =
        "Reads the registered branches <param filter> asks for, or one branch's recorded name "
        + "bases. Main has no row, so it is never in the answer; the caller special-cases it."
      fn =
        (function
        | _, _, _, [| f |] ->
          uply {
            let! rows = Branches.query (branchFilterFromDT f)
            return
              rows
              |> List.map branchInfoToDT
              |> Dval.list (KTCustomType(branchInfoTypeName (), []))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }
    { name = fn "scmCommits" 0
      typeParams = []
      parameters =
        [ Param.make
            "filter"
            (TCustomType(NR.ok (commitFilterTypeName ()), []))
            "which commits"
          Param.make
            "limit"
            TInt64
            "at most this many; negative for all of them, 0 for only the count" ]
      returnType = TCustomType(NR.ok (commitPageTypeName ()), [])
      description =
        "Reads the commits <param filter> selects, at most <param limit> of them. With a "
        + "<param limit> of 0 it counts them instead, and the page holds no commits."
      fn =
        (function
        | _, _, _, [| f; DInt64 limit |] ->
          uply {
            let! page = Commits.query (commitFilterFromDT f) limit
            return commitPageToDT page
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "scmConflicts" 0
      typeParams = []
      parameters =
        [ Param.make
            "filter"
            (TCustomType(NR.ok (conflictFilterTypeName ()), []))
            "which recorded conflicts to read" ]
      returnType = TList(TCustomType(NR.ok (conflictRowTypeName ()), []))
      description =
        "Reads the recorded conflicts (and constraint acks) that <param filter> selects. "
        + "Candidates come back as the JSON the store holds."
      fn =
        (function
        | _, _, _, [| f |] ->
          uply {
            let! rows = Conflicts.query (conflictFilterFromDT f)
            return
              rows
              |> List.map conflictRowToDT
              |> Dval.list (KTCustomType(conflictRowTypeName (), []))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }
    { name = fn "scmOps" 0
      typeParams = []
      parameters =
        [ Param.make
            "filter"
            (TCustomType(NR.ok (opFilterTypeName ()), []))
            "which ops"
          Param.make
            "limit"
            TInt64
            "at most this many; negative for all of them, 0 for only the count" ]
      returnType = TCustomType(NR.ok (opPageTypeName ()), [])
      description =
        "Reads the ops <param filter> selects from the op log, at most <param limit> of "
        + "them, each with its op decoded (None when this build cannot read it). With a "
        + "<param limit> of 0 it counts them instead, and the page holds no ops."
      fn =
        (function
        | _, _, _, [| f; DInt64 limit |] ->
          uply {
            let! page = Ops.query (opFilterFromDT f) limit
            return opPageToDT page
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
