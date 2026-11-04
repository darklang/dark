/// Typed reads over the SCM tables, for the Dark SCM code.
///
/// Each builtin takes a query enum and answers one family of questions, so Dark asks for data and
/// never writes SQL against the store. The decisions (scope, merge, propagation) stay in Dark; the
/// queries are in `LibDB`, one module per family (`LibDB.ScmDeps`, ...).
module Builtins.Matter.Libs.PM.Scm

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Deps = LibDB.ScmDeps


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
  let name k o m n : Deps.Name = { kind = k; owner = o; modules = m; name = n }
  match q with
  | DEnum(_, _, _, "Of", [ DString h ]) -> Deps.Of h
  | DEnum(_, _, _, "EdgesOf", [ DString h ]) -> Deps.EdgesOf h
  | DEnum(_, _, _, "EdgesOfAll", [ DList(_, hs) ]) -> Deps.EdgesOfAll(strings hs)
  | DEnum(_, _, _, "NamesOf", [ DString h ]) -> Deps.NamesOf h
  | DEnum(_, _, _, "NamesOfFqn", [ DString fqn ]) -> Deps.NamesOfFqn fqn
  | DEnum(_, _, _, "Closure", [ DList(_, hs) ]) -> Deps.Closure(strings hs)
  | DEnum(_, _, _, "Dependents", [ DString k; DString o; DString m; DString n ]) ->
    Deps.Dependents(name k o m n)
  | DEnum(_, _, _, "DependentHashes", [ DString k; DString o; DString m; DString n ]) ->
    Deps.DependentHashes(name k o m n)
  | DEnum(_,
          _,
          _,
          "RenamedAway",
          [ DString h; DString k; DString o; DString m; DString n ]) ->
    Deps.RenamedAway(h, name k o m n)
  | DEnum(_, _, _, "Referenced", [ DString h ]) -> Deps.Referenced h
  | DEnum(_, _, _, "Outdated", []) -> Deps.Outdated
  | DEnum(_, _, _, "StaleBySeed", [ DList(_, seeds) ]) ->
    seeds
    |> List.map (function
      | DTuple(DString o, DString m, [ DString n; DString h ]) -> (o, m, n, h)
      | other -> Exception.raiseInternal "expected a seed tuple" [ "got", other ])
    |> Deps.StaleBySeed
  | other -> Exception.raiseInternal "unknown DepQuery" [ "got", other ]

let private depEdgeToDT (e : Deps.Edge) : Dval =
  let tn = depEdgeTypeName ()
  DRecord(
    tn,
    tn,
    [],
    Map
      [ "itemHash", DString e.itemHash
        "itemType", DString e.itemType
        "owner", DString e.owner
        "modules", DString e.modules
        "name", DString e.name
        "dependsOnHash", DString e.dependsOnHash
        "dependsOnType", DString e.dependsOnType
        "dependsOnOwner", DString e.dependsOnOwner
        "dependsOnModules", DString e.dependsOnModules
        "dependsOnName", DString e.dependsOnName
        "currentHash", DString e.currentHash
        "currentType", DString e.currentType
        "seed", DInt64 e.seed ]
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
        "Reads the recorded dependency edges (who references whom) for <param query>. "
        + "The cases that name a dependent answer about main; the branch overlay is the "
        + "caller's to apply."
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
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
