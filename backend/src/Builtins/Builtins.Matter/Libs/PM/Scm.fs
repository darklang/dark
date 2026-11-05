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
    Map
      [ "itemHash", DString e.itemHash
        "dependsOnHash", DString e.dependsOnHash
        "dependsOnType", DString e.dependsOnType ]
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
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
