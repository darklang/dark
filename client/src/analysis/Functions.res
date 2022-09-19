open Prelude
module E = FluidExpression

@ppx.deriving(show({with_path: false}))
type rec t = {
  builtinFunctions: list<RT.BuiltInFn.t>,
  packageFunctions: packageFns,
  // We do analysis to determine which functions are safe and which are not.
  // This stores the result
  previewUnsafeFunctions: Set.String.t,
  allowedFunctions: list<Function.t>,
}

@ppx.deriving(show({with_path: false}))
type rec props = {
  usedFns: Map.String.t<int>,
  userFunctions: TLID.Dict.t<PT.UserFunction.t>,
  allowTuples: bool,
}

/* Returns the function named `name`. Returns Nothing if the function
 * can't be found - this shouldn't happen in theory but often does
 * in practice; for example, someone might delete a function and
 * then do a local undo. */
let find = (name: FQFnName.t, functions: t): option<Function.t> =>
  List.find(functions.allowedFunctions, ~f=f => f.fnName == name)

let findByStr = (name: string, functions: t): option<Function.t> =>
  List.find(functions.allowedFunctions, ~f=f => FQFnName.toString(f.fnName) == name)

let empty: t = {
  builtinFunctions: list{},
  packageFunctions: TLID.Dict.empty,
  allowedFunctions: list{},
  previewUnsafeFunctions: Set.String.empty,
}

let globalRef = ref(empty)

let global = () => globalRef.contents

/* Return a set containing names of previewUnsafe user functions.
 *
 * A userfunction is unsafe if it calls an unsafe function (and so is also
 * unsafe if any of its callees (the functions it calls) itself calls an unsafe
 * function. The functions that we know are unsafe are the builtin functions
 * that are marked as unsafe.
 *
 * You can think of this as a callgraph. The bottom of the callgraph are
 * functions that dont call any more functions (and thus are safe) or are
 * builtins (and thus have a flag to tell us if they are safe).
 *
 * So to figure out the complete set of unsafe userfunctions, we start at the
 * bottom of the callgraph, from the unsafe builtins. Then we go up the tree,
 * and mark any callers of those functions as unsafe. And that's the whole
 * algorithm. */
let calculateUnsafeUserFunctions = (props: props, t: t): Set.String.t => {
  // Construct a dependency tree (which is a reverse callgraph) so that we get
  // from a callee to a caller
  let dependencyTree =
    props.userFunctions
    |> Map.mapValues(~f=(uf: PT.UserFunction.t) =>
      if uf.name != "" {
        E.filterMap(FluidAST.toExpr(uf.body), ~f=x =>
          switch x {
          | EBinOp(_, callee, _, _, _) => Some(PT.InfixStdlibFnName.toString(callee), uf.name)
          | EFnCall(_, callee, _, _) => Some(FQFnName.toString(callee), uf.name)
          | _ => None
          }
        )
      } else {
        list{}
      }
    )
    |> List.flatten
    |> List.fold(~initial=Map.String.empty, ~f=(dict, (callee, caller)) =>
      Map.update(~key=callee, dict, ~f=x =>
        switch x {
        | Some(callers) => Some(list{caller, ...callers})
        | None => Some(list{caller})
        }
      )
    )

  // Get the initial set of unsafe functions
  // TODO: what about unsafe packagemanager functions
  let unsafeBuiltins = t.builtinFunctions |> List.filterMap(~f=(f: RT.BuiltInFn.t) =>
    if f.previewable != Pure {
      Some(f.name |> FQFnName.StdlibFnName.toString)
    } else {
      None
    }
  )

  let worklist = ref(unsafeBuiltins)
  // The result set
  let unsafeFns = ref(Set.String.empty)
  /* The worklist algorithm:
   *
   * Go through worklist of unsafe functions, starting with known-unsafe builtins:
   * - mark this function unsafe
   * - add the callers to the worklist
   */
  while worklist.contents != list{} {
    switch worklist.contents {
    | list{callee, ...rest} =>
      // already processed
      if Set.member(~value=callee, unsafeFns.contents) {
        worklist := rest
      } else {
        unsafeFns := Set.add(~value=callee, unsafeFns.contents)
        // add callers to be processed
        let callers = Map.get(~key=callee, dependencyTree) |> Option.unwrap(~default=list{})

        worklist := Belt.List.concat(rest, callers)
      }
    | _ => ()
    }
  }
  Set.removeMany(~values=unsafeBuiltins, unsafeFns.contents)
}

let testCalculateUnsafeUserFunctions = calculateUnsafeUserFunctions

let asFunctions = (t: t): list<Function.t> => t.allowedFunctions

let builtins = (t: t): list<Function.t> => t.builtinFunctions |> List.map(~f=Function.fromBuiltinFn)

let calculateAllowedFunctionsList = (props: props, t: t): list<Function.t> => {
  // We hide functions that are deprecated unless they are in use
  let filterAndSort = (fns: list<Function.t>): list<Function.t> => {
    let isUsedOrIsNotDeprecated = (f: Function.t): bool =>
      if f.fnDeprecated {
        Map.get(~key=FQFnName.toString(f.fnName), props.usedFns)
        |> Option.unwrap(~default=0)
        |> (count => count > 0)
      } else {
        true
      }

    let isExperimentalAndOptedIn = (f: Function.t): bool =>
      // TUPLETODO remove this filter when the experimental setting is removed
      switch f.fnName {
      | Stdlib(fnName) if String.startsWith(~prefix="Tuple", fnName.module_) => props.allowTuples
      | _ => false
      }

    let fnNameWithoutVersion = (f: Function.t): string =>
      f.fnName
      |> FQFnName.toString
      |> String.toLowercase
      |> String.split(~on="_v")
      |> List.getAt(~index=0)
      |> Option.unwrap(~default=FQFnName.toString(f.fnName))

    fns
    |> List.filter(~f=(f: Function.t) =>
      isUsedOrIsNotDeprecated(f) || (!f.fnDeprecated && isExperimentalAndOptedIn(f))
    )
    |> List.sortBy(~f=(f: Function.t) =>
      // don't call List.head here - if we have DB::getAll_v1 and
      // DB::getAll_v2, we want those to sort accordingly!
      f.fnName |> FQFnName.toString |> String.toLowercase |> String.split(~on="_v")
    )
    |> List.groupWhile(~f=(f1, f2) => fnNameWithoutVersion(f1) == fnNameWithoutVersion(f2))
    |> List.map(~f=List.reverse)
    |> List.flatten
  }

  let userFunctionMetadata =
    props.userFunctions
    |> Map.values
    |> List.filterMap(~f=Function.fromUserFn)
    |> List.map(~f=(f: Function.t) => {
      ...f,
      fnPreviewSafety: if Set.member(t.previewUnsafeFunctions, ~value=FQFnName.toString(f.fnName)) {
        Unsafe
      } else {
        Safe
      },
    })

  let packageFunctions = t.packageFunctions |> Map.values |> List.map(~f=Function.fromPkgFn)

  Belt.List.concatMany([builtins(t), userFunctionMetadata, packageFunctions]) |> filterAndSort
}

let update = (props: props, t: t): t => {
  let allowedFunctions = calculateAllowedFunctionsList(props, t)
  let previewUnsafeFunctions = calculateUnsafeUserFunctions(props, t)
  let result = {
    ...t,
    allowedFunctions: allowedFunctions,
    previewUnsafeFunctions: previewUnsafeFunctions,
  }
  // ugh
  globalRef := result
  result
}

let setBuiltins = (builtins: list<RT.BuiltInFn.t>, props: props, t: t): t =>
  {...t, builtinFunctions: builtins} |> update(props)

let setPackages = (fns: packageFns, props: props, t: t): t =>
  {...t, packageFunctions: fns} |> update(props)
