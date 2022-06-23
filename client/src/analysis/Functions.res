open Prelude
module B = BlankOr
module E = FluidExpression

@ppx.deriving(show({with_path: false})) type rec t = Types.functionsType

@ppx.deriving(show({with_path: false})) type rec props = Types.functionsProps

/* Returns the function named `name`. Returns Nothing if the function
 * can't be found - this shouldn't happen in theory but often does
 * in practice; for example, someone might delete a function and
 * then do a local undo. */
let find = (name: string, functions: t): option<function_> =>
  List.find(functions.allowedFunctions, ~f=f => f.fnName == name)

let empty: t = {
  builtinFunctions: list{},
  packageFunctions: TLIDDict.empty,
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
  /* Construct a dependency tree (which is a reverse callgraph) so that we get
   * from a callee to a caller */
  let dependencyTree =
    props.userFunctions
    |> Map.mapValues(~f=uf =>
      uf.ufMetadata.ufmName
      |> B.toOption
      |> Option.map(~f=caller =>
        E.filterMap(FluidAST.toExpr(uf.ufAST), ~f=x =>
          switch x {
          | EBinOp(_, callee, _, _, _) => Some(callee, caller)
          | EFnCall(_, callee, _, _) => Some(callee, caller)
          | _ => None
          }
        )
      )
      |> Option.unwrap(~default=list{})
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
  let unsafeBuiltins = t.builtinFunctions |> List.filterMap(~f=f =>
    if f.fnPreviewSafety == Unsafe {
      Some(f.fnName)
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

let asFunctions = (t: t): list<function_> => t.allowedFunctions

let builtins = (t: t): list<function_> => t.builtinFunctions

let calculateAllowedFunctionsList = (props: props, t: t): list<function_> => {
  // We hide functions that are deprecated unless they are in use
  let filterAndSort = (fns: list<function_>): list<function_> => {
    let isUsedOrIsNotDeprecated = (f: function_): bool =>
      if f.fnDeprecated {
        Map.get(~key=f.fnName, props.usedFns) |> Option.unwrap(~default=0) |> (count => count > 0)
      } else {
        true
      }

    let fnNameWithoutVersion = (f: function_): string =>
      f.fnName
      |> String.toLowercase
      |> String.split(~on="_v")
      |> List.getAt(~index=0)
      |> Option.unwrap(~default=f.fnName)

    fns
    |> List.filter(~f=isUsedOrIsNotDeprecated)
    |> List.sortBy(~f=f =>
      /* don't call List.head here - if we have DB::getAll_v1 and
       * DB::getAll_v2, we want those to sort accordingly! */
      f.fnName |> String.toLowercase |> String.split(~on="_v")
    )
    |> List.groupWhile(~f=(f1, f2) => fnNameWithoutVersion(f1) == fnNameWithoutVersion(f2))
    |> List.map(~f=List.reverse)
    |> List.flatten
  }

  let userFunctionMetadata =
    props.userFunctions
    |> Map.mapValues(~f=x => x.ufMetadata)
    |> List.filterMap(~f=UserFunctions.ufmToF)
    |> List.map(~f=f => {
      ...f,
      fnPreviewSafety: if Set.member(t.previewUnsafeFunctions, ~value=f.fnName) {
        Unsafe
      } else {
        Safe
      },
    })

  let packageFunctions =
    t.packageFunctions |> Map.values |> List.map(~f=PackageManager.fn_of_packageFn)

  Belt.List.concatMany([
    t.builtinFunctions,
    userFunctionMetadata,
    packageFunctions,
  ]) |> filterAndSort
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

let setBuiltins = (builtins: list<function_>, props: props, t: t): t =>
  {...t, builtinFunctions: builtins} |> update(props)

let setPackages = (fns: packageFns, props: props, t: t): t =>
  {...t, packageFunctions: fns} |> update(props)
