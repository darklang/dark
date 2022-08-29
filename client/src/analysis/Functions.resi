@ppx.deriving(show({with_path: false}))
type rec t = {
  builtinFunctions: list<RuntimeTypes.BuiltInFn.t>,
  packageFunctions: TLID.Dict.t<ProgramTypes.Package.Fn.t>,
  // We do analysis to determine which functions are safe to preview and which are
  // not. This stores the result
  previewUnsafeFunctions: Tc.Set.String.t,
  allowedFunctions: list<Function.t>,
}

@ppx.deriving(show({with_path: false}))
type rec props = {
  usedFns: Tc.Map.String.t<int>,
  userFunctions: TLID.Dict.t<ProgramTypes.UserFunction.t>,
}

/* Returns the function named `name`. Returns Nothing if the function
 * can't be found - this shouldn't happen in theory but often does
 * in practice; for example, someone might delete a function and
 * then do a local undo. */
let find: (FQFnName.t, t) => option<Function.t>

let findByStr: (string, t) => option<Function.t>

/* For legacy reasons, we stash a global reference to the result of the latest
 * update. This was only to be used by OldExpr, but it snuck in elsewhere. DO
 * NOT USE! */
let global: unit => t

let empty: t

let asFunctions: t => list<Function.t>

let builtins: t => list<Function.t>

let setBuiltins: (list<RuntimeTypes.BuiltInFn.t>, props, t) => t

let setPackages: (Types.packageFns, props, t) => t

/* Update the cached function data; should be called anytime the functions
 * change. */
let update: (props, t) => t

/* For testing */
let testCalculateUnsafeUserFunctions: (props, t) => Tc.Set.String.t
